"""Auditoria de lectura. No modifica datos ni resultados originales.

Ejecutar desde cualquier directorio. Dependencias: pandas, pyarrow, numpy,
statsmodels. Las sensibilidades NO son una reconstruccion validada de Qualtrics.
"""
from pathlib import Path
import json
import re
import numpy as np
import pandas as pd
import statsmodels.api as sm

OUT = Path(__file__).resolve().parent
ROOT = OUT.parents[2]
code = (ROOT / '2. Code/0. main.R').read_text(encoding='utf-8-sig')
vectors = {
    name: set(re.findall(r'alumno_\d_\d', body))
    for name, body in re.findall(r'(vector_\w+)\s*<-\s*c\((.*?)\)', code, re.S)
}
names = dict(zip(vectors, ['male', 'background', 'failed', 'competence', 'absent', 'disruptive']))
design = pd.DataFrame([
    {'card': f'alumno_{i}_{j}', 'block': i,
     **{names[k]: int(f'alumno_{i}_{j}' in v) for k, v in vectors.items()}}
    for i in range(1, 9) for j in range(1, 9)
])
attrs = list(names.values())
design['duplicate_profile'] = design[attrs].duplicated(keep=False)
design.to_csv(OUT / 'inventario_tarjetas.csv', index=False)
d = pd.read_parquet(ROOT / '1. Data/processed data/cleandata.parquet')
raw = pd.read_parquet(ROOT / '1. Data/raw data/df_named.parquet')
cards = design.card.tolist()
used = [c for c in cards if c not in ['alumno_1_1', 'alumno_2_1']]
n = d[used].notna().sum(axis=1)
flags = d[[f'grupo{i}' for i in range(1, 9)]].notna()
favourite = d[['orden_pref_refuerzo', 'orden_pref_criterios_promo', 'orden_pref_formacion_prof']].eq(1).any(axis=1)
fl = [c for c in raw if re.search(r'FL_.*DO.*[Gg]rupo', c)]
audit = {
    'raw_records': len(raw), 'clean_records': len(d),
    'raw_start_min': str(raw.StartDate.min()), 'raw_start_max': str(raw.StartDate.max()),
    'clean_duplicate_ids': int(d.id.duplicated().sum()),
    'coded_card_ids': len(design), 'coded_unique_profiles': len(design[attrs].drop_duplicates()),
    'coded_attribute_totals': design[attrs].sum().to_dict(),
    'current_teacher_count': int((n > 0).sum()), 'current_decision_count': int(n.sum()),
    'current_decisions_per_teacher_distribution': n.value_counts().sort_index().to_dict(),
    'clean_records_with_treatment': int(d.treatment.notna().sum()),
    'current_teachers_with_favourite': int(((n > 0) & favourite).sum()),
    'records_with_any_group_flag': int(flags.any(axis=1).sum()),
    'multiple_group_flags': int((flags.sum(axis=1) > 1).sum()),
    'current_teachers_without_group_flag': int(((n > 0) & ~flags.any(axis=1)).sum()),
    'raw_card_order_columns': len(fl),
    'raw_records_with_card_order': int(raw[fl].notna().any(axis=1).sum()),
}
long = d[['id'] + cards].melt(id_vars='id', var_name='card', value_name='decision').dropna(subset='decision')
assert set(long.decision.unique()) <= {'pasa', 'repite'}
long['y'] = long.decision.eq('repite').astype(float)
long = long.merge(design, on='card', validate='many_to_one')
group = flags.apply(lambda row: next((i for i, v in enumerate(row, 1) if v), 0), axis=1)
long = long.merge(pd.DataFrame({'id': d.id, 'assigned_block': group}), on='id', validate='many_to_one')
current = long[long.card.isin(used)].copy()
group_only = long[long.block.eq(long.assigned_block)].copy()
count_group = group_only.groupby('id').size()
complete_group = group_only[group_only.id.isin(count_group[count_group.eq(8)].index)].copy()
audit['provisional_group_card_count'] = len(group_only)
audit['provisional_complete_group_teachers'] = complete_group.id.nunique()
audit['provisional_complete_group_cards'] = len(complete_group)
rows = []
for label, frame in [('current_code', current), ('provisional_assigned_block', group_only), ('provisional_complete_assigned_block', complete_group)]:
    demeaned = frame[['y'] + attrs] - frame.groupby('id')[['y'] + attrs].transform('mean')
    fit = sm.OLS(demeaned.y, demeaned[attrs]).fit(cov_type='cluster', cov_kwds={'groups': frame.id})
    for attr in attrs:
        rows.append({'specification': label, 'attribute': attr, 'estimate_pp': 100 * fit.params[attr],
                     'cluster_se_pp': 100 * fit.bse[attr], 'teachers': frame.id.nunique(), 'cards': len(frame)})
pd.DataFrame(rows).to_csv(OUT / 'sensibilidad_tarjetas.csv', index=False)
rf = pd.read_csv(ROOT / '3. Output/tables/paper_rewrite_rf_summary.csv').iloc[0]
audit['existing_rf_rmse_sd_ratio'] = float(rf.rmse_over_sd)
audit['approximate_existing_rf_oob_r2_from_ratio'] = float(1 - rf.rmse_over_sd ** 2)

# Descriptive check of post-task beliefs: not a mediation or causal analysis.
age = pd.to_numeric(d.edad, errors='coerce')
effort = pd.to_numeric(d.meritocracia, errors='coerce')
mask = age.notna() & effort.notna() & (n > 0)
audit['exploratory_age_effort_correlation'] = float(age[mask].corr(effort[mask]))
audit['exploratory_age_effort_n'] = int(mask.sum())

# Exact centering property of the current leave-one-out score.
current['loo'] = (current.groupby('card').y.transform('sum') - current.y) / (current.groupby('card').y.transform('size') - 1)
current['residual'] = current.y - current.loo
audit['loo_sum_over_decisions'] = float(current.residual.sum())
audit['loo_teacher_mean_unweighted'] = float(current.groupby('id').residual.mean().mean())

def convert(value):
    if isinstance(value, np.generic):
        return value.item()
    raise TypeError(type(value))
(OUT / 'auditoria.json').write_text(json.dumps(audit, ensure_ascii=False, indent=2, default=convert), encoding='utf-8')
print(json.dumps(audit, ensure_ascii=False, indent=2, default=convert))
print(pd.DataFrame(rows).round(4).to_string(index=False))
