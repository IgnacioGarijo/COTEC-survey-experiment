"""Build current A assets from corrected B aggregates; preserve archived analyses."""
from pathlib import Path
import csv,json
ROOT=Path(__file__).resolve().parents[2]
RUN=ROOT/'3. Output/paper_A_final_revision'
def rows(path):
    with path.open(encoding='utf-8-sig') as f:return list(csv.DictReader(f))
s=rows(RUN/'B/summary.csv')[0]
# Reuse formatting only; never invoke historical estimation or archived tables.
src=(ROOT/'2. Code/33. A_revision_tables.py').read_text(encoding='utf8').split("a=rows('nested_prediction_summary')")[0]
src=src.replace("ROOT=Path(__file__).resolve().parents[1]", "ROOT=Path(__file__).resolve().parents[2]")
src=src.replace("OUT=ROOT/'3. Output/paper_A'", "OUT=ROOT/'3. Output/paper_A_final_revision'")
src=src.replace("OUT/'tables'", "OUT/'B'")
src=src.replace("if r['p_holm']!='NA':", "if r['p_holm'] not in ('NA',''):")
src=src.replace('22,006','20,452').replace('2,587','2,589').replace('2,157','2,164')
src=src.replace('same identifier','same six-attribute profile').replace('by identifier frequency','by profile frequency').replace('same-card','same-profile')
src=src.replace('the asymptotic normal reference','a t reference with residual degrees of freedom')
ns={'__file__':__file__};exec(compile(src,'current_table_format','exec'),ns)
write=ns['write'];pp=ns['pp']
mapping=rows(ROOT/'3. Output/paper_A_reconstruction/tables/card_mapping.csv')
design=[]
for k in range(1,9):
    rr=[r for r in mapping if r['card']==f'alumno_{k}_1' or r['card'].startswith(f'alumno_{k}_')]
    assert len(rr)==8,(k,len(rr))
    design.append([str(k)]+[str(sum(int(r[v]) for r in rr)) for v in ['male','background','failed','competence','absent','disruptive']])
write('design_sets',['Set','Male','Complex context','Failures','Low competences','Absence','Discipline'],design,'Number of cards carrying each attribute among the eight assigned positions. Each set contains two cards of each gender-by-background combination. There are 56 distinct profiles across the 64 positions; eight profiles occur twice.','lrrrrrr')
control=rows(RUN/'B/attributes_control.csv');pooled=rows(RUN/'B/attributes.csv')
write('context_comparison',['Attribute','Pooled','Control'],[[ns['labels'][v],ns['cell'](next(r for r in pooled if r['term']==v)),ns['cell'](next(r for r in control if r['term']==v))] for v in ns['attrs']], 'Teacher fixed effects and teacher-clustered SE. Pooled: 20,452 decisions, 2,589 teachers; control: 2,142 decisions, 276 teachers. The comparison checks sensitivity to context restriction; differences in significance are not tests of coefficient differences.'+ns['stars_note'])
comps=['Student','Professional','Beliefs','Harshness','Sensitivities','Residual']
labs=['Student information',r'\shortstack[l]{Professional /\\demographic}','Beliefs','Observed harshness','Attribute sensitivities','Residual']
models=rows(RUN/'tables/covariance_candidates.csv')
mods=[next(r for r in models if r['model']==m and r['optimizer']=='bobyqa') for m in ['diagonal','domains','full']]
write('covariance_audit',['Component','Diagonal','Three pairs','Unrestricted'],[[lab]+[pp(r[c]) for r in mods] for c,lab in zip(comps,labs)]+[['Covariance parameters','6','9','21'],['Boundary estimate','No','Yes','Yes'],['REML AIC']+[f"{float(r['AIC']):.2f}" for r in mods]],'Same 17,312 decisions and fixed design in all columns. Entries are percentages of model-implied variance. The diagonal specification is primary; the other specifications are boundary solutions. Two optimizers agree on the objective and allocations. AIC describes working-model fit, not validation of a generative model for response-derived harshness.','p{4.7cm}rrr')
scenarios=['B','C','NoScreen','B_full8','ByID']
summ={m:rows(RUN/m/'summary.csv')[0] for m in scenarios}
decomp={m:{r['component']:r['share'] for r in rows(RUN/m/'decomposition_point.csv')} for m in scenarios}
write('reconstruction_sensitivity',['Quantity','Main B','No shared','No screen','Eight only','By ID'],[['Teachers']+[summ[m]['teachers'] for m in scenarios],['Decisions']+[summ[m]['decisions'] for m in scenarios],['Harshness SD (pp)']+[pp(summ[m]['sd_hb']) for m in scenarios],['Harshness $R^2$ (\\%)']+[pp(summ[m]['r2_full']) for m in scenarios]]+[[lab]+[pp(decomp[m][c]) for m in scenarios] for c,lab in zip(comps,labs)],'No shared removes experimental positions sharing their response field with that respondent\'s examples. No screen retains the ordered responses without the favorable-example exclusion. Eight only retains complete eight-answer records. By ID uses original card identifiers rather than pooling identical profiles, on the same B records. The last six rows are variance shares from the same diagonal specification, on each scenario\'s complete covariate sample.','p{3.6cm}rrrrr')
# Existing visual design, supplied exclusively with revised aggregates.
src=(ROOT/'2. Code/32. A_revision_figures.py').read_text(encoding='utf8').split('# Show the actual dispersion')[0]
src=src.replace("ROOT=Path(__file__).resolve().parents[1]", "ROOT=Path(__file__).resolve().parents[2]")
src=src.replace("OUT/'tables'/f'{name}.csv'", "RUN/'B'/f'{name}.csv'")
src=src.replace("fig.savefig(FIG/f'{name}.png',dpi=230,bbox_inches='tight',facecolor='white');plt.close(fig)", """from io import BytesIO
    buffer=BytesIO();fig.savefig(buffer,format='png',dpi=230,bbox_inches='tight',facecolor='white');plt.close(fig)
    target=FIG/f'{name}.png';data=buffer.getvalue()
    if not target.exists() or target.read_bytes()!=data:target.write_bytes(data)""")
src=src.replace("flow=json.loads((OUT/'reports/survey_exit_audit.json').read_text(encoding='utf8'))", "flow=json.loads((RUN/'diagnostics/flow.json').read_text(encoding='utf8'))")
keys={'participation_denominator':'engaged','any_recorded_card':'any_card','reached_decision_end':'reached_end','decision_end_passed_filter':'end_retained','completed_passed_filter':'finished','before_decisions':'before_cards','during_decisions':'during','filter_exclusions_at_decision_end':'screen_after_end','after_decisions':'after_cards'}
for a,b in keys.items():src=src.replace("flow['"+a+"']","flow['"+b+"']")
src=src.replace("OUT/'reports/harshness_histogram_candidate.csv'", "RUN/'B/harshness_histogram.csv'")
start=src.index("s=json.loads((OUT/'reports/sample_aggregates.json')")
end=src.index('def ref(v,c):',start)
src=src[:start]+'''rr=rows('population_comparison')
s={co:{'marginals':{}} for co in ['main','complete']}
for co in s:
 for v in set(r['variable'] for r in rr):
  subset=[r for r in rr if r['cohort']==co and r['variable']==v]
  s[co]['marginals'][v]={'observed':int(subset[0]['sample_n']),'counts':{r['category']:int(r['sample_count']) for r in subset}}
'''+src[end:]
src=src.replace("v=rows('final_decomposition')", "v=rows('decomposition')")
exec(compile(src,'current_figure_format','exec'),{'__file__':__file__,'RUN':RUN})
