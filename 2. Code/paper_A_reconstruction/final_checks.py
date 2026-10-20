"""Validate aggregate deliverables and prevent committing individual survey data."""
from pathlib import Path
import csv, json, re
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT/'3. Output/paper_A_reconstruction'
CHECKS = []


def check(name, passed):
    CHECKS.append({'check': name, 'passed': bool(passed)})


def main():
    for path in OUT.rglob('*.csv'):
        if 'local' in path.relative_to(OUT).parts:
            continue
        with path.open(encoding='utf-8-sig', newline='') as f:
            reader = csv.reader(f)
            header = next(reader, [])
            forbidden = {'responseid', 'id', 'teacher_id', 'email', 'ipaddress', 'recipientemail'}
            check('aggregate header '+str(path.relative_to(OUT)), not forbidden.intersection(h.lower() for h in header))
            check('no response identifiers '+str(path.relative_to(OUT)), not any(re.fullmatch(r'R_[A-Za-z0-9]{8,}', value) for row in reader for value in row))
    for s in ['A','B','C','D']:
        d = pd.read_csv(OUT/s/'decomposition_bootstrap_draws.csv')
        state = json.loads((OUT/s/'bootstrap_status.json').read_text())
        check(s+' distinct exact draws', len(d)==199 and set(d.draw)==set(range(1,200)))
        check(s+' acceptance', state['accepted']>=190 and state['attempted']==199)
        pp = pd.read_csv(OUT/s/'decomposition.csv')
        check(s+' new uncertainty finite', pp[['share','se','lo_percentile','hi_percentile']].notna().all().all())
        check(s+' normalized shares', abs(pp.share.sum()-1)<1e-10)
        if s=='B':
            check('B excludes trapped warm-start replicas', d.configuration.eq('v2-automatic-full-derivatives').all())
        if 'derivatives_checked' in d:
            v2 = d[d.configuration.eq('v2-automatic-full-derivatives') & d.accepted]
            check(s+' v2 admitted numerical diagnostics', (v2.gradient.lt(.002) & v2.min_hessian.gt(0) & v2.optimizer.eq(0)).all())
    audit = pd.read_csv(OUT/'tables/optimizer_control_audit.csv')
    check('all predefined numerical audit cases', len(audit)==9 and audit.groupby('scenario').draw.apply(set).map(lambda x:x=={1,50,199}).all())
    check('default and tight objectives agree', audit.delta_vs_tight_automatic.abs().max()<.0005)
    point = pd.read_csv(OUT/'tables/B_point_start_audit.csv')
    check('B point no materially better automatic objective', point.automatic_minus_saved_logLik.max()<.0005)
    check('protected files unchanged', json.loads((OUT/'protection_verification.json').read_text())['unchanged'])
    check('numerical invariants passed', pd.read_csv(OUT/'tables/verification_checks.csv').passed.all())
    check('paired uncertainties complete', pd.read_csv(OUT/'tables/paired_differences.csv').successful.eq(999).all())
    result = pd.DataFrame(CHECKS)
    result.to_csv(OUT/'diagnostics/final_deliverable_checks.csv', index=False)
    failed = result[~result.passed]
    if len(failed):
        raise SystemExit(failed.to_string(index=False))
    print(f'{len(result)} aggregate/privacy/completeness checks passed')


if __name__=='__main__':
    main()
