"""Resume the isolated reconstruction; never invoke the canonical write pipeline.

Use --status while another run owns the computations. Run without flags only
after those processes have exited. The numerical source manifest is strict:
editing a numerical script requires an explicit cache review, not silent reuse.
"""
from pathlib import Path
import argparse, hashlib, json, subprocess, sys

ROOT = Path(__file__).resolve().parents[2]
CODE = ROOT / '2. Code/paper_A_reconstruction'
OUT = ROOT / '3. Output/paper_A_reconstruction'
MAIN = ['A', 'B', 'C', 'D']
ALL = MAIN + ['B_full8', 'C_full8', 'C_inclusive', 'C_example_screen']
NUMERICAL = ['reconstruct.R', 'supplement.R', 'paired.R', 'verify.R', 'bridges.R', 'bootstrap_start_audit.R', 'optimizer_audit.R', 'bootstrap_v2.R', 'point_start_audit.R']


def signatures():
    return {name: hashlib.sha256((CODE/name).read_bytes()).hexdigest() for name in NUMERICAL}


def call(*args):
    print('Running:', ' '.join(map(str, args)), flush=True)
    subprocess.run(list(map(str, args)), cwd=ROOT, check=True)


def status():
    for s in ALL:
        p = OUT/s
        draws = p/'decomposition_bootstrap_draws.csv'
        n = max(0, sum(1 for _ in draws.open(encoding='utf-8'))-1) if draws.exists() else 0
        state = json.loads((p/'bootstrap_status.json').read_text()) if (p/'bootstrap_status.json').exists() else {}
        print(f'{s}: point={bool((p/"decomposition_point.csv").exists())}, '
              f'appendix={bool((p/"split_half_summary.csv").exists())}, '
              f'bootstrap saved={n}/199, accepted={state.get("accepted", "not finalized")}')


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--status', action='store_true')
    ap.add_argument('--record-source-manifest', action='store_true', help='Initial capture only; never overwrites an existing manifest')
    ap.add_argument('--workers', type=int, default=4)
    ap.add_argument('--rscript', default='Rscript')
    args = ap.parse_args()
    manifest = OUT/'diagnostics/numerical_source_manifest.json'
    if args.status:
        status()
        return
    if args.record_source_manifest:
        if manifest.exists():
            raise SystemExit('Manifest exists. Review changed methods and cache ownership explicitly.')
        manifest.parent.mkdir(parents=True, exist_ok=True)
        manifest.write_text(json.dumps({'sha256': signatures(), 'scope': 'Numerical code used for the accepted isolated reconstruction. Check before resuming caches; no changed estimators may be mixed.'}, indent=2), encoding='utf-8')
        return
    if not manifest.exists():
        raise SystemExit('Capture the reviewed numerical source first with --record-source-manifest.')
    if json.loads(manifest.read_text())['sha256'] != signatures():
        raise SystemExit('Numerical source changed: review caches before resuming.')
    def r(name, *rest):
        call(args.rscript, '--vanilla', CODE/name, *rest)
    if not (OUT/'protected_manifest.json').exists():
        call(sys.executable, CODE/'protect.py', 'freeze')
    call(sys.executable, CODE/'protect.py', 'verify')
    if not all((OUT/'local'/f'scenario_{s}.rds').exists() for s in ALL[:-1]):
        r('reconstruct.R', 'prepare')
    if not (OUT/'local/scenario_C_example_screen.rds').exists():
        r('supplement.R', 'screen')
    for s in ALL:
        if not (OUT/s/'summary.csv').exists():
            r('reconstruct.R', 'core', s)
    r('reconstruct.R', 'points', ','.join(ALL))
    if not (OUT/'tables/participation_comparison.csv').exists():
        r('supplement.R', 'appendix')
    missing = [s for s in ALL if not (OUT/s/'characterization_bootstrap.csv').exists()]
    if missing:
        r('supplement.R', 'coefficient_bootstrap', ','.join(missing))
    if not (OUT/'tables/paired_differences.csv').exists():
        r('paired.R')
    if not (OUT/'tables/shared_teacher_summaries.csv').exists():
        r('bridges.R')
    # Main fits all precede the uncertainty stage. Resume uses saved draw IDs,
    # scenario/formula/seed/version/start-covariance fingerprints and this code manifest.
    for s in MAIN:
        state_path = OUT/s/'bootstrap_status.json'
        st = json.loads(state_path.read_text()) if state_path.exists() else {}
        compatible = s != 'B' or st.get('configuration_counts', {}).get('v2-automatic-full-derivatives') == 199
        if st.get('attempted') != 199 or st.get('accepted', 0) < 190 or not compatible:
            if s == 'A':
                r('reconstruct.R', 'bootstrap', s, 199, args.workers)
            else:
                r('bootstrap_v2.R', s, args.workers)
    audit = OUT/'tables/bootstrap_start_audit.csv'
    if not audit.exists() or len(audit.read_text().splitlines()) < 13:
        r('bootstrap_start_audit.R')
    if not (OUT/'tables/optimizer_control_audit.csv').exists() or len((OUT/'tables/optimizer_control_audit.csv').read_text().splitlines()) < 10:
        r('optimizer_audit.R')
    if not (OUT/'tables/B_point_start_audit.csv').exists():
        r('point_start_audit.R')
    r('verify.R')
    r('export_harshness.R')
    r('figures.R')
    call(sys.executable, CODE/'protect.py', 'verify')
    call(sys.executable, CODE/'final_checks.py')
    call(sys.executable, CODE/'report.py', '--final')
    status()


if __name__ == '__main__':
    main()
