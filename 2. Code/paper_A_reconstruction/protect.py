"""Freeze current aggregate results and verify that protected files stay unchanged."""
from pathlib import Path
import hashlib, json, shutil, subprocess, sys

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / '3. Output/paper_A_reconstruction'

def sha(path):
    h = hashlib.sha256()
    with path.open('rb') as f:
        for block in iter(lambda: f.read(1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()

def main():
    OUT.mkdir(parents=True, exist_ok=True)
    manifest = OUT / 'protected_manifest.json'
    if sys.argv[1] == 'freeze':
        if manifest.exists():
            raise SystemExit('Snapshot already exists; use verify, never replace it.')
        tracked = subprocess.check_output(['git','ls-files','-z'], cwd=ROOT).decode().split('\0')
        selected = [p for p in tracked if p.startswith(('4. Latex/', '3. Output/paper_A/', '3. Output/paper_B/', '3. Output/shared/', '3. Output/population/'))]
        selected += ['1. Data/raw data/df_named.parquet', '1. Data/processed data/cleandata.parquet']
        hashes = {p: sha(ROOT / p) for p in selected if p and (ROOT/p).is_file()}
        baseline = OUT / 'baseline_snapshot'
        for folder in ['3. Output/paper_A/tables','3. Output/paper_A/figures','4. Latex/paper_A']:
            src=ROOT/folder
            for f in src.rglob('*'):
                if f.is_file() and f.suffix.lower() in {'.csv','.png','.tex','.pdf'}:
                    dest=baseline/f.relative_to(ROOT)
                    dest.parent.mkdir(parents=True,exist_ok=True)
                    shutil.copy2(f,dest)
        manifest.write_text(json.dumps({'sha256':hashes,'git_status_at_start':subprocess.check_output(['git','status','--short'],cwd=ROOT).decode()},ensure_ascii=False,indent=2),encoding='utf-8')
        print(f'Frozen {len(hashes)} protected files and copied current A aggregate/manuscript snapshot.')
    else:
        spec=json.loads(manifest.read_text(encoding='utf-8'))
        changed=[p for p,h in spec['sha256'].items() if not (ROOT/p).exists() or sha(ROOT/p)!=h]
        result={'protected_files':len(spec['sha256']),'unchanged':not changed,'changed':changed}
        (OUT/'protection_verification.json').write_text(json.dumps(result,indent=2),encoding='utf-8')
        print(json.dumps(result))
        if changed: raise SystemExit(1)

if __name__=='__main__': main()
