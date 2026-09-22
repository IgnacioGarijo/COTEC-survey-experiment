from pathlib import Path
import csv,hashlib,json,re
from pypdf import PdfReader
import pandas as pd
ROOT=Path(__file__).resolve().parents[1];D=ROOT/'4. Latex/two_papers_2026-09-22';O=ROOT/'3. Output/two_papers_2026-09-22'
with (O/'reports/results_frozen_manifest.csv').open(encoding='utf-8-sig',newline='') as f:
    frozen=list(csv.DictReader(f))
for row in frozen:assert hashlib.sha256(Path(row['Path']).read_bytes()).hexdigest().upper()==row['Hash'],row['Path']
a=pd.read_csv(O/'tables/nested_prediction_repeats.csv');b=pd.read_csv(O/'tables/nested_prediction_summary.csv')
for r in b.itertuples():
    z=a[(a.population==r.population)&(a.model==r.model)]
    assert len(z)==5
    assert abs(z.r2.min()-r.r2_min)<1e-12 and abs(z.r2.max()-r.r2_max)<1e-12
    assert abs(z.r2.mean()-r.r2)<1e-12
blue=json.loads((D/'provenance_blue.json').read_text(encoding='utf8'));assert len(blue)==10 and all(x['verbatim']for x in blue)
details=[]
for p in D.glob('paper_*.pdf'):
    source=p.with_suffix('.tex').read_text(encoding='utf8');reader=PdfReader(p)
    log=p.with_suffix('.log').read_text(errors='replace')
    assert not re.search(r'Overfull|undefined|^!',log,re.M),p
    texts=[q.extract_text()or''for q in reader.pages]
    assert all('??'not in s and len(s)>100 for s in texts)
    for path in re.findall(r'\\inputtab\{[^}]+\}\{([^}]+)\}',source):assert (D/'tables'/f'{path}.tex').exists()
    for path in re.findall(r'\\plot\{[^}]+\}\{([^}]+)\}',source):assert (O/'figures'/f'{path}.png').exists()
    if 'paper_A'in p.name:
        assert '3.06'in ''.join(texts) and '1.48'in ''.join(texts) and '49.1'in ''.join(texts)
    else:assert '1,999'in ''.join(texts) and '0.031'in ''.join(texts) and '0.058'in ''.join(texts)
    details.append(dict(file=p.name,pages=len(reader.pages),tables=source.count('\\inputtab{'),figures=source.count('\\plot{')+source.count('\\begin{figure}'),sha256=hashlib.sha256(p.read_bytes()).hexdigest()))
(O/'reports/delivery_audit.json').write_text(json.dumps(dict(frozen_tables_verified=len(frozen),blue_passages=10,prediction_ranges_verified=True,documents=details),indent=2),encoding='utf8')
print(json.dumps(details,indent=2))
