"""Validate APA author-year bibliography and record numerical provenance."""
from pathlib import Path
import re,json,hashlib
ROOT=Path(__file__).resolve().parents[1];DOC=ROOT/'4. Latex/paper_A';OUT=ROOT/'3. Output/paper_A'
tex=(DOC/'paper_A_teacher_decisions.tex').read_text(encoding='utf-8')
order=list(dict.fromkeys(','.join(re.findall(r'\\cite(?:p|t|yearpar)\{([^}]+)\}',tex)).split(',')))
bib=(DOC/'references_A.tex').read_text(encoding='utf-8')
items={m.group(1):m.group(0).strip() for m in re.finditer(r'\\bibitem\[[^\]]+\]\{([^}]+)\}.*?(?=\\bibitem|\\end\{thebibliography\})',bib,re.S)}
assert set(order)==set(items),(set(order)-set(items),set(items)-set(order))
# Preserve explicit alphabetical APA order rather than first-citation order.
oldkeys=set(json.loads((DOC/'provenance_review.json').read_text(encoding='utf-8'))['baseline_reference_keys'])
# Provenance colors are assigned by the reviewed-source edit, never recolored by a build.
paths=[ROOT/'1. Data/raw data/df_named.parquet',ROOT/'1. Data/processed data/cleandata.parquet',ROOT/'1. Data/raw data/teacher_population.xlsx']
manifest={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}
(OUT/'reports/input_manifest.json').write_text(json.dumps(manifest,indent=2),encoding='utf-8')
print('APA bibliography validated:',len(order),'references')
