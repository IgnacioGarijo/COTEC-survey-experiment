"""Order bibliography by first citation and record reproducible numerical provenance."""
from pathlib import Path
import re,json,hashlib
ROOT=Path(__file__).resolve().parents[1];DOC=ROOT/'4. Latex/paper_A_2026-09-27';OUT=ROOT/'3. Output/paper_A_2026-09-27'
tex=(DOC/'paper_A_teacher_decisions.tex').read_text(encoding='utf-8')
order=list(dict.fromkeys(','.join(re.findall(r'\\cite\{([^}]+)\}',tex)).split(',')))
bib=(DOC/'references_A.tex').read_text(encoding='utf-8')
items={m.group(1):m.group(0).strip() for m in re.finditer(r'\\bibitem\{([^}]+)\}.*?(?=\\bibitem|\\end\{thebibliography\})',bib,re.S)}
assert set(order)==set(items),(set(order)-set(items),set(items)-set(order))
(DOC/'references_A.tex').write_text('\\begin{thebibliography}{99}\\small\\setstretch{1.0}\\setlength{\\itemsep}{3pt}\n'+'\n'.join(items[k] for k in order)+'\n\\end{thebibliography}\n',encoding='utf-8')
paths=[ROOT/'Cambios_pendientes_manuscrito_A.txt',ROOT/'cambios_pendientes_manuscrito_A_autores.txt',ROOT/'3. Output/two_papers_2026-09-21/reports/analysis_objects.rds',ROOT/'3. Output/two_papers_2026-09-22/reports/deepened_objects.rds',Path('C:/Users/ignac/Downloads/1. PROFESORADO RÉGIMEN GENERAL (1).xlsx')]
manifest={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}
(OUT/'reports/input_manifest.json').write_text(json.dumps(manifest,indent=2),encoding='utf-8')
print('Bibliography ordered:',len(order),'references')
