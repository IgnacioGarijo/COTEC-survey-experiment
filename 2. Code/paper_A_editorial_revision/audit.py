from pathlib import Path
import json,re,csv,subprocess
from pypdf import PdfReader
R=Path(__file__).resolve().parents[2];D=R/'4. Latex/paper_A';O=R/'3. Output/paper_A_editorial_revision';T=R/'tmp/paper_A_redline'
s=(D/'paper_A_content.tex').read_text(encoding='utf8');d=(D/'paper_A_teacher_decisions.tex').read_text(encoding='utf8')
m=json.loads((O/'comparison_manifest.json').read_text());assert m['counts']=={'old':13,'new':19}
assert m['old_figures_preserved']==5 and m['old_tables_preserved']==7
refs=(D/'references_A.tex').read_text(encoding='utf8');keys=set(re.findall(r'\\bibitem\[[^\]]+\]\{([^}]+)\}',refs));cit=set(k.strip() for a in re.findall(r'\\cite\w*\{([^}]+)\}',s) for k in a.split(','));assert cit<=keys
for name in ['paper_A_content','paper_A_teacher_decisions']:
 log=(T/name/(name+'_reviewbuild.log')).read_text(errors='replace');assert 'undefined' not in log.lower();assert 'Overfull' not in log;assert 'destination with the same' not in log
pdf=PdfReader(T/'paper_A_content/paper_A_content_reviewbuild.pdf');txt='\n'.join(p.extract_text() for p in pdf.pages)
for n in ['2,604','20,467','2,164','17,312','47.93','12.78','18.80']:assert n in txt,n
for n in ['2,705','22,124','2,157','18,608']:assert n not in txt,n
assert 'castilla' not in txt.lower()
for p in (D/'comparison_baseline').glob('*.png'):
 old=subprocess.check_output(['git','show','09f9cd4:3. Output/paper_A/figures/'+p.name],cwd=R)
 assert p.read_bytes()==old,p
result={'baseline':'09f9cd4','accepted_pages':len(pdf.pages),'review_pages':len(PdfReader(D/'paper_A_teacher_decisions.pdf').pages),'preserved_old_tables':7,'preserved_old_figures':5,'status':'passed','note':'Historical numbers intentionally remain in redline, not in accepted article.'}
(O/'final_audit.json').write_text(json.dumps(result,indent=2),encoding='utf8');print(result)
