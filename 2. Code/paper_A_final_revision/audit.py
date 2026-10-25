"""Cross-check current aggregate counts, assets, colors, citations and PDF text."""
from pathlib import Path
import csv,json,re,ast
from pypdf import PdfReader
R=Path(__file__).resolve().parents[2];O=R/'3. Output/paper_A_final_revision';D=R/'4. Latex/paper_A'
def rows(p):
 with p.open(encoding='utf-8-sig') as f:return list(csv.DictReader(f))
s=rows(O/'B/summary.csv')[0];t=(D/'paper_A_teacher_decisions.tex').read_text(encoding='utf8');p=PdfReader(D/'paper_A_teacher_decisions.pdf');text='\n'.join(x.extract_text() for x in p.pages)
assert (int(s['teachers']),int(s['decisions']),int(s['common_teachers']))==(2604,20467,2164)
v=rows(O/'B/decomposition.csv');assert abs(sum(float(x['share']) for x in v)-1)<1e-10
boot=json.loads((O/'B/bootstrap_status.json').read_text());assert boot['accepted']==199 and boot['all_derivatives_checked']
for n in ['2,604','20,467','2,164','17,312','47.93','12.78','18.80']:assert n in text,n
for n in ['2,705','22,124','2,157','18,608','48.63','17.10','389 exclusions','non-takers']:assert n not in text,n
assert 'castilla' not in text.lower() and 'mancha' not in text.lower()
for name in re.findall(r'\\inputtab\{[^}]+\}\{([^}]+)\}',t):assert (D/'tables'/f'{name}.tex').exists()
labels=re.findall(r'\\label\{([^}]+)\}',t)+[re.findall(r'\{((?:fig|tab):[^}]+)\}',line)[-1] for line in t.splitlines() if '\\inputtab{' in line or '\\plot{' in line];assert len(labels)==len(set(labels))
assert set(re.findall(r'\\ref\{([^}]+)\}',t))<=set(labels)
refs=(D/'references_A.tex').read_text(encoding='utf8');keys=set(re.findall(r'\\bibitem\[[^\]]+\]\{([^}]+)\}',refs))
cit=set(k.strip() for m in re.findall(r'\\cite\w*\{([^}]+)\}',t) for k in m.split(','));assert cit<=keys,cit-keys
log=(D/'paper_A_teacher_decisions.log').read_text(errors='replace')
assert 'undefined' not in log.lower() and 'Overfull' not in log
pre=(D/'preamble.tex').read_text();assert all(h in pre for h in ['2455A4','D97706','C62828'])
for f in (R/'2. Code/paper_A_final_revision').glob('*.py'):ast.parse(f.read_text(encoding='utf8'))
result={'pages':len(p.pages),'teachers':2604,'decisions':20467,'bootstrap_accepted':199,'bootstrap_boundary':boot['singular'],'tables':len(re.findall(r'\\inputtab\{',t)),'figures':len(re.findall(r'\\plot\{',t))+1,'citations':len(cit),'checks':'passed; visual QA recorded separately'}
(O/'final_audit.json').write_text(json.dumps(result,indent=2),encoding='utf8');print(result)
