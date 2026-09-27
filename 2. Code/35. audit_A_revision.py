"""Checks on aggregate results, manuscript links and final PDF; render every page."""
from pathlib import Path
import csv,json,re
from pypdf import PdfReader
import pypdfium2 as pdfium
from PIL import Image,ImageOps,ImageDraw
ROOT=Path(__file__).resolve().parents[1];OUT=ROOT/'3. Output/paper_A_2026-09-27';DOC=ROOT/'4. Latex/paper_A_2026-09-27'
def rows(name):
 with (OUT/'tables'/f'{name}.csv').open(encoding='utf-8-sig') as f:return list(csv.DictReader(f))
flow={r['stage']:int(r['n']) for r in rows('sample_flow')}
assert flow['Raw survey records']==flow['Screening exclusions']+flow['Retained records']
assert flow['Retained records']==flow['Retained without decisions']+flow['Teachers with decisions']
assert flow['Teachers with decisions']==2705 and flow['Complete analysis covariates']==2157
v=rows('variance_partition');assert abs(sum(float(r['share']) for r in v)-1)<1e-10
assert all(float(r['share'])>=0 for r in v)
assert all(int(r['successful'])>=379 for r in v)
for r in rows('variance_diagnostics'):
 assert r['singular']=='FALSE' and not r['convergence'],r
c=rows('characterization');assert all(r['term'] not in ['resources','antiguedad3'] for r in c)
assert all(int(r['n'])==2157 for r in c if r['model'].endswith('_common'))
p=rows('nested_prediction_summary');assert all(int(r['n'])==2157 for r in p if r['population']=='Common')
tex=(DOC/'paper_A_teacher_decisions.tex').read_text(encoding='utf-8')
assert not re.search(r'@@\w+@@',tex),'Unfilled numerical tokens'
for n in re.findall(r'\\inputtab\{[^}]+\}\{([^}]+)\}',tex):assert (DOC/'tables'/f'{n}.tex').exists(),n
for n in re.findall(r'\\plot\{[^}]+\}\{([^}]+)\}',tex):assert (OUT/'figures'/f'{n}.png').exists(),n
labels=set(re.findall(r'\\label\{([^}]+)\}',tex))|set(re.findall(r'\\(?:inputtab|plot)\{[^}]+\}\{[^}]+\}\{.*?\}\{([^}]+)\}',tex))
refs=set(re.findall(r'\\ref\{([^}]+)\}',tex));assert refs<=labels,(refs-labels)
for label in labels:
 if label.startswith(('tab:','fig:')):assert tex.count(r'\ref{'+label+'}')>=1,('Unmentioned exhibit',label)
bib=(DOC/'references_A.tex').read_text(encoding='utf-8');keys=set(re.findall(r'\\bibitem\{([^}]+)\}',bib));cites=set(','.join(re.findall(r'\\cite\{([^}]+)\}',tex)).split(','));assert cites<=keys,cites-keys
assert keys<=cites,('Uncited references',keys-cites)
log=(DOC/'paper_A_teacher_decisions.log').read_text(encoding='utf-8',errors='replace');assert 'undefined' not in log.lower();assert 'Overfull' not in log
pdf=DOC/'paper_A_teacher_decisions.pdf';reader=PdfReader(pdf);doc=pdfium.PdfDocument(pdf)
qa=ROOT/'tmp/paper_A_2026-09-27_QA';qa.mkdir(parents=True,exist_ok=True);thumbs=[];pages=[]
for i,page in enumerate(doc):
 im=page.render(scale=1.4).to_pil().convert('RGB');im.save(qa/f'page_{i+1:02}.png')
 thumb=ImageOps.contain(im,(280,396));tile=Image.new('RGB',(300,423),'#dddddd');tile.paste(thumb,((300-thumb.width)//2,5));ImageDraw.Draw(tile).text((10,404),f'Page {i+1}',fill='black');thumbs.append(tile)
 text=reader.pages[i].extract_text() or '';assert '??' not in text;pages.append(dict(page=i+1,characters=len(text)))
for k in range(0,len(thumbs),9):
 grid=Image.new('RGB',(900,423*((min(9,len(thumbs)-k)+2)//3)),'white')
 for j,tile in enumerate(thumbs[k:k+9]):grid.paste(tile,((j%3)*300,(j//3)*423))
 grid.save(qa/f'contact_{k//9+1}.png')
report=dict(checks='passed',pages=len(reader.pages),tables=len(re.findall(r'\\inputtab\{',tex)),figures=len(re.findall(r'\\plot\{',tex))+len(re.findall(r'\\begin\{figure\}',tex)),references=len(keys),page_details=pages)
(OUT/'reports/final_audit.json').write_text(json.dumps(report,indent=2),encoding='utf-8')
print(json.dumps(report,indent=2))
