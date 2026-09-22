from pathlib import Path
import pypdfium2 as pdfium
from pypdf import PdfReader
from PIL import Image,ImageOps,ImageDraw
import json,re
root=Path(__file__).resolve().parents[1]
src=root/'4. Latex/two_papers_2026-09-21'
out=root/'tmp/two_papers_pdf_qa';out.mkdir(exist_ok=True,parents=True)
report=[]
for path in src.glob('paper_*.pdf'):
 doc=pdfium.PdfDocument(path);reader=PdfReader(path);thumbs=[];pages=[]
 for i,page in enumerate(doc):
  im=page.render(scale=1.4).to_pil().convert('RGB');im.save(out/f'{path.stem}_{i+1:02}.png')
  thumb=ImageOps.contain(im,(285,405));canvas=Image.new('RGB',(305,435),'#dddddd');canvas.paste(thumb,((305-thumb.width)//2,18));ImageDraw.Draw(canvas).text((10,418),f'{path.stem[6]} / page {i+1}',fill='black');thumbs.append(canvas)
  text=reader.pages[i].extract_text() or '';pages.append({'page':i+1,'characters':len(text),'figures':re.findall(r'Figure \d+',text),'tables':re.findall(r'Table \d+',text)})
  assert '??' not in text,(path,i,'unresolved reference')
 for j in range(0,len(thumbs),9):
  grid=Image.new('RGB',(915,435*((min(9,len(thumbs)-j)+2)//3)),'white')
  for k,im in enumerate(thumbs[j:j+9]):grid.paste(im,((k%3)*305,(k//3)*435))
  grid.save(out/f'{path.stem}_contact_{j//9+1}.png')
 report.append({'file':path.name,'pages':len(doc),'page_details':pages})
(out/'page_audit.json').write_text(json.dumps(report,indent=2),encoding='utf8')
print(json.dumps(report,indent=2))
