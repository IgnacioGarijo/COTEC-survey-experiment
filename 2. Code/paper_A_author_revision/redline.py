"""Author revision review against accepted source at 8306e22: word changes plus intact deleted exhibits.

Edit paper_A_content.tex, never the generated paper_A_teacher_decisions.tex.
Exhibits are diffed as atoms so old tables/images stay legible and complete.
"""
from pathlib import Path
import subprocess,re,json,hashlib,os
R=Path(__file__).resolve().parents[2];D=R/'4. Latex/paper_A';O=R/'3. Output/paper_A_author_revision';T=R/'tmp/paper_A_author_revision';T.mkdir(parents=True,exist_ok=True)
BASE='8306e22';assets={};counts={};manifest=[]
def git(path):return subprocess.check_output(['git','show',BASE+':'+path],cwd=R)
def uncolor(s):
 while (m:=re.search(r'\\(?:old|rev|new)\{',s)):
  value,end=arg(s,m.end()-1);s=s[:m.start()]+value+s[end:]
 s=re.sub(r'\\(?:begin|end)\{(?:revision|addition)\}\n?','',s)
 return re.sub(r'\\color\{(?:reused|revised|added)\}','',s)
def arg(s,pos):
 assert s[pos]=='{';depth=1;k=pos+1
 while depth:
  escaped=(len(s[:k])-len(s[:k].rstrip('\\')))%2==1
  if not escaped:
   if s[k]=='{':depth+=1
   elif s[k]=='}':depth-=1
  k+=1
 return s[pos+1:k-1],k
def token(asset,side):
 identity=hashlib.sha256(json.dumps({k:v for k,v in asset.items() if k not in ('path','number')},sort_keys=True).encode()).hexdigest()[:16].upper()
 key='COTECEXHIBIT'+identity+'END'
 assets.setdefault(key,{'asset':asset,'sides':set()})['sides'].add(side)
 counts[side]=counts.get(side,0)+1
 manifest.append({'side':side,'token':key,'kind':asset['kind'],'name':asset['name'],'label':asset.get('label')})
 return '\n\n'+key+'\n\n'
def prepare(source,side):
 source=source.replace('\r\n','\n')
 source=uncolor(source);source=source.replace('\\input{preamble}','').replace('\\input{preamble_content}','')
 source=source.replace('{\\scriptsize\\inputtab','\\inputtab').replace('{tab:alternatives}}','{tab:alternatives}')
 # Timeline is a genuine unchanged figure and must appear exactly once.
 pat=r'\\begin\{figure\}.*?\\end\{figure\}'
 source=re.sub(pat,lambda m:token({'kind':'inline','name':'timeline','body':m.group(0)},side),source,flags=re.S)
 nfig=1;ntab=0
 while (m:=re.search(r'\\(plot|inputtab)\{',source)):
  vals=[];end=m.end()-1
  for _ in range(4):v,end=arg(source,end);vals.append(v)
  _,name,caption,label=vals;kind='figure' if m.group(1)=='plot' else 'table'
  if kind=='figure':
   nfig+=1;number=nfig
   if side=='old':
    data=git(f'3. Output/paper_A/figures/{name}.png');p=D/'comparison_baseline'/f'{name}.png';p.parent.mkdir(exist_ok=True)
    if not p.exists() or p.read_bytes()!=data:p.write_bytes(data)
    path='comparison_baseline/'+name+'.png'
   else:
    path='../../3. Output/paper_A/figures/'+name+'.png';data=(D/path).read_bytes()
   content={'path':path,'sha256':hashlib.sha256(data).hexdigest()}
  else:
   ntab+=1;number=ntab
   data=git(f'4. Latex/paper_A/tables/{name}.tex').decode('utf8').replace('\r\n','\n') if side=='old' else (D/'tables'/f'{name}.tex').read_text(encoding='utf8')
   content={'body':uncolor(data)}
  item={'kind':kind,'name':name,'caption':caption,'label':label,'number':number,**content}
  source=source[:m.start()]+token(item,side)+source[end:]
 refs=git('4. Latex/paper_A/references_A.tex').decode('utf8').replace('\r\n','\n') if side=='old' else (D/'references_A.tex').read_text(encoding='utf8')
 source=source.replace('\\input{references_A}',uncolor(refs))
 return source
baseline_source=git('4. Latex/paper_A/paper_A_content.tex').decode('utf8').replace('\r\n','\n')
# Author-accepted passages become part of the effective review origin individually.
acceptances_path=D/'accepted_passages.json'
acceptances=json.loads(acceptances_path.read_text(encoding='utf8')) if acceptances_path.exists() else {'baseline':BASE,'passages':[]}
assert acceptances['baseline']==BASE
for passage in acceptances['passages']:
 assert baseline_source.count(passage['old_text'])==1
 baseline_source=baseline_source.replace(passage['old_text'],passage['accepted_text'],1)
old=prepare(baseline_source,'old')
new=prepare((D/'paper_A_content.tex').read_text(encoding='utf8'),'new')
pre=(D/'preamble_content.tex').read_text(encoding='utf8')
for name,body in [('old',old),('new',new)]: (T/(name+'.tex')).write_text(pre+'\n'+body,encoding='utf8')
perl=os.environ.get('PERL_PATH','C:/Program Files/Git/usr/bin/perl.exe')
diff=os.environ.get('LATEXDIFF_SCRIPT',str(Path.home()/'AppData/Local/Programs/MiKTeX/scripts/latexdiff/latexdiff-so'))
cmd=[perl,diff,'--encoding=utf8','--type=UNDERLINE','--math-markup=whole','--graphics-markup=none',str(T/'old.tex'),str(T/'new.tex')]
p=subprocess.run(cmd,cwd=D,stdout=subprocess.PIPE,stderr=subprocess.PIPE,check=True)
(O/'latexdiff_log.txt').write_text(p.stderr.decode('utf8',errors='replace'),encoding='utf8')
result=p.stdout.decode('utf8').replace('\r\n','\n')
(T/'raw_diff.tex').write_text(result,encoding='utf8')
def render(key):
 item=assets[key];a=item['asset'];deleted=item['sides']=={'old'};added=item['sides']=={'new'}
 if a['kind']=='inline':return a['body']
 color='deletedreview' if deleted else 'addedreview' if added else 'black'
 kind=a['kind'];content=(r'\includegraphics[width=.94\linewidth]{'+a['path']+'}') if kind=='figure' else '\\small\n'+a['body']
 if deleted:
  if kind=='figure':content=r'\setlength{\fboxsep}{3pt}\fcolorbox{deletedreview}{white}{\includegraphics[width=.90\linewidth]{'+a['path']+'}}'
  caption=r'\caption*{\textcolor{deletedreview}{\textbf{Deleted / superseded --- baseline '+kind.title()+' '+str(a['number'])+'.} '+a['caption']+'}}'
  # Deleted caption refs resolve to current document; identifiers retained only in metadata.
  body=content+'\n'+caption if kind=='figure' else caption+'\n'+content
  return '\n\\begin{'+kind+'}[!htbp]\n\\centering\\begingroup\\color{'+color+'}\n'+body+'\n\\endgroup\\end{'+kind+'}\n'
 caption=r'\caption{\textcolor{'+color+'}{'+a['caption']+'}}\\label{'+a['label']+'}'
 body=content+'\n'+caption if kind=='figure' else caption+'\n'+content
 return '\n\\begin{'+kind+'}[!htbp]\n\\centering\\begingroup\\color{'+color+'}\n'+body+'\n\\endgroup\\end{'+kind+'}\n'
# Split highlight arguments around tokens, so no float is nested in a strikeout.
pattern=r'\\(DIF(?:add|del)(?:FL)?)\{'
pos=0
while (m:=re.search(pattern,result[pos:])):
 a=pos+m.start();brace=pos+m.end()-1;value,end=arg(result,brace)
 if 'COTECEXHIBIT' in value:
  chunks=re.split(r'(COTECEXHIBIT[A-F0-9]+END)',value)
  replacement=''.join(render(c) if c in assets else ('\\'+m.group(1)+'{'+c+'}' if c.strip() else c) for c in chunks)
  result=result[:a]+replacement+result[end:];pos=a+len(replacement)
 else:pos=end
for key in assets:
 if key in result:result=result.replace(key,render(key))
assert 'COTECEXHIBIT' not in result
# Removed exhibits keep baseline numbering in deleted prose, without live labels.
old_numbers={a['asset'].get('label'):str(a['asset'].get('number')) for a in assets.values() if 'old' in a['sides'] and a['asset'].get('label')}
pos=0
while (m:=re.search(r'\\DIFdel(?:FL)?\{',result[pos:])):
 a=pos+m.start();brace=pos+m.end()-1;value,end=arg(result,brace)
 value=re.sub(r'\\ref\{([^}]+)\}',lambda q:old_numbers.get(q.group(1),q.group(0)),value)
 replacement=result[a:brace+1]+value+'}';result=result[:a]+replacement+result[end:];pos=a+len(replacement)
style=r'''
% Cumulative review styling. Complex deleted exhibits remain intact and red.
\sloppy
\definecolor{deletedreview}{HTML}{B3261E}
\definecolor{addedreview}{HTML}{1756A9}
\renewcommand{\DIFadd}[1]{{\color{addedreview}#1}}
\renewcommand{\DIFdel}[1]{{\color{deletedreview}\sout{#1}}}
\renewcommand{\DIFaddFL}[1]{{\color{addedreview}#1}}
\renewcommand{\DIFdelFL}[1]{{\color{deletedreview}\sout{#1}}}
\pdfstringdefDisableCommands{\def\DIFadd#1{#1}\def\DIFdel#1{}\def\DIFaddFL#1{#1}\def\DIFdelFL#1{}}
'''
result=result.replace('\\begin{document}',style+'\n\\begin{document}',1)
# Retired headings remain visible without consuming current section letters.
result=re.sub(r'\\section\{(\\DIFdel\{[^{}]*\})\}',r'\\section*{\1}',result)
result=result.replace(r'\addtocounter{section}{-1}%DIFAUXCMD','')
legend=r'''
\begin{center}\fbox{\begin{minipage}{.94\linewidth}\footnotesize
\textbf{Cumulative review copy.} Compared with the accepted author version immediately before the present revision (8306e22), incorporating passages subsequently accepted by the author. Earlier historical deletions have been reset. Unchanged and accepted text is black; \textcolor{addedreview}{additions are blue}; \textcolor{deletedreview}{\sout{deleted text is struck out in red}}. Deleted or superseded exhibits remain visible in red and are explicitly labelled; they are not current results. Moved passages may appear as a deletion at their original location and an addition at their new location.
\end{minipage}}\end{center}
'''
result=result.replace('\\maketitle','\\maketitle\n'+legend,1)
result='\n'.join(line.rstrip() for line in result.replace('\r','').split('\n')).rstrip()+'\n'
(D/'paper_A_teacher_decisions.tex').write_text(result,encoding='utf8',newline='\n')
(O/'comparison_manifest.json').write_text(json.dumps({'baseline':BASE,'source':'paper_A_content.tex','accepted_passages':[a['id'] for a in acceptances['passages']],'counts':counts,'exhibits':manifest,'old_figures_preserved':sum(a['side']=='old' and a['kind']=='figure' for a in manifest),'old_tables_preserved':sum(a['side']=='old' and a['kind']=='table' for a in manifest)},indent=2),encoding='utf8')
print('Redline generated; exhibits:',counts)
