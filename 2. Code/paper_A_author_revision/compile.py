"""Compile both views in temporary folders; publish only the cumulative review PDF."""
from pathlib import Path
import subprocess,shutil,os,time
R=Path(__file__).resolve().parents[2];D=R/'4. Latex/paper_A';T=R/'tmp/paper_A_author_revision'
tex=os.environ.get('PDFLATEX_PATH') or shutil.which('pdflatex')
for name in ['paper_A_content','paper_A_teacher_decisions']:
 out=T/name;out.mkdir(parents=True,exist_ok=True)
 job=name+'_authorbuild'
 for i in range(2):
  p=subprocess.run([tex,'-interaction=nonstopmode','-halt-on-error','-jobname='+job,'-output-directory='+str(out),name+'.tex'],cwd=D,stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
  (out/f'pass{i+1}.txt').write_bytes(p.stdout)
  if p.returncode:raise RuntimeError(p.stdout.decode(errors='replace')[-2500:])
 print(name,': compiled twice')
p=T/'paper_A_teacher_decisions/paper_A_teacher_decisions_authorbuild.pdf';target=D/'paper_A_teacher_decisions.pdf'
staging=D/'paper_A_teacher_decisions.publish.pdf';shutil.copyfile(p,staging)
for attempt in range(5):
 try:os.replace(staging,target);break
 except PermissionError:
  if attempt==4:
   # PDF viewers may allow writes while denying replacement/deletion on Windows.
   shutil.copyfile(p,target)
   staging.unlink(missing_ok=True)
   break
  time.sleep(1)
print('Published',target)
