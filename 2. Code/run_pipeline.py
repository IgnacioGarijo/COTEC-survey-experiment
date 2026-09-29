"""Build the current papers, optionally rerun their analyses or render presentations.
Run from any directory. Python dependencies: requirements.txt in this folder.
"""
from pathlib import Path
import argparse,os,shutil,subprocess,sys
ROOT=Path(__file__).resolve().parents[1]
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--analysis',action='store_true',help='Reestimate all current paper analyses (slow; private local data required).')
p.add_argument('--presentations',action='store_true',help='Also compile the three presentations using their retained assets.')
p.add_argument('--no-pdf',action='store_true',help='Regenerate tables and figures without compiling papers.')
a=p.parse_args()
def program(name):
 candidates=[os.environ.get(name.upper()+'_PATH'),shutil.which(name)]
 if name=='quarto':candidates += ['C:/Program Files/Quarto/bin/quarto.exe','C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe']
 for path in candidates:
  if path and Path(path).exists():return path
 raise SystemExit(f'{name} unavailable. Put it on PATH or set {name.upper()}_PATH.')
def run(cmd,cwd=ROOT):
 env=os.environ.copy()
 if os.name=='nt':
  # C.UTF-8 is not a valid Windows R locale and corrupts Spanish Quarto JSON.
  for key in ['LC_ALL','LC_CTYPE','LANG']:env[key]='Spanish_Spain.utf8'
 print('Running:',*cmd,flush=True);subprocess.run([str(x) for x in cmd],cwd=cwd,check=True,env=env)
def py(name):run([sys.executable,ROOT/'2. Code'/name])
if a.analysis:
 rs=program('Rscript')
 for name in ['9. two_papers_analysis.R','11. two_papers_sensitivity.R','15. deepen_two_papers.R','27. revise_A_analysis.R','28. revised_A_prediction.R','29. A_variance_decomposition.R','30. A_participation.R','31. A_population_aggregates.R','36. A_logit_observed_variance.R']:
  run([rs,'--vanilla',ROOT/'2. Code'/name])
for name in ['population_reference.py','32. A_revision_figures.py','33. A_revision_tables.py','build_B_tables.py','34. finalize_A_sources.py']:py(name)
if not a.no_pdf:
 tex=program('pdflatex')
 for folder,source in [('paper_A','paper_A_teacher_decisions.tex'),('paper_B','paper_B_policy_scenarios.tex')]:
  for _ in range(2):run([tex,'-interaction=nonstopmode','-halt-on-error',source],ROOT/'4. Latex'/folder)
 py('35. audit_A_revision.py')
if a.presentations:
 quarto=program('quarto')
 for folder,source in [('COTEC','ppt_total.qmd'),('Prelim results','ppt.qmd')]:
  run([quarto,'render',source],ROOT/'5. Presentation'/folder)
 tex=program('pdflatex')
 for _ in range(2):run([tex,'-interaction=nonstopmode','-halt-on-error','presentacion_seet.tex'],ROOT/'5. Presentation/SEET2026')
py('check_repository.py')
