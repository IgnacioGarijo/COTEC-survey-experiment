"""Check inputs, artifact dependencies and source syntax without rerunning models."""
from pathlib import Path
import ast,json,re,subprocess,sys,hashlib
ROOT=Path(__file__).resolve().parents[1]
def need(p):
 p=Path(p)
 if not p.is_absolute():p=ROOT/p
 assert p.is_file(),f'Missing dependency: {p}'
 return p
for f in ['1. Data/raw data/df_named.parquet','1. Data/processed data/cleandata.parquet','1. Data/raw data/teacher_population.xlsx','1. Data/raw data/cotec_presentation_clean.parquet','4. Latex/Pre_registration.pdf']:need(f)
manifest=json.loads(need('2. Code/artifact_manifest.json').read_text())
for folder,spec in manifest.items():
 for kind,names in spec.items():
  for n in names:
   need(f'3. Output/{folder}/'+(n if kind=='files' else kind+'/'+n+('.csv' if kind=='tables' else '.png' if kind=='figures' else '')))
for letter,filename in [('A','paper_A_teacher_decisions'),('B','paper_B_policy_scenarios')]:
 doc=ROOT/f'4. Latex/paper_{letter}'
 text=need(doc/f'{filename}.tex').read_text(encoding='utf-8-sig')
 for name in re.findall(r'\\inputtab\{[^}]+\}\{([^}]+)\}',text):need(doc/'tables'/f'{name}.tex')
 for name in re.findall(r'\\plot\{[^}]+\}\{([^}]+)\}',text):need(ROOT/f'3. Output/paper_{letter}/figures/{name}.png')
 for name in re.findall(r'\\input\{([^}]+)\}',text):need(doc/(name+'.tex'))
 need(doc/f'{filename}.pdf')
 if letter=='A':assert 'castilla' not in text.lower() and 'mancha' not in text.lower()
for folder,filename in [('COTEC','ppt_total.qmd'),('Prelim results','ppt.qmd')]:
 doc=ROOT/'5. Presentation'/folder;text=need(doc/filename).read_text(encoding='utf-8-sig')
 for path in re.findall(r'!\[.*?\]\(([^)]+)\)',text):need(doc/path)
 for path in re.findall(r'src="([^"`]+)"',text):
  if not path.startswith(('http','data:')):need(doc/path)
 for path in re.findall(r"figures_path, '([^']+)'",text):need('3. Output/figures/'+path)
 for path in re.findall(r"tables_path, '([^']+)'",text):need('3. Output/tables/'+path)
 if folder=='COTEC':need(doc/'graficos/tabla_resumen.rds')
 need(doc/'index.html')
for name in ['supporting_analyses.tex','supporting_analyses.pdf','references_supporting.tex']:
 need('4. Latex/paper_A/'+name)
for path in (ROOT/'2. Code').glob('*.py'):ast.parse(path.read_text(encoding='utf-8-sig'),filename=str(path))
print('All current paper and presentation dependencies exist; Python syntax checks passed.')
