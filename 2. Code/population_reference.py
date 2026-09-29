from pathlib import Path
import csv, hashlib, json, math
import openpyxl
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'3. Output/population'; OUT.mkdir(parents=True,exist_ok=True)
SOURCE=ROOT/'1. Data/raw data/teacher_population.xlsx'
sample=json.loads((ROOT/'3. Output/paper_A/reports/sample_aggregates.json').read_text(encoding='utf-8'))
w=openpyxl.load_workbook(SOURCE,data_only=True)
wf=openpyxl.load_workbook(SOURCE,data_only=False)
audit=[]
def val(s,c):
    v=w[s][c].value
    assert isinstance(v,(int,float)),(s,c,v)
    audit.append(dict(sheet=s,cell=c,value=v,formula=wf[s][c].value if wf[s][c].data_type=='f' else ''))
    return v
total=val('1.5','B7'); women=val('1.5','D7'); public=val('1.5','B22')
assert total==women+val('1.5','C7')==public+val('1.5','B37')==val('1.1','B7')
ages=['20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65+']
age_counts=[val('1.5',f'B{r}') for r in range(8,18)]
assert sum(age_counts)==total
provinces=['Albacete','Ciudad Real','Cuenca','Guadalajara','Toledo']
province_counts=[val('1.1',f'B{r}') for r in range(8,13)]
assert sum(province_counts)==total
primary_rows=[8,9,13];secondary_rows=[10,11,12,15,16]
stage={}
for label,rows in [('Primary',primary_rows),('Secondary',secondary_rows)]:
    stage[label]={k:sum(val('1.2',f'{col}{r}') for r in rows) for k,col in [('n','B'),('women','D'),('public','E')]}
stage_n=sum(x['n'] for x in stage.values())
sheet12_total=val('1.2','B7'); excluded=sum(val('1.2',f'B{r}') for r in [14,17,18])
assert stage_n+excluded==sheet12_total
assert total-sheet12_total==4
ref={'sex':{'Women':women,'Men':total-women},'ownership':{'Public':public,'Private':total-public},'age_band':dict(zip(ages,age_counts)),'province':dict(zip(provinces,province_counts)),'level':{k:v['n'] for k,v in stage.items()}}
rows=[]; diagnostics=[]
for cohort in ['main','complete']:
 for variable,counts in ref.items():
  m=sample[cohort]['marginals'][variable];den=m['observed'];refden=sum(counts.values());diff=[]
  assert sum(m['counts'].values())==den
  for cat,num in counts.items():
   n=m['counts'].get(cat,0);p=n/den;q=num/refden;diff.append(abs(p-q))
   rows.append(dict(cohort=cohort,variable=variable,category=cat,reference_count=num,reference_n=refden,sample_count=n,sample_n=den,reference_pct=100*q,sample_pct=100*p,difference_pp=100*(p-q),representation_ratio=p/q))
  diagnostics.append(dict(cohort=cohort,variable=variable,total_variation_pp=50*sum(diff),largest_gap_pp=100*max(diff),missing=sample[cohort]['n']-den))
def savecsv(name,data):
 with (OUT/name).open('w',encoding='utf-8-sig',newline='') as f:
  wr=csv.DictWriter(f,fieldnames=list(data[0]));wr.writeheader();wr.writerows(data)
savecsv('comparison.csv',rows);savecsv('distribution_distances.csv',diagnostics);savecsv('workbook_cell_audit.csv',audit)

print('Population comparison extracted from local workbook.')
