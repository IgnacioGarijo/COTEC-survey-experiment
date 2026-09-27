"""Univariate population comparison; compact figure for the future sample appendix."""
from pathlib import Path
import csv, hashlib, json, math
import openpyxl
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle, Image, PageBreak
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib import colors
from reportlab.lib.enums import TA_LEFT
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'3. Output/population_comparison_simulation_2026-09-26'
PDFDIR=ROOT/'output/pdf'; PDFDIR.mkdir(parents=True,exist_ok=True)
SOURCE=Path('C:/Users/ignac/Downloads/1. PROFESORADO RÉGIMEN GENERAL (1).xlsx')
sample=json.loads((OUT/'sample_aggregates.json').read_text(encoding='utf-8'))
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
def row(var,cat,cohort='main'):
 return next(r for r in rows if r['cohort']==cohort and r['variable']==var and r['category']==cat)
def pct(var,cat,cohort='main'):return row(var,cat,cohort)['sample_pct']
def es(x,d=1):return f'{x:,.{d}f}'.replace(',','X').replace('.',',').replace('X','.')
def num(x):return es(x,0)
def delta(x):return ('+' if x>0 else '')+es(x)


# One panel per variable; no conditional distributions or duplicated tables.
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':11,
                     'axes.spines.top':False,'axes.spines.right':False})
fig=plt.figure(figsize=(10,8.1))
gs=fig.add_gridspec(3,2,height_ratios=[1,1.5,1],hspace=.72,wspace=.66)
panels=[
 (fig.add_subplot(gs[0,0]),'sex',['Women','Men'],['Mujeres','Hombres'],'A. Sexo/género',100),
 (fig.add_subplot(gs[0,1]),'ownership',['Public','Private'],['Públicos','Privados*'],'B. Titularidad',100),
 (fig.add_subplot(gs[1,0]),'age_band',ages,ages,'C. Edad',35),
 (fig.add_subplot(gs[1,1]),'province',provinces,provinces,'D. Provincia',45),
 (fig.add_subplot(gs[2,:]),'level',['Primary','Secondary'],['Infantil/Primaria','Secundaria'],'E. Etapa educativa (aproximación)',100)]
series=[('reference_pct',None,'Población 2023/24','#7d8791','s',-.18),
        ('sample_pct','main','Muestra con decisiones','#205b70','o',0),
        ('sample_pct','complete','Casos completos','#b46630','^',.18)]
for ax,var,cats,labels,title,limit in panels:
 y=np.arange(len(cats))
 for field,cohort,label,color,marker,offset in series:
  values=[row(var,c,cohort or 'main')[field] for c in cats]
  ax.scatter(values,y+offset,label=label,color=color,marker=marker,s=32,zorder=3)
 ax.set_yticks(y,labels);ax.set_ylim(len(cats)-.5,-.5);ax.set_xlim(0,limit)
 ax.set_xlabel('Porcentaje');ax.grid(axis='x',alpha=.18)
 ax.set_title(title,loc='left',fontweight='bold',fontsize=12,pad=25)
 m=sample['main']['marginals'][var]['observed']
 c=sample['complete']['marginals'][var]['observed']
 ax.text(0,1.04,f'N: {num(sum(ref[var].values()))} / {num(m)} / {num(c)}',
         transform=ax.transAxes,fontsize=9,color='#52636c')
handles,labels=panels[0][0].get_legend_handles_labels()
fig.legend(handles,labels,loc='upper center',ncol=3,frameon=False,bbox_to_anchor=(.5,1),fontsize=11)
fig.subplots_adjust(left=.14,right=.98,top=.88,bottom=.07)
grid=OUT/'univariate_distributions.png'
fig.savefig(grid,dpi=220)
plt.close(fig)

pdfmetrics.registerFont(TTFont('Arial','C:/Windows/Fonts/arial.ttf'))
pdfmetrics.registerFont(TTFont('Arial-Bold','C:/Windows/Fonts/arialbd.ttf'))
pdfmetrics.registerFontFamily('Arial',normal='Arial',bold='Arial-Bold',italic='Arial',boldItalic='Arial-Bold')
styles=getSampleStyleSheet()
styles.add(ParagraphStyle(name='BodyA',fontName='Arial',fontSize=9.5,leading=12,spaceAfter=7))
styles.add(ParagraphStyle(name='SmallA',fontName='Arial',fontSize=8,leading=10,spaceAfter=6))
styles.add(ParagraphStyle(name='TitleA',fontName='Arial-Bold',fontSize=17,leading=21,spaceAfter=10,textColor=colors.HexColor('#163e51')))
story=[]
def p(text,style='BodyA'):story.append(Paragraph(text,styles[style]))
p('Composición de la muestra docente','TitleA')
p('Comparación con el profesorado de Castilla-La Mancha, curso 2023/24. La encuesta de marzo-abril de 2025 incluye 2.705 docentes con decisiones y 2.156 casos completos en las regresiones actuales.')
story.append(Image(str(grid),width=505,height=409.05))
p('<b>Figura 1.</b> Distribuciones univariantes. Cada panel muestra porcentajes sobre datos clasificables; los N siguen el orden de la leyenda. Las escalas horizontales difieren entre paneles. * Privados incluye concertados.','SmallA')
p('La muestra contiene más profesorado público (93,0 % frente a 86,1 %) y de 45-54 años (42,1 % frente a 34,3 %), y menos mujeres (67,3 % frente a 70,8 %). La mayor diferencia provincial corresponde a Toledo (-3,2 puntos porcentuales). Los casos completos presentan una composición muy similar a la muestra con decisiones.')
p('<b>Fuente y categorías.</b> Estadística de Enseñanzas no Universitarias de Castilla-La Mancha, Excel facilitado, hojas 1.1 (provincia), 1.5 (sexo, titularidad y edad) y 1.2 (etapa). Se utiliza como referencia poblacional. La provincia procede del código postal declarado del centro: 80 casos no clasificables (61 por formato y 19 por prefijo externo). Sexo/género: 57 sin clasificación binaria; edad: 5 sin dato; etapa: 3 sin clasificación.','SmallA')
p('La agrupación educativa es aproximada: Infantil/Primaria reúne filas 8, 9 y 13 de la hoja 1.2; Secundaria reúne ESO, Bachillerato, FP y combinaciones (10, 11, 12, 15 y 16). Quedan fuera 855 docentes de categorías mixtas, Especial y otras. Esa hoja suma 35.493 docentes, frente a 35.497 en las demás. El periodo y el ámbito educativo difieren entre fuentes; estas comparaciones describen composición, sin establecer representatividad en creencias o decisiones.','SmallA')

def footer(canvas,doc):
 canvas.saveState();canvas.setFont('Arial',8);canvas.setFillColor(colors.HexColor('#52636c'))
 canvas.drawString(45,27,'Comparación poblacional - Versión compacta - 27/09/2026')
 canvas.drawRightString(550,27,str(doc.page));canvas.restoreState()
pdf=PDFDIR/'comparacion_distribuciones_docentes_2026-09-27.pdf'
doc=SimpleDocTemplate(str(pdf),pagesize=(595.28,841.89),rightMargin=45,leftMargin=45,topMargin=35,bottomMargin=45,title='Composición de la muestra docente',author='Equipo COTEC')
doc.build(story,onFirstPage=footer,onLaterPages=footer)
manifest={'source_file':str(SOURCE),'source_sha256':hashlib.sha256(SOURCE.read_bytes()).hexdigest(),'sample_rds_md5':sample['input_md5'],'pdf':str(pdf),'reference_total':total,'stage_reference_total':stage_n,'sheet12_total':sheet12_total,'scope':'Workbook accepted by user as population reference on 2026-09-27; univariate comparison approved for future common sample/attrition appendix, not yet incorporated into manuscripts'}
(OUT/'manifest.json').write_text(json.dumps(manifest,ensure_ascii=False,indent=2),encoding='utf-8')
print(str(pdf))
