"""Hypothetical reference comparison: read workbook, aggregate diagnostics, separate PDF."""
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

# Report figures: observed proportions only, no probability-sample confidence claims.
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':10,'axes.spines.top':False,'axes.spines.right':False})
def bars(variable,cats,filename,horizontal=False):
 a=[row(variable,c)['reference_pct'] for c in cats];b=[pct(variable,c) for c in cats];c=[pct(variable,c,'complete') for c in cats]
 fig,ax=plt.subplots(figsize=(8.3,3.3));x=np.arange(len(cats));width=.25
 for off,y,label,color in [(-width,a,'Excel de prueba','#929da7'),(0,b,'Muestra con decisiones','#205b70'),(width,c,'Casos completos','#b46630')]:
  ax.bar(x+off,y,width,label=label,color=color)
 ax.set_xticks(x,cats);ax.set_ylabel('Porcentaje');ax.set_ylim(0,max(a+b+c)*1.22);ax.legend(frameon=False,ncol=3,fontsize=8,loc='upper center',bbox_to_anchor=(.5,1.18));ax.grid(axis='y',alpha=.15);ax.set_axisbelow(True)
 fig.tight_layout();fig.savefig(OUT/filename,dpi=180,bbox_inches='tight');plt.close(fig)
bars('age_band',ages,'age_distribution.png');bars('province',provinces,'province_distribution.png')

pdfmetrics.registerFont(TTFont('Arial','C:/Windows/Fonts/arial.ttf'))
pdfmetrics.registerFont(TTFont('Arial-Bold','C:/Windows/Fonts/arialbd.ttf'))
pdfmetrics.registerFontFamily('Arial',normal='Arial',bold='Arial-Bold',italic='Arial',boldItalic='Arial-Bold')
styles=getSampleStyleSheet()
styles.add(ParagraphStyle(name='BodyA',fontName='Arial',fontSize=10,leading=14,spaceAfter=8))
styles.add(ParagraphStyle(name='SmallA',fontName='Arial',fontSize=8,leading=10.5,spaceAfter=6))
styles.add(ParagraphStyle(name='TitleA',fontName='Arial-Bold',fontSize=19,leading=23,spaceAfter=13,textColor=colors.HexColor('#163e51')))
styles.add(ParagraphStyle(name='HeadA',fontName='Arial-Bold',fontSize=13,leading=17,spaceBefore=8,spaceAfter=9,textColor=colors.HexColor('#163e51')))
story=[]
def p(text,style='BodyA'):story.append(Paragraph(text,styles[style]))
def head(text):p(text,'HeadA')
def table(headers,data,widths):
 cell=lambda v:Paragraph(str(v),styles['SmallA'])
 t=Table([[cell(x) for x in headers]]+[[cell(x) for x in r] for r in data],colWidths=widths,repeatRows=1,hAlign='LEFT')
 t.setStyle(TableStyle([('BACKGROUND',(0,0),(-1,0),colors.HexColor('#e8eef1')),('VALIGN',(0,0),(-1,-1),'TOP'),('TOPPADDING',(0,0),(-1,-1),5),('BOTTOMPADDING',(0,0),(-1,-1),4),('LINEBELOW',(0,0),(-1,0),.7,colors.HexColor('#54707d')),('LINEBELOW',(0,-1),(-1,-1),.5,colors.HexColor('#bbc7cd')),('ROWBACKGROUNDS',(0,1),(-1,-1),[colors.white,colors.HexColor('#f6f8f9')])]))
 story.extend([t,Spacer(1,9)])
def page(title):story.append(PageBreak());p(title,'TitleA')

p('Simulacro de apéndice<br/>Composición de la muestra docente','TitleA')
p('<b>Uso interno. Referencia hipotética. No incorporado a los manuscritos.</b> Fecha: 26 de septiembre de 2026.')
p('Se trata el Excel facilitado como si fuera una referencia oficial para ensayar una comparación de distribuciones. No se verifica su autenticidad ni se declara representatividad de la encuesta. Se mantienen los datos y muestras actuales, sin aplicar las revisiones pendientes.')
head('A.1. Fuente, muestras y alcance')
p('El libro identifica Castilla-La Mancha y el curso <b>2023/24</b>. Nuestra encuesta se recogió entre el <b>14 de marzo y el 29 de abril de 2025</b>. El referente incluye todo el profesorado de régimen general, con Infantil y otras enseñanzas; la encuesta distingue dos ramas amplias. El desfase temporal y el ámbito impiden interpretar las diferencias como selección pura.')
table(['Conjunto','N','Función en este ejercicio'],[['Referencia global del Excel',num(total),'Denominador de sexo, titularidad, edad y provincia'],['Docentes con decisiones',num(sample['main']['n']),'Muestra principal del manuscrito A'],['Casos completos actuales',num(sample['complete']['n']),'Muestra común de caracterización y predicción']],[185,55,265])
head('A.2. Comparación resumida')
overview=[]
for label,var,cat in [('Mujeres entre respuestas clasificables','sex','Women'),('Profesorado de centros públicos','ownership','Public'),('Rama secundaria (aproximación)','level','Secondary')]:
 r=row(var,cat);overview.append([label,es(r['reference_pct'])+' %',es(r['sample_pct'])+' %',es(pct(var,cat,'complete'))+' %',delta(r['difference_pp'])])
table(['Indicador','Excel','Decisiones','Completos','Diferencia*'],overview,[211,66,76,76,76])
p('* Diferencia en puntos porcentuales: muestra con decisiones menos Excel. Los denominadores específicos y la aproximación de etapa se detallan en las páginas siguientes.','SmallA')
p(f'<b>Lectura provisional:</b> frente a este referente amplio, la muestra tiene más profesorado público (+{es(pct("ownership","Public")-100*public/total)} puntos), menos mujeres ({delta(pct("sex","Women")-100*women/total)} puntos) y mayor concentración en edades intermedias. La composición de los casos completos es parecida a la de la muestra con decisiones en estas dimensiones; eso no elimina selección en variables no observadas.')
p('Fuente de referencia: <i>1. PROFESORADO RÉGIMEN GENERAL (1).xlsx</i>, hojas 1.1, 1.2 y 1.5; el propio archivo atribuye la fuente a la Consejería de Educación, Cultura y Deportes. Esta atribución se reproduce como metadato del archivo, no como verificación externa.','SmallA')

page('A.3. Edad: similitud general y diferencias por tramo')
p(f'La edad está disponible para 2.700 de los 2.705 docentes y para los 2.156 casos completos. La media de la muestra con decisiones es {es(sample["main"]["age"]["mean"])} años. El Excel ofrece intervalos, incluido uno abierto de 65 o más años: no se fabrica una media poblacional exacta a partir de ellos.')
story.append(Image(str(OUT/'age_distribution.png'),width=505,height=205))
data=[]
for cat in ages:
 r=row('age_band',cat);data.append([cat,num(r['reference_count']),num(r['sample_count']),es(r['reference_pct']),es(r['sample_pct']),es(pct('age_band',cat,'complete')),delta(r['difference_pp'])])
table(['Edad','N Excel','N muestra','Excel %','Muestra %','Completos %','Dif. pp'],data,[55,69,69,69,80,86,77])
young_ref=100*sum(age_counts[:3])/total;young_s=sum(pct('age_band',a) for a in ages[:3]);old_ref=100*sum(age_counts[7:])/total;old_s=sum(pct('age_band',a) for a in ages[7:]);mid_s=sum(pct('age_band',a) for a in ages[5:7]);mid_ref=100*sum(age_counts[5:7])/total
p(f'El tramo de 45-54 años reúne el <b>{es(mid_s)} %</b> de la muestra frente al <b>{es(mid_ref)} %</b> del referente. Menores de 35: {es(young_s)} % frente a {es(young_ref)} %. Con 55 o más años: {es(old_s)} % frente a {es(old_ref)} %. La comparación sugiere concentración en edades intermedias, no un simple desplazamiento uniforme hacia edades mayores o menores.')
p('Fuente: hoja 1.5, B7:B17. Porcentajes de la muestra sobre edades observadas. Las comparaciones de edad no pueden restringirse exactamente a etapas educativas con las tablas disponibles.','SmallA')

page('A.4. Distribución territorial')
p('La provincia se aproxima mediante los dos primeros dígitos del código postal del centro declarado, tras la limpieza ya existente. Se utilizan solo códigos de cinco cifras con prefijo regional. No se utilizan localización IP ni códigos geográficos imputados.')
story.append(Image(str(OUT/'province_distribution.png'),width=505,height=205))
data=[]
for cat in provinces:
 r=row('province',cat);data.append([cat,num(r['reference_count']),num(r['sample_count']),es(r['reference_pct']),es(r['sample_pct']),es(pct('province',cat,'complete')),delta(r['difference_pp'])])
table(['Provincia','N Excel','N muestra','Excel %','Muestra %','Completos %','Dif. pp'],data,[97,68,68,63,70,76,63])
p(f'En los códigos clasificables, Toledo tiene menor presencia ({delta(row("province","Toledo")["difference_pp"])} puntos), mientras que Guadalajara y Cuenca presentan diferencias de {delta(row("province","Guadalajara")["difference_pp"])} y {delta(row("province","Cuenca")["difference_pp"])} puntos. Albacete y Ciudad Real quedan más próximos al referente.')
p('<b>Cobertura:</b> se clasifican 2.625 de 2.705 docentes. Hay 61 códigos con formato no utilizable y 19 con prefijo ajeno a Castilla-La Mancha. Estos 80 casos se excluyen solo del porcentaje provincial, sin eliminarlos de las demás comparaciones. En casos completos se clasifican 2.099 de 2.156: 42 formatos no utilizables y 15 prefijos externos. Un prefijo externo no demuestra por sí solo que el docente esté fuera de la población objetivo.')
p('Fuente: hoja 1.1, A8:B12. No se presentan tasas de respuesta por provincia: el número de docentes del Excel no es un denominador verificado de personas invitadas.','SmallA')

page('A.5. Etapa, género y titularidad: qué es comparable')
p('La pregunta inicial distingue Primaria y Secundaria, pero las preguntas posteriores permiten declarar Infantil, Bachillerato y FP. Por ello se ensaya una correspondencia amplia por enseñanza impartida; no se confunde tipo de centro con etapa del docente.')
table(['Rama aproximada','Filas de hoja 1.2','N Excel'],[['Infantil/Primaria','8, 9 y 13: Infantil; Primaria; ambas',num(stage['Primary']['n'])],['Secundaria','10, 11, 12, 15 y 16: ESO, Bachillerato, FP y sus combinaciones',num(stage['Secondary']['n'])],['Sin asignación a las dos ramas','14, 17 y 18: Primaria/ESO, Especial y otras',num(excluded)]],[124,311,70])
p(f'El porcentaje de secundaria se calcula sobre {num(stage_n)} docentes del Excel y 2.702 respuestas clasificadas en la encuesta: <b>{es(100*stage["Secondary"]["n"]/stage_n)} % frente a {es(pct("level","Secondary"))} %</b>. Esta equivalencia es orientativa: no reconstruye una misma elegibilidad individual en ambas fuentes.')
strat=[]
for key,label in [('Public','Centros públicos'),('Private','Centros privados'),('Primary','Infantil/Primaria, aprox.'),('Secondary','Secundaria, aprox.')]:
 s=next(a for a in sample['main']['strata'] if a['stratum']==key)
 if key=='Public': n=public;f=val('1.5','D22')
 elif key=='Private':n=total-public;f=val('1.5','D37')
 else:n=stage[key]['n'];f=stage[key]['women']
 strat.append([label,es(100*f/n),f'{num(s["women"])} / {num(s["sex_n"])}',es(100*s['women']/s['sex_n']),delta(100*s['women']/s['sex_n']-100*f/n)])
table(['Mujeres dentro de...','Excel %','Mujeres / N muestra','Muestra %','Dif. pp'],strat,[154,70,126,80,75])
p('La menor proporción global de mujeres no desaparece al comparar por titularidad. La lectura por etapa es más sensible a cómo se agrupa Infantil. Como ilustración de esa sensibilidad, si se excluye del referente amplio únicamente Infantil exclusivo, la proporción de mujeres cambia de '+es(100*sum(a['women'] for a in stage.values())/stage_n)+' % a '+es(100*(sum(a['women'] for a in stage.values())-val('1.2','D8'))/(stage_n-val('1.2','B8')))+' %. Este último valor <b>no es una comparación equivalente</b> con nuestra muestra sin excluir también a sus docentes de Infantil exclusivo.')
p('Titularidad: las 187 respuestas de centros concertados y las 3 de privados se agrupan como no públicos para compararlas con «Centros Privados» del Excel; la equivalencia debe confirmarse con los metadatos de la fuente real. Sexo/género: el Excel publica hombres/mujeres y la encuesta pregunta género; 57 respuestas no tienen clasificación binaria disponible en la base limpia.','SmallA')

page('A.6. Balance del simulacro y límites de uso')
head('Qué enseña esta comparación')
p('Las distribuciones comparten rasgos generales, pero no coinciden: la muestra da más peso a centros públicos, a edades de 45-54 años y, entre códigos clasificables, a Guadalajara y Cuenca; da menos peso a Toledo. La proporción de mujeres es menor frente al universo global del Excel. Estas diferencias describen la composición respecto a un referente concreto, no su causa.')
metrics=[]
for var,label in [('sex','Sexo/género'),('ownership','Titularidad'),('age_band','Tramos de edad'),('province','Provincia'),('level','Rama educativa aproximada')]:
 a=next(z for z in diagnostics if z['cohort']=='main' and z['variable']==var);b=next(z for z in diagnostics if z['cohort']=='complete' and z['variable']==var)
 metrics.append([label,es(a['total_variation_pp']),es(b['total_variation_pp'])])
table(['Distancia descriptiva entre distribuciones','Muestra / Excel','Completos / Excel'],metrics,[305,100,100])
p('Distancia = mitad de la suma de diferencias absolutas entre porcentajes de categorías; 0 indica distribuciones idénticas. Se expresa en puntos porcentuales de masa de distribución. No es una prueba de representatividad ni se usa un umbral universal de aceptación. Etapa y provincia conservan las salvedades de comparabilidad descritas.','SmallA')
head('Qué no puede concluirse')
p('La proximidad de algunos márgenes no convierte una encuesta voluntaria en una muestra probabilística ni garantiza semejanza en creencias o decisiones. Tampoco identifica sesgo en los coeficientes del paper. No se calculan contrastes de significación bajo muestreo aleatorio ni se aplican pesos de calibración a los resultados del experimento.')
head('Qué debe confirmarse antes de un apéndice real')
p('Hay que fijar la población objetivo y el curso de referencia, confirmar la inclusión de Infantil/FP y concertada, aclarar qué cuenta como docente en ambas fuentes y resolver la clasificación provincial. El libro presenta 35.497 docentes en las hojas 1.1 y 1.5, pero 35.493 en la 1.2; no se corrigen ni se mezclan silenciosamente esos cuatro casos. Las hojas 1.7-1.9 se excluyen porque describen personal sin función docente, administrativo o de servicios.')
p('No se compara jornada ni categoría profesional: falta una variable armonizada equivalente en la muestra analítica utilizada. El Excel no proporciona experiencia, antigüedad, empatía ni creencias comparables. Las muestras de 2.705 y 2.156 corresponden al manuscrito congelado; la segunda puede cambiar al aplicar las revisiones pendientes.')
p('<b>Conclusión de uso:</b> el formato y los cálculos sirven como ensayo reproducible. La inclusión en un paper requiere la decisión posterior del equipo y la sustitución o confirmación del archivo de referencia. Este documento no modifica los manuscritos ni adelanta esa decisión.')

def footer(canvas,doc):
 canvas.saveState();canvas.setStrokeColor(colors.HexColor('#b9c8cf'));canvas.line(45, forty:=40,550,forty);canvas.setFont('Arial',8);canvas.setFillColor(colors.HexColor('#52636c'));canvas.drawString(45,27,'SIMULACRO INTERNO - Referencia hipotética - 26/09/2026');canvas.drawRightString(550,27,str(doc.page));canvas.restoreState()
pdf=PDFDIR/'simulacro_apendice_composicion_docente_2026-09-26.pdf'
doc=SimpleDocTemplate(str(pdf),pagesize=(595.28,841.89),rightMargin=45,leftMargin=45,topMargin=43,bottomMargin=52,title='Simulacro interno: composición de la muestra docente',author='Equipo COTEC - análisis interno')
doc.build(story,onFirstPage=footer,onLaterPages=footer)
savecsv('workbook_cell_audit.csv',audit)
manifest={'source_file':str(SOURCE),'source_sha256':hashlib.sha256(SOURCE.read_bytes()).hexdigest(),'sample_rds_md5':sample['input_md5'],'pdf':str(pdf),'reference_total':total,'stage_reference_total':stage_n,'sheet12_total':sheet12_total,'scope':'Hypothetical internal exercise, not official-source verification or manuscript incorporation'}
(OUT/'manifest.json').write_text(json.dumps(manifest,ensure_ascii=False,indent=2),encoding='utf-8')
print(json.dumps({'pdf':str(pdf),'overview':overview,'distances':diagnostics},ensure_ascii=True,indent=2))
