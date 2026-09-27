"""Generate manuscript tables and numerical tokens directly from revised aggregate results."""
from pathlib import Path
import csv,json,shutil
ROOT=Path(__file__).resolve().parents[1];OUT=ROOT/'3. Output/paper_A_2026-09-27';DEST=ROOT/'4. Latex/paper_A_2026-09-27';TAB=DEST/'tables';TAB.mkdir(parents=True,exist_ok=True)
def rows(n):
 with (OUT/'tables'/f'{n}.csv').open(encoding='utf-8-sig') as f:return list(csv.DictReader(f))
def n(x,d=2):return f'{float(x):.{d}f}'
def pp(x):return n(100*float(x))
def esc(s):return str(s).replace('_',r'\_').replace('%',r'\\%').replace('&',r'\&')
def write(name,headers,data,note,widths=None):
 align=widths or ('l'+'r'*(len(headers)-1))
 text='\\begin{tabular}{'+align+'}\n\\toprule\n'+' & '.join(headers)+r' \\'+'\n\\midrule\n'
 text+='\n'.join(' & '.join(map(str,r))+r' \\' for r in data)
 text+='\n\\bottomrule\n\\end{tabular}\n\\tabnote{'+note+'}\n';(TAB/f'{name}.tex').write_text(text,encoding='utf-8')
labels={'male':'Male student','background':'Complex social background','failed':'Three or more failed subjects','competence':'Low basic competences','absent':'Frequent unjustified absence','disruptive':'Serious disciplinary problems','level':'Educational stage','levelSecondary':'Secondary stage','experiencia':'Experience (years)','female':'Female teacher','edad':'Age (years)','public':'Public school','public_permanent':'Reported permanent public appointment','grupos_docencia':'Teaching groups','impact':'Perceived teacher impact (0--10)','empathy':'Empathy (1--5)','meritocracia':'Effort attribution (1--10)','resources_excess':'Excess resources for repeaters (0--10)','(Intercept)':'Constant','failed:competence':'Failures $\\times$ low competences','failed:absent':'Failures $\\times$ absence','failed:disruptive':'Failures $\\times$ discipline'}
attrs=['failed','competence','absent','disruptive','male','background']
a=rows('attributes');lookup=lambda rr,**kw:next(r for r in rr if all(r[k]==v for k,v in kw.items()))
data=[]
for v in attrs:
 r=lookup(a,model='All',term=v);c=lookup(a,model='Control',term=v);data.append([labels[v],pp(r['estimate']),f"[{pp(r['lo'])}, {pp(r['hi'])}]",pp(c['estimate']),f"[{pp(c['lo'])}, {pp(c['hi'])}]"])
write('attributes',['Attribute','All','95\\% CI','Control','95\\% CI'],data,'Coefficients in percentage points from additive teacher fixed-effect models. Intervals cluster by teacher. Full sample: 22,006 identifying decisions from 2,587 teachers; control: 2,292 from 274. Teachers with only one retained response do not identify within-teacher coefficients.')
a=rows('academic_interactions');write('interactions',['Term','Coefficient','95\\% CI'],[[labels[r['term']],pp(r['estimate']),f"[{pp(r['lo'])}, {pp(r['hi'])}]"] for r in a],'Teacher fixed effects, 22,006 decisions, 2,587 teachers. Teacher-clustered 95\\% intervals. Holm-adjusted p-values for the three interactions: $<0.001$, 0.663 and 0.683, respectively. All coefficients are percentage points.')
a=rows('academic_categories');write('academic',['Failed subjects / competences','Repeat (\\%)','95\\% CI','Disagree (\\%)'],[[r['academic'].replace('Few failures','Fewer than three').replace('Many failures','Three or more'),pp(r['estimate']),f"[{pp(r['lo'])}, {pp(r['hi'])}]",pp(r['disagreement'])] for r in a],'Observed category rates with teacher-clustered intervals. Disagreement compares two distinct teachers evaluating the same identifier and averages those probabilities by identifier frequency within category. Other attributes are not standardized across these categories.','p{5.5cm}rrr')
a=rows('tercile_descriptives');fields=[('n','Teachers',1,0),('mean_hb','Mean harshness (pp)',100,2),('mean_ha','Repetition recommendations (\\%)',100,2),('age','Age (years)',1,2),('experience','Experience (years)',1,2),('secondary','Secondary teachers (\\%)',100,1),('female','Female teachers (\\%)',100,1),('effort','Effort attribution (1--10)',1,2),('resources_excess','Excess resources (0--10)',1,2)]
write('tercile_summary',['Characteristic','Lower','Middle','Upper'],[[label]+[n(float(r[k])*mult,dec) for r in a] for k,label,mult,dec in fields],'Terciles use empirical one-third and two-thirds cutoffs of harshness, retaining ties. Characteristic means use available responses within each tercile. Higher repetition shares partly follow from the group definition.')
a=rows('attributes_by_tercile');data=[]
for v in attrs:
 line=[labels[v]]
 for g in ['Lower','Middle','Upper']:
  r=lookup(a,model=g,term=v);line.append(r'\shortstack{'+pp(r['estimate'])+r'\\{\scriptsize ['+pp(r['lo'])+', '+pp(r['hi'])+']}}')
 data.append(line)
write('tercile_slopes',['Attribute','Lower','Middle','Upper'],data,'Separate within-teacher regressions within observed harshness terciles; coefficients in percentage points and teacher-clustered 95\\% intervals. Descriptive slopes use the same responses as the grouping and do not identify three exogenous teacher types.')
def coef_table(filename,models,outname,note):
 a=rows(filename);data=[]
 terms=[r['term'] for r in a if r['model']==models[-1]]
 for v in terms:
  line=[labels.get(v,esc(v))]
  for m in models:
   rr=[r for r in a if r['term']==v and r['model']==m]
   if rr:
    r=rr[0];line.append(r'\shortstack{'+pp(r['estimate'])+r'\\{\scriptsize ['+pp(r['lo'])+', '+pp(r['hi'])+']}}')
   else:line.append('--')
  data.append(line)
 data.extend([['Teachers']+[str(int(float(lookup(a,model=m,term='(Intercept)')['n']))) for m in models],['$R^2$ (\\%)']+[pp(lookup(a,model=m,term='(Intercept)')['r2']) for m in models]])
 headers=['Characteristic']+(['$X_1$','$X_2$','$X_3$'] if len(models)==3 else ['$H^a$','$H^b$','$H^c$','$H^d$'])
 write(outname,headers,data,note,('p{5.6cm}'+'r'*len(models)))
coef_table('characterization',['X1_common','X2_common','X3_common'],'characterization','Outcome: harshness, in percentage points. HC3 95\\% intervals. Same 2,157 teachers in every column. Age and experience enter jointly; resource agreement is in original 0--10 units. School tenure is excluded.')
coef_table('four_outcomes',['ha','hb','hc','hd'],'alternatives','Identical 2,157 complete cases and $X_3$ covariates. Coefficients in percentage points, HC3 95\\% intervals. $H^a$: raw mean; $H^b$: same-card reference; $H^c$: attribute-logit residual; $H^d$: residual additionally adjusted for stage and ownership.')
a=rows('nested_prediction_summary');names={'Baseline':'Training mean','OLS_X1':'OLS: professional/demographic','OLS_X2':'OLS: + general beliefs','OLS_X3':'OLS: + excess resources','Ridge':'Ridge: all covariates','RF_tuned':'Tuned random forest'}
for pop,file in [('Common','prediction'),('X1 available','prediction_available')]:
 write(file,['Model','RMSE (pp)','$R^2$ (\\%)','Partition range'],[[names[r['model']],pp(r['rmse']),pp(r['r2']),f"[{pp(r['r2_min'])}, {pp(r['r2_max'])}]"] for r in a if r['population']==pop],'Five repetitions of five teacher folds. Targets and tuning are rebuilt within training samples. Partition ranges describe variation across five repetitions, not confidence intervals. N='+('2,157' if pop=='Common' else '2,642')+'.')
a=rows('prediction_paired_corrected');write('prediction_paired',['Comparison','MSE difference','95\\% interval'],[[esc(r['contrast']),n(float(r['mse_difference'])*10000),f"[{n(float(r['lo'])*10000)}, {n(float(r['hi'])*10000)}]"] for r in a if r['population']=='Common'],'Differences in squared percentage points. Approximate overlap-corrected intervals over 25 paired folds. Positive forest-minus-OLS values favor OLS.')
a=rows('nested_shap');b=rows('nested_permutation_importance');write('prediction_importance',['Predictor','Mean $|$SHAP$|$ (pp)','Permutation MSE'],[[labels.get(r['variable'],esc(r['variable'])),pp(r['mean_abs_shap']),n(float(lookup(b,variable=r['variable'])['mse_increase'])*10000)] for r in sorted(a,key=lambda r:-float(r['mean_abs_shap']))],'Held-out forest diagnostics from the first outer partition. Permutation increases are in squared percentage points. These describe the fitted prediction function, whose overall accuracy is modest; they are not coefficient estimates.')
a=rows('characterization_bootstrap');write('bootstrap',['Characteristic','Teacher-bootstrap 95\\% interval'],[[labels.get(r['term'],esc(r['term'])),f"[{pp(r['lo'])}, {pp(r['hi'])}]"] for r in a],'999 teacher resamples, rebuilding card reference rates and refitting $X_3$ each time. Coefficients in percentage points; complements the HC3 intervals in the main text.')
if (OUT/'tables/variance_partition.csv').exists():
 a=rows('variance_sensitivity');mods=list(dict.fromkeys(r['model'] for r in a));components=list(dict.fromkeys(r['component'] for r in a));lab={'Student':'Student attributes','Professional':'Professional/demographic','Beliefs':'Beliefs/attitudes','Context':'Assignment context','Teacher_residual':'Residual teacher component','Decision_residual':'Remaining decision variation'}
 write('variance_sensitivity',['Component','Main linear','Eight or more','Additive','Logit latent'],[[lab[c]]+[pp(lookup(a,model=m,component=c)['share']) for m in mods] for c in components],'Percentages sum to 100 within each column. Linear columns use a model-implied response-scale denominator; the logistic column uses a latent-scale denominator including $\\pi^2/3$. These scales are not numerically equivalent. The main figure reports teacher-bootstrap uncertainty.','p{5cm}rrrr')
print('Tables built in',TAB)
