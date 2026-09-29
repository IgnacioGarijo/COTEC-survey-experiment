"""Generate manuscript tables and numerical tokens directly from revised aggregate results."""
from pathlib import Path
import csv,json,shutil,math
ROOT=Path(__file__).resolve().parents[1];OUT=ROOT/'3. Output/paper_A';DEST=ROOT/'4. Latex/paper_A';TAB=DEST/'tables';TAB.mkdir(parents=True,exist_ok=True)
def rows(n):
 with (OUT/'tables'/f'{n}.csv').open(encoding='utf-8-sig') as f:return list(csv.DictReader(f))
def n(x,d=2):return f'{float(x):.{d}f}'
def pp(x):return n(100*float(x))
def esc(s):return str(s).replace('_',r'\_').replace('%',r'\\%').replace('&',r'\&')

stars_note=r' $^{*}p<.05$, $^{**}p<.01$, $^{***}p<.001$. SE in parentheses.'
def stars(r):
 p=float(r['p']) if r.get('p') not in (None,'NA','') else math.erfc(abs(float(r['estimate'])/float(r['se']))/math.sqrt(2))
 return r'$^{'+('***' if p<.001 else '**' if p<.01 else '*' if p<.05 else '')+'}$' if p<.05 else ''
def cell(r):return r'\shortstack{'+pp(r['estimate'])+stars(r)+r'\\{\scriptsize ('+pp(r['se'])+')}}'
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
 r=lookup(a,model='All',term=v);c=lookup(a,model='Control',term=v);data.append([labels[v],cell(r),cell(c)])
write('attributes',['Attribute','All','Control'],data,'Coefficients in percentage points from additive teacher fixed-effect models; teacher-clustered SE. Full sample: 22,006 decisions from 2,587 teachers; control: 2,292 from 274.'+stars_note)
a=rows('academic_interactions')
for r in a:
 if r['p_holm']!='NA':r['p']=r['p_holm']
write('interactions',['Term','Coefficient (SE)'],[[labels[r['term']],cell(r)] for r in a],'Teacher fixed effects, 22,006 decisions, 2,587 teachers. Teacher-clustered SE. Stars for the three interactions use Holm-adjusted p-values. All coefficients are percentage points.'+stars_note)
a=rows('academic_categories');write('academic',['Failed subjects / competences','Repeat (\\%)','SE','Disagree (\\%)'],[[r['academic'].replace('Few failures','Fewer than three').replace('Many failures','Three or more'),pp(r['estimate']),pp(r['se']),pp(r['disagreement'])] for r in a],'Observed category rates with teacher-clustered SE. Disagreement compares two distinct teachers evaluating the same identifier and averages those probabilities by identifier frequency within category. Other attributes are not standardized. No zero-mean hypothesis tests are attached to descriptive rates.','p{5.5cm}rrr')
a=rows('tercile_descriptives');fields=[('n','Teachers',1,0),('mean_hb','Mean harshness (pp)',100,2),('mean_ha','Repetition recommendations (\\%)',100,2),('age','Age (years)',1,2),('experience','Experience (years)',1,2),('secondary','Secondary teachers (\\%)',100,1),('female','Female teachers (\\%)',100,1),('effort','Effort attribution (1--10)',1,2),('resources_excess','Excess resources (0--10)',1,2)]
write('tercile_summary',['Characteristic','Lower','Middle','Upper'],[[label]+[n(float(r[k])*mult,dec) for r in a] for k,label,mult,dec in fields],'Terciles use empirical one-third and two-thirds cutoffs of harshness, retaining ties. Characteristic means use available responses within each tercile. Higher repetition shares partly follow from the group definition.')
a=rows('attributes_by_tercile');data=[]
for v in attrs:
 line=[labels[v]]
 for g in ['Lower','Middle','Upper']:
  r=lookup(a,model=g,term=v);line.append(cell(r))
 data.append(line)
write('tercile_slopes',['Attribute','Lower','Middle','Upper'],data,'Separate within-teacher regressions within observed harshness terciles; coefficients in percentage points and teacher-clustered SE.'+stars_note)
def coef_table(filename,models,outname,note):
 a=rows(filename);data=[]
 terms=[r['term'] for r in a if r['model']==models[-1]]
 for v in terms:
  line=[labels.get(v,esc(v))]
  for m in models:
   rr=[r for r in a if r['term']==v and r['model']==m]
   if rr:
    r=rr[0];line.append(cell(r))
   else:line.append('--')
  data.append(line)
 data.extend([['Teachers']+[str(int(float(lookup(a,model=m,term='(Intercept)')['n']))) for m in models],['$R^2$ (\\%)']+[pp(lookup(a,model=m,term='(Intercept)')['r2']) for m in models]])
 headers=['Characteristic']+(['$X_1$','$X_2$','$X_3$'] if len(models)==3 else ['$H^a$','$H^b$','$H^c$','$H^d$'])
 write(outname,headers,data,note+stars_note+' HC3 stars use the asymptotic normal reference.',('p{5.6cm}'+'r'*len(models)))
coef_table('characterization',['X1_common','X2_common','X3_common'],'characterization','Outcome: harshness, in percentage points. HC3 SE. Same 2,157 teachers in every column. Age and experience enter jointly; resource agreement is in original 0--10 units.')
coef_table('four_outcomes',['ha','hb','hc','hd'],'alternatives','Identical 2,157 complete cases and $X_3$ covariates. Coefficients in percentage points, HC3 SE. $H^a$: raw mean; $H^b$: same-card reference; $H^c$: attribute-logit residual; $H^d$: residual additionally adjusted for stage and ownership.')
a=rows('nested_prediction_summary');names={'Baseline':'Training mean','OLS_X1':'OLS: professional/demographic','OLS_X2':'OLS: + general beliefs','OLS_X3':'OLS: + excess resources','Ridge':'Ridge: all covariates','RF_tuned':'Tuned random forest'}
for pop,file in [('Common','prediction'),('X1 available','prediction_available')]:
 write(file,['Model','RMSE (pp)','$R^2$ (\\%)','Partition range'],[[names[r['model']],pp(r['rmse']),pp(r['r2']),f"[{pp(r['r2_min'])}, {pp(r['r2_max'])}]"] for r in a if r['population']==pop],'Five repetitions of five teacher folds. Targets and tuning are rebuilt within training samples. Partition ranges describe variation across five repetitions, not confidence intervals. N='+('2,157' if pop=='Common' else '2,642')+'.')
a=rows('prediction_paired_corrected');write('prediction_paired',['Comparison','MSE difference','SE'],[[esc(r['contrast']),n(float(r['mse_difference'])*10000)+stars(r),n(float(r['se'])*10000)] for r in a if r['population']=='Common'],'Differences in squared percentage points. Approximate overlap-corrected SE over 25 paired folds; stars use a t reference with 24 degrees of freedom. Positive forest-minus-OLS values favor OLS.'+stars_note.replace(' SE in parentheses.',''))
a=rows('nested_shap');b=rows('nested_permutation_importance');write('prediction_importance',['Predictor','Mean $|$SHAP$|$ (pp)','Permutation MSE'],[[labels.get(r['variable'],esc(r['variable'])),pp(r['mean_abs_shap']),n(float(lookup(b,variable=r['variable'])['mse_increase'])*10000)] for r in sorted(a,key=lambda r:-float(r['mean_abs_shap']))],'Held-out forest diagnostics from the first outer partition. Permutation increases are in squared percentage points. These describe the fitted prediction function, whose overall accuracy is modest; they are not coefficient estimates.')
a=rows('characterization_bootstrap');base=rows('characterization')
for r in a:r['estimate']=lookup(base,model='X3_common',term=r['term'])['estimate']
write('bootstrap',['Characteristic','Coefficient (bootstrap SE)'],[[labels.get(r['term'],esc(r['term'])),cell(r)] for r in a],'999 teacher resamples, rebuilding card reference rates and refitting $X_3$. SE are the standard deviations of bootstrap estimates. Coefficients are the original sample estimates, in percentage points. Stars use an approximate normal Wald test with bootstrap SE.'+stars_note)
if (OUT/'tables/variance_partition.csv').exists():
 a=rows('variance_sensitivity');mods=list(dict.fromkeys(r['model'] for r in a));components=list(dict.fromkeys(r['component'] for r in a));lab={'Student':'Student attributes','Professional':'Professional/demographic','Beliefs':'Beliefs/attitudes','Context':'Assignment context','Teacher_residual':'Residual teacher component','Decision_residual':'Remaining decision variation'}
 write('variance_sensitivity',['Component','Main linear','Eight or more','Additive','Logit latent'],[[lab[c]]+[pp(lookup(a,model=m,component=c)['share']) for m in mods] for c in components],'Percentages sum to 100 within each column. Linear columns use a model-implied response-scale denominator; the logistic column uses a latent-scale denominator including $\\pi^2/3$. These scales are not numerically equivalent. The main figure reports teacher-bootstrap uncertainty.','p{5cm}rrrr')
print('Tables built in',TAB)
