"""Build accepted Paper A tables/figures from author-approved revision aggregates."""
from pathlib import Path
import csv,json,math,subprocess
from io import BytesIO
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
R=Path(__file__).resolve().parents[2];O=R/'3. Output/paper_A_author_revision';D=R/'4. Latex/paper_A';T=D/'tables';F=R/'3. Output/paper_A/figures'
def rows(n,s='Main'):
 p=O/s/(n+'.csv') if s else O/'tables'/(n+'.csv')
 with p.open(encoding='utf-8-sig') as f:return list(csv.DictReader(f))
def n(x,d=2):return f'{float(x):.{d}f}'
def pp(x):return n(100*float(x))
def star(p):
 p=float(p);return '$^{'+('***' if p<.001 else '**' if p<.01 else '*')+'}$' if p<.05 else ''
def cell(r,p=None):return r'\shortstack{'+pp(r['estimate'])+star(r['p'] if p is None else p)+r'\\{\scriptsize ('+pp(r['se'])+')}}'
def write(name,heads,body,note,align=None):
 a=align or 'l'+'r'*(len(heads)-1);s='\\begin{tabular}{'+a+'}\n\\toprule\n'+' & '.join(heads)+r' \\'+'\n\\midrule\n';s+='\n'.join(' & '.join(map(str,row))+r' \\' for row in body);s+='\n\\bottomrule\n\\end{tabular}\n\\tabnote{'+note+'}\n';(T/(name+'.tex')).write_text(s,encoding='utf8')
labels={'male':'Male student','background':'Complex social background','failed':'Three or more failed subjects','competence':'Low basic competences','absent':'Frequent unjustified absence','disruptive':'Serious disciplinary problems','levelSecondary':'Secondary stage','experiencia':'Experience (years)','female':'Female teacher','edad':'Age (years)','public':'Public school','public_permanent':'Reported permanent public appointment','grupos_docencia':'Teaching groups','impact':'Perceived teacher impact (0--10)','empathy':'Empathy (1--5)','meritocracia':'Effort attribution (1--10)','resources_excess':'Excess resources for repeaters (0--10)','(Intercept)':'Constant','failed:competence':r'Failures $\times$ low competences','failed:absent':r'Failures $\times$ absence','failed:disruptive':r'Failures $\times$ discipline'}
attrs=['failed','competence','absent','disruptive','male','background'];inter=['failed:competence','failed:absent','failed:disruptive'];stars=r' $^{*}p<.05$, $^{**}p<.01$, $^{***}p<.001$. SE in parentheses.'
s=rows('summary')[0];a={r['term']:r for r in rows('attributes')};b={r['term']:r for r in rows('academic_interactions')}
write('attributes_merged',['Attribute','(1) Additive','(2) Interactions'],[[labels[v],cell(a[v]) if v in a else '--',cell(b[v],b[v]['p_holm'] if v in inter else b[v]['p'])] for v in attrs+inter]+[['Teacher fixed effects','Yes','Yes'],['Teachers','2,520','2,520'],['Decisions','20,160','20,160']],r'Coefficients in percentage points. Both specifications use the same eight decisions from each teacher. Teacher-clustered SE; the three interaction stars use Holm-adjusted p-values.'+stars,'p{6.4cm}rr')
a=rows('academic_categories');write('academic',['Failed subjects / competences',r'Repeat (\%)','SE',r'Disagree (\%)'],[[r['academic'].replace('Few failures','Fewer than three').replace('Many failures','Three or more'),pp(r['estimate']),pp(r['se']),pp(r['disagreement'])] for r in a],r'Observed category repetition rates with teacher-clustered SE. Disagreement is calculated between distinct teachers within each six-attribute profile, then averaged by response frequency within the category. Other attributes retain their observed category distributions. 2,520 teachers and 20,160 decisions.','p{7.2cm}rrr')
a=rows('tercile_descriptives');body=[]
for lab,key,scale in [('Teachers','n',1),('Mean harshness (pp)','mean_hb',100),('Age (years)','age',1),('Experience (years)','experience',1),(r'Secondary (\%)','secondary',100),(r'Female teachers (\%)','female',100),('Effort attribution (1--10)','effort',1),('Excess resources (0--10)','resources_excess',1)]:body.append([lab]+[str(r[key]) if key=='n' else n(float(r[key])*scale) for r in a])
write('tercile_summary',['Characteristic','Lower','Middle','Upper'],body,'Groups use Main-sample harshness and empirical tercile cutoffs. Ties produce unequal group sizes. Available covariate responses are used within each group; the complete-case regression sample is defined separately.')
a=rows('attributes_by_tercile');write('tercile_slopes',['Attribute','Lower','Middle','Upper'],[[labels[v]]+[cell(next(r for r in a if r['term']==v and r['model']==g)) for g in ['Lower','Middle','Upper']] for v in attrs],r'Separate within-teacher regressions for observed harshness terciles; coefficients in percentage points and teacher-clustered SE. These response-defined groups describe differences in attribute associations.'+stars,'p{6.1cm}rrr')
a=rows('characterization');models=['X1_common','X2_common','X3_common'];terms=[r['term'] for r in a if r['model']=='X3_common'];lookup=lambda m,v:next((r for r in a if r['model']==m and r['term']==v),None)
write('characterization',['Characteristic','(1)','(2)','(3)'],[[labels[v]]+[cell(r) if (r:=lookup(m,v)) else '--' for m in models] for v in terms]+[['Teachers']+['2,164']*3,[r'$R^2$ (\%)']+[pp(lookup(m,'(Intercept)')['r2']) for m in models]],r'Outcome: harshness, in percentage points, retaining the Main-sample profile references. Column (1) includes professional/demographic characteristics; (2) adds general beliefs; (3) adds excess-resource agreement. HC3 SE use a t reference with residual degrees of freedom. All columns use the same 2,164 teachers. Covariates retain their original units.'+stars,'p{6.5cm}rrr')
a=rows('four_outcomes');terms=[r['term'] for r in a if r['model']=='hb'];write('alternatives',['Characteristic',r'$H^a$',r'$H^b$',r'$H^c$',r'$H^d$'],[[labels[v]]+[cell(next(r for r in a if r['term']==v and r['model']==m)) for m in ['ha','hb','hc','hd']] for v in terms]+[['Teachers']+[next(r['n'] for r in a if r['model']==m) for m in ['ha','hb','hc','hd']],[r'$R^2$ (\%)']+[pp(next(r['r2'] for r in a if r['model']==m)) for m in ['ha','hb','hc','hd']]],r'Outcomes are in percentage points: $H^a$, raw repetition rate; $H^b$, same-profile reference; $H^c$, pooled-logit residual; $H^d$, logit residual additionally adjusted for stage and ownership. Same complete covariate cases and HC3 SE in all columns.'+stars,'p{5.6cm}rrrr')
# Common-teacher comparison retains Main profile references/cuts in characterization.
c={r['term']:r for r in rows('attributes','Common')};cs=rows('summary','Common')[0]
write('common_sample',['Attribute / quantity','Main sample','Complete covariates'],[[labels[v],cell(next(r for r in rows('attributes') if r['term']==v)),cell(c[v])] for v in attrs]+[['Teachers','2,520','2,164'],['Decisions','20,160','17,312'],[r'Repetition rate (\%)',pp(s['mean_repeat']),pp(cs['mean_repeat'])],[r'Same-profile disagreement (\%)',pp(s['disagreement']),pp(cs['disagreement'])]],'Attribute estimates use teacher fixed effects and teacher-clustered SE. Complete covariates restricts the same Main teachers to the common sample. Harshness and tercile membership retain Main-sample definitions.'+stars,'p{6.4cm}rr')
control=rows('attributes_control');write('context_comparison',['Attribute','Pooled','Control'],[[labels[v],cell(next(r for r in rows('attributes') if r['term']==v)),cell(next(r for r in control if r['term']==v))] for v in attrs],f"Teacher fixed effects and teacher-clustered SE. Pooled: 20,160 decisions, 2,520 teachers; control: {int(control[0]['n']):,} decisions, {int(control[0]['teachers']):,} teachers. Each teacher has eight cards."+stars,'p{6.4cm}rr')
comps=['Student','Professional','Beliefs','Harshness','Sensitivities','Residual'];labs=['Student information',r'\shortstack[l]{Professional /\\demographic}','Beliefs','Observed harshness','Attribute sensitivities','Residual']
mods=rows('covariance_candidates',None);mods=[next(r for r in mods if r['model']==m and r['optimizer']=='bobyqa') for m in ['diagonal','domains','full']]
write('covariance_audit',['Component','Diagonal','Three pairs','Unrestricted'],[[lab]+[pp(r[c]) for r in mods] for c,lab in zip(comps,labs)]+[['Covariance parameters','6','9','21'],['Boundary estimate','No','Yes','Yes'],['REML AIC']+[n(r['AIC']) for r in mods]],r'Same 2,164 teachers, 17,312 decisions and fixed design in all columns. Shares are percentages of model-implied variance. Both optimizers agree closely. Lower AIC indicates better working-model fit; correlated alternatives reach a covariance boundary. The diagonal structure is the primary interior approximation.','p{4.9cm}rrr')
sc=['Main','Partial','NoShared','NoScreen','ByID'];sm={m:rows('summary',m)[0] for m in sc};dc={m:{r['component']:r['share'] for r in rows('decomposition_point',m)} for m in sc}
write('reconstruction_sensitivity',['Quantity','Main',r'\shortstack{With\\partial}',r'\shortstack{No shared\\fields}',r'\shortstack{No\\screen}','By ID'],[['Teachers']+[sm[m]['teachers'] for m in sc],['Decisions']+[sm[m]['decisions'] for m in sc],['Harshness SD (pp)']+[pp(sm[m]['sd_hb']) for m in sc],[r'Harshness $R^2$ (\%)']+[pp(sm[m]['r2_full']) for m in sc]]+[[lab]+[pp(dc[m][c]) for m in sc] for c,lab in zip(comps,labs)],r'Main retains eight cards per teacher. With partial adds 84 records with fewer cards. No shared fields removes own-example/task shared fields within the Main cohort. No screen uses eight-card records before the favorable-example exclusion. By ID retains separate card identifiers in Main. References and harshness are rebuilt in each specification. Decomposition rows use each specification\textquotesingle s complete covariate sample.','p{3.7cm}rrrrr')
# Standardization and prediction: compact appendix tables.
a=rows('standardized_profiles',None);fc=['00','01','10','11'];names=['Fewer than three / adequate','Fewer than three / low','Three or more / adequate','Three or more / low'];stat=lambda r:pp(r['estimate'])+' ('+pp(r['se'])+')'
write('standardized_academic',['Failed subjects / competences',r'Repeat (\%)',r'Disagree (\%)'],[[name]+[stat(next(r for r in a if r['fc']==k and r['metric']==q)) for q in ['rate','disagreement']] for name,k in zip(names,fc)],r'Uniform weights on the same twelve combinations of gender, background, absence and discipline in all four academic categories: 48 profiles, 17,636 decisions from the Main cohort of 2,520 teachers. Teacher-bootstrap SE in parentheses (1,999 draws).','p{7cm}rr')
a=rows('standardized_contrasts',None);cn=['Rate: failures with adequate versus few failures with low competences','Disagreement: same contrast','Rate complementarity (difference of differences)'];write('standardized_contrasts',['Contrast','Estimate (pp)','Bootstrap SE'],[[name,pp(r['estimate']),pp(r['se'])] for name,r in zip(cn,a)],r'The three prespecified contrasts are jointly positive under approximate 95\% simultaneous bootstrap bands based on the maximum standardized deviation. Calculations use the same common support and 1,999 teacher draws as Table~\ref{tab:standardized}.','p{8.5cm}rr')
configs=['B_random','B_ordered','C_random','NoScreen_random','Matched_random'];cl=['Four random / four reserved','First four / last four','No shared fields','No favorable-example screen','Full observed teacher information'];body=[]
for key,name in zip(configs,cl):
 a=rows('prediction_intervals_'+key,None);a={r['metric']:r for r in a};body.append([name,a['brier_gain']['teachers'],n(a['brier_base']['estimate'],5),n(a['brier_history']['estimate'],5),r'\shortstack{'+n(a['brier_gain']['estimate'],5)+r'\\{\scriptsize ('+n(a['brier_gain']['se'],5)+')}}',r'\shortstack{'+n(a['log_gain']['estimate'],5)+r'\\{\scriptsize ('+n(a['log_gain']['se'],5)+')}}'])
write('prediction_validation',['Comparison','Teachers',r'\shortstack{Brier\\baseline}',r'\shortstack{Brier\\+ history}',r'\shortstack{Brier\\reduction}',r'\shortstack{Log-loss\\reduction}'],body,r'Paired five-fold teacher-level validation. Both predictors include the same profile-frequency offset and treatment/set/stage controls; the last row additionally includes all observed teacher covariates in both predictors. History uses four cards in complete-eight rows; no shared fields uses floor(n/2) histories among records with at least four nonshared responses. Teacher-bootstrap SE for reductions in parentheses, 399 full pipeline refits; folds and partitions held fixed.','p{4.3cm}rrrrr')
# Keep the design table, with simplified profile support description.
p=T/'design_sets.tex';txt=p.read_text(encoding='utf8');txt=txt.replace('There are 56 distinct profiles across the 64 positions; eight profiles occur twice.','The implemented design covers 56 of the 64 possible six-attribute profiles.');p.write_text(txt,encoding='utf8')
# Scientific figures retain the approved wide solid histogram and population grid.
plt.rcParams.update({'font.size':11,'axes.spines.top':False,'axes.spines.right':False})
def save(fig,name):
 buf=BytesIO();fig.savefig(buf,format='png',dpi=230,bbox_inches='tight',facecolor='white');plt.close(fig);p=F/(name+'.png');b=buf.getvalue()
 if not p.exists() or p.read_bytes()!=b:p.write_bytes(b)
a=rows('harshness_histogram');fig,ax=plt.subplots(figsize=(10,4.3))
for r in a:ax.bar(float(r['left']),float(r['share']),width=float(r['right'])-float(r['left']),align='edge',color='#215e70',edgecolor='white',linewidth=.5)
ax.axvline(0,color='#555',ls='--',lw=.9);ax.set(xlim=(-100,75),xlabel='Harshness (percentage points)',ylabel='Teachers (%)',xticks=[-75,-50,-25,0,25,50,75]);fig.tight_layout();save(fig,'harshness_distribution')
# Original eligible-cohort progression, unaffected by complete-eight analysis selection.
a=rows('task_progression');pos=[int(r['position']) for r in a];fig,axs=plt.subplots(1,2,figsize=(9.5,3.5));axs[0].plot(pos,[100*float(r['response_rate']) for r in a],'o-',color='#215e70');axs[0].set(xlabel='Recorded card position',ylabel='Cards answered (%)',ylim=(0,102),xticks=pos,title='A. Percentage of cards answered');axs[1].plot(pos,[float(r['median_seconds']) for r in a],'o-',color='#215e70');axs[1].fill_between(pos,[float(r['p25']) for r in a],[float(r['p75']) for r in a],alpha=.15,color='#215e70');axs[1].set(xlabel='Recorded card position',ylabel='Page-submit time (seconds)',xticks=pos,title='B. Median and interquartile range');fig.tight_layout();save(fig,'task_progression')
# Same progression cohort/data: retain the exact baseline rendering.
(F/'task_progression.png').write_bytes(subprocess.check_output(['git','show','8306e22:3. Output/paper_A/figures/task_progression.png'],cwd=R))
a=rows('population_comparison');fig=plt.figure(figsize=(10,7.8));gs=fig.add_gridspec(3,2,height_ratios=[1,1.5,1],hspace=.72,wspace=.68)
ages=['20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65+'];prov=['Albacete','Ciudad Real','Cuenca','Guadalajara','Toledo'];panels=[(gs[0,0],'sex',['Women','Men'],['Women','Men'],'A. Sex/gender',100),(gs[0,1],'ownership',['Public','Private'],['Public','Private*'],'B. School ownership',100),(gs[1,0],'age_band',ages,ages,'C. Age',35),(gs[1,1],'province',prov,['Province A','Province B','Province C','Province D','Province E'],'D. Province (anonymized)',45),(gs[2,:],'level',['Primary','Secondary'],['Infant/primary','Secondary'],'E. Educational stage (approximation)',100)]
for spec,v,cats,lab,title,limit in panels:
 ax=fig.add_subplot(spec);yy=np.arange(len(cats))
 for j,(co,legend,col,marker) in enumerate([(None,'Population 2023/24','#7d8791','s'),('main','Eight-card teachers','#215e70','o'),('complete','Complete covariates','#b46630','^')]):
  vals=[float(next(r['reference_pct'] if co is None else r['sample_pct'] for r in a if r['variable']==v and r['category']==c and (co is None or r['cohort']==co))) for c in cats];ax.scatter(vals,yy+(j-1)*.18,s=29,color=col,marker=marker,label=legend)
 ax.set_yticks(yy,lab);ax.set_ylim(len(cats)-.5,-.5);ax.set_xlim(0,limit);ax.set_xlabel('Percent');ax.set_title(title,loc='left',fontweight='bold',fontsize=12,pad=24);ax.grid(axis='x',alpha=.15)
 refn=int(float(next(r['reference_n'] for r in a if r['variable']==v)));mn=[int(next(r['sample_n'] for r in a if r['variable']==v and r['cohort']==co)) for co in ['main','complete']];ax.text(0,1.04,f'N: {refn:,} / {mn[0]:,} / {mn[1]:,}',transform=ax.transAxes,fontsize=9,color='#52636c')
h,l=ax.get_legend_handles_labels();fig.legend(h,l,loc='upper center',ncol=3,frameon=False,fontsize=10);fig.subplots_adjust(left=.15,right=.98,top=.87,bottom=.07);save(fig,'population_grid')
# Two labelled flows separate questionnaire progress from analytical sample selection.
fig,ax=plt.subplots(figsize=(10.5,6.8));ax.set(xlim=(0,14),ylim=(0,11));ax.axis('off');ax.text(3.1,10.65,'A. Questionnaire progression',ha='center',weight='bold');ax.text(10.3,10.65,'B. Analysis sample',ha='center',weight='bold')
def box(x,y,text,w=4.6):
 ax.add_patch(FancyBboxPatch((x-w/2,y-.44),w,.88,boxstyle='round,pad=.02',facecolor='#eaf1f4',edgecolor='#687d88',lw=.8));ax.text(x,y,text,ha='center',va='center',fontsize=10)
def arrow(x,y1,y2,label=''):
 ax.annotate('',xy=(x,y2+.48),xytext=(x,y1-.48),arrowprops={'arrowstyle':'->','color':'#687d88'});
 if label:ax.text(x+2.48,(y1+y2)/2,label,ha='left',va='center',fontsize=8.5,color='#81563c')
left=[('Substantive survey entries\n4,037',9.7),('Reached the student cards\n3,103',7.8),('Reached the end of card block\n2,928',5.9),('Retained at card-block end\n2,580',4.0),('Completed final questionnaire\n2,362',2.1)]
right=[('Responded to student cards\n3,103',9.7),('Passed favorable-example screen\n2,724',7.8),('Recoverable experimental responses\n2,604',5.9),('Eight-card main sample\n2,520',4.0),('Complete analysis covariates\n2,164',2.1)]
for x,items,labelsx in [(2.5,left,['934 before\ncards','175 during\ncard block','348 screen\nexclusions','218 after\ncard block']),(9.8,right,['379 screen\nexclusions','120 without\nusable sequence/response','84 with fewer\nthan eight cards','356 incomplete\ncovariates'])]:
 for text,y in items:box(x,y,text,4.4)
 for ((_,y1),(_,y2)),lab in zip(zip(items,items[1:]),labelsx):arrow(x,y1,y2,lab)
ax.text(.2,.65,'Progression uses the recorded block-closing flag.\nThe analytical sample uses recovered card responses.',fontsize=9,color='#52636c');ax.text(7.4,.65,'Analysis exclusions are applied to recorded responses.\nMissing covariates may occur without survey exit.',fontsize=9,color='#52636c');save(fig,'sample_flow')
# Updated six-share plot waits until bootstrap output exists.
p=O/'Main/decomposition.csv'
if p.exists():
 a=rows('decomposition');ll=dict(zip(comps,['Student attributes + interactions','Professional / demographic','Beliefs','Observed harshness','Teacher attribute sensitivities','Remaining decision variation']));colors=['#215e70','#548d9d','#93b9b6','#be713f','#8c637b','#dfd6ca'];fig,axs=plt.subplots(2,1,figsize=(9,4.8),gridspec_kw={'height_ratios':[.6,3.3]});left=0
 for r,col in zip(a,colors):
  v=100*float(r['share']);axs[0].barh(0,v,left=left,color=col,height=.6)
  if v>5:axs[0].text(left+v/2,0,f'{v:.1f}%',ha='center',va='center',fontsize=10,color='black' if col==colors[-1] else 'white')
  left+=v
 axs[0].set(xlim=(0,100),yticks=[],xticks=[])
 for i,(r,col) in enumerate(zip(a,colors)):
  v=100*float(r['share']);se=100*float(r['se']);axs[1].plot([v-se,v+se],[i,i],color=col,lw=2);axs[1].scatter(v,i,color=col,s=40);axs[1].text(v+se+1,i,f'{v:.2f}% (SE {se:.2f})',va='center',fontsize=10)
 axs[1].set_yticks(range(6),[ll[r['component']] for r in a]);axs[1].invert_yaxis();axs[1].set(xlabel='Share of model-implied variance (%)',xlim=(0,69));axs[1].grid(axis='x',alpha=.15);fig.tight_layout();save(fig,'variance_partition')
print('Built current author-revision tables and figures.')
