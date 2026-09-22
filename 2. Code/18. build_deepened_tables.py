from pathlib import Path
import pandas as pd
import shutil
ROOT=Path(__file__).resolve().parents[1]
OLD=ROOT/'4. Latex/two_papers_2026-09-21'
DEST=ROOT/'4. Latex/two_papers_2026-09-22'
DATA=ROOT/'3. Output/two_papers_2026-09-22/tables'
(DEST/'tables').mkdir(parents=True,exist_ok=True)
for p in (OLD/'tables').glob('*.tex'): shutil.copy2(p,DEST/'tables'/p.name)
for n in ['preamble.tex','references_A.tex','references_B.tex','provenance_blue.json']: shutil.copy2(OLD/n,DEST/n)
p=(DEST/'preamble.tex').read_text(encoding='utf8').replace('2026-09-21','2026-09-22').replace('21 September 2026','22 September 2026')
p+=r'\renewcommand{\floatpagefraction}{0.8}'+'\n'+r'\renewcommand{\topfraction}{0.9}'+'\n'+r'\renewcommand{\textfraction}{0.07}'+'\n'
p+=r'\makeatletter\newcommand{\sectionfloatbarrier}{\ifx\@deferlist\@empty\else\clearpage\fi}\makeatother'+'\n'
# Preserve page format so the expansion reflects content, not larger type.
(DEST/'preamble.tex').write_text(p,encoding='utf8')
def rd(n):return pd.read_csv(DATA/f'{n}.csv')
def f(x):return f'{100*x:.2f}'
def esc(x):return str(x).replace('_',r'\_').replace('%',r'\%').replace('&',r'\&')
def ci(r,level=''):return '['+f(getattr(r,'lo'+level))+', '+f(getattr(r,'hi'+level))+']'
def cell(r):return r'\shortstack{'+f(r.estimate)+r'\\{'+ci(r)+'}}'
def pv(x):return '$<0.001$' if x<.001 else f'{x:.3f}'
def short(s):return s.replace('Awareness','A').replace('Revelation','R').replace('Policy','P').replace('Control','C')
def table(name,headers,rows,note,spec=None):
    spec=spec or '@{}p{5.2cm}'+'r'*(len(headers)-1)+'@{}'
    text='\\begin{tabular}{'+spec+'}\n\\toprule\n'+' & '.join(headers)+r' \\'+'\n\\midrule\n'
    text+='\n'.join(' & '.join(map(str,row))+r' \\'+(r'\addlinespace[2pt]' if any('shortstack' in str(x) for x in row) else '') for row in rows)
    text+='\n\\bottomrule\n\\end{tabular}\n\\tabnote{'+note+'}\n'
    (DEST/'tables'/f'{name}.tex').write_text(text,encoding='utf8')
names={'levelSecondary':'Secondary stage','experiencia':'Experience (year)','antiguedad3':'School tenure (year)','female':'Female teacher','edad':'Age (year)','public':'Public school','public_permanent':'Permanent public appointment','grupos_docencia':'Teaching groups','impact':'Perceived teacher impact','empathy':'Empathy','meritocracia':'Effort attribution','resources':'Resource skepticism (SD)'}
a=rd('characterization');models=['X1_common','X2_common','Resources_common'];keys=list(names)
rows=[]
for key in keys:
    rows.append([names[key]]+[cell(z.iloc[0]) if len(z:=a[(a.model==m)&(a.term==key)]) else '--' for m in models])
rows += [['N']+['2,156']*3,[r'$R^2$']+[f'{a[a.model==m].iloc[0].r2:.4f}'for m in models]]
table('A_characterization_full',['Predictor',r'$X_1$',r'$X_2$',r'$X_3$'],rows,r'Coefficients in percentage points, HC3 95\% intervals below. Same complete-case sample in every column. $X_3=X_2+$resource skepticism. Available-sample $X_1$ uses 2,641 teachers and has $R^2=0.0189$. All original scale definitions are retained.','@{}p{4.5cm}rrr@{}')
a=rd('four_outcomes');rows=[]
for key in keys:rows.append([names[key]]+[cell(a[(a.model==m)&(a.term==key)].iloc[0])for m in ['ha','hb','hc','hd']])
table('A_four_outcomes',['Predictor',r'$H^a$',r'$H^b$',r'$H^c$',r'$H^d$'],rows,r'All columns: 2,156 complete cases, all $X_3$ predictors, HC3 intervals, percentage points. $H^d$ subtracts fitted probabilities from a pooled logit including six attributes, stage and three-category ownership; it has already adjusted for two characterization predictors.','@{}p{3.6cm}rrrr@{}')
a=rd('coherence_permutation')
table('A_coherence',['Sample','N','Observed SD','Reference SD','Ratio'],[[r['sample'],int(r.n_teachers),f(r.observed_sd),f(r.null_sd),f'{r.variance_ratio:.2f}']for _,r in a.iterrows()],r'SDs in percentage points. Reference SD is the square root of mean permuted variance; ratio is observed variance divided by mean permuted variance. Each sample recomputes its own card benchmarks. 1,999 permutations within card identifier and assignment cell preserve exposure and response counts. One-sided $p=0.0005$ in each sample. These are internal exchangeability comparisons, not reliability estimates.','@{}lrrrr@{}')
a=rd('academic_categories')
labels=['Few failures, adequate competences','Few failures, low competences','Many failures, adequate competences','Many failures, low competences']
table('A_academic',['Observed academic information','Repetition', 'Disagreement','Responses'],[[lab,cell(r),f(r.disagreement),f'{r.decisions:,}']for lab,r in zip(labels,a.itertuples())],r'Percentages; repetition entries include teacher-clustered 95\% intervals. Many failures means three or more failed subjects. Disagreement is computed within each exact identifier, then weighted by its response frequency within the category. The categories contain 18, 12, 12 and 20 observed identifiers. Other attributes are not held fixed in the category means.','@{}p{5.8cm}rrr@{}')
a=rd('academic_interactions');labs={'male':'Male student','background':'Complex background','failed':'Many failed subjects','competence':'Low competences','absent':'Absenteeism','disruptive':'Disciplinary problems','failed:competence':'Failures $\\times$ low competences','failed:absent':'Failures $\\times$ absenteeism','failed:disruptive':'Failures $\\times$ discipline'}
table('A_interactions',['Attribute or interaction','Estimate',r'95\% CI','Holm $p$'],[[labs[r.term],f(r.estimate),ci(r),pv(r.p_holm)if pd.notna(r.p_holm)else '--']for r in a.itertuples()],r'Teacher fixed effects and clustered errors; 22,006 identifying decisions. Percentage-point contrasts. Holm adjusts the three interaction tests only. Main coefficients refer to the interacting attributes being zero.','@{}p{6cm}rrr@{}')
a=rd('tercile_descriptives');vars=[('n','Teachers',False),('mean_hb','Mean relative propensity',True),('mean_ha','Mean repetition rate',True),('age','Age (years)',False),('secondary','Secondary stage',True),('female','Female teacher',True),('effort','Effort attribution',False),('resources','Resource skepticism (SD)',False)]
table('A_tercile_summary',['Characteristic','Lower','Middle','Upper'],[[lab]+[(f(v)if pct else f'{v:.2f}')if key!='n'else str(int(v))for v in a[key]]for key,lab,pct in vars],r'Proportions and relative propensity in percent or percentage points; other variables in their original units. Missing covariates are omitted item by item. Groups are defined using the observed score, with ties kept together.')
a=rd('nested_prediction_summary')
for pop,name in [('Common','A_prediction_nested'),('X1 available','A_prediction_available')]:
    b=a[a.population==pop];table(name,['Model','RMSE (pp)',r'$R^2$ (\%)','Partition range'],[[r.model.replace('_',' '),f(r.rmse),f(r.r2),f'[{f(r.r2_min)}, {f(r.r2_max)}]']for r in b.itertuples()],r'Five repetitions of five outer teacher folds. Forest and ridge settings selected by three inner folds with all outcome references and resource transformations rebuilt. Full $X_3$ inputs for common-sample forest and ridge; only $X_1$ inputs for available-sample models. Partition ranges are not confidence intervals. N = '+str(int(b.iloc[0].n))+'.','@{}lrrr@{}')
a=rd('prediction_paired_corrected')
table('A_prediction_paired',['Sample and comparison',r'$\Delta$ MSE',r'Approx. 95\% interval'],[[('Common'if r.population=='Common'else'Available')+': '+r.contrast.replace('_',' '),f'{1e4*r.mse_difference:.2f}',f'[{1e4*r.lo:.2f}, {1e4*r.hi:.2f}]']for r in a.itertuples()],r'MSE differences in squared percentage points; negative favors the first model. Intervals use the overlap correction $(1/25+1/4)s_d^2$ and a $t_{24}$ reference. This approximation is a sensitivity summary, not an exact sampling guarantee for generated fold-specific targets.','@{}p{7cm}rr@{}')
a=rd('nested_shap').merge(rd('nested_permutation_importance'),on='variable').sort_values('mean_abs_shap',ascending=False)
table('A_prediction_importance',['Predictor','Mean absolute SHAP',r'Permutation $\Delta$ MSE'],[[names.get(r.variable,r.variable),f(r.mean_abs_shap),f'{1e4*r.mse_increase:.3f}']for r in a.itertuples()],r'Tuned forests from first outer partition. SHAP in percentage points (32 simulations, 128 training-background cases); permutation loss in squared percentage points (20 permutations per variable per fold). Correlated variables share information; these diagnostics are not causal effects.','@{}p{6cm}rr@{}')
a=rd('policy_academic_slopes')
table('B_academic_slopes',['Procedure contrast','Academic criterion','Difference',r'95\% CI','Holm $p$'],[[short(r.model),'Failed subjects'if 'failed'in r.term else'Competences',f(r.estimate),ci(r),pv(r.p_holm)]for r in a.itertuples()],r'Differences in within-teacher slopes, in percentage points. Six attributes and two arm interactions; teacher fixed effects and clustered errors. Holm family: six slope contrasts. P = Policy, R = Revelation, A = Awareness, C = Control. Joint two-slope Holm $p$: 0.969, 0.223 and 0.058, respectively.','@{}llrrr@{}')
a=rd('policy_academic_robustness')
table('B_academic_robust',['Specification and contrast','Criterion','Difference',r'95\% CI'],[[short(r.model).replace('Profile FE teacher weights','Weighted FE').replace('Profile FE','Card FE'),'Failures'if'failed'in r.term else'Competences',f(r.estimate),ci(r)]for r in a.itertuples()],r'Teacher and exact-card fixed effects; teacher-clustered 95\% intervals. Weighted version gives every teacher total weight one before singleton removal. These are checks of the same six contrasts, not independent corroborating experiments.','@{}p{5cm}lrr@{}')
a=rd('ranking_complete_shares');rows=[]
for arm in ['Control','Policy','Revelation','Awareness']:
    for k in [1,2,3]:
        b=a[(a.arm==arm)&(a.policy==k)];vals=[]
        for side in ['favorite','least']:
            r=b[b.side==side].iloc[0];vals.append(r'\shortstack{'+f(r.share)+r'\\{'+ci(r)+'}}')
        rows.append([arm,k,*vals,int(b.iloc[0].n)])
table('B_ranking_shares',['Arm','Policy','Favorite','Least favorite','N'],rows,r'Percentages and Wilson 95\% intervals. Policies: 1 reinforcement, 2 unanimity/secret vote, 3 training. Rankings follow recommendations in Control/Policy and precede assignment in Revelation/Awareness. Every retained ranking respondent is included (N=2,546).','@{}llrrr@{}')
a=rd('ranking_complete_contrasts')
table('B_ranking_contrasts',['Rank, policy and arm contrast','Difference',r'95\% CI','Holm $p$'],[[short(r.model),f(r.estimate),ci(r),pv(r.p_holm)]for r in a.itertuples()],r'Percentage-point differences with HC3 intervals. Holm family includes all 18 favorite/least by policy by adjacent-arm contrasts. Different ranking timing means these are contrasts between procedures, not identical postassignment measurements.','@{}p{5.9cm}rrr@{}')
a=rd('policy_bootstrap_refitted')
table('B_refitted_bootstrap',['Method and contrast',r'95\% interval',r'90\% interval',r'Within $\pm3$?'],[[('IPW'if'IPW'in r.method else'Reference')+': '+short(r.contrast),ci(r),ci(r,'90'),'Yes'if r.equivalent_3 else'No']for r in a.itertuples()],r'Percentile intervals in percentage points from 1,999 teacher resamples of the assignment universe. Every draw reconstructs profile references; IPW also refits the observation logit. Baseline median imputations remain fixed. These intervals evaluate the same observed-composite estimand under additional selection assumptions, not assumption-free full-assignment effects.','@{}lrrr@{}')
a=rd('policy_teacherweighted_profiles')
table('B_weighted_profiles',['Contrast','Estimate',r'95\% CI','Decisions'],[[short(r.model),f(r.estimate),ci(r),r.n]for r in a.itertuples()],r'Card fixed effects and inverse number-of-responses weights, so teachers receive equal total weight. Errors cluster by teacher. No teacher fixed effects: those would absorb the arm effect.','@{}lrrr@{}')
a=rd('alignment_precision_complete');rows=[]
for r in a.itertuples():
    if str(r.model)=='Awareness versus Revelation by alignment': continue
    label=short(r.contrast).replace(' pre-assignment ranking alignment',': ').replace('alignment','').replace('interaction','').replace('Awareness versus Revelation by alignment','A vs R')
    if r.family.startswith('Study I'):label=('Adjusted 'if'adjusted'in r.family and'unadjusted'not in r.family else'Unadjusted ')+label
    margins=', '.join(str(k)for k in [2,3,5]if getattr(r,f'equivalent_{k}'))or'None'
    rows.append([label,f(r.estimate),ci(r,'90'),f(r.mde80),margins])
table('B_alignment_precision',['Comparison','Estimate',r'90\% CI','MDE','Margins met'],rows,r'Percentage points. MDE: normal approximation for 80\% power at 5\%. Margins tested: 2, 3 and 5 points; individual, not simultaneous equivalence statements. Comparisons conditioning on postassignment rankings remain descriptive even if an interval fits within a margin. The final four rows use prior rankings and compare Favorite or Least with Middle within R or A. Unadjusted/adjusted comparisons to C concern the Policy arm.','@{}p{6cm}rrrr@{}')
ref=DEST/'references_A.tex'
txt=ref.read_text(encoding='utf8').replace(r'\end{thebibliography}',r"\bibitem{nadeau} C. Nadeau and Y. Bengio, ``Inference for the generalization error,'' \textit{Machine Learning}, vol. 52, pp. 239--281, 2003. \href{https://doi.org/10.1023/A:1024068626366}{doi:10.1023/A:1024068626366}."+'\n'+r'\end{thebibliography}')
ref.write_text(txt,encoding='utf8')
print('Built expanded manuscript tables without modifying original sources.')
