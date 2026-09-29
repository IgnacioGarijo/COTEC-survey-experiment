from pathlib import Path
import pandas as pd
import re,json
ROOT=Path(__file__).resolve().parents[1]
DATA=ROOT/'3. Output/shared/tables'
DEST=ROOT/'4. Latex/paper_B'
(DEST/'tables').mkdir(exist_ok=True)
def rd(n): return pd.read_csv(DATA/f'{n}.csv')
def f(x): return f'{100*x:.2f}'
def ci(r): return f'[{f(r.lo)}, {f(r.hi)}]'
def pv(x): return '$<0.001$' if x<.001 else f'{x:.3f}'
def esc(s): return str(s).replace('_',r'\_').replace('%',r'\%').replace('&',r'\&')
def table(name,headers,rows,note,spec=None):
    spec=spec or '@{}p{6.0cm}'+'r'*(len(headers)-1)+'@{}'
    s='\\begin{tabular}{'+spec+'}\n\\toprule\n'+' & '.join(headers)+r' \\'+'\n\\midrule\n'
    s+='\n'.join(' & '.join(map(str,row))+r' \\' for row in rows)
    s+='\n\\bottomrule\n\\end{tabular}\n\\tabnote{'+note+'}\n'
    (DEST/'tables'/f'{name}.tex').write_text(s,encoding='utf8')
names={'male':'Male student','background':'Complex background','failed':'Three or more failed subjects','competence':'Low basic competences','absent':'Frequent unjustified absence','disruptive':'Serious disciplinary problem','levelSecondary':'Secondary rather than primary','experiencia':'Teaching experience (year)','antiguedad3':'School tenure (year)','female':'Female teacher','edad':'Age (year)','public':'Public school','public_permanent':'Reported permanent public appointment','grupos_docencia':'Teaching groups (one group)','impact':'Perceived teacher impact (scale unit)','empathy':'Empathy (five-category unit)','meritocracia':'Effort attribution (1--10 unit)','resources':'Resource skepticism (one SD)'}
a=rd('flow_by_arm')
table('B_flow',['Arm','Assigned','Retained','Score','Task flag','Finished'],[[r.arm,r.assigned,r.retained,r.score_observed,r.task_complete,r.survey_complete]for r in a.itertuples()],r'Raw assignment denominator: 3,274. Retained denotes membership in the cleaned sample after exclusions, not full survey completion. Score sample: 2,705. Task flag and Finished are raw-export indicators. Stages are not a universally ordered attrition chain.','@{}lrrrrr@{}')
p=rd('policy_contrasts');a=p.query("family=='Broad arms'")
short=lambda s:s.replace('Awareness','A').replace('Revelation','R').replace('Policy','P').replace('Control','C').replace('policy','policy')
table('B_broad',['Contrast','Estimate',r'95\% CI','MDE','N'],[[short(r.contrast),f(r.estimate),ci(r),f(r.mde),r.n]for r in a.itertuples()],r'All magnitudes are percentage points. P = Policy; R = Revelation; A = Awareness; C = Control. Each contrast is first group minus second. HC3 covariance; two-sided 95\% intervals. MDE approximates 80\% power at a two-sided 5\% test using realized standard errors. N counts teachers in the pair. Holm-adjusted p-values: 0.673, 0.581, 0.603.','@{}lrrrr@{}')
a=p[p.family.isin(['Policies within arm','Arms within policy'])]
table('B_policy_contrasts',['Contrast','Estimate',r'95\% CI','Holm $p$'],[[short(r.contrast),f(r.estimate),ci(r),pv(r.p_holm)]for r in a.itertuples()],r'Percentage points and HC3 intervals from the ten-cell model. P/R/A denote arms. Policy 1 = reinforcement; 2 = promotion rules; 3 = training. Holm correction is applied separately to the six within-arm policy contrasts and the six across-arm contrasts within policy. Full model N = 2,705; individual cell counts are in the appendix.', '@{}p{5.7cm}rrr@{}')
a=rd('alignment_preassignment');a=a[a.model.isin(['Revelation pre-assignment ranking','Awareness pre-assignment ranking']) & a.term.str.startswith('alignment')]
table('B_alignment',['Arm and contrast','Estimate',r'95\% CI','N'],[[r.model.split()[0]+': '+r.term.replace('alignment','')+' - Middle',f(r.estimate),ci(r),r.n]for r in a.itertuples()],r'Percentage-point coefficients with HC3 intervals, adjusting for favorite-policy identity. All rankings in these two arms precede policy assignment. These compare policy packages defined by prior ranks, not a pure psychological preference effect.', '@{}p{6.0cm}rrr@{}')
a=rd('preference_effects_all_rankings')
table('B_preferences',['Ranking outcome','Estimate',r'95\% CI','Holm $p$'],[[r.model,f(r.estimate),ci(r),pv(r.p_holm)]for r in a.itertuples()],r'Percentage-point linear probability differences, assigned this policy versus another policy, within Policy. N = 761 with retained ranking. HC3 covariance with residual t reference; Holm correction across six outcomes. The exact adjusted p-value for policy 3 favorite is 0.050685.', '@{}lrrr@{}')
a=rd('flow_by_policy')
table('B_cells_flow',['Arm','Policy','Assigned','Retained','Score'],[[r.arm,r.policy,r.assigned,r.retained,r.score_observed]for r in a.itertuples()],r'Control has 337 assigned, 302 retained, and 281 with a score. Policy numbers are 1 reinforcement, 2 promotion rules, 3 training. A stored policy code in Control does not create a policy-assigned control cell.', '@{}lrrrr@{}')
a=rd('attrition_contrasts').query("family=='score_observed'")
table('B_availability',['Contrast','Difference',r'95\% CI','N assigned'],[[short(r.contrast),f(r.estimate),ci(r),r.n]for r in a.itertuples()],r'Percentage-point differences in the probability of a retained score from the raw assignment denominator; HC3 intervals. Each includes screening retention and response availability, not solely abandonment.', '@{}lrrr@{}')
a=rd('attrition_balance').query("selection=='score_observed'");bn={'age':'Age','female':'Female (proportion)','experience':'Experience','secondary':'Secondary (proportion)','public':'Public school (proportion)'}
table('B_balance',['Characteristic','Score observed','No score','SMD'],[[bn[r.variable],f'{r.mean_yes:.3f}',f'{r.mean_no:.3f}',f'{r.smd:.3f}']for r in a.itertuples()],r'Means in original units; standardized mean differences divide by the square root of the average group variance. Available N differs by item. Age and experience are measured in years. Comparisons include all assigned records, including those excluded from the cleaned sample.','@{}lrrr@{}')
rows=[]
for source,label in [('policy_card_level','Card-level'),('policy_ipw','IPW'),('policy_duration_trim','Duration trim')]:
 for r in rd(source).itertuples():
  if hasattr(r,'term') and r.term=='(Intercept)':continue
  con=r.contrast if hasattr(r,'contrast') else r.model
  rows.append([label+': '+short(con),f(r.estimate),ci(r),r.n])
table('B_sensitivity',['Specification and contrast','Estimate',r'95\% CI','N'],rows,r'Percentage points. Card-level N counts decisions and intervals cluster by teacher; other N counts teachers. IPW conditions on estimated observation weights. Duration trimming removes values below the 1.75th and above the 98.25th percentiles in the score sample.', '@{}p{5.8cm}rrr@{}')
a=rd('policy_equivalence').query("family=='Broad arms'");rows=[]
for con in a.contrast.unique():
 z=a[a.contrast==con];rows.append([short(con)]+[pv(z[z.margin==m].iloc[0].p_tost)for m in [.02,.03,.05]])
table('B_equivalence',['Contrast',r'$\pm2$ points',r'$\pm3$ points',r'$\pm5$ points'],rows,r'Entries are the larger of the two one-sided p-values; equivalence at a margin requires p below 0.05. Individual tests use primary HC3 standard errors and residual t reference. These tests are not simultaneous guarantees across all comparisons.', '@{}lrrr@{}')
a=rd('alignment');rows=[]
for r in a.itertuples():rows.append([short(r.contrast)+(' (fav. adjusted)' if r.family=='Study I adjusted' else ''),f(r.estimate),ci(r),r.n])
table('B_secondary_alignment',['Comparison','Estimate',r'95\% CI','N'],rows,r'Percentage points; HC3 intervals. The first four rows compare Policy respondents assigned favorite/least-favorite with Control, unadjusted and favorite-identity adjusted. Remaining rows adjust for favorite identity within alignment strata. Policy and Control rankings occur later; the corresponding conditioning is potentially posttreatment.', '@{}p{6.0cm}rrr@{}')

DATA=ROOT/'3. Output/paper_B/tables'
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

print('Built current paper B tables.')
