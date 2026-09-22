from pathlib import Path
import pandas as pd
import re,json
ROOT=Path(__file__).resolve().parents[1]
DATA=ROOT/'3. Output/two_papers_2026-09-21/tables'
DEST=ROOT/'4. Latex/two_papers_2026-09-21'
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
a=rd('attributes').query("model=='All'")
table('A_attributes',['Student information','Coefficient',r'95\% CI'],[[names[r.term],f(r.estimate),ci(r)]for r in a.itertuples()],r'Coefficients in percentage points. Teacher fixed effects and teacher-clustered standard errors. Identifying sample: 22,006 decisions, 2,587 teachers; retained score sample: 22,124 decisions, 2,705 teachers. Binary attributes compare presence with absence; male compares with female. The retained coding represents 56 combinations among 64 original identifiers.')
a=rd('characterization').query("model=='Resources_common' and term!='(Intercept)'")
table('A_characterization',['Predictor','Coefficient',r'95\% CI'],[[names[r.term],f(r.estimate),ci(r)]for r in a.itertuples()],r'Outcome and intervals in percentage points; HC3 covariance. All 2,156 observations are complete cases for the full specification. Same-sample $R^2$: $X_1=0.0273$; $X_2=0.0306$; $X_2+$resources$=0.0422$. The public-appointment indicator does not classify private contracts as temporary. Age conditions on experience and vice versa.')
a=rd('predictive_metrics')
table('A_prediction',['Model','RMSE (p.p.)',r'$R^2$ (\%)',r'$R^2$ range (\%)'],[[esc(r.model),f(r.rmse),f(r.r2),f'[{f(r.r2_min)}, {f(r.r2_max)}]']for r in a.itertuples()],r'2,156 teachers for every model. Five folds repeated three times, splitting by teacher. Profile references and resource transformations are learned in training folds. Entries are partition averages; ranges are not confidence intervals. $X_3$ denotes $X_2$ plus resources.', '@{}lrrr@{}')
a=rd('attributes_by_tercile')
table('A_terciles',['Attribute','Lower','Middle','Upper'],[[names[k]]+[r'\shortstack{'+f((rr:=a.query('model==@g and term==@k').iloc[0]).estimate)+r'\\{'+ci(rr)+'}}' for g in ['Lower','Middle','Upper']]for k in ['male','background','failed','competence','absent','disruptive']],r'Percentage-point coefficients from separate teacher fixed-effect regressions; score terciles are formed from these same responses. These are descriptive comparisons, not exogenous subgroup treatment effects. Tercile sizes including singleton teachers: 908, 864, 933. Entries report coefficients and 95\% teacher-clustered intervals. Identifying observation counts are in attributes\_by\_tercile.csv.', '@{}p{4.5cm}rrr@{}')
a=rd('alternative_outcomes'); keys=['levelSecondary','edad','meritocracia','resources']
table('A_alternatives',['Predictor',r'$H^b$',r'$H^a$',r'$H^c$'],[[names[k]]+[f(a.query('model==@g and term==@k').iloc[0].estimate)for g in ['hb','ha','hc']]for k in keys],r'Percentage-point coefficients. Each column uses the full predictor set and 2,156 complete-case teachers. $H^a$ is the unadjusted mean; $H^b$ uses identifier-specific leave-one-out rates; $H^c$ averages residuals from a pooled attribute logit. The comparison concerns conditional associations, not equality of the outcome definitions.')
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
# Each manuscript includes only cited references, retaining appearance order.
bib=(DEST/'references.tex').read_text(encoding='utf8')
items={m.group(1):m.group(0) for m in re.finditer(r'\\bibitem\{([^}]+)\}.*?(?=\\bibitem|\\end\{thebibliography\})',bib,re.S)}
for letter,filename in [('A','paper_A_teacher_decisions.tex'),('B','paper_B_policy_scenarios.tex')]:
 s=(DEST/filename).read_text(encoding='utf8');keys=[]
 for group in re.findall(r'\\cite\{([^}]+)\}',s):
  for k in group.split(','):
   if k not in keys:keys.append(k)
 assert all(k in items for k in keys)
 (DEST/f'references_{letter}.tex').write_text('\\begin{thebibliography}{99}\\small\n'+''.join(items[k]for k in keys)+'\\end{thebibliography}\n',encoding='utf8')
# Verify blue prose is copied from the original, ignoring whitespace only.
original=(ROOT/'4. Latex/paper_preregistered_rewrite.tex').read_text(encoding='utf8')
norm=lambda s:re.sub(r'\s+',' ',s).strip()
audit=[]
for paper in DEST.glob('paper_*.tex'):
 s=paper.read_text(encoding='utf8');pos=0
 while (start:=s.find('\\old{',pos))!=-1:
  i=start+5;depth=1;j=i
  while depth:
   if s[j]=='{' and s[j-1]!='\\':depth+=1
   if s[j]=='}' and s[j-1]!='\\':depth-=1
   j+=1
  text=s[i:j-1];ok=norm(text) in norm(original);audit.append({'paper':paper.name,'verbatim':ok,'text':text});pos=j
assert all(x['verbatim'] for x in audit),[x for x in audit if not x['verbatim']]
(DEST/'provenance_blue.json').write_text(json.dumps(audit,ensure_ascii=False,indent=2),encoding='utf8')
print('Generated tables and manuscript-specific references; verified',len(audit),'verbatim blue passages.')
