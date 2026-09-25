"""Compose second versions from preserved material and new, result-led sections."""
from pathlib import Path
import re,json
ROOT=Path(__file__).resolve().parents[1]
OLD=ROOT/'4. Latex/two_papers_2026-09-21';DEST=ROOT/'4. Latex/two_papers_2026-09-22'
def between(s,start,end,new):
    assert start in s and end in s,(start,end)
    i=s.index(start);j=s.index(end,i)
    return s[:i]+new+'\n\n'+s[j:]
A=(OLD/'paper_A_teacher_decisions.tex').read_text(encoding='utf8')
A=A.replace('The Decision to Hold Back: Student Information, Teacher Disagreement, and the Limits of Predictability',"Shared Academic Criteria and Systematic Disagreement in Teachers' Grade-Repetition Recommendations")
A=between(A,r'\begin{abstract}',r'\end{abstract}',r'''\begin{abstract}\color{added}
Teachers can agree about the importance of academic difficulties while disagreeing about whether a particular student should repeat a grade. We study that distinction using 22,124 recorded recommendations from 2,705 teachers in a regional survey experiment in Spain. Within-teacher differences are dominated by failed subjects and low basic competences, with evidence that the two signals reinforce each other. Disagreement is concentrated where they conflict: for profiles with many failed subjects but adequate competences, two respondents disagree in 49.1 percent of same-profile pairs. Between-teacher dispersion is also systematic within the task. Its variance is 2.11 times a benchmark that redistributes responses within identical cards and assignment contexts while preserving teachers' exposures. Yet professional characteristics and reported beliefs explain little of this variation. Nested validation on 2,156 complete cases yields out-of-fold $R^2$ values of 3.06 percent for a linear model, 3.01 percent for ridge regression, and 1.48 percent for a tuned random forest. The results locate a consequential boundary in teachers' application of shared academic criteria, while showing why detectable group associations provide a weak basis for predicting an individual teacher's relative recommendation propensity.''')
A=between(A,r'\section{Introduction}',r'\section{Related literature}',r'''\section{Introduction}
Grade-repetition recommendations translate several kinds of evidence into a binary educational judgment. Failed subjects may indicate that a student has not met the requirements for progression. Basic competences provide a partly different signal of what the student can do. Attendance, classroom behavior, and background can further shape how a teacher interprets the difficulty. Even when teachers broadly agree that academic performance should matter, they may disagree about how much evidence is enough, which evidence should take precedence, and whether repetition is an appropriate response.

Understanding this disagreement requires more than comparing raw repetition rates across classrooms. A teacher who recommends repetition frequently may teach students with greater difficulties. Conversely, teachers facing similar cases may apply different decision thresholds or interpret the same signals differently. Standardized student profiles make the information under evaluation comparable. They allow us to ask both which attributes organize recommendations and where comparable information leaves room for divergent judgments.

Research on the consequences of retention offers no universal decision rule that resolves these cases. Estimated effects vary with research design, institutional setting and the students compared \cite{allen,goos}. Research on teachers' beliefs and judgments establishes that their interpretations also matter \cite{tomchin,bonvin,klapproth}. The remaining empirical challenge is to connect the common structure of recommendations to the distribution of disagreement across cases and across teachers. A large average response to failed subjects does not tell us whether teachers agree when grades and competences point in different directions.

We use a survey experiment with primary and secondary teachers in Castilla-La Mancha, Spain. The retained data contain 22,124 recommendations from 2,705 teachers evaluating recorded profiles with six coded attributes. Our analysis proceeds from the decision to the decision-maker. First, we estimate within-teacher attribute contrasts and examine conflicting academic signals. Second, we measure disagreement about identical cards and compare the dispersion of teachers' relative recommendation propensity with an exchangeability benchmark. Third, we ask how well professional characteristics and reported beliefs predict that propensity for held-out teachers.

\new{Studying these questions together helps distinguish disagreement on individual cases from broader patterns in teachers' recommendations. Disagreement over one student profile need not imply that the same teachers also differ across other profiles. If some teachers repeatedly recommend retention more often than their peers on comparable cases, however, there is a pattern to explain---even if their observed characteristics provide little help in predicting it.}

The results support all three distinctions. Failed subjects and low competences have the largest additive within-teacher coefficients, 54.6 and 27.1 percentage points. An interaction model indicates that low competences add more to repetition recommendations when several subjects are failed. Nevertheless, the profile category combining many failed subjects with adequate competences divides respondents almost evenly. Its same-profile disagreement rate is 49.1 percent, compared with 5.8 percent where neither academic difficulty is present.

\new{The dispersion of teachers' relative propensity is not reproduced by shuffling answers among respondents who received the same recorded card in the same assignment context. Observed variance is 2.11 times the average permuted variance; the finding remains among teachers with at least eight responses and within the control context. This is evidence of concentration within the survey encounter, without establishing a persistent trait across time. Observable teacher information nonetheless has limited predictive value. Nested validation gives a mean $R^2$ of 3.06 percent for the full linear model; neither regularization nor a tuned forest uncovers a large predictive gain.}

Our contribution is therefore not simply that academic performance matters or that teachers differ. It is to locate the academic cases that divide judgment, demonstrate an internal respondent-level structure to the disagreement, and assess how far common teacher descriptors can anticipate it. These results identify a substantive boundary of shared decision criteria while avoiding a typology inferred solely from significant regression coefficients. The recommendations concern hypothetical cases; their appropriateness and their consequences for students are separate questions.''')
A=A.replace('find that stronger research designs yield less negative retention effects than weaker designs, correcting a common reversal of their result.','find that stronger research designs yield less negative retention effects than weaker designs.')
# Reframe contribution in literature without discarding the verified antecedents.
A=A.replace('Our extension is to connect variation in multiple presented attributes with directly measured disagreement and a separate test of out-of-sample predictability in a larger teacher sample.', 'Our extension connects conflicting academic signals, respondent-level concentration of disagreement, and a separate test of out-of-sample predictability in a larger teacher sample. The same evidence can therefore distinguish a common decision pattern from the ability to anticipate individual departures from it.')
start=r'\begin{addition}'+'\n'+r'\subsection{Prediction for teachers outside the training sample}'
A=between(A,start,r'\section{Results}',r'''\begin{addition}
\subsection{Is disagreement concentrated within teachers?}
The dispersion of $H^b_j$ contains both respondent-level structure and variation due to observing a limited set of binary decisions. To distinguish these, we redistribute answers among teachers within the same recorded card and assignment cell. Every teacher retains the same cards and number of observations; each card--context stratum retains its number of repetition recommendations. What is broken is the association between a particular teacher and that teacher's answers across cards.

For each of 1,999 permutations, we reconstruct the score and calculate its variance across teachers. We compare the observed variance with this conditional reference, reporting the ratio and the proportion of permutations with at least as much dispersion. We repeat the entire exercise among teachers with at least eight recorded responses and in the no-policy-assignment control. Each restricted sample uses its own card reference population. The exercise asks whether responses are exchangeable across teachers under these conditioning variables. It does not estimate temporal reliability or distinguish every possible source of within-teacher dependence.

\subsection{Conflicting signals and prediction}
We cross failed subjects with basic competences to form four observed academic categories. Their recommendation rates use teacher-clustered intervals. Same-profile disagreement is first calculated within exact identifiers and then averaged within the category. Because other attributes have different distributions across categories, these rates are descriptions of observed cases. A separate teacher fixed-effect model adds three interactions of failed subjects with competences, absence and discipline to the six main attributes. Holm adjustment covers these three interaction tests. The model tests departures from the additive summary over the retained support.

Prediction uses five repetitions of five outer teacher folds. On the common sample of 2,156 complete cases, we compare a training-mean baseline, OLS with $X_1$, $X_2$ and $X_3$, and ridge and forest models using $X_3$. We separately evaluate baseline-only models on their available sample of 2,641 teachers. In every split, the training target uses training-only leave-one-out card rates; test targets subtract the training card means. A test teacher never contributes to its reference population or the fitting of its predictor.

The ridge penalty and forest settings are selected by three inner teacher folds, wholly within each outer training set. Card references and resource standardization are rebuilt at the inner level as well. Forest tuning considers two candidate-variable counts and three minimum node sizes; final fits use 500 trees. Ridge considers ten penalties after training-based predictor scaling. Appendix~\ref{app:prediction} gives the complete specification. Performance is
\[
R^2_{\mathrm{test}}=1-\frac{\sum_j(H_j^{\mathrm{test}}-\widehat H_j)^2}{\sum_j(H_j^{\mathrm{test}}-\overline H_{\mathrm{train}(j)})^2}.
\]
We report partition-averaged RMSE, $R^2$, and paired loss differences. An overlap-corrected approximation assesses the uncertainty of comparisons across the 25 outer test folds \cite{nadeau}; partition ranges alone are not confidence intervals. Held-out SHAP and permutation loss describe the tuned forest's use of the covariates rather than their causal effects.
\end{addition}''')
# Insert a substantive academic-results subsection ahead of overall dispersion.
anchor=r'\subsection{Comparable cases still generate disagreement}'
new=r'''\subsection{The disagreement boundary lies between grades and competences}
\begin{addition}
Table~\ref{tab:academicA} locates the aggregate disagreement. Only 3.0 percent of recommendations favor repetition when the profile has few failed subjects and adequate competences. With low competences but few failures, the share rises to 23.8 percent. With many failures but adequate competences, it is 51.5 percent. When both difficulties occur, it reaches 85.5 percent. The asymmetry shows that low competences and failed subjects are not interchangeable signals in the recorded judgments.
\end{addition}
\inputtab{added}{A_academic}{Conflicting academic signals and same-profile disagreement}{tab:academicA}
\begin{addition}
The disagreement figures sharpen this comparison. Where many failures coexist with adequate competences, the probability that two distinct teachers evaluating the same identifier disagree is 49.1 percent. This is close to the maximum of a balanced binary choice, and much larger than the 5.8 percent for profiles with neither academic difficulty. The other two categories yield 35.6 and 24.7 percent disagreement. These are averages of within-identifier disagreement, so they do not merely compare different profiles grouped under the same academic label.

In the interaction specification, failed subjects and low competences reinforce one another: the interaction is 10.56 points (95 percent interval 8.17 to 12.95), remaining clear after the three-test Holm adjustment. The low-competence coefficient is 21.66 points when the failed-subject indicator is zero and 32.22 points when it is one. Interactions with absence and discipline are much smaller and uncertain. Table~\ref{tab:interactionsA} reports the full specification.

This pattern is consistent with a cumulative use of academic evidence on the probability scale: the marginal association of one academic difficulty is larger in the presence of the other. It does not uniquely establish a psychological decision rule; a nonlinear response to an underlying threshold can produce a similar interaction. The substantive point is that agreement is greatest when the academic signals align, while failed subjects with adequate competences mark a contested decision boundary. The experiment has no measure establishing which recommendation at that boundary is educationally correct.
\end{addition}
\inputtab{added}{A_interactions}{Academic complementarities in within-teacher recommendations}{tab:interactionsA}

'''
A=A.replace(anchor,new+anchor)
# Add coherence result directly after distribution, before characterization.
anchor=r'\subsection{Associations are detectable, but predictive gains are small}'
new=r'''\subsection{Disagreement has a respondent-level structure within the task}
\begin{addition}
The observed score SD is 20.97 points. Under the response-redistribution benchmark, the square root of average variance is 14.45 points; the central 95 percent range of permuted SDs is 13.76--15.18 points. Observed variance is 2.11 times the reference variance, and none of the 1,999 permutations reaches it ($p=0.0005$ using the plus-one correction). Thus finite binary responses and the observed exposures do not reproduce the dispersion under this conditional exchangeability benchmark.
\end{addition}
\inputtab{added}{A_coherence}{Observed dispersion compared with a fixed-exposure reference}{tab:coherenceA}
\begin{addition}
Two checks separate this result from obvious compositional explanations. Restricting the exercise to 2,369 teachers with at least eight responses gives SDs of 18.69 and 12.33 points and a variance ratio of 2.30. Restricting it to the 281 control teachers gives a ratio of 2.43. Both permutation comparisons yield $p=0.0005$. Response counts affect measurement precision, but the excess dispersion is not confined to short records or to pooling different assigned contexts.

The finding supports systematic within-task differences in recommendation patterns. It does not mean that 52 percent of variance is a reliably measured personal trait, even though that is approximately the excess share relative to this benchmark. The excess can reflect persistent beliefs, interpretation of the task, within-encounter consistency, shared school environments, or other respondent-level influences. We therefore retain terciles as descriptions of the realized score and turn separately to whether the observed covariates predict it.
\end{addition}

'''
A=A.replace(anchor,new+anchor)
A=A.replace('reports the full characterization model and the change in fit on the common sample.','reports all three characterization specifications on the same sample.')
A=A.replace(r'\inputtab{revised}{A_characterization}{Teacher characteristics and relative recommendation propensity}{tab:charA}',r'\inputtab{revised}{A_characterization_full}{Teacher characteristics: comparable specifications}{tab:charA}')
start=r'\begin{addition}'+'\n'+r'Table~\ref{tab:predA}'
A=between(A,start,r'\subsection{Exploratory sequence analysis}',r'''\begin{addition}
Table~\ref{tab:predA} evaluates the models for held-out teachers. The mean-prediction RMSE is 18.57 points. The full OLS model reduces it to 18.28 points, a gain of 0.29 points or approximately 1.5 percent of baseline RMSE. Its mean test $R^2$ is 3.06 percent, compared with 1.95 percent using baseline characteristics alone. Ridge performs almost identically to full OLS ($R^2=3.01$ percent); the tuned forest attains 1.48 percent.
\end{addition}
\inputtab{added}{A_prediction_nested}{Nested validation of teacher-level predictions}{tab:predA}
\begin{addition}
The approximate paired comparison favors full OLS over both the training mean and the tuned forest. The OLS--baseline mean squared error difference is $-10.56$ squared percentage points, with an overlap-corrected interval of [$-17.20$,$-3.93$]. Forest minus OLS is 5.48 [1.99,8.96]; ridge minus OLS is 0.19 [$-2.14$,2.53]. These intervals describe an approximate comparison of the fitted procedures and do not certify an exact generalization bound for the generated target. The practical improvement remains small even where the comparison is distinguishable from zero.

Available-sample baseline prediction addresses a different population: 2,641 teachers rather than the complete-case 2,156. OLS with $X_1$ yields mean test $R^2=1.29$ percent and reduces RMSE from 20.76 to 20.62 points. This result prevents interpreting complete-case performance as a prediction assessment of every retained teacher. It also shows that the full-sample baseline variables do not uncover a large signal hidden by the common-sample restriction.

Together, the permutation and validation results provide a more precise interpretation than either alone. Recommendations cluster within respondents beyond the fixed-exposure benchmark, yet the available predictors account for only a small fraction of the finite-response variation in held-out teachers. The evidence is about these measures and models, rather than a proof that every latent difference in judgment is intrinsically unpredictable.
\end{addition}

\new{Recovered-order diagnostics are reported in Appendix~\ref{app:sequence}. They do not provide persuasive evidence that teachers systematically alternate recommendations to compensate for their immediately preceding decision.}''')
# Remove old sequence subsection from main; appendix already contains full analysis.
A=between(A,r'\subsection{Exploratory sequence analysis}',r'\section{Discussion}','')
A=between(A,r'\section{Discussion}',r'\clearpage',r'''\section{Discussion}
The central result is a combination of common criteria and a contested decision boundary. Academic difficulties organize recommendations strongly, and their signals reinforce one another. But many failed subjects with adequate basic competences leave teachers divided. A simple account in which all respondents attach similar meaning to academic difficulty and differ only randomly across answers does not capture the concentration of relative propensity within respondents.

\new{The location of disagreement matters for interpreting the task. A failed subject and a deficit in basic competences need not carry the same implication for progression. The observed asymmetry suggests that teachers distinguish formal performance from the broader competence information presented. When those indicators conflict, their recommendations can diverge even while their average attribute coefficients point in the same direction. This identifies an empirical decision boundary rather than establishing that either group applies an inappropriate standard.}

Standardized profiles make this distinction visible by reducing confounding with the students teachers normally encounter. The analysis compares identical recorded cards for disagreement and uses within-teacher variation for attribute coefficients. It complements studies of actual retention consequences, which answer whether retaining particular students helps them, and studies of beliefs, which explain how teachers understand that practice. Our outcome measures a recommendation given limited common information, not the educational value of carrying it out.

The characterization results show why group associations and individual prediction should be kept separate. Secondary teaching, age, effort attribution and resource skepticism have detectable conditional associations. Age is considered jointly with experience; the resource composite preserves two related judgments rather than a validated latent scale. The restored four-outcome comparison shows that these patterns are generally similar across score constructions, while the stage coefficient nearly disappears when stage is already removed in constructing $H^d$. That disappearance follows the definition of the outcome and is not evidence that stage was irrelevant to the original decisions.

\new{The strong internal dispersion comparison makes the weak predictive performance substantively informative, but does not remove measurement noise. A few binary judgments are an imperfect measure of any stable propensity. The permutation reference demonstrates nonexchangeability within this encounter; it cannot provide a reliability correction or partition the excess into lasting beliefs and temporary response behavior. Consequently, our findings discourage confident individual classification using these covariates, without concluding that all meaningful teacher differences are unmeasurable.}

The study also has defined limits. Recruitment was voluntary within one region. The maintained response rule includes recorded unfavorable-example information, and question reuse prevents a perfect separation of every example and experimental response. The coded support contains 56 distinct combinations rather than a complete factorial design. Belief measures follow the task, and school identifiers do not permit reliable institutional clustering. These features bound causal and population claims; they do not alter the reported conditional comparisons or justify replacing them with an assumed complete design.

\section{Conclusion}
Teachers share a strong academic structure in their repetition recommendations, yet grades and competences can define a contested boundary. Disagreement at that boundary coexists with systematic clustering of responses within teachers. Professional characteristics and reported beliefs offer only modest predictive gains for the observed relative propensity, even after nested tuning and regularization. The substantive contribution is to distinguish the common criteria, the cases that divide judgment, and the limited ability of familiar teacher descriptors to anticipate individual recommendations.''')
# Restore complete outcome comparisons and characterization exhibits in appendix.
start='Two alternative outcome summaries retain the same response sample:'
A=between(A,start,r'\section{Predictive specification and interpretation}',r'''\inputtab{revised}{A_tercile_summary}{Teacher characteristics across descriptive score terciles}{tab:tercsummaryA}
The lower tercile contains fewer secondary teachers (43.0 percent) than the upper tercile (53.1 percent), and its average resource skepticism is $-0.15$ SD rather than 0.16 SD. Age differs by about two years. These shifts are much smaller than the response differences used to define the groups. Terciles offer an accessible description, while the continuous regressions and external folds assess associations and predictive value without treating the groups as independent types.
\plot{revised}{characterization_curves}{Unadjusted score means across rank-based bins of age, experience, effort attribution and resource skepticism. Intervals are mean $\pm1.96$ standard errors. These are descriptive curves; the conditional regression holds age and experience jointly constant.}{fig:curvesA}

\subsection{Four outcome definitions}
The unadjusted teacher mean is $H^a$. The preferred $H^b$ compares each response with its identifier-specific leave-one-out rate. $H^c$ averages residuals from a pooled logistic regression of the six attributes. $H^d$ adds educational stage and three-category school ownership to that pooled logit before averaging residuals. All characterization columns use the same 2,156 complete cases and $X_3$ predictors.

\new{The distinction between $H^c$ and $H^d$ is substantive. The latter has already adjusted for stage and ownership at outcome construction. It therefore describes residual differences net of those contextual variables; it should not replace $H^b$ when the question is whether stage characterizes the observed relative propensity. Its stage coefficient is 0.19 points, versus 3.35 for $H^b$, while age and resource coefficients remain close. Table~\ref{tab:altA} reports all predictors and intervals rather than a selection based on significance.}
{\scriptsize\inputtab{revised}{A_four_outcomes}{Complete characterization under four score definitions}{tab:altA}}''')
A=between(A,r'\section{Predictive specification and interpretation}',r'\section{Recovered order and sequential dependence}',r'''\section{Predictive specification and interpretation}\label{app:prediction}
\begin{addition}
Each outer repetition assigns every teacher to exactly one test fold. All methods within a population use identical folds and identical targets. Baseline and common-sample evaluations have different eligible teachers and therefore different training reference populations. Their RMSEs should not be read as a direct comparison of models fitted to the same outcomes.

For $p$ input variables, the forest grid crosses $\lfloor\sqrt p\rfloor$ and $p$ candidate variables with minimum node sizes 5, 20 and 50. Each candidate has 250 trees during inner validation and the chosen setting has 500 for the outer fit. Ridge uses ten log-spaced penalties from $10^{-4}$ to $10^2$, an unpenalized intercept, and training-standardized numeric/dummy columns. The criterion for both is mean inner-fold squared error. Selection is repeated independently in each outer training set. The inner validation outcomes are constructed against their own training teachers, not the larger outer training population.

The paired uncertainty summary uses the sample variance $s_d^2$ of 25 paired outer-fold loss differences, with $\widehat{SE}=\sqrt{(1/25+1/4)s_d^2}$ and a $t_{24}$ reference \cite{nadeau}. The additional term approximates dependence induced by overlapping training sets. It does not assume 25 independent experiments and is not an exact confidence procedure for this generated-outcome setting. In particular, it should not be used to turn small practical gains into a strong claim of universal algorithmic superiority.
\end{addition}
\inputtab{added}{A_prediction_available}{Baseline prediction using its larger available sample}{tab:availableA}
\inputtab{added}{A_prediction_paired}{Paired predictive losses with overlap-corrected uncertainty}{tab:pairedA}
\inputtab{added}{A_prediction_importance}{Held-out diagnostics for the tuned forest}{tab:importanceA}
\begin{addition}
SHAP values use the first outer partition, 128 training-background observations per fold, and 32 Monte Carlo simulations per feature with additive adjustment. The directional display averages held-out contributions within quartiles of each plotted variable. It describes how the fitted model allocates predictions across those values, not a one-unit regression contrast holding everything else constant. Age and experience are correlated, as are some belief items, so such attributions depend on the model and background distribution. Permutation importance independently perturbs each covariate in held-out observations; it can also be affected by predictor dependence. Both should be interpreted in light of the model's limited predictive accuracy.
\end{addition}
\plot{added}{nested_shap_direction}{Directional SHAP contributions from the tuned forest for held-out teachers. Quartiles run from low to high observed values; columns are descriptive model contributions in percentage points, not causal effects or regression coefficients.}{fig:shapA}''')
A=A.replace('Policy Scenarios, Preference Elicitation, and Grade-Repetition Recommendations', 'Policy Messages and Teacher Judgment: Recommendations, Academic Criteria, and Stated Support')
A=A.replace('Only adjacent positions with observed retained responses enter the pair analysis.', 'Only adjacent positions with observed retained responses enter the pair analysis. Eight distinct positions are recovered for 2,981 raw records, including 2,612 retained teachers; observed adjacent positions give 16,656 pairs. In 1,885 complete eight-response sequences, the switch rate is 47.45 percent, compared with 47.14 percent under 999 within-teacher permutations (two-sided $p=0.397$).')
A=A.replace(r'\appendix',r'\appendix\let\originalsection\section\renewcommand{\section}{\sectionfloatbarrier\originalsection}')
A=A.replace(r'\section{Recovered order and sequential dependence}',r'\clearpage\section{Recovered order and sequential dependence}')
(DEST/'paper_A_teacher_decisions.tex').write_text(A,encoding='utf8')

B=(OLD/'paper_B_policy_scenarios.tex').read_text(encoding='utf8')
B=B.replace('Policy Scenarios, Preference Elicitation, and Grade-Repetition Recommendations: Evidence from a Teacher Survey Experiment','Policy Messages and Teacher Judgment: Recommendations, Academic Criteria, and Stated Support')
B=between(B,r'\begin{abstract}',r'\end{abstract}',r'''\begin{abstract}\color{added}
Brief policy messages may affect the mean level of a teacher's recommendations, the academic criteria used, or the policies the teacher endorses. A survey experiment in Spain separates assignment to three grade-repetition policy scenarios from prior preference elicitation and an additional alignment reminder. Of 3,274 assigned respondents, 2,705 provide a retained recommendation score. Policy assignment, earlier elicitation, and the reminder change mean profile-adjusted propensity by 0.62, $-1.32$, and $-1.11$ percentage points, respectively, with 95 percent intervals spanning zero. A complementary exploratory analysis finds that the reminder reduces the failed-subject slope by 4.98 points (95 percent interval $-8.47$ to $-1.50$; six-test Holm $p=0.031$), although the multiplicity-adjusted joint test of two academic slopes is less conclusive. Assignment to teacher training also raises its subsequent favorite ranking by 9.76 points, with adjusted $p=0.051$. Reestimated weighting and reconstructed-reference bootstraps leave the mean estimates small, but narrow equivalence is not uniform and missing-outcome bounds remain wide. The findings distinguish limited average responsiveness from possible changes in the application of academic criteria and stated support, without treating hypothetical scenarios as implemented educational reforms.''')
B=between(B,r'\section{Introduction}',r'\section{Related literature and empirical questions}',r'''\section{Introduction}
How should responsiveness to an educational policy be measured? A teacher can endorse a proposal without changing a recommendation about a particular student. A message can alter the importance attached to one piece of student information while leaving the overall frequency of repetition recommendations almost unchanged. Conversely, expressing a favorable opinion after exposure may reveal a change in stated support without demonstrating a change in educational judgment. These distinctions matter when evaluating brief policy interventions through average survey responses.

Grade repetition is a useful setting because the individual recommendation connects beliefs about learning to an institutional decision. Policies can change the support anticipated for a promoted student, the rule under which an evaluation board approves repetition, or teachers' perceived capacity to teach pupils with different needs. Those channels are substantively different. Research on retention consequences does not resolve them: estimated outcomes vary across contexts and designs \cite{allen,goos}, while teachers' judgments and beliefs supply an additional interpretive layer \cite{tomchin,bonvin}.

We study three hypothetical scenarios in a survey experiment with teachers in Castilla-La Mancha, Spain: small-group educational reinforcement, unanimity and secret voting for retention, and staff training in inclusive teaching and multilevel classroom management. The design compares no assigned scenario with scenario assignment, then earlier elicitation of policy preferences, then an explicit reminder of how the assigned scenario matches the teacher's earlier ranking. These procedures distinguish policy content from the timing and salience of expressed preferences.

\new{We evaluate responsiveness along three dimensions. The first is the mean recommendation propensity after adjusting for the recorded student cards. The second is the within-teacher association of the two central academic indicators---failed subjects and basic competences---with the recommendation. The third is reported support for the policy scenarios, measured by favorite and least-favorite rankings. The latter two dimensions test whether a small average response adequately describes everything the task records. They do not turn every contrast into a separate confirmatory hypothesis.}

The retained sample includes 2,705 teachers out of 3,274 with an assignment recorded in the raw export. The three adjacent-arm mean contrasts are 0.62, $-1.32$, and $-1.11$ percentage points. All have 95 percent intervals spanning zero. Narrow equivalence depends on the contrast and inferential method: the primary interval for earlier elicitation lies just within a three-point margin, but an expanded bootstrap rebuilding the outcome reference crosses it. Similar mean estimates after adjustment and weighting therefore should not be summarized as evidence of no effect under every procedure.

\new{The academic-criterion analysis provides a different result. Relative to prior elicitation alone, adding the alignment reminder reduces the failed-subject slope by approximately five points. The estimate is stable to exact-card adjustment and equal total teacher weights. Its individual multiplicity-adjusted evidence is stronger than the joint two-slope test, and the analysis is exploratory. Later stated support also shows a suggestive response: assignment to teacher training increases its probability of being ranked first by 9.76 points, with a six-outcome adjusted $p$ of 0.051. Together these findings make a uniform ``teachers do not respond'' interpretation too broad.}

The paper contributes a differentiated assessment of responses to policy messages. It separates means from criteria, earlier from later preferences, and observed-outcome precision from missing-outcome uncertainty. The procedures are short and hypothetical. They neither implement resources or training nor convene an evaluation board. Their value is to show how different dimensions of teacher judgment can respond differently to policy descriptions and preference procedures, with the extent of the evidence made explicit.''')
B=B.replace('leaving student-attribute weights and teacher prediction to a separate companion analysis.', 'leaving the general structure of student-attribute weights, disagreement and teacher prediction to a companion analysis. Here, attribute slopes enter only to examine whether the randomized procedures change the use of academic information.')
anchor=r'\section{Experimental design and data}'
extra=r'''\begin{addition}
These questions imply separate, testable response patterns. If a scenario changes the perceived feasibility of promotion broadly, it may move the average recommendation. If it changes which academic evidence the teacher regards as decisive, a slope contrast may emerge even with a small average difference. A shift in the ranking of the assigned policy can instead reflect increased perceived relevance, familiarity or a desire for response consistency. The design measures these response patterns, but cannot identify those psychological channels separately.

The policy identities also warrant different interpretations. Reinforcement and training describe additional capacity to support pupils, though only the former directly specifies support for the student. Unanimity and secret voting describe a collective rule. That rule can reduce enacted retention without changing any respondent's individual preference for retention. Consequently, an individual recommendation is an informative but incomplete outcome for judging the institutional implications of the scenarios. We report all policy contrasts rather than select a policy-specific direction after observing the estimates.
\end{addition}

'''
B=B.replace(anchor,extra+anchor)
# Add explicit model distinction, in direct terms.
anchor=r'\subsection{Precision and selection sensitivity}'
extra=r'''\subsection{Changes in academic criteria}
\begin{addition}
The mean comparison uses assignment between teachers. A separate comparison uses the multiple cards answered by each teacher to ask whether that assignment changes academic slopes. For each adjacent-arm pair, we estimate
\[
Y_{ij}=\alpha_j+X_{ij}'\beta+\gamma_F T_j F_{ij}+\gamma_C T_j C_{ij}+u_{ij},
\]
where $F$ denotes many failed subjects, $C$ low basic competences, and $T$ the first arm in the contrast. The six attributes enter $X$. A teacher fixed effect absorbs every characteristic constant within teacher, including the arm's main effect. It does not absorb $T_jF_{ij}$ or $T_jC_{ij}$ because the academic attributes change across that teacher's cards. Thus $\gamma_F$ compares the failed-subject slope between procedures; it is not the procedure's effect on the mean recommendation.

We cluster errors by teacher and adjust the six slope tests using Holm. Three additional joint tests, one per arm pair, test the two academic changes together and form their own Holm family. The interactions are a limited exploratory extension selected for their substantive role in the decision task. We also replace the six common attribute coefficients with exact-card fixed effects, and give every teacher equal total weight in a further specification. These checks assess whether the result depends on the additive profile adjustment or the number of responses. None identifies a psychological mechanism independently of the maintained coding, recorded-outcome definition and outcome-availability assumptions.
\end{addition}

'''
B=B.replace(anchor,extra+anchor)
B=B.replace('HC3 intervals in this exercise condition on estimated weights; it is a sensitivity check rather than a complete solution to estimated-selection uncertainty.', 'The expanded bootstrap resamples teachers from the assignment universe 1,999 times, reconstructs the profile reference and reestimates the observation model and its weights in every draw. Baseline median imputations remain fixed. Percentile intervals therefore incorporate coefficient and weight estimation, conditional on that preprocessing and the selection model.')
B=B.replace('These are transparent sensitivity thresholds, not retrospectively declared preregistered smallest effects of interest.', 'A three-point change corresponds to three additional recommendations per 100 comparable decisions, or about six percent of the observed teacher-average recommendation rate. Whether that is negligible depends on the purpose and costs of the policy. The two- and five-point margins show how conclusions change with a stricter or looser standard; none is asserted as a prospectively chosen smallest effect of interest.')
anchor=r'\section{Results}'
extra=r'''\new{We supplement extreme bounds with a gradual missing-outcome sensitivity for the raw teacher mean $H^a$. For each arm, an unobserved mean can differ from its observed mean by a specified amount. We vary these departures independently across the two arms, truncating means to $[0,1]$. This makes the selection assumptions needed to change the full-assignment mean contrast visible, without confusing them with confidence intervals for $H^b$.}

'''
B=B.replace(anchor,extra+anchor)
B=B.replace('Bootstrap 95 percent intervals rebuilding the score reference are [$-2.23$, 3.51], [$-3.39$, 0.73], and [$-2.98$, 1.01] points. These checks support describing the average estimates as small and imprecise around zero, but the close equivalence boundary for prior elicitation should not be presented as a robust qualitative separation between procedures.', 'In 1,999 bootstrap draws from the assignment universe, rebuilding the score reference gives 95 percent intervals of [$-2.03$, 3.45], [$-3.39$, 0.55], and [$-3.24$, 1.02] points. The 90 percent interval for Revelation minus Policy becomes [$-3.10$, 0.27], crossing the three-point margin. Only Awareness minus Revelation remains inside that margin in both the primary and expanded bootstrap analyses. The broad message is small mean estimates with sensitivity in narrow equivalence, rather than a robust qualitative separation between the two preference procedures.')
anchor=r'\subsection{Prior alignment and later preference reports answer different questions}'
extra=r'''\subsection{Small average changes coexist with a change in the failed-subject slope}
\begin{addition}
Table~\ref{tab:slopesB} reports the six academic-slope contrasts. Scenario assignment versus control produces changes close to zero for both indicators. Earlier preference elicitation increases the failed-subject slope by 3.72 points and reduces the low-competence slope by 1.96 points, with neither individual contrast clear after multiplicity adjustment. Adding the reminder then changes the two slopes by $-4.98$ and 2.72 points, respectively.
\end{addition}
\inputtab{added}{B_academic_slopes}{Procedure differences in the use of academic information}{tab:slopesB}
\begin{addition}
The reminder's failed-subject contrast has a 95 percent interval of [$-8.47$,$-1.50$] and a six-test Holm $p$ of 0.031. Its direction means that the gap in recommendations between profiles with many and few failed subjects is smaller under Awareness than under Revelation. It does not mean that Awareness reduces recommendations by five points for every profile, or that academic failures cease to dominate decisions.

The joint test of both academic changes yields $p=0.019$ before and 0.058 after correction across the three arm-pair tests. The distinction is relevant: one component has clearer evidence than the broader claim that the vector of academic weights changes. Exact-card fixed effects give a failed-subject contrast of $-4.97$ points; adding inverse response-count weights gives $-4.88$ points. The point estimate is therefore not sensitive to those two modeling choices, although these are robustness checks on the same data rather than independent confirmations.

This is an exploratory qualification to the mean results. Making preference alignment explicit may alter how respondents apply an academic criterion even when the average composite barely moves. The data do not establish whether this reflects greater consideration of alternatives, consistency with stated views, task framing, or another response process. They also do not demonstrate that the reminder improves decision quality. The mean contrasts alone therefore do not settle whether academic criteria change.
\end{addition}

'''
B=B.replace(anchor,extra+anchor)
B=B.replace('The evidence is suggestive of a change in reported valuation or response consistency, but does not distinguish those mechanisms and does not survive a conventional five-percent Holm threshold.', 'The estimated ranking response is substantively larger than the mean recommendation contrasts, although the outcomes and scales capture different behavior. Its adjusted $p$ of 0.0507 indicates uncertainty near a conventional threshold; it should not be converted into a sharp distinction between an established effect and no response. The result is compatible with changed valuation or response consistency, without separating them.')
anchor=r'\subsection{Outcome availability and robustness delimit the interpretation}'
extra=r'''\begin{addition}
The complete ranking distributions help interpret what changes and what remains common across procedures (Table~\ref{tab:ranksharesB}). Reinforcement is the most frequently favored scenario: 51.6--58.4 percent place it first across arms. Unanimity and secret voting are most frequently ranked last: 72.0--78.4 percent. Training is generally intermediate, with first-place shares of 30.8--39.3 percent. This pattern suggests stronger stated support for capacity-building descriptions than for the collective promotion-rule description, but it is not a preference between experimentally isolated policy features.

None of the 18 adjacent-arm first/last-place contrasts is clear after a common Holm adjustment. The raw distributions are also measured at different points in the procedure: rankings in Revelation and Awareness precede policy assignment and the reminder. Their difference cannot be attributed to receiving the reminder. The within-Policy assignment contrast instead concerns a genuinely later ranking. Keeping these comparisons separate avoids treating timing differences as a common posttreatment preference outcome.
\end{addition}
\inputtab{added}{B_ranking_shares}{Favorite and least-favorite policies across procedures}{tab:ranksharesB}

'''
B=B.replace(anchor,extra+anchor)
B=B.replace('A direct card-level model with profile effects is reported in the appendix; its observation weighting differs from giving each teacher equal weight.', 'A card-level model with exact-profile effects and inverse response-count weights gives 0.64, $-1.25$, and $-1.07$ points. Its close agreement with the teacher-level contrasts shows that equal teacher weighting reconciles much of the difference from the unweighted card-level model. Appendix tables report both the weighting comparison and bootstrap intervals with reestimated selection weights.')
B=between(B,r'\section{Discussion}',r'\clearpage',r'''\section{Discussion}
The experiment distinguishes three dimensions of responsiveness to policy messages: average recommendations, the application of academic criteria, and stated support. Average composite contrasts are small and do not reject zero, while narrower statements of equivalence depend on the procedure and inferential method. The criterion analysis identifies an exploratory reduction in the failed-subject slope with the alignment reminder. The later-ranking analysis suggests that assignment can also influence reported policy support. These findings require a more differentiated interpretation than either policy effectiveness or teacher resistance.

\new{A change in criteria is conceptually distinct from a change in the mean. Teachers can attach a different marginal importance to failed subjects while other signals, offsets across profiles, or the recorded response composition leave the aggregate little changed. The slope result is stable to exact-card effects and equal teacher weights, but its exploratory status and the weaker adjusted joint test limit how strongly to generalize it. The evidence supports reporting the signal and its boundaries, not building an unverified mechanism around it.}

The policy content further limits what recommendations can establish. Reinforcement does not supply an actual support teacher, and the training message does not increase instructional skill. The unanimity scenario changes a collective decision rule whose implementation could alter retention without changing an individual's preferred recommendation at all. Small individual mean responses therefore do not establish that these institutional channels are ineffective. Equally, the strong preference for reinforcement and rejection of the voting-rule scenario do not measure their causal educational benefits.

Preference timing has both substantive and methodological implications. Earlier elicitation exposes respondents to the menu and asks them to commit to an ordering; the additional reminder makes an already elicited alignment explicit. Later rankings can change with exposure or response consistency and should be analyzed as outcomes rather than automatically controlled as baseline preferences. Earlier rankings support conditional assignment comparisons, but favorite-policy assignment remains a bundle of policy identities and personal rankings, not an isolated psychological treatment of liking.

\new{Precision also has several layers. For observed composite means, the data exclude some large average differences under the specified estimators. They do not uniformly establish effects smaller than three points, particularly for prior elicitation when references are rebuilt in the expanded bootstrap. Specific policy and alignment comparisons are less precise. For the full assignment universe, missing outcomes permit much wider differences without assumptions, and the gradual sensitivity display shows which departures from observed means would change the sign or size of those contrasts. Reestimated inverse-observation weights address observed selection under a model; they do not resolve selection on unmeasured recommendations.}

The main measurement limitation is that the retained response definition includes example information and cannot perfectly separate reused question instances. This can dilute responsiveness and complicate the interpretation of the composite as a postassignment outcome. Recruitment is voluntary and regional, and reliable school-level clustering is unavailable. These limitations are most important for extending the results to implementation or populations beyond the observed respondents. Within those boundaries, the experiment provides evidence on several dimensions of response that should not be collapsed into a single null-policy conclusion.

\section{Conclusion}
Brief scenarios and preference procedures yield small mean contrasts in recorded repetition recommendations, but that is not the whole response pattern. An exploratory reminder contrast indicates less weight on failed subjects, and exposure to the training scenario is associated with greater subsequent stated support. The evidence is uneven across these dimensions and remains sensitive to measurement and selection. The paper's contribution is to distinguish them explicitly: policy endorsement, decision criteria and average recommendations are related outcomes, not interchangeable measures of teacher responsiveness.''')
# Expanded robustness appendix, preserving all existing estimates as labeled sensitivities.
anchor=r'\inputtab{added}{B_equivalence}'
extra=r'''\inputtab{added}{B_weighted_profiles}{Exact-profile adjustment with equal teacher weights}{tab:weightedB}
\inputtab{added}{B_refitted_bootstrap}{Reconstructed-reference and reestimated-weight bootstrap intervals}{tab:bootB}
\begin{addition}
The bootstrap resamples all 3,274 assigned records with replacement, allowing both observed counts and composition to vary. An observed teacher selected multiple times contributes that multiplicity to every card total. Each bootstrap copy's leave-one-out rate excludes its own response, rather than excluding all copies of the original teacher. The selection logit is reestimated using assignment multiplicities, arm, baseline characteristics and their missingness indicators; inverse probabilities are bounded using a 0.05 floor. No inferential claim of ignorability follows from the similarity of point estimates.

For the gradual missing-outcome display, let $r_a$ be the observed fraction and $\mu_a$ the observed $H^a$ mean in arm $a$. A hypothesized full-assignment mean is
\[
M_a(\delta_a)=r_a\mu_a+(1-r_a)\max\{0,\min[1,\mu_a+\delta_a]\}.
\]
We vary each arm's $\delta$ independently between $-0.30$ and 0.30 and plot $M_a(\delta_a)-M_b(\delta_b)$. The origin assumes missing respondents have their own arm's observed mean. Moving along one axis relaxes that assumption in one arm; a common movement along both axes need not cancel because availability fractions differ. The contour is the zero contrast, not a confidence boundary. The exercise concerns $H^a$, avoiding an assumption that the profile-adjusted $H^b$ is bounded by zero and one.
\end{addition}
\plot{added}{missing_outcome_grid}{Full-assignment raw-mean contrasts under independent departures of missing from observed outcomes. Axis units are percentage points; colors show the hypothesized contrast and the black line marks zero. These are selection scenarios, not estimates of the missing responses.}{fig:missingB}

'''
B=B.replace(anchor,extra+anchor)
anchor=r'\section{Secondary alignment and ranking-adjusted analyses}'
extra=r'''\section{Academic criteria and the full ranking comparisons}
\inputtab{added}{B_academic_robust}{Robustness of the procedure-by-academic contrasts}{tab:sloperobB}
\inputtab{added}{B_ranking_contrasts}{Complete adjacent-arm comparisons of favorite and least-favorite rankings}{tab:rankcontrastsB}
\new{The exact-card specifications absorb common differences between identifiers, including common attribute interactions not represented by an additive six-attribute model. Teacher effects remain in these slope specifications because the target is a difference in within-teacher criteria. This differs from the mean-assignment model, which cannot include teacher effects. No significance search over additional teacher subgroups is used to select the reported academic contrasts.}

'''
B=B.replace(anchor,extra+anchor)
anchor=r'\section{Scope, plan, and replication disclosure}'
extra=r'''\inputtab{added}{B_alignment_precision}{Precision of all retained alignment comparisons}{tab:alignprecisionB}
\new{Equivalence is evaluated for the full retained set of alignment comparisons, including descriptive comparisons based on later rankings. It does not repair their timing or confer causal interpretation. MDEs summarize realized precision; they are not retrospective probabilities that the null is true. The displayed individual margins also do not constitute a joint equivalence statement over the collection of policies, ranks and procedures.}

'''
B=B.replace(anchor,extra+anchor)
B=B.replace('The Decision to Hold Back: Student Information, Teacher Disagreement, and the Limits of Predictability',"Shared Academic Criteria and Systematic Disagreement in Teachers' Grade-Repetition Recommendations")
B=B.replace('A bootstrap with 399 teacher resamples', 'A bootstrap with 1,999 assignment-universe teacher resamples')
B=B.replace(r'\appendix',r'\appendix\let\originalsection\section\renewcommand{\section}{\sectionfloatbarrier\originalsection}')
B=B.replace('The present paper does not claim those results as a separate contribution.', 'The present paper uses attribute interactions only to study differences induced by the assigned procedures, rather than claiming the companion paper\'s overall attribute and disagreement results as a separate contribution.')
(DEST/'paper_B_policy_scenarios.tex').write_text(B.rstrip()+'\n',encoding='utf8')
# Recheck every blue body passage against original wording; author names checked previously.
original=(ROOT/'4. Latex/paper_preregistered_rewrite.tex').read_text(encoding='utf8')
normalize=lambda x:re.sub(r'\s+',' ',x).strip()
blue=[]
for name in ['paper_A_teacher_decisions.tex','paper_B_policy_scenarios.tex']:
    text=(DEST/name).read_text(encoding='utf8');pos=0
    while (pos:=text.find('\\old{',pos))>=0:
        start=pos+5;end=start;depth=1
        while depth:
            if text[end]=='{':depth+=1
            elif text[end]=='}':depth-=1
            end+=1
        value=text[start:end-1];ok=normalize(value)in normalize(original)
        assert ok,value[:100]
        blue.append(dict(paper=name,verbatim=ok,text=value));pos=end
(DEST/'provenance_blue.json').write_text(json.dumps(blue,ensure_ascii=False,indent=2),encoding='utf8')
print(f'Composed two manuscripts; verified {len(blue)} blue passages.')
