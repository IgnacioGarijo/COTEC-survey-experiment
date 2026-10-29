"""One-off author-voice revision after 72006a6. Never run in the build pipeline."""
from pathlib import Path
from collections import Counter
import hashlib, json, re, subprocess

R = Path(__file__).resolve().parents[2]
D = R / '4. Latex/paper_A'
P = D / 'paper_A_content.tex'
s = P.read_text(encoding='utf8')
origin = subprocess.check_output(['git', 'show', '72006a6:4. Latex/paper_A/paper_A_content.tex'], cwd=R).decode('utf8').replace('\r\n', '\n')
assert s == origin, 'One-off revision: source has changed since the preserved author version.'
before = s
edits = []

def paragraph(start, new, reason):
    global s
    matches = list(re.finditer(r'(?m)^' + re.escape(start), s))
    assert len(matches) == 1, (start, len(matches))
    begin = matches[0].start()
    boundary = re.search(r'\n\n|\n\\\[|\n\\begin\{equation\}|\n\\(?:plot|inputtab|section|subsection|enlargethispage|clearpage|FloatBarrier)\b', s[begin:])
    end = begin + boundary.start() if boundary else len(s)
    old = s[begin:end]
    s = s[:begin] + new.strip() + s[end:]
    edits.append({'starts': start, 'reason': reason, 'old_sha256': hashlib.sha256(old.encode()).hexdigest(), 'new_sha256': hashlib.sha256(new.strip().encode()).hexdigest()})

def phrase(old, new, reason):
    global s
    assert s.count(old) == 1, old
    s = s.replace(old, new, 1)
    edits.append({'starts': old, 'reason': reason})

phrase('The findings distinguish common criteria, disagreement on comparable cases and differences in how teachers apply those criteria.',
       'Teachers therefore differ both in their overall severity and in the importance they give to particular student characteristics.',
       'Close abstract with the concrete finding rather than an abstract list.')

paragraph('Because the educational case for repetition is contingent,', r'''
We approach this decision by separating two questions: whether a student is likely to face difficulties in the next grade, and whether repeating would help. The same academic difficulty can lead to different recommendations if teachers disagree about the benefits and costs of another year in the same grade. This distinction guides the decision framework and our interpretation of the results.
''', 'State the intuition before naming the conceptual distinction.')

paragraph("Teachers' judgments can incorporate information beyond", r'''
Previous studies show that teachers take more than academic performance into account. Comparing teacher-assigned and blindly assigned scores in Brazil, \citet{ferman} find that classroom behavior enters grading and promotion decisions. In a British experiment, \citet{doyle} find that an identical piece of written work receives less favorable evaluations when attributed to a student of lower socioeconomic status. These findings motivate examining academic information alongside conduct and social circumstances, while recognizing the differences between evaluating written work and recommending grade retention.
''', 'Connect the cited findings directly to the information studied.')

paragraph('Research on retention beliefs provides a complementary foundation.', r'''
Teachers' beliefs may also help explain their decisions. Tomchin and Impara \citeyearpar{tomchin} examine the reasoning underlying retention judgments, and \citet{bonvin} connects attitudes and judgments to retention decisions. Empathy, views about the role of effort and assessments of educational resources capture different considerations \citep{pajares}. We use these measures, together with teachers' professional characteristics, to examine whether teachers with different levels of harshness also differ in their reported beliefs and circumstances.
''', 'Replace generic literature framing with the question the measures answer.')

paragraph('Multidimensional vignette designs allow researchers', r'''
Multidimensional vignette designs allow researchers to study how several characteristics jointly enter a judgment \citep{conjoint,hainmueller2015}. In our experiment, we can compare how the same teacher responds to different student profiles and how different teachers respond to the same profile. We then examine how much of the variation in recommendations can be represented by the student information, the measured teacher characteristics and the patterns observed in teachers' decisions.
''', 'Explain the comparisons in terms of teachers and profiles.')

paragraph('We study 2,520 teachers evaluating eight profiles each,', r'''
We study 2,520 teachers evaluating eight profiles each, varying in six attributes. The analysis addresses four questions. First, which student characteristics change a teacher's recommendation when the teacher evaluates different profiles? Second, how do teachers combine signals, particularly when failed subjects suggest repeating but competences suggest that the student is prepared to progress? Third, when teachers evaluate an identical profile, how often do they disagree, and how does the weight given to attributes differ across observed levels of severity? Fourth, how much of the variation in recommendations is represented by professional characteristics, reported beliefs and patterns revealed by the decisions themselves?

To answer these questions, we first estimate how recommendations change with student attributes within each teacher. We then examine disagreement over identical profiles and differences across teachers using harshness, defined as their average departure from other teachers' recommendations on the same profiles. A held-out-card exercise checks whether a summary built from four responses helps anticipate the other four. Finally, we bring the student and teacher information together in a decomposition of the variation in recommendations.
''', 'Retain the developed questions, then explain the sequence of analysis directly.')

paragraph('Studying these questions together helps distinguish', r'''
These comparisons help establish whether disagreement is confined to particular student profiles or also reflects differences across several decisions by the same teacher. A teacher who recommends repetition more often than peers across the profiles they evaluate will have higher harshness. We then ask how far the professional characteristics and beliefs recorded in the survey help explain these differences.
''', 'Develop the reason for the comparisons without a generic contribution statement.')

phrase('The results distinguish common criteria from their application.',
       'The results show that academic information has the largest role in recommendations.',
       'Give the substantive result before reporting its magnitude; retain the author-edited remainder.')
phrase(' The contribution is to connect common criteria, disagreement on comparable cases and differences in how those criteria are applied.', '',
       'Remove a repeated abstract closing statement from the introduction.')

paragraph('A recommendation to repeat can reflect both concern', r'''
A teacher may recognize that a student is struggling and still favor progression if another year in the same grade is unlikely to help. To represent this decision, let $x_i$ denote student $i$'s profile. Teacher $j$ perceives a probability $q(x_i,z_j,\eta_j^q)$ that the student will face important difficulties if they progress, a gain $g(x_i,z_j,\eta_j^g)$ from repetition rather than progression when that difficulty occurs, and a cost $c(x_i,z_j,\eta_j^c)$ of repetition. The teacher recommends repetition when its expected benefit exceeds its cost:
''', 'Introduce the model through the educational decision, retaining all notation and the rule.')

paragraph('Here, $z_j$ collects the teacher characteristics', r'''
Here, $z_j$ collects the teacher characteristics and beliefs measured in the survey. The terms $\eta_j^q$, $\eta_j^g$, and $\eta_j^c$ account for aspects of the teacher's judgment that we do not observe: how they assess the risks of progression, the benefits of repetition and its costs. For example, two teachers with similar measured characteristics may interpret the same failed subjects differently or disagree about whether another year would address the underlying difficulty. Such differences can affect both how often they recommend repetition and which student attributes matter to them. The framework helps interpret these possibilities; the data do not identify the three components separately.
''', 'Keep the explanation of unobserved judgments concrete and retain the identification qualification.')

paragraph('The distinction supplies three useful implications.', r'''
This framework helps interpret several possible patterns in the data. First, teachers can agree that a student faces difficulties but disagree about whether repeating is the appropriate response. Second, a difficult social context can increase concern about progression while also increasing the perceived costs of retention or reducing its expected gains. A teacher may therefore favor progression because repetition seems more costly or less useful for that student. Third, teachers with similar overall harshness can still disagree about which students should repeat.
''', 'Explain the substantive implications affirmatively.')

paragraph('We estimate linear probability models with teacher fixed effects', r'''
We compare the eight recommendations made by each teacher using linear probability models with teacher fixed effects and standard errors clustered by teacher. This allows us to account for differences in teachers' average recommendation levels, including the set of cards or policy context assigned to them. The coefficients show how recommendations change with a student attribute, adjusting for the other attributes and teacher fixed effects, within the combinations represented in our design. Both specifications use the same 20,160 decisions from 2,520 teachers.
''', 'Explain the purpose of fixed effects alongside the estimator.')

paragraph('We check whether a summary of four responses contains', r'''
We then ask whether knowing four of a teacher's decisions helps anticipate the other four. To do this, we divide teachers into five groups. For each group, we estimate the predictive models using teachers in the other four groups and assess performance on the four reserved decisions of teachers in that group. This is five-fold teacher-level cross-validation. The baseline logistic model starts from the retention frequency for the full student profile and adjusts for treatment, card set and educational stage. The second predictor adds the teacher's average deviation from profile references across the four history cards. For teachers used in training, those references exclude their own responses.

We evaluate the predictions using Brier loss and log loss \citep{gneiting}. Brier loss is the mean squared difference between the predicted probability and the observed binary recommendation, so a lower value means better predictions. We average losses within each teacher and then equally across teachers. To examine whether the history adds information beyond the measured characteristics, we also compare the predictors on the same 2,164 complete covariate cases, including all observed teacher characteristics and beliefs in both. Appendix~\ref{app:prediction} details the algorithm, teacher-bootstrap uncertainty, repeated partitions and the first-four/last-four check.
''', 'Separate the question, out-of-teacher evaluation and interpretation of the score.')

paragraph('The final analysis asks how much variation in recorded', r'''
Finally, we bring together the information examined throughout the paper to assess how much of the variation in decisions it represents. We consider the student's profile, the teacher's measured characteristics and beliefs, the teacher's overall harshness, and differences in the importance teachers give to each attribute. Harshness describes whether a teacher recommends repetition more often than peers overall. Attribute sensitivities describe which kinds of students lead that teacher to respond more strongly. Including both allows teachers with similar overall harshness to place different weights on the attributes.
''', 'Explain why the final model contains both overall severity and attribute sensitivities.')

paragraph('This diagonal specification provides a simple interior approximation', r'''
The main specification estimates a separate variance for each attribute sensitivity, with their covariances set to zero. Appendix~\ref{app:prediction} checks how the allocation changes when correlations are allowed within three attribute pairs or among all six attributes. These alternatives fit the data better and reach a covariance boundary, but give similar aggregate shares. The appendix explains the estimates and their numerical checks.
''', 'State what the diagonal assumption does before discussing alternatives.')

paragraph('The second term measures the joint contribution of attribute sensitivities', r'''
The total contains three parts. The first measures variation in the fitted fixed part of the model. The second measures the joint contribution of attribute sensitivities across the observed profiles; in the diagonal specification, it adds each slope variance multiplied by the mean square of its centered predictor. The third captures the remaining decision-level variation. This calculation accounts for how the student attributes vary in the observed design when measuring the contribution of the sensitivities \citep{nakagawa}.
''', 'Explain the three terms of the unchanged formula in their displayed order.')

paragraph('Student attributes, professional characteristics, beliefs and harshness share information.', r'''
Student attributes, professional characteristics, beliefs and harshness share information, so a block's additional contribution can depend on which blocks enter first. We use a Shapley allocation to average over these possible orders \citep{groemping}. For each subset of the four blocks, we project the same full-model predictor onto that subset and measure the variance of the projection. A block receives its average increment across all 24 possible orders. The calculation therefore allocates the variation in one fitted predictor, keeping the estimated full model fixed throughout. Dividing these four contributions, the joint sensitivity variance and the residual variance by $T$ gives six shares that sum to 100 percent. Each decision receives equal weight.
''', 'Motivate the order averaging before explaining the existing projection procedure.')

paragraph('The within-teacher coefficients in Table~', r'''
Table~\ref{tab:attr} shows that academic information has the largest role in teachers' recommendations. Three or more failed subjects increase the recommendation probability by about 55 percentage points, and low basic competences by about 27 points. The two indicators describe different aspects of the student's situation: failed subjects refer to formal performance, while competences describe preparedness more broadly.
''', 'Lead with the result and explain the distinction between its two indicators.')

paragraph('Same-profile disagreement shows where judgments diverge.', r'''
Across the 56 profiles, the frequency-weighted probability that two teachers give different recommendations for the same profile is 25.06 percent. Table~\ref{tab:academic} shows that disagreement is greatest for students with several failed subjects but adequate competences: teachers disagree in 49.09 percent of pairs. When neither academic difficulty is present, disagreement is 5.41 percent. The two conflicting academic categories also have different repetition rates: 51.67 percent for several failures with adequate competences, compared with 23.77 percent for low competences with few failures. Teachers thus respond strongly to academic information on average, while remaining divided when the two indicators point in different directions.
''', 'Explain the pairwise percentage and then interpret the conflicting academic categories.')

paragraph('Within a profile, pairwise disagreement is greatest', r'''
For an identical profile, disagreement is greatest when teachers are divided approximately equally between progression and repetition. However, a repetition rate close to one half for an entire academic category could also arise if teachers agree on some profiles and agree on the opposite decision for others. Calculating disagreement within each six-attribute profile allows us to distinguish these situations. The high disagreement for failures with adequate competences therefore concerns teachers evaluating the same information. The pattern also holds when we give the other four attributes identical weights across academic categories on their common support (Appendix~\ref{app:harshness}, Tables~\ref{tab:standardized}--\ref{tab:standardcontrasts}).
''', 'Develop the reason for the profile-level calculation in ordinary language.')

paragraph('The disagreement comparisons locate cases that divide teachers.', r'''
We next use harshness terciles to compare teachers who recommend repetition less or more often than peers. Tables~\ref{tab:tercsummary} and~\ref{tab:tercslopes} show their characteristics and the weight they give to student attributes. The lower and upper terciles have mean harshness of $-20.51$ and 18.90 points. Their demographic and professional characteristics overlap substantially: mean age differs by about two years, and women constitute approximately two-thirds of each group. Secondary teachers account for 42.6 percent of the lower tercile and 54.5 percent of the upper. Agreement that repeaters receive too many resources rises from 3.07 to 4.02 on the 0--10 scale. Effort attribution averages close to seven in all three groups.
''', 'Introduce the terciles through the comparison they make possible.')

paragraph('Teachers may differ in how strongly recommendations respond', r'''
Table~\ref{tab:tercslopes} compares the within-teacher attribute coefficients across the same harshness terciles. The failed-subject coefficient is about 32 points in the lower tercile and 66 in the other two, while the competence coefficient stays between 27 and 28 points. The difference between the groups is therefore much larger for failed subjects than for competences. Absence has a positive coefficient in all three terciles, largest in the middle group. Discipline has positive coefficients in the lower and middle groups but a negative coefficient in the upper group. These groups are formed from the teachers' responses and describe how attribute associations vary in the observed sample. This motivates including differences in attribute sensitivities alongside overall harshness in the final decomposition.
''', 'Link the magnitudes to the different weights teachers give attributes; retain the descriptive grouping.')

paragraph('A held-out-card check asks whether the observed severity pattern', r'''
The four-card history also helps anticipate recommendations on the reserved cards. In the 2,520-teacher sample, adding this history reduces Brier loss from 0.12514 to 0.10950, a reduction of 0.01565 (teacher-bootstrap SE 0.00141), or 12.5 percent of the baseline loss. Using the first four responses to anticipate the last four gives a reduction of 0.01273 (SE 0.00124). In the matched comparison on 2,164 teachers, both predictors include all observed characteristics and beliefs: adding history still reduces Brier loss from 0.12356 to 0.10946, a reduction of 0.01409 (SE 0.00137). Thus, the teacher's decisions provide information about other recommendations within the same survey encounter, beyond that available from the measured characteristics. Appendix~\ref{app:prediction}, Table~\ref{tab:prediction}, reports the full comparisons and sensitivities.
''', 'Start from what improves prediction and explain what the matched comparison adds.')

phrase('These associations coexist with substantial overlap between groups and limited overall fit.',
       'Despite these associations, most of the variation in harshness remains unexplained by the measured characteristics.',
       'Interpret low fit in terms of the variation the regressions leave unexplained.')

paragraph('The final decomposition in Figure~', r'''
Figure~\ref{fig:variance} shows how the variation in recommendations is divided among the components of the final model. Harshness captures the teacher's overall severity, while attribute sensitivities allow the same information to have different weights for different teachers, as illustrated by the tercile comparisons. Student attributes and their pairwise interactions account for 47.92 percent of model-implied variance. Professional and demographic characteristics contribute 0.18 percent and reported beliefs 0.18 percent. Observed harshness contributes 12.78 percent, while differences in teachers' attribute sensitivities contribute 18.80 percent. The remaining decision-level component is 20.13 percent.
''', 'Explain the allocation before listing unchanged shares.')

paragraph('The distinction between overall severity and criteria matters substantively.', r'''
The sensitivity component helps explain how teachers can have similar overall harshness yet disagree on individual students. One teacher may respond more strongly to failed subjects, while another places more weight on absence or discipline. The model measures these differences jointly across all six attributes.
''', 'Use a concrete comparison to interpret the existing sensitivity component.')

paragraph('Together, measured information and teacher-specific response patterns represent', r'''
Together, measured information and teacher-specific response patterns represent 79.87 percent of model-implied variance. This gives a summary of how much variation the information assembled in the paper represents within the observed decisions. The predictive exercise answers the separate question of whether a short history helps anticipate reserved recommendations, which it does within the same survey encounter.
''', 'Explain the final total while retaining the distinction from held-out prediction.')

paragraph('The findings connect common criteria with different applications', r'''
Academic information has a large role in teachers' recommendations, but failed subjects and competences convey different information. Their joint presence strengthens the recommendation to repeat, while several failed subjects with adequate competences leave teachers especially divided. The decision framework helps interpret this result: teachers may agree that a student faces difficulties and still differ in whether they expect another year in the same grade to help. Understanding their recommendations therefore requires considering how they assess both the student's situation and the educational response.
''', 'Begin the discussion with the substantive finding and develop its interpretation.')

paragraph('Attendance, discipline and background extend this account beyond grades.', r'''
Attendance and discipline show that teachers also consider engagement and behavior. This is consistent with evidence that behavior enters grading and promotion judgments \citep{ferman}, although our coefficients leave open whether teachers expect educational gains or apply normative criteria. The negative complex-background coefficient is compatible with teachers taking the costs of repetition into account. Studies finding less favorable grading of identical work attributed to low-SES students \citep{doyle} concern a different decision and a different description of background. Similarly, the connection between gender differences in grading and subsequent achievement \citep{terrier} concerns a wider educational process than the recommendations studied here. Our comparisons hold the stated student information constant when examining how background and gender enter the repetition decision.
''', 'Relate the nonacademic attributes to the literature without repeated negative caveats.')

paragraph('Harshness supplies a common measure for describing', r'''
Harshness allows us to compare teachers after accounting for the profiles they evaluated. Its distribution shows substantial differences in overall severity, while the tercile comparisons show that teachers also give different weights to the attributes. The multilevel analysis brings these differences together. Measured teacher characteristics and beliefs account for little variation, whereas the patterns observed in teachers' decisions account for considerably more. Moreover, knowing four decisions improves predictions of other recommendations even when the measured teacher characteristics and beliefs are included.
''', 'Explain what the successive analyses establish using the author\'s comparative reasoning.')

paragraph('The results also delimit what can be inferred from observed', r'''
This is relevant for interpreting the associations with educational stage, age and resource agreement. These associations are present, but teachers with similar observed characteristics still differ substantially. Consequently, those characteristics alone provide a limited basis for identifying which particular teachers will recommend repetition more often. The decomposition suggests looking at the criteria teachers apply to student information as well as their overall severity. More specific measures of perceived benefits, costs and alternatives might help explain these differences, although their contribution would need to be assessed with additional evidence.
''', 'Connect the limited fit to its practical interpretation without asserting new policy effects.')

paragraph("The design's main strength is the comparison", r'''
The vignette experiment allows us to compare teachers evaluating the same information and to observe how each teacher responds to different student profiles. In classroom data, differences in repetition rates also reflect which students attend each school and which profiles a teacher encounters. Standardizing the profiles makes the comparisons more direct. The results concern recommendations based on the information shown in the cards. Participation is voluntary, and the regional composition comparison shows overrepresentation of public-school teachers and middle age groups. The availability of final belief responses is also selective.
''', 'Describe the design advantage and the actual sample limitations directly.')

paragraph('The substantive implication is that clarifying promotion', r'''
These findings suggest that clarifying promotion decisions requires discussing how teachers interpret conflicting information and what they expect repetition to achieve, alongside identifying the academic difficulties themselves.
''', 'State the implication affirmatively and concretely.')

paragraph("Student information strongly organizes teachers' repetition recommendations.", r'''
Student characteristics have a clear role in teachers' repetition recommendations. Failed subjects and low competences have the largest coefficients, attendance and discipline add information, and a complex social background reduces the recommendation probability. However, teachers remain divided over comparable students, especially when grades and competences point in different directions.

The observed professional characteristics and beliefs explain little of the differences in harshness. The decision-level decomposition shows that both overall severity and differences in the weight given to individual attributes contribute to the variation in recommendations. The predictive exercise also shows that a short history of a teacher's decisions helps anticipate other recommendations within the same survey encounter.

These results suggest that understanding retention decisions requires examining the student's circumstances together with the criteria teachers use to decide whether another year in the same grade would help.
''', 'Develop the conclusion in substantive steps, preserving its claims and scope.')

phrase("These categories describe recorded progression, not respondents' reasons for stopping.",
       'These figures track the last recorded questionnaire stage reached by each respondent.',
       'Use an affirmative description of the flow counts.')

paragraph('The common-support comparison fixes the distribution', r'''
To compare the four academic categories with the same composition in the other attributes, we give gender, social background, absence and discipline identical weights in each category. Twelve combinations of those attributes have all four academic versions, giving 48 profiles and 17,636 decisions from the main sample. Each combination receives weight $1/12$ in every category. The supported profiles exclude the combination of absence and disciplinary problems. We calculate disagreement within each full profile before applying the common weights. Table~\ref{tab:standardized} reports the resulting rates and disagreement; Table~\ref{tab:standardcontrasts} reports three prespecified contrasts. Teacher resampling (1,999 draws) keeps each teacher's cards together, and simultaneous bands use the maximum standardized bootstrap deviation across the three contrasts.
''', 'Explain the purpose and steps of the common-composition calculation.')

paragraph('All three specifications use the same 17,312 decisions,', r'''
We compare the three specifications using the same 17,312 decisions, fixed predictors and 2,164 teachers, with harshness referenced to the main sample. The fixed design has 34 full-rank columns, and the covariance design distinguishes each proposed component from the residual variance. We also fit each specification with two optimizers to check that the results agree; their objectives and allocations are closely matched.

The diagonal estimate is interior, with a small scaled gradient and a positive-definite Hessian. Allowing correlations gives lower REML AIC, indicating better working-model fit, but the estimates reach a covariance boundary. This means that some combination of sensitivities has estimated variance close to zero. We retain the diagonal as the simple primary approximation under the adopted selection rule and compare its allocations with those of the more flexible models. Table~\ref{tab:covariance} shows that every aggregate share differs by less than one percentage point across the three specifications.
''', 'Separate comparability and optimization checks from the substantive meaning of the boundary.')

phrase(r'\inputtab{reused}{prediction_validation}', r'\enlargethispage{\baselineskip}' + '\n' + r'\inputtab{reused}{prediction_validation}',
       'Keep the final appendix prediction table close to its explanation after reflow.')

assert s != before
for pattern in [r'\\\[.*?\\\]', r'\\begin\{equation\}.*?\\end\{equation\}', r'\\(?:plot|inputtab)\{[^\n]*', r'\\label\{[^}]*\}']:
    assert re.findall(pattern, s, flags=re.S if '.*?' in pattern else 0) == re.findall(pattern, before, flags=re.S if '.*?' in pattern else 0), ('Protected mathematics/exhibits changed', pattern)
assert Counter(re.findall(r'\\(?:citep|citet|citeyearpar)\{[^}]*\}', s)) == Counter(re.findall(r'\\(?:citep|citet|citeyearpar)\{[^}]*\}', before)), 'Citation set changed'
assert Counter(re.findall(r'(?<![A-Za-z])[-+]?\d+(?:[.,]\d+)*', s)) == Counter(re.findall(r'(?<![A-Za-z])[-+]?\d+(?:[.,]\d+)*', before)), 'Numerical text changed'
P.write_text(s, encoding='utf8')
report = {'date': '2026-10-29', 'origin_commit': '72006a6', 'comparison_baseline': '8306e22', 'source_before_sha256': hashlib.sha256(before.encode()).hexdigest(), 'source_after_sha256': hashlib.sha256(s.encode()).hexdigest(), 'edits': edits, 'scope': 'Author-voice prose revision only; no new analyses, estimates, references or exhibits.'}
(R / '3. Output/paper_A_author_revision/style_revision_changes.json').write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding='utf8')
print('Author-voice revision:', len(edits), 'targeted replacements')
