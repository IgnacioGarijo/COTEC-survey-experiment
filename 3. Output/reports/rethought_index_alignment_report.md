# Rethought belief index and alignment results

Generated after updating and running `2. Code/4. decision_card_models.R`.

## 1. Why the broad index is not the right main measure

The previous `strictness_beliefs_index` combined meritocracy, student blame, reverse system blame, passing without competencies, excessive resources for repeaters, and ineffective resources for repeaters. This is useful as a broad descriptive battery, but it is too heterogeneous to be the main construct. Its internal consistency is low: alpha = 0.361, with mean inter-item correlation = 0.086.

The new screening compares narrower candidate indices:

| Index | Items | Alpha | Bivariate association with hb |
|---|---:|---:|---:|
| Resource skepticism | 2 | 0.651 | +2.21 pp |
| Academic standards | 2 | 0.424 | +0.42 pp |
| Student/system attribution | 2 | 0.443 | +1.16 pp |
| Merit + student blame | 2 | 0.022 | +1.18 pp |
| Original remediation skepticism | 3 | 0.470 | +2.20 pp |
| Broad strictness battery | 6 | 0.361 | +2.40 pp |

The most defensible main index is therefore the two-item `resource_skepticism_index`, built only from:

- belief that too many resources are devoted to repeating students;
- belief that resources for repeating students are ineffective.

This index is narrower, easier to interpret, and more internally coherent. It should replace the broad strictness index in the main text. The broad index can remain in the appendix as an exploratory battery.

## 2. Results with the homogeneous resource-skepticism index

The homogeneous index remains predictive:

- Card-level model: +1.43 percentage points in repetition probability per 1 SD, with card fixed effects and controls.
- Teacher-level harshness model: +1.45 percentage points per 1 SD.

This is smaller than the broad index in bivariate terms, but it is cleaner conceptually and still statistically meaningful.

In within-teacher decision-rule models, resource skepticism mainly changes how teachers weigh academic signals:

- Failed subjects: +2.77 pp per 1 SD of resource skepticism.
- Low competence: -2.58 pp per 1 SD.
- Gender, complex/migrant background, absenteeism, and disruptive behavior: no clear interaction.

Interpretation: the belief difference is not a general tendency to punish every negative signal. It is more specifically related to how teachers interpret academic failure and competence.

## 3. Alignment results

The new alignment models focus on treated teachers and compare:

- middle assigned policy;
- favorite assigned policy;
- least favorite assigned policy.

They control for treatment arm and favorite policy, but do not also include assigned policy, because assigned policy, favorite policy, and alignment are mechanically related and can create multicollinearity.

Main results:

- Favorite assigned vs middle assigned: +0.70 pp at card level, not statistically significant.
- Least favorite assigned vs middle assigned: +1.08 pp at card level, not statistically significant.
- Teacher-level estimates are similar: +0.85 pp for favorite assigned, +0.80 pp for least favorite assigned, both not statistically significant.

The important contrast is that favorite-policy identity remains predictive:

- Favoring promotion criteria predicts substantially higher harshness.
- Favoring teacher training predicts lower harshness.

So policy preferences matter as stable correlates of harshness, but being assigned to a policy that is aligned or misaligned with those preferences does not meaningfully shift the decision.

## 4. What this changes in the paper

The paper should not claim that a broad ideology index causes repetition decisions. The safer and stronger claim is:

> Teachers' repetition decisions are resistant to policy assignment and preference-alignment treatments. However, cross-sectional differences in policy preferences and resource-skepticism beliefs predict who is harsher.

The alignment story becomes sharper:

> It is not the momentary fit between assigned policy and stated preference that moves behavior. Rather, stated policy preferences and beliefs reveal stable priors associated with teachers' baseline decision thresholds.

This helps connect the preregistered nulls with the exploratory belief results. The main contribution is not just a null experiment. It is evidence that light-touch policy exposure and alignment do not shift professional decision rules, even though stated preferences and resource beliefs identify more and less punitive teachers.
