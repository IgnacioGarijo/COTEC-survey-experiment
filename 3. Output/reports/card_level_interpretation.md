# Card-level analysis: interpretation for the paper

Generated after running `2. Code/4. decision_card_models.R`.

## What the new script adds

The script rebuilds the data at the teacher-card decision level. The sample has 2,705 teachers and 22,124 card decisions, with an average repetition rate of 47.6 percent and 8.18 valid decisions per teacher. Standard errors in the card-level models are clustered by teacher.

It adds four blocks of analyses:

1. Card-level treatment effects with card fixed effects.
2. Equivalence tests and 80 percent MDEs for the main experimental contrasts.
3. An exploratory strictness/beliefs index and two subindices: responsibility beliefs and remediation skepticism.
4. Within-teacher models that ask whether treatments or beliefs change the weights teachers place on student-card attributes.

## Main results

The treatment effects on card decisions are very small:

- Policy treatment versus Control: -0.18 percentage points, MDE80 = 3.75 pp.
- Revelation versus Policy: -0.83 percentage points, MDE80 = 2.64 pp.
- Awareness versus Revelation: -1.33 percentage points, MDE80 = 2.70 pp.

With equivalence bounds of +/-3 percentage points, all three card-level treatment contrasts are statistically equivalent to zero. With tighter +/-2 pp bounds, they are not. The right interpretation is therefore not just "null and underpowered": the experiment can rule out medium-small effects around 3 pp, but not very small behavioral effects.

The within-teacher decision-rule model shows that teachers respond strongly to academic signals in the cards:

- Failed subjects: +54.6 pp.
- Low competence: +27.1 pp.
- Absenteeism: +6.4 pp.
- Disruptive behavior: +4.6 pp.
- Complex/migrant background: -1.9 pp.
- Boy: +0.3 pp, not statistically meaningful.

Treatment-by-attribute interactions are all small and statistically weak. This is important: the interventions do not only fail to move average repetition rates; they also do not visibly change the rule teachers use to map student attributes into decisions.

Beliefs and stated policy preferences do predict harsher decisions:

- A one-SD increase in the strictness beliefs index predicts +1.64 pp in repetition probability in the card-level model with card fixed effects.
- At the teacher harshness level, the same index predicts +1.62 pp.
- Responsibility beliefs predict about +0.9 to +1.0 pp.
- Remediation skepticism predicts about +1.3 pp.
- Relative to teachers whose favorite policy is reinforcement, teachers whose favorite policy is promotion criteria are about +4.5 pp harsher, while teachers whose favorite policy is teacher training are about -4.0 pp less harsh.

The within-teacher belief-rule model adds nuance. Strictness beliefs mainly change the weight placed on failed subjects: +3.46 pp per SD of the index. They reduce the additional weight placed on low competence by about -1.53 pp, and do not meaningfully change the weight placed on gender, migrant/complex background, absenteeism, or disruptive behavior.

## What this contributes

The strongest contribution is the gap between stated malleability and behavioral rigidity. The experimental material can move declared preferences in some places, especially policy rankings, but it does not move the actual repetition decision or the decision rule. That is a much richer story than a simple null result.

The "stubborn teachers" framing is directionally tempting but too blunt. The data are better described as belief-anchored decision rules. Teachers are not randomly stubborn: they follow a stable and academically interpretable rule, centered on failed subjects and competence. The interesting result is that light-touch exposure does not shift that rule, even when preferences or expressed policy support are more movable.

The ideology story is plausible but should be written carefully. The beliefs index has low internal consistency: alpha is 0.361 for the six-item index, 0.285 for responsibility, and 0.470 for remediation skepticism. That means this should not be presented as a clean latent ideology scale. It is better to call it a battery of strictness-oriented beliefs, or a family of beliefs about responsibility, merit, and remediation. The evidence is correlational, not causal, because beliefs and policy preferences are not experimentally assigned and may be post-treatment or jointly determined with repetition attitudes.

## How I would change the paper

I would make the paper about limited behavioral updating in high-stakes professional judgment. The key narrative would be:

1. Teachers have stable decision rules for repetition.
2. These rules are strongly organized around academic failure and competence.
3. Brief informational or reflective interventions do not change the repetition decision or the weights in the decision rule.
4. Stated policy preferences and strictness-oriented beliefs are correlated with harsher decisions.
5. Therefore, the policy problem is not only information. It is also the belief structure and institutional decision threshold that organize how teachers interpret student signals.

I would avoid making "stubbornness" the headline word. It risks sounding accusatory and may make reviewers think the paper overclaims a psychological mechanism. I would use phrases like "behavioral rigidity", "belief-anchored decision rules", "limited updating in professional judgment", or "stable decision thresholds".

Methodologically, I would include the card-level model and the equivalence tests in the main results, not just the appendix. The null is more credible when paired with MDEs and TOST bounds. I would present the belief index as exploratory and add item-level or subindex robustness in the appendix. I would also show the within-teacher attribute-weight figure, because it makes the decision process concrete and helps explain why the experimental treatment may not move behavior.

The paper should not say "ideology causes repetition decisions." A stronger and safer formulation is: "Repetition decisions are resistant to brief informational interventions, while cross-sectional differences in strictness-oriented beliefs and policy preferences predict who is harsher." That is publishable, cleaner, and closer to the evidence.
