# Hypothesis and Conclusions: Learning Effect (Secondary Outcome)

## Hypothesis

This analysis tests whether deviation from static posture decreases systematically across repeated experimental sessions.

The core hypothesis is that the first session produces the highest deviation from static posture, as participants are naive to the perturbation paradigm. As sessions progress, participants develop an internal model of the expected disturbance, allowing the vestibular and motor systems to anticipate and counteract incoming pulses more effectively. This predicts a monotonically decreasing relationship between session number and deviation from static posture, consistent with a learning or habituation effect across experiments.

Only the first 10 pulses per session are used, as these best capture the initial response before within-session adaptation obscures the between-session learning signal.

---

## Conclusions

A Gamma mixed model (log link) was fitted to the data, with median deviation from static posture (first 10 pulses) as the outcome, log-transformed trial number, pulse direction, participant height, and a random intercept per sample (n = 185 observations across 19 samples).

The results strongly support the hypothesis. The log-transformed trial number was highly significant (p < 0.001), with a negative coefficient of -0.28. This means deviation from static posture decreases across sessions in a logarithmic pattern: the largest improvement occurs early on, with smaller gains as sessions accumulate. This is consistent with a learning or habituation effect where participants adapt most rapidly after their first exposure to the paradigm.

Pulse direction (forward vs. backward) was not significant (p = 0.656), suggesting the direction of the perturbation did not meaningfully affect deviation from static posture. Height was also not significant (p = 0.162).

In summary, the data support the hypothesis: participants show a clear reduction in deviation from static posture across sessions, with the steepest improvement in the early trials, consistent with progressive motor learning or vestibular habituation.
