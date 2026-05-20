# Hypothesis and Conclusions: Forward vs. Backward Deviation (Secondary Outcome)

## Hypothesis

This analysis evaluates whether backward platform perturbations produce larger deviation from static posture than forward perturbations, and whether a learning effect is present across the first 10 pulses separately for each direction.

The hypothesis is that the first perturbation elicits the largest deviation from static posture, with deviation decreasing systematically over subsequent pulses as participants adapt to the repeated stimulus. A key confound is present in the data: the first trial was always a forward perturbation, meaning participants were naive exclusively to the forward direction first. This inflated the baseline deviation for forward trials and prompted a separate direction-stratified learning curve analysis.

A Gamma mixed model with a log-transformed pulse number, direction, their interaction, and a random intercept per subject was used to test whether adaptation rates differed between directions.

---

## Conclusions

The results show strong and significant effects for all key terms in the model.

The significant negative effect of log(pulse) (p < 0.001, estimate = -0.39) confirms a learning effect for backward trials: deviation from static posture decreases in a logarithmic pattern across the first 10 pulses, with the steepest drop early on.

The significant positive effect of direction (p < 0.001, estimate = +0.88) indicates that forward trials had substantially higher deviation from static posture than backward trials at the first pulse. However, this should be interpreted with caution: because the first trial was always forward, participants were completely naive to forward perturbations at pulse 1, which likely inflated this estimate. This is a data artefact rather than a true directional difference at baseline.

The significant negative interaction between log(pulse) and forward direction (p < 0.001, estimate = -0.39) shows that forward trials also had a steeper rate of adaptation than backward trials. Again, this is likely driven by the same artefact: forward trials started from a higher naive baseline and therefore had more room to decrease. By the later pulses, the gap between directions narrows considerably.

In summary, both directions show a clear learning effect across the first 10 pulses. The apparent advantage of backward over forward trials at pulse 1 and the steeper forward adaptation rate are most plausibly explained by the trial ordering artefact rather than a genuine biomechanical asymmetry between directions.
