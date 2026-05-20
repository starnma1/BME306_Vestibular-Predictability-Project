# Hypothesis and Conclusions

## Hypothesis

This analysis tests whether the relationship between **p11** (Markov chain transition probability) and **StdBodyX** (standardized body displacement, a proxy for balance disruption) is non-linear.

The core hypothesis is that predictability at the extremes allows the vestibular system to anticipate and adapt to incoming pulses, resulting in smaller body displacement:

- **p11 = 0** (pure alternating): high predictability, effective adaptation, low displacement
- **p11 = 1** (pure single-direction): high predictability, effective adaptation, low displacement
- **p11 = 0.5** (maximum unpredictability): adaptation is impaired, displacement is largest

This predicts an **inverted U-shaped (concave) curve** with a peak at p11 = 0.5.

---

## Conclusions

A Gamma mixed model (log link) was fitted to the data, with StdBodyX as the outcome, a second-degree polynomial of p11, participant height, and a random intercept per sample (n = 4,640 observations across 20 samples).

The results support the hypothesis. Both the linear (p = 0.038) and quadratic (p = 0.003) terms of p11 were significant and negative. The negative quadratic term confirms the predicted inverted U-shape: deviation from static posture is smallest at the extremes of p11 (pure alternating at p11 = 0 and pure single-direction at p11 = 1), increases toward the middle, and peaks around p11 = 0.5 where the pulse sequence is most unpredictable. This suggests the vestibular system is better able to adapt when the stimulus pattern is regular, and struggles most when it is random.

The negative linear term indicates the curve is not perfectly symmetric around p11 = 0.5. Height was not significant (p = 0.156), meaning body stature did not meaningfully explain deviation from static posture once stimulus structure was accounted for.

In summary, the data support the hypothesis: predictable stimuli at the extremes of the Markov chain lead to less deviation from static posture, while maximum deviation occurs at intermediate transition probabilities.
