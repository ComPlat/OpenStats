# Interaction in n-way ANOVA sample size

The user enters one mean per cell, so the simulated data can contain an
interaction, but the test is always additive:

```r
formula_obj <- stats::formula(paste("values ~", paste(predictor_names, collapse = " + ")))
```

Model and simulated data then disagree: residual variance is inflated and
the sample size is off.

## Plan

- Option in OpenDOE: additive vs. with interaction (`Dose * Temperature`).
- `alphas` and `power_by_effect` also for `Dose:Temperature`.
- ELN export needs no new field, `formula` carries it.
- Note for users: interactions need roughly 4x the n for the same effect
  size.
