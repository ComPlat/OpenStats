# Pairwise Welch t-test in OpenStats

OpenDOE offers "pairwise Welch t-test" (`run_type = "mc"`), OpenStats has no
equivalent. `PairwiseComparison` calls `pairwise.t.test` with the default
`pool.sd = TRUE`:

```r
# OpenStats/R/Backend_V1_2_Engine.R, pairwise_comparisons_V1_2
res <- pairwise.t.test(df[[dep]], interaction(df[predictors]),
  alternative = alternative_hyp, p.adjust.method = p_val_adj_method)
```

So the ELN export cannot produce an `analysis_plan` for this leaf.

## Plan (postponed)

- New `Method` value in `PairwiseComparison` with `pool.sd = FALSE`.
- Then add the mapping in the ELN export.
