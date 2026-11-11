# Configurable alternative hypothesis

All sample size calculations are two-sided because `alternative` is never
set (R default `"two.sided"`):

```r
# Randomization/R/sample_size_monte_carlo.R
pairwise_t_test <- function(x, z) stats::t.test(x, z, var.equal = TRUE)$p.value
pairwise_wilcoxon_test <- function(x, z) stats::wilcox.test(x, z)$p.value
# Randomization/R/power_design_ttest.R: pwr::pwr.t.test(...) -> also two.sided
```

The user does not see this. OpenStats requires `alternative hypothesis`
explicitly (`TTest`, `WilcoxRankSumTest`, `PairwiseComparison`). If the
analysis later uses `"greater"`, it no longer matches the planning
(one-sided needs a smaller n).

Until then the ELN export writes `"alternative": "two.sided"`.

## Plan

- Input in OpenDOE, passed through to the Randomization test functions and
  `pwr`.
- Write the same value into the OpenStats `analysis_plan`.
