# OpenDOE roadmap

Working notes for `R/opendoe.R`. Only the "Predictors" tab is implemented so far
(user defines predictors and sets their levels). Everything below is planned.

## UI tabs

### Sample size calculation
Implemented as two subtabs so far, more planned. User picks a primary factor;
all approaches only ever return `n` per predictor combination now (not a design) --
`completely_randomised_design` is a separate, later step for all of them:

- **Power analysis** (implemented) -> auto-picks t-test (`calc_n_two_sample_ttest`) or
  anova (`calc_n_one_way_anova`) depending on whether the primary factor has 2 or
  >2 levels. Only shown for 1-2 predictors total.
- **Monte Carlo: multiple comparison** (implemented) -> `estimate_sample_size(means, sds, power_target, alpha, mcc, family, nsim, n_min, n_max, seed)`,
  `n` is then distributed across the other predictors' combinations the same way
  `calc_n_one_way_anova` does it internally.
- **Monte Carlo: anova** (not built yet) -> `determine_sample_size(levels, means, cv, interactions, formula, alphas, power_target, seed, nsim, n_min, n_max)`

### Design of experiment (fully expanded)
Needed after every sample-size subtab now (they all return `n_per_level`, not a design).
Not built yet -- `completely_randomised_design(predictors, n_per_level)`.

```r
completely_randomised_design(predictors, n_per_level)
```

### Random assignment of infinite groups

```r
random_assign(df, groups, ratios, col, block_col, strata_cols, randomization_method, n_quantiles = 10L, seed)
```

Subtabs set `randomization_method` accordingly:
- simple: inputs `df, groups, ratios, col, seed`
- block: inputs `df, groups, ratios, col, block_col, seed`
- block stratified: inputs `df, groups, ratios, col, strata_cols, seed`

### Random assignment of finite groups

```r
random_finite_assign(seed, groups, design, max_iter, ridge, loss_function, verbose, ids, w, lambda_m2, lambda_cov)
```

Note: `groups` here is a data.frame (target covariate profile), not the character
group-label vector used in the "infinite groups" tab above -> needs a distinct input.

## State

- `df`
- `predictors` <- implemented, built from the predictor/level inputs
- `n_per_level`
- `design_matrix`
- `design_matrix_assigned`

## Server call sketches

```r
# State$df <- import_data()

# State$n_per_level <- calc_n_one_way_anova(
#   State$predictors, input$primary_factor, input$cohens_f, input$sig_level, input$desired_power
# )
# State$n_per_level <- estimate_sample_size(
#   input$means, input$sds, input$power_target, input$alpha,
#   input$mcc, input$family, input$nsim, input$n_min, input$n_max,
#   input$seed
# )
# State$n_per_level <- determine_sample_size(
#   State$predictors, input$means, input$cv, input$interactions,
#   input$formula, input$alphas, input$power_target, input$seed,
#   input$nsim, input$n_min, input$n_max
# )

# output$design_matrix <- completely_randomised_design(State$predictors, input$n_per_level)

# random_assign(df, groups, ratios, col, block_col, strata_cols, randomization_method, n_quantiles = 10L, seed)
# Simple:
# output$design_matrix_assigned <-
#   random_assign(
#     df = State$df, groups = input$groups, ratios = input$ratios, col = input$col,
#     block_col = NULL, strata_cols = NULL, randomization_method = "simple", seed = input$seed
#   )
# Block:
# output$design_matrix_assigned <-
#   random_assign(
#     df = State$df, groups = input$groups, ratios = input$ratios, col = input$col,
#     block_col = input$block_col, strata_cols = NULL, randomization_method = "block", seed = input$seed
#   )
# Block stratified:
# output$design_matrix_assigned <-
#   random_assign(
#     df = State$df, groups = input$groups, ratios = input$ratios, col = input$col,
#     block_col = NULL, strata_cols = input$strata_cols, randomization_method = "block_stratified",
#     seed = input$seed
#   )

# random_finite_assign(seed, groups, design, max_iter, ridge, loss_function, verbose, ids, w, lambda_m2, lambda_cov)
# output$design_matrix_assigned <-
#   random_finite_assign(
#     seed = input$seed, groups = input$groups, design = State$design_matrix(_assigned),
#     max_iter = input$max_iter, ridge = input$ridge, loss_function = input$loss_function,
#     verbose = input$verbose, ids = input$ids,
#     w = input$w, lambda_m2 = input$lambda_m2, lambda_cov = input$lambda_cov
#   )
```

## Open UX question

Considered porting the drag/drop expression-builder (`OpenStats/inst/www/expression.js`)
for the predictor/level inputs. Doesn't fit: that component works because there's a
known, finite vocabulary to drag from (existing columns + fixed operators). Here the
user is inventing new predictor/level names, so there's nothing to drag from. A chip/token
input for the levels field (type + Enter to add a removable chip, instead of one
comma-separated text field) could still be worth it once manual comma-parsing becomes
a real pain point -- not needed for the first working sketch.
