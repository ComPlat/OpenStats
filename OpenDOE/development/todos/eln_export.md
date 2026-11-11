# ELN export of sample size results

Target format: `development/eln_example_n_way_anova.json`. Only the n-way
ANOVA (Monte Carlo) case exists as example so far; other methods follow one
by one.

## Format decisions

- Envelope like OpenStats `summary_data_frame_to_json`: `id`, `request_id`,
  `element_info` echoed back, everything from OpenDOE under `Output`.
- `Output.display` is rendered by the ELN, everything else is only stored.
  OpenDOE decides per method what goes into `key_settings`, so new methods
  need no coordination with the ELN developers. ELN renders `assumptions`,
  `key_settings`, `result` as generic key-value lists.
- One list of settings per model (`sample_size.settings`), no shared schema.
- `n_per_group` everywhere (group = cell or level).
- `status`: `success` | `failed`; on failure `result: null` + `message`.
- `software`: R, OpenDOE, Randomization versions + `RNGkind` (seed alone is
  not enough for reproducibility).
- `analysis_plan` is an OpenStats history (V1_2) so OpenStats can call
  `eval_history()` on the real data. `family` (any/all) is sent but ignored
  by OpenStats.
- Predictors: `name`, `role` (primary_factor, secondary_factor, covariate),
  `type` (continuous, ordinal, factor), `levels`.

## Missing in code

- User selects which sample size result is exported.
- Response name: comes from the ELN; standalone needs an input field.
- `role` / `type` of predictors are not in `State`.
- Wizard leaf (test + assumptions) is not stored with the result:
  ```r
  add_result(State, leaf$run_type, label, params, ...)  # leaf path lost
  ```
- `power`, `power_se`, `power_by_effect` are computed but dropped:
  ```r
  methods::new("sampleSizeResult", n = res$n)
  ```
- `formula` uses `values ~ ...` internally, needs the real response name.
- Mapping wizard leaf -> OpenStats history entry:

  | OpenDOE leaf          | OpenStats `type`                       |
  |-----------------------|----------------------------------------|
  | t-test (Student)      | `TTest`, variances `eq`                |
  | Welch t-test          | `TTest`, variances `noeq`              |
  | Wilcoxon rank-sum     | `WilcoxRankSumTest`                    |
  | ANOVA (Student)       | `ANOVA`                                |
  | Welch-ANOVA           | `WELCH_ANOVA`                          |
  | Kruskal-Wallis        | `Kruskal-Wallis Test`                  |
  | pairwise t (Student)  | `PairwiseComparison`, `Method: T-Test` |
  | pairwise Wilcoxon     | `PairwiseComparison`, `Method: Wilcox` |
  | pairwise Welch        | missing, see `pairwise_welch_openstats.md` |

- Factors must stay factors in OpenStats: numeric-looking levels
  (`"20"`, `"37"`) become continuous in `aov`.
