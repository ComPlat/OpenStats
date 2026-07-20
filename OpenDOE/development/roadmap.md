# OpenDOE roadmap

Working notes for `R/opendoe.R`. Implemented so far: Data import (one CSV
upload tab, feeds every other tab), Predictors, Sample size (all 3 subtabs),
Design of experiment (`completely_randomised_design`), Random assignment of
infinite groups (all 3 methods), Random assignment of finite groups
(`random_finite_assign`, background + Cancel), Results list + History (list
+ replay: per-row "Replay" button, plus a paste-JSON-and-replay box mirroring
OpenStats' `HistoryEditorUI`) + Excel export (`Save results & history`,
global button), background execution + cancel for long-running ops
(`Backend_Engine.R`). Everything below is still planned.

### History replay

No per-row "Replay" button -- pasting the History JSON back in (textarea +
one button, mirroring OpenStats' `HistoryEditorUI` exactly) is the only way
to replay. Every entry replays by recomputing
(`replay_history_entry()`, `Server_History.R`), not by restoring saved
values -- same philosophy as OpenStats. Only works for entry types whose
params are fully self-contained (`ttest`, `anova`, `mc`, `mc_anova`,
`predictor_table`, `design` -- plain scalars/vectors, nothing external
needed), matching OpenStats where a history entry never references another
result. `random_assign`/`random_finite_assign`/`import_csv` entries do
reference a dataset by Results id (see the Random assignment sections
below) rather than being self-contained, so they can't be replayed this way
-- attempting to hits the generic "cannot replay this entry type" error
(as a `print_warn`, not `print_err`), cleanly, not a crash.

Every replayable entry (even the fast/synchronous ones -- `ttest`, `anova`,
`predictor_table`, `design`) runs through `bg_process$start()`, not just
`mc`/`mc_anova`, so the whole paste-JSON batch is uniformly cancellable via
one "Cancel" button (`State$history_replay_running`, shown next to a
"Replaying history..." status while a batch is in flight) rather than only
individual mc/mc_anova entries being interruptible. Entries are queued
(`State$replay_queue`) and drained one at a time -- `bg_process` only runs
one job at a time, so firing every entry in a batch in one loop would drop
all but the first. Cancel clears the *whole* remaining queue and kills
whatever's currently running (`State$bgp$cancel()`), not just the current
entry.

## Data model: everything table-shaped lives in Results

There's one CSV upload, in the "Data" tab (`UI_Data.R`/`Server_Data.R`,
first tab) -- not one per feature. It wraps the upload in `importedData`
(slot `df`) and adds it via `add_result()`, so it shows up in the Results
list exactly like a computed result (Excel export, remove button, etc.).

Every other result type that's fundamentally a data.frame --
`predictorTable`, `assignmentResult`, `designResult`,
`finiteAssignmentResult`, `importedData` -- is listed in
`df_result_classes` (`StorageClasses.R`, a plain character vector, no
shared base class). `df_result_choices(State, classes = df_result_classes)`
(`utils.R`) builds a "pick any of these from Results" dropdown; pass a
narrower `classes` (e.g. `"designResult"`) to restrict it. Any tab that
needs a dataset (`random_assign`'s `df`, `random_finite_assign`'s
`groups`/`design`) uses this instead of its own upload widget -- so a CSV
uploaded once, or the output of one randomization step, can feed straight
into the next without re-uploading.

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
- **Monte Carlo: anova** (implemented) -> `determine_sample_size(levels, means, cv, interactions, formula, alphas, power_target, seed, nsim, n_min, n_max)`

### Design of experiment (fully expanded)

```r
completely_randomised_design(predictors, n_per_level)
```

**Implemented** (`UI_Design.R`/`Server_Design.R`, own tab between Sample size
and Random assignment). The `n_per_level` input is a dropdown of every
previously computed sample size result in `State$results` (labelled
`"<result label> (n=<n>)"`) plus a "Free (enter manually)" option that reveals
a `numericInput` for a custom n. Picking a sample size result keys off its
result id, so it always reflects the *current* value even if that result was
computed a while ago. Runs synchronously (fast, just `expand.grid` +
replication). Result type: `designResult` S4 class (slot `df`), same
append-only Results-list + Excel-export treatment as the other df-shaped
results. Only `completely_randomised_design` is wired up -- Randomization
also has `blocked_design`, `latin_square_design`, `split_plot_design`,
`split_block_design` (`R/design_generators.R`), not exposed here yet.

### Random assignment of infinite groups

```r
random_assign(df, groups, ratios, col, block_col, strata_cols, randomization_method, n_quantiles = 10L, seed)
```

**Fully implemented.** `df` is picked via `df_result_choices()` (see Data
model above), not its own upload. All three methods run synchronously (fast,
single-pass sampling -- no background process needed for this one, unlike
random_finite_assign below). Method selector rebuilds the whole controls box
(same convention as the Sample size sidebar), so switching methods resets the
groups/ratios/col inputs.

Subtabs set `randomization_method` accordingly (all implemented):
- simple: inputs `df, groups, ratios, col, seed`
- block: inputs `df, groups, ratios, col, block_col, seed` -- `block_col` is a
  `selectInput` populated from the selected df's column names
- block stratified: inputs `df, groups, ratios, col, strata_cols, seed` --
  `strata_cols` is a multi-select over the selected df's column names

**Validated**: block/block_stratified reject a `block_col`/`strata_cols`
choice that would produce any block smaller than `length(groups)`. Root
cause: `Randomization::random_assign`'s block-size rounding uses R's
round-to-even, so a block of size 1 always resolves to the first group
deterministically -- blocking on a near-continuous column (e.g. a weight
measurement) silently assigns *every* row to the first group. Confirmed by
reproducing it directly against `Randomization::random_assign()`.

Result type: `assignmentResult` S4 class (slot `df`), same append-only
Results-list + Excel-export treatment as `predictorTable`/`sampleSizeResult`.
The dataset itself is *not* stored in history params (can be arbitrarily
large -- would bloat every Excel export's History JSON block); `df_source`
(the Results id it came from) is kept instead, so the entry documents which
dataset + groups/ratios/column/seed were used, not a way to reproduce
without that dataset still existing in Results.

### Random assignment of finite groups

```r
random_finite_assign(seed, groups, design, max_iter, ridge, loss_function, verbose, ids, w, lambda_m2, lambda_cov)
```

**Implemented** (`UI_FiniteAssign.R`/`Server_FiniteAssign.R`, own tab after
Random assignment). Both `groups` (target covariate profile, numeric columns
only) and `design` are picked via `df_result_choices()` (see Data model
above), both unrestricted -- `design` can be a `designResult` built in the
Design of experiment tab *or* a directly imported CSV (e.g. a design built
externally), not just the former. Client-side checks all `groups` columns
are numeric (the package itself doesn't validate this, it would just
silently normalize garbage) and that `nrow(groups) >= nrow(design)` (the
optimizer indexes `groups` rows per design slot and fails confusingly deep
inside if there aren't enough).

Runs through `bg_process$start()` (real `callr::r_bg`, not the sync
fallback) since it's an iterative greedy optimizer (`max_iter` swaps) plus
an `ast2ast::translate()` JIT compile on first call -- both slow enough to
justify the Cancel button, shown next to a "Optimizing assignment..."
status while running. `ridge`/`w`/`lambda_m2`/`lambda_cov` are **not**
exposed in the UI (left at the Randomization function's defaults) to keep
the form small; only `loss_function` (Default/Mahalanobis), `max_iter`, and
`seed` are. `ids` is also not exposed -- results only carry `unit_index`,
not `unit_id`.

`Randomization::random_finite_assign()`'s returned `assigned` data.frame
doesn't include which design slot each row landed in (`blocks` comes back as
a separate parallel vector, one `"level1.level2"` string per row when design
has more than one column) -- `run_random_finite_assign()`
(`Backend_Operations.R`) binds `design`'s own columns back in instead of
that string (`cbind(res$assigned, design)`), since the two are row-aligned
(verified directly against `Randomization::random_finite_assign()`) as long
as `nrow(groups) >= nrow(design)`, which is already enforced above. Result type:
`finiteAssignmentResult` S4 class (slots `df`, `loss`), same append-only
treatment as the other result types, `loss` also shown as text above the
table. `groups`/`design` are not stored in history params (same bloat
reasoning as `random_assign`'s `df`); `design_id` is kept so the entry
documents which design was used.

## Open UX question

Considered porting the drag/drop expression-builder (`OpenStats/inst/www/expression.js`)
for the predictor/level inputs. Doesn't fit: that component works because there's a
known, finite vocabulary to drag from (existing columns + fixed operators). Here the
user is inventing new predictor/level names, so there's nothing to drag from. A chip/token
input for the levels field (type + Enter to add a removable chip, instead of one
comma-separated text field) could still be worth it once manual comma-parsing becomes
a real pain point -- not needed for the first working sketch.
