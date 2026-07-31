# OpenDOE roadmap

Working notes for `R/opendoe.R`. Implemented so far: Predictors, Sample size
(all 3 subtabs), Design of experiment (`completely_randomised_design`),
Random assignment of infinite groups (all 3 methods), Random assignment of
finite groups (`random_finite_assign`, background + Cancel), Results list +
History (list + replay: per-row "Replay" button, plus a
paste-JSON-and-replay box mirroring OpenStats' `HistoryEditorUI`) + Excel
export (`Save results & history`, global button), background execution +
cancel for long-running ops (`Backend_Engine.R`), a step diagram running
above the tabs instead of a plain tab strip. Everything below is still
planned.

## Navigation: step diagram above the tabs

**Implemented**, third iteration. First was a locked text-link bar above the
tabs; second made the diagram the sole landing page (tabs/sidebar/Results
hidden behind it, a "Back" button to return); this settled on the diagram as
a permanent horizontal header instead, with tabs/sidebar/Results always
visible below it exactly like before any of this navigation work -- no
overview/step toggle, no Back button, the tabsetPanel just opens on its
first tab like it always did. Real-user feedback (chemists, not
statisticians) driving all three iterations: the old left-to-right tab
strip, while logically ordered, still surfaced too much at once -- terms
like "predictor" and the concept of replication were confusing on their own,
even before getting to which tab to use. Prototyped standalone in
`development/app.R` before being ported in (that prototype still shows the
landing-page version with inline explanation text -- superseded here).

`Server_Main.R::output$diagram` (styled via `#diagram` in `styles.css`)
renders one box per step in `step_defs()` (`R/utils.R`), left-to-right,
connected by ⇒. Each box shows a friendlier display label
(`step_display_labels()`); the explanation (`step_explanations()`, one
running example -- drug-A/B/C vs. Control -- threaded through every step)
is a native hover tooltip (`title=`) rather than inline text, since the
box now sits permanently above your work instead of being the only thing on
the page -- no room for a paragraph per box. Locked steps
(`step_unlocked()`) render as plain greyed-out, non-clickable boxes (🔒)
instead of `actionButton`s; the box matching the currently open tab
(`input$conditionedPanels`) is highlighted. Opening a step calls the same
`updateTabsetPanel()` the tabs always used -- nothing about the underlying
`input.conditionedPanels == '<Tab>'` wiring needed to change.

The tabsetPanel's own native nav stays CSS-hidden
(`#conditionedPanels { display: none; }`) even though its content is always
visible, so a direct click on a tab can't bypass the diagram's lock logic --
navigation only ever happens through the diagram. "Data" is *not* one of the
diagram's boxes -- it isn't a pipeline step and isn't scoped to or gated by
any of them (see Data model below), so it gets the same treatment as
History: a plain, always-reachable `actionLink` ("Import data",
`input$goto_data`) next to the download button, immediately left of the
"History" link. Both just call `updateTabsetPanel()` like the diagram boxes
do.

Predictor-vocabulary rename ("predictor" -> "Gruppen") is a separate,
not-yet-done step -- kept in mind, not blocking this one. The diagram's
`step_display_labels()` already uses "Define groups" for that step (ported
as-is from the approved prototype text) even though the underlying tab
value and module id are still "Predictors" -- the two are intentionally
decoupled now, so the vocabulary rename, when it happens, only needs to
touch the label lookup, not the tab wiring.

### History replay

No per-row "Replay" button -- pasting the History JSON back in (textarea +
one button, mirroring OpenStats' `HistoryEditorUI` exactly) is the only way
to replay. Every entry replays by recomputing
(`replay_history_entry()`, `Server_History.R`), not by restoring saved
values -- same philosophy as OpenStats. Fully self-contained entry types
(`ttest`, `anova`, `mc`, `mc_anova`, `predictor_table`, `design` -- plain
scalars/vectors, nothing external needed) always replay, matching OpenStats
where a history entry never references another result.

`random_assign`/`random_finite_assign` reference a dataset by Results id
(`df_source`/`groups_id`+`design_id`) instead of being self-contained --
**these do replay now**, but only conditionally: `replay_history_entry()`
resolves the id against `State$results` *at replay time*, so it succeeds if
(and only if) a dataset with that id already exists in the current session
-- e.g. because you rebuilt the same design, or re-uploaded the same
covariate file, first. If not, it's a clean `print_warn` ("dataset '...'
doesn't exist in this session"), not a crash. Getting the referenced ids to
line up is entirely on the user (project discussion: rejected building
automatic id-remapping or a pause-mid-replay-and-prompt-for-upload
mechanism as unnecessary complexity) -- replay a JSON into a fresh session
by redoing any upload/build steps the original session did, in the same
order, *before* pasting and replaying; since `State$counter` is monotonic
and append-only, doing so keeps this session's ids identical to the
original's. `import_csv` itself can never be replayed (there's no stored
file content to reconstruct from) -- it always hits the generic "cannot
replay this entry type" `print_warn`, permanently; this is expected, not a
bug -- re-uploading that dataset (via the Data tab) is the user's job,
which is exactly what the rest of this section's id-matching relies on.

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

**Standalone "Data" tab, restored** (`UI_Data.R`/`Server_Data.R`, first tab
-- one CSV upload, not one per feature). Briefly removed and folded into
Finite assignment's sidebar (the one place that currently genuinely needs
externally measured data, e.g. real animal weights) on the reasoning that
almost nothing else in OpenDOE needs external data: Predictors are
user-defined, Sample size and Design are generated, and Random assignment
only ever consumes the generated design. Reverted (project discussion):
that scoping doesn't generalize -- once this runs embedded in an ELN
(Chemotion), the dataset arrives automatically before the user does
anything at all, so "where does upload live" can't be tied to, or gated
behind, whatever downstream feature happens to need it today. It also
directly unblocks History replay: replaying a saved session needs its
original dataset to exist in Results before any entry that references it
by id runs, which a Data-first, always-unlocked step guarantees; scoping
upload to a locked-until-a-design-exists tab did not (see History replay
above). It wraps the upload in `importedData` (slot `df`) and adds it via
`add_result()`, so it shows up in the Results list exactly like a computed
result (Excel export, remove button, etc.) and can be picked from anywhere
`df_result_choices()` is used with the default (unrestricted) `classes`
argument -- Finite assignment's covariate picker included.

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

Core idea: the sample-size method and the final statistical test should
(ideally) be the same test -- chemists were confused when shown every method
and left to guess which one was "correct" for their design. So instead of a
fixed set of subtabs, `sample_size_visible_tabs(State, primary_factor,
assume_normal)` (`Server_SampleSize.R`) computes which subtab(s) actually make
sense for the current predictors/levels, and only those are rendered
(`output$tabs_ui` / `output$sidebar_ui` are both dynamic, driven by the same
function). A `assume_normal` ("Yes"/"No") radio button sits above everything
else in the sidebar and picks the parametric vs. non-parametric branch.

Decision table (`n_predictors`/`n_levels` refer to the *primary* factor when
there are 2 predictors):

| Predictors | Levels (primary) | Normal? | Visible tabs |
|---|---|---|---|
| 1-2 | 2 | yes | Power analysis (t-test) + Pairwise comparisons (t-test) |
| 1-2 | >2 | yes | Power analysis (ANOVA) + Pairwise comparisons (t-test) + Monte Carlo: anova |
| >2 | - | yes | Monte Carlo: anova + Pairwise comparisons (t-test) |
| 1 | 2 | no | Pairwise comparisons (Wilcoxon) |
| 1 | >2 | no | Monte Carlo: Kruskal-Wallis + Pairwise comparisons (Wilcoxon) |
| >1 | - | no | only Pairwise comparisons (Wilcoxon) |

Notes on the table:
- Monte Carlo: anova is skipped only in the exact case
  `n_predictors == 1 && n_levels_primary == 2` -- that's the one case fully
  redundant with Power analysis's t-test. With 2 predictors and a 2-level
  primary factor it's still shown, since Power analysis can't model
  interactions and Monte Carlo: anova can.
- No non-parametric omnibus test is offered for >1 predictor: Kruskal-Wallis
  is strictly one-way, and a permutation-ANOVA nested inside a Monte Carlo n
  search is computationally impractical (confirmed with Konrad -- "läuft ja
  ewig"). Pairwise comparisons (Wilcoxon) still covers that case, since
  pairwise comparisons don't care how many predictors produced the group
  labels being compared.
- All approaches only ever return `n` per predictor combination (not a
  design) -- `completely_randomised_design` is a separate, later step for all
  of them.

Subtabs:

- **Power analysis** (implemented) -> auto-picks t-test (`calc_n_two_sample_ttest`) or
  anova (`calc_n_one_way_anova`) depending on whether the primary factor has 2 or
  >2 levels. Only shown for 1-2 predictors total, normal data.
- **Pairwise comparisons** (implemented, renamed from "Monte Carlo: multiple
  comparison") -> `estimate_sample_size(means, sds, power_target, alpha, mcc, family, nsim, n_min, n_max, seed)`
  when `assume_normal == "yes"`, or `estimate_sample_size_wilcoxon(...)` (same
  signature, `wilcox.test` instead of `t.test`) when `"no"`. Always shown.
- **Monte Carlo: anova** (implemented) -> `determine_sample_size(levels, mu, cv, sd, formula, alphas, target_effect, power_target, seed, nsim, n_min, n_max)`.
  Normal data only.
- **Monte Carlo: Kruskal-Wallis** (implemented) -> `determine_sample_size_kruskal(level, mu, cv, sd, alpha, power_target, seed, nsim, n_min, n_max)`.
  Single predictor, >2 levels, non-normal only -- rejects a list of several
  predictors with a clear error (Kruskal-Wallis is one-way only).

**Planned:** the two non-parametric methods (`estimate_sample_size_wilcoxon`,
`determine_sample_size_kruskal`) currently simulate data via `rnorm(means,
sds)` and only differ from their parametric siblings in which test is applied
(`wilcox.test`/`kruskal.test` instead of `t.test`/`aov`) -- the *simulation*
distribution is still normal. Since "non-normal" was the whole reason to pick
these in the first place, a real improvement is letting the user pick the
simulation distribution itself (e.g. gamma, Weibull, beta, t, exponential),
not just the test. Not implemented yet -- would need a distribution-family
input plus matching parameters (shape/scale etc. instead of just mean/sd) in
both `Randomization` functions and the OpenDOE sidebars.

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

**Fully implemented.** `df` is picked via `df_result_choices(State, classes =
"designResult")` -- deliberately restricted to designs generated in the
"Design of experiment" tab, not an arbitrary upload or any other df-shaped
result (project discussion: this app should only ever randomize its own
generated design, not a raw import -- if a direct-upload path turns out to
be needed later it can be added back). All three methods run synchronously (fast, single-pass sampling -- no background
process needed for this one, unlike random_finite_assign below). Method
selector rebuilds the whole controls box (same convention as the Sample size
sidebar), so switching methods resets the groups/ratios/col inputs.

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
Random assignment). `groups` (target covariate profile, numeric columns
only) is picked via `df_result_choices()` (see Data model above),
unrestricted; `design` is picked via `df_result_choices(State, classes =
c("designResult", "assignmentResult"))` -- restricted, same reasoning as
Random assignment's restriction below (only ever something generated in
this app, no "upload a CSV as the design" escape hatch anymore).
`assignmentResult` counts too: a Random-assignment result is still
design-shaped (same rows, one extra assigned column, e.g. experimental day)
and a legitimate thing to balance further via Finite assignment. The CSV
upload widget lives
directly in this tab's sidebar (`FINITEASSIGN-upload`, rendered
unconditionally so it's available even before anything else exists in
Results) -- this is the only place in the app to import external data at
all (see Data model above), and it's now understood to be for the
covariate dataset specifically, not the design. This also means Finite
assignment is locked in the step diagram (`step_unlocked()`, `R/utils.R`)
behind `designResult` existing, exactly like Random assignment -- safe to
do only because the design restriction above removed the circular
dependency (needing a design to unlock the tab that used to be the only
way to import one). Client-side checks all `groups` columns
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
`seed` are.

`ids` **is** exposed now, and required (project discussion:
`Randomization::random_finite_assign()` itself keeps `ids` optional, this
is enforced only at the OpenDOE layer, not in the package) -- a required
"ID column" `selectInput` (`FINITEASSIGN-id_col`, populated from whichever
covariate dataset is currently selected) lets the user pick which column of
their covariate file identifies the physical unit (animal/sample ID). That
column is stripped out of what's passed as `groups` (so it isn't treated as
a covariate to balance) and passed separately as `ids`; the result then
carries a real `unit_id` instead of the previous `unit_index` (a
meaningless row-position number auto-added by the package, always dropped
now in `run_random_finite_assign()`, `Backend_Operations.R`). Rationale:
same one as removing the design matrix's `id` column above -- without a
real identifier the assignment result can't be traced back to which actual
animal/sample landed where, so it isn't optional.

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
