# OpenStats — Architecture

A developer-oriented map of how the package fits together. Aimed at someone new
to the codebase (or the maintainer six months from now). It documents the
*deliberate* design decisions, not just the structure.

---

## 1. What it is

A Shiny application (shipped as an R package) for interactive statistics:
data import, data wrangling, model building, assumptions, statistical tests,
correlation, visualisation, and dose-response, with a recordable, replayable
history. It can run standalone (local) or embedded with an ELN (Chemotion).

---

## 2. Versioning strategy (read this first)

This is the single most important convention and explains several "why is it
one giant file?" questions.

- Every backend component lives in a **version-suffixed environment or class**,
  e.g. env_operations_V1_2, env_check_ast_V1_2, and the R6 operation classes
  in Backend_V1_2_Engine.R.
- zzz.R::.onLoad sets a package-global VERSION <<- "1.2" and binds the
  unsuffixed names (env_operations, env_check_ast, …) to the right version
  via the **factory functions in Getter.R** (get_env_operations(VERSION) etc.).
- Getter.R is the single switchboard: get_*(version) returns the component
  for that version. The rest of the code asks the getter; it never hard-codes a
  version.

**Why the Engine is one ~2,600-line file (on purpose):** to bump to 1.3 you copy
Backend_V1_2_Engine.R --> Backend_V1_3_Engine.R. Classes that don't change are
aliased (some_class_V1_3 <- some_class_V1_2); only the changed ones are
rewritten. Keeping a version in one file makes "what changed in 1.3" a single
diff and a single getter switch. This is a migration strategy, **not** debt — do
not split it for tidiness.

---

## 3. Application state

Created once in Server_MainApp.R as reactiveValues and passed by reference
into every module (modules mutate them directly — there is no getter/setter
layer; the contract is implicit and held by convention):

| State                       |                           Key fields                                       |               Role                                                                   |
|-----------------------------|----------------------------------------------------------------------------|--------------------------------------------------------------------------------------|
| DataModelState              | df, formula, backup_df, active_df_name, rhs_string, counter_id             | active dataset + the current model formula                                           |
| ResultsState                | all_data (named list), history, counter, bgp, registered_pagers            | every computed result + the action log                                               |
| DataWranglingState          | df, df_name, intermediate_vars, code_string, counter_id                    | the data-wrangling working set                                                       |
| MethodState                 | method, storage_class                                                      | different states set in Chemotion ELN (Default, DoseResponse or VariationStatistics) |

**Result naming is collision-free by construction:** new results are keyed using
ResultsState$counter, which is incremented immediately after use and **never
decremented** (deleting a result does not roll it back). So names are unique for
the life of the session. Do not "optimise" the counter to reuse numbers.

---

## 4. The operation/command pattern

All real work is an **R6 "operation" class** in Backend_V1_2_Engine.R, one per
user action (create formula, run t-test, fit GLM, dose-response, ...). Lifecycle:

1. $new(...) — capture inputs.
2. $validate() — preconditions + **check_ast** for anything that will be evaluated (see §5).
3. $eval(ResultsState, [DataModelState, ...]) — compute, **append to ResultsState$all_data** (side effect), and log a history step.

Server modules are thin: render UI --> observeEvent --> instantiate the op class
via its getter --> validate() --> eval(...). They don't compute; they wire.
**Important: always run 1-3 in a try, or tryCatch block**

Everything which requires too much code to directly write it in the engine class
(e.g. Statistical/plotting compute) lives in dedicated env-modules:
Backend_SummarisingModel.R (model summaries + prediction plots),
Backend_Optimizing.R (curve fitting via optim/nls), Backend_LC50.R
(dose-response, drc), Backend_DiagnosticPlots.R, Backend_PlottingInternally.R.

Results are typed **S4 objects** StorageClasses.R:
LinearFormula, GeneralisedLinearFormula, OptimFormula, summaryModel, plot,
doseResponse, etc.) and rendered by Server_ResultsList.R.

---

## 5. Safe expression language (security model)

Users type/build R-like expressions for data wrangling and model formulas. The
sandbox is **Backend_CheckAst.R**:

- allowed_fcts() is a whitelist of permitted call heads (arithmetic, comparison,
  math, the Backend_Operations.R vocabulary, ~, |).
- check_ast() walks the parsed expression recursively and **rejects any call
  head or symbol** not in the whitelist (and any variable not in the known
  columns). Escapes like get, do.call, eval, ::, $, [[, {,
  function, <- are simply absent from the whitelist, so get("system")(...),
  base::system(...), x[[1]], function definitions, etc. are all rejected.

---

## 6. The expression builder component

A reusable drag/drop + type-with-autocomplete expression editor. **Decoupled by design**
into four data pieces per mode, plus a parser, plus a Shiny binding:

- **JS** (inst/www/): parser.js (pure tokenizer + recursive-descent parser +
  AST --> flat-string + arity/variable checks — no DOM/Shiny deps, unit-tested via V8 in test_Parser.R);
  expression.js (the Shiny InputBinding: drag/drop, autocomplete, docs panel, status box).
- **R** (Expression_Builder.R): per-mode creators —
  *_operator_set() (what parses)
  *_docs() (hover text)
  *_specs() (arity),
  *_palette() (which buttons show).
- **Message protocol:** the server pushes {variables, checkVariables, docs,
  columnTypes, operatorSet, specs} to the ...-expr input via
  expr_send_payload(). The binding's value is read as input[["<id>-expr"]]$text.
- **Remount handling:** on (re)mount the binding emits ...-expr-ready
  (expression.js), and the server re-sends the payload on that event — this is
  what makes the builder survive modal reopen / tab switch without a stale state.

Modes wired: data wrangling (Server_OperationsModule.R), and the formula editor
(Server_FormulaModule.R) for Linear / GLM / Linear-Mixed / Optimization-free,
each selected by builder_config() keyed on model_type.

---

## 7. History / replay

Every operation appends a JSON-serialisable step to ResultsState$history.
Backend_History.R::eval_entry() is a dispatch switch over ~30 action types;
eval_history() re-instantiates state and replays the steps on a fresh dataset.
Adding an action type means adding a case here (and the op class + getter +
storage class + server wiring) — the main change-amplification point in the code.

---

## 8. UI shell & module map

UI_MainApp.R: sidebarLayout with a tabsetPanel(id = "conditionedPanels").
Each tab's sidebar content is a conditionalPanel(condition = "input.conditionedPanels == '<Tab>'").

| Tab            | UI                                     | Server                             | Module id     |
|----------------|----------------------------------------|------------------------------------|---------------|
| Data           | DT::DTOutput                           | Server_MainApp                     |               |
| DataWrangling  | OperatorEditorUI + OperatorSidebarUI   | OperationEditorServer              | OP            |
| Visualisation  | visUI / visSidebarUI                   | visServer                          | VIS           |
| Assumption     | assUI                                  | assServer                          | ASS           |
| Correlation    | corrUI                                 | corrServer                         | CORR          |
| Tests          | testsUI                                | testsUIServer                      | TESTS         |
| Dose Response  | DoseResponseUI                         | DoseResponseServer                 | DOSERESPONSE  |
| History        | HistoryEditorUI                        | HistoryServer                      | HISTORY       |

Formula editor (FO) and split-by-group (SG) are modal-launched (Server_OpenFormula.R, Server_OpenSplitByGroup.R).

**Id convention (intentional):** ids are hardcoded with their module prefix ("OP-expr", "FO-buttons")
rather than NS()-wrapped. Each module is mounted exactly once, so the prefix *is* the namespace,
and a readable id tells you which section you're in. Do not "fix" this to NS() unless a module ever needs two
instances on screen at once.

---

## 9. Import / export

- Import: Backend_Import.R (CSV/Excel/TSV, multi-table detection, size guards),
  Backend_import_export_dose_response.R (JSON + endpoint --> assay-type table),
  Backend_import_export_variations.R (reaction variations table JSON).
- Export/download (Server_export_results.R, Server_download_file.R): branch on
  RUN_MODE (SERVER / LOCAL / serverless) x MethodState$storage_class
  (Default / DoseResponse / VariationStatistics). Local offers zip + Excel; ELN
  Default mode sends Excel back. It's intentionally branchy because it genuinely
  covers all those combinations — a refactor target, but the cases are real.
  (download.js is obsolete — R's own zip can replace it.)

---

## 10. Testing

inst/tinytest/. Two styles:
- **Functional testServer** tests drive modules by setting inputs and asserting
  on results. The builder-based modules use **background mode**
  (options(OpenStats.background = FALSE) --> handlers read code_string / rhs_string instead of
  the JS-only input$...-expr$text), so set those state fields directly in tests.
- **Parser** is tested headless via **V8** (test_Parser.R, needs the V8 Suggests).

---

## 11. Conventions to preserve (quick reference)

- One file per engine version; alias unchanged classes on bump (§2).
- ResultsState$counter is monotonic; never reuse numbers (§3).
- Anything eval'd must pass check_ast; keep get/eval/::/$/[[ out of allowed_fcts() (§5).
- glm() family objects are built via env_utils_V1_2$build_glm_family() (match.arg'd against the known set); never reintroduce eval(str2lang(paste0("stats::", family, ...))).
- Empty operator-set fields are character(0), not c() (§6).
- Hardcoded PREFIX-id is intentional; not a bug (§8).

---

## 12. Design principles & deliberate strengths

The choices below define the app's character. Keep them in mind before
"simplifying" anything.

- **Model-driven workflow (the organising idea).** The user first builds a
  statistical *model* in the formula editor; everything downstream is *derived*
  from it. The Tests tab only offers tests that fit the model (two-group vs
  >2-group, parametric vs non-parametric, LM/GLM/mixed); Assumptions and
  Visualisation likewise branch on the S4 class of DataModelState$formula. The
  model is the single source of truth for "what can I do next."

- **One centralised, reverse-ordered results list.** Every result-table, plot,
  summary, dose-response --> lands in ResultsState$all_data and is rendered
  newest-first by Server_ResultsList.R. Deceptively simple, but centralising
  output instead of scattering it per-tab (and putting the latest on top) was a
  large, deliberate UX win.

- **Reproducible, transportable analyses (history + replay).** Every action emits
  both a human-readable step (an audit trail / methods record) and a JSON entry
  (§7). The JSON replays the whole analysis on fresh data, and is **version-tagged**:
  replay dispatches to the engine version that produced it (get_correct_eval), so
  an analysis archived under 1.2 stays replayable after 1.3 ships. This is the
  deeper payoff of the §2 versioning discipline — the results list, the
  human-readable history, and the replay JSON are one append-only event stream
  viewed three ways. (For exact replay, every stochastic step must record + restore
  its seed.)

- **Multi-table import.** Backend_Import.R detects and extracts *multiple*
  tables from a single file (scanning for blank separator rows/columns), not just
  one sheet --> one frame.

- **Loss functions built from the model.** Backend_Optimizing.R constructs the
  objective (SSE) from the user's free-form formula and fits via optim/nls.

- **Whitelist-AST sandbox** (§5) — user expressions are validated against an
  allow-list, never blacklisted.

- **Defensive by construction.** Every validate(), eval() / engine call runs
  inside try/tryCatch (§4), so malformed input or a failed fit surfaces as a
  message and never crashes a session. This is a core invariant, not incidental.

- **Errors / warnings / missing state are communicated, not swallowed.**
  Helper.R provides print_err, print_warn, print_success and print_req`;
  a missing dataset or model is reported to the user rather than failing silently.

- **Minimal dependencies.** New external packages are added reluctantly; prefer
  base R or deps already in use.

- **Heavily tested.** 1000+ tinytest cases
