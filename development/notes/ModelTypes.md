# Model Types Kanbas ToDo board

- [ ] Add Optimization tab
  - [x] Backend for fitting a formula to data ./development/Refression.R
  - [ ] Implement engine class
  - [ ] Update replay history
  - [ ] Write UI

- [ ] Organise the Test-Tab as following:

| Tests      |                       |                     |     |     |
|------------|-----------------------|---------------------|-----|-----|
| Two groups | More than two groups  | PostHoc Parameteric | GLM | LMM |

- [ ] Make the seperation between Parameteric and NonParameteric tests clear.
      Add a note to Kruskall Wallis tests

- [ ] Organise the Assumptions-tab as following:

| Assumptions              |     |     |
|--------------------------|-----|-----|
| Linear model             | GLM | LMM |

- [ ] Implement assumptions for glm and lmm
- [ ] GeneralizedLinear: Only partial support
    * Residual plots are possible but depend on link and family.
    * Diagnostic plots require care.
    * Recommend summarised output and dispersion/residual analysis.
- [ ] LinearMixed: Use residuals and random effects diagnostics
    * Plots of residuals per group
    * Check for overfitting
    * Optionally support packages like `DHARMa` (you could skip external deps at first)

- [ ] The current formula editor is used as input for all tabs except: Optimization, GLM and LMM.
      Implement specialised formula editors for:
  - [ ] Optimization
  - [ ] GLM
  - [ ] LMM
  - [ ] Within DataModelState:
    * [ ] formula
    * [ ] optimization_formula
    * [ ] glm_formula --> A class holding a formula and a link function
    * [ ] llm_formula
