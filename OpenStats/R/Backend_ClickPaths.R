# Click paths for the workflow guide.
# A step is skipped when its `done` selector matches, e.g. the formula editor is already open
click_path_titles_V1_2 <- c(
  "Create a linear model" = "create_linear_model",
  "Check the assumptions" = "check_assumptions",
  "t test (two groups)" = "t_test",
  "ANOVA (more than two groups)" = "anova",
  "Scatterplot" = "scatterplot"
)

click_path_tab_V1_2 <- function(tab, label) {
  list(
    label = label,
    selector = sprintf("a[data-value='%s']", tab),
    done = sprintf("li.active > a[data-value='%s'], a.active[data-value='%s']", tab, tab)
  )
}

# skip_if_model: as first part of another path, skipped when a model is shown in the sidebar
click_path_linear_model_V1_2 <- function(skip_if_model) {
  model_exists <- if (skip_if_model) ", #OPENFORMULA-formulaUI .var-box-output" else ""
  # "" instead of NULL: jsonlite turns NULL into {}, which is truthy in JS
  only_model_exists <- if (skip_if_model) "#OPENFORMULA-formulaUI .var-box-output" else ""
  list(
    list(
      label = "'Open formula editor' in the sidebar",
      selector = "#OPENFORMULA-open_formula_editor",
      done = paste0("#formula_modal.in, #formula_modal.show", model_exists)
    ),
    list(
      label = "'Change the model type (optional)': choose 'Linear'",
      selector = ".form-group:has(#FO-model_type)",
      done = paste0("#FO-model_type option[value='Linear']:checked", model_exists)
    ),
    list(
      label = "'Dependent Variable': choose the outcome",
      selector = "#FO-colnames_dropdown",
      done = only_model_exists
    ),
    list(
      label = "Predictors: the 'Variables' and 'Functions' buttons or the text field",
      selector = "#FO-buttons",
      done = only_model_exists
    ),
    list(
      label = "'Create statistical model'",
      selector = "#FO-create_formula",
      done = only_model_exists
    ),
    list(
      label = "'Close' the formula editor",
      selector = "#formula_modal [data-dismiss='modal']",
      done = "#formula_modal:not(.in):not(.show)"
    )
  )
}

click_paths_V1_2 <- list(
  create_linear_model = click_path_linear_model_V1_2(skip_if_model = FALSE),
  check_assumptions = c(
    click_path_linear_model_V1_2(skip_if_model = TRUE),
    list(
      click_path_tab_V1_2("Assumption", "Tab 'Assumption'"),
      list(label = "'diagnostic plots' in the sidebar", selector = "#ASS-DiagnosticPlot")
    )
  ),
  t_test = c(
    click_path_linear_model_V1_2(skip_if_model = TRUE),
    list(
      click_path_tab_V1_2("Tests", "Tab 'Tests'"),
      click_path_tab_V1_2("Two groups", "Sub-tab 'Two groups'"),
      list(
        label = "'Test family': 'Parametric'",
        selector = "#TESTS-ParametricOrNonParametric",
        done = "#TESTS-ParametricOrNonParametric input[value='parametric']:checked"
      ),
      list(label = "'t test' in the sidebar", selector = "#TESTS-tTest")
    )
  ),
  anova = c(
    click_path_linear_model_V1_2(skip_if_model = TRUE),
    list(
      click_path_tab_V1_2("Tests", "Tab 'Tests'"),
      click_path_tab_V1_2("More than two groups", "Sub-tab 'More than two groups'"),
      list(
        label = "'Test family': 'Parametric'",
        selector = "#TESTS-ParametricOrNonParametric",
        done = "#TESTS-ParametricOrNonParametric input[value='parametric']:checked"
      ),
      list(label = "'ANOVA' in the sidebar", selector = "#TESTS-aovTest")
    )
  ),
  scatterplot = list(
    click_path_tab_V1_2("Visualisation", "Tab 'Visualisation'"),
    click_path_tab_V1_2("Scatterplot", "Sub-tab 'Scatterplot'"),
    list(label = "'Y Variable' in the sidebar", selector = ".form-group:has(#VIS-yVar)"),
    list(label = "'X Variable' in the sidebar", selector = ".form-group:has(#VIS-xVar)"),
    list(label = "'Create plot'", selector = "#VIS-CreatePlotScatter")
  )
)
