history_to_table <- function(history) {
  hist <- lapply(history, function(step) {
    result_name <- ifelse(is.null(step[["Result name"]]), "", step[["Result name"]])
    df <- data.frame(Step = step$type, Result = result_name)
    step <- step[!names(step) %in% c("type", "Result name")]
    names_step <- names(step)
    df$details <- paste(names_step, step, sep = ": ", collapse = "; ")
    df
  })
  hist <- do.call(rbind, hist) |> as.data.frame()
  list(hist) # NOTE: required to add it to result list otherwise the dataframe is split
}

eval_entry_V1_2 <- function(entry, DataModelState,
                            DataWranglingState, ResultsState, get_result) {
  res <- NULL
  switch(
    entry$type,
    Correlation = {
      res <- correlation_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        entry[["Correlation method"]],
        entry[["alternative hypothesis"]],
        entry[["Confidence level of the interval"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    Visualisation = {
      res <- visualisation_V1_2$new(
        df = DataModelState$df, x = entry$x, y = entry$y, method = entry[["Plot-type"]],
        xlabel = entry[["X axis label"]], type_of_x = entry[["Type of x"]], ylabel = entry[["Y axis label"]],
        colour_var = entry[["Colour variable"]], colour_legend_title = entry[["Legend title for colour"]], colour_theme = entry[["Colour theme"]],
        fill_var = entry[["Fill variable"]], fill_legend_title = entry[["Legend title for fill"]], fill_theme = entry[["Fill theme"]],
        facet_var = entry[["Split by"]], facet_y_scaling = entry[["How to scale y in subplots"]],
        xrange = char_to_orig_type(entry[["X-Range"]]), yrange = char_to_orig_type(entry[["Y-Range"]]),
        width = as.numeric(entry["Width"]), height = as.numeric(entry[["Height"]]), resolution = as.numeric(entry[["Resolution"]]),
        com = backend_communicator_V1_2
      )
      res$validate()
      res <- res$eval(ResultsState)
      get_result(ResultsState)
    },
    VisualizationModel = {
      res <- visualisation_model_V1_2$new(
        df = DataModelState$df, DataModelState$formula, entry[["Layer"]]
      )
      res$validate()
      res$eval(ResultsState)
      get_result(ResultsState)
    },
    ApplyFilter = {
      res <- apply_filter_V1_2$new(
        entry[["Variable"]], entry[["Variable levels"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(DataModelState, ResultsState)
    },
    RemoveFilter = {
      res <- remove_filter_V1_2$new()
      res$validate()
      res$eval(ResultsState, DataModelState)
    },
    CreateIntermediateVariable = {
      res <- create_intermediate_var_V1_2$new(
        DataWranglingState$df, DataWranglingState$df_name,
        DataWranglingState$intermediate_vars,
        entry[["operation"]], entry[["name"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, DataWranglingState)
    },
    RemoveIntermediateVariable = {
      res <- remove_intermediate_var_V1_2$new(
        entry[["Intermediate variable"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, DataWranglingState)
    },
    CreateNewColumn = {
      res <- create_new_col_V1_2$new(
        DataWranglingState$df, DataWranglingState$df_name,
        DataWranglingState$intermediate_vars,
        entry[["operation"]], entry[["column name"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, DataWranglingState, DataModelState)
    },
    CreateFormula = {
      f <- entry[["formula"]] |> as.formula()
      res <- create_formula_V1_2$new(
        f[2], f[3], DataModelState$df,
        backend_communicator_V1_2
      )
      res$validate()
      if (entry[["Model Type"]] == "Linear") {
        res$eval(ResultsState, DataModelState, entry[["Model Type"]])
      } else if (entry[["Model Type"]] == "Generalised Linear Model") {
        res$eval(ResultsState, DataModelState, entry[["Model Type"]],
          entry[["family"]], entry[["Link function"]])
      } else if (entry[["Model Type"]] == "Optimization Model") {
        res$eval(ResultsState, DataModelState, entry[["Model Type"]],
          entry[["lower"]], entry[["upper"]], entry[["seed"]])
      }
    },
    ShapiroOnData = {
      res <- shapiro_on_data_V1_2$new(
        DataModelState$df,  DataModelState$formula,
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    ShapiroOnResiduals = {
      res <- shapiro_on_residuals_V1_2$new(
        DataModelState$df, DataModelState$formula,
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    LeveneTest = {
      res <- levene_V1_2$new(
        DataModelState$df, DataModelState$formula,
        entry[["Data center"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    DiagnosticPlots = {
      res <- diagnostic_plots_V1_2$new(
        DataModelState$df, DataModelState$formula,
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
      get_result(ResultsState)
    },
    DoseResponse = {
      res <- dose_response_V1_2$new(
        DataModelState$df,
        entry[["Log transform x-axis"]],
        entry[["Log transform y-axis"]],
        entry[["Column containing the names"]],
        DataModelState$formula,
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, entry[["Result name"]])
      get_result(ResultsState)
    },
    TTest= {
      res <- t_test_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        entry[["The two variances are"]],
        entry[["Confidence level of the interval"]],
        entry[["alternative hypothesis"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
      get_result(ResultsState)
    },
    ANOVA = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "aov")
      get_result(ResultsState)
    },
    `Kruskal-Wallis Test` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "kruskal")
      get_result(ResultsState)
    },
    `Tukey HSD` = {
      bal <- "Unbalanced"
      if (entry[["Balanced design"]]) bal <- "Balanced"
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        bal, entry[["P-value"]], NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "HSD")
      get_result(ResultsState)
    },
    `Kruskal Wallis post hoc test` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, entry[["P-value"]], entry[["Adjusted p value method"]], backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "kruskalTest")
      get_result(ResultsState)
    },
    `Least significant difference test` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, entry[["P-value"]], entry[["Adjusted p value method"]], backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "LSD")
      get_result(ResultsState)
    },
    `Scheffe post hoc test` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, entry[["P-value"]], NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "scheffe")
      get_result(ResultsState)
    },
    `REGW post hoc test` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, entry[["P-value"]], NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "REGW")
      get_result(ResultsState)
    },
    # Start glm
    `ANOVA-Chisq` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "aov")
      get_result(ResultsState)
    }, # Kruskal-Wallis is the same as for lm and also recognised there
    `GLM PostHoc adjusted by: tukey` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "tukey")
      get_result(ResultsState)
    },
    `GLM PostHoc adjusted by: sidak` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "sidak")
      get_result(ResultsState)
    },
    `GLM PostHoc adjusted by: bonferroni` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "bonferroni")
      get_result(ResultsState)
    },
    `GLM PostHoc adjusted by: scheffe` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "scheffe")
      get_result(ResultsState)
    },
    `GLM PostHoc adjusted by: none` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "none")
      get_result(ResultsState)
    },
    `GLM PostHoc adjusted by: fdr` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "fdr")
      get_result(ResultsState)
    },
    `GLM PostHoc adjusted by: holm` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "holm")
      get_result(ResultsState)
    },
    `GLM PostHoc adjusted by: hochberg` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "hochberg")
      get_result(ResultsState)
    },
    `GLM PostHoc adjusted by: hommel` = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df,
        DataModelState$formula,
        NULL, NULL, NULL, backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, "hommel")
      get_result(ResultsState)
    },
    RemoveResult = {
      res <- remove_result_V1_2$new(entry[["Entry removed"]])
      res$validate()
      res$eval(ResultsState)
    },
    SetActiveTable = {
      res <- set_active_table_V1_2$new(entry[["NewActiveTable"]])
      res$eval(ResultsState, DataModelState)
    }
  )
  if (is.null(res)) {
    stop(sprintf("Unknown entry in history called: %s", entry$type))
  }
  return(res)
}

get_current_version <- function() {
  return("1_2")
}

get_available_versions <- function() {
  return(
    c("1_2")
  )
}

get_correct_eval <- function(version) {
  list("1_2" = eval_entry_V1_2)[version]
}
get_correct_result_state <- function(version) {
  list("1_2" = backend_result_state_V1_2)[version]
}
get_correct_data_model_state <- function(version) {
  list("1_2" = backend_data_model_state_V1_2)[version]
}
get_correct_data_wrangling_state <- function(version) {
  list("1_2" = backend_data_wrangling_state_V1_2)[version]
}
get_correct_get_result_fct <- function(version) {
  list("1_2" = backend_get_result_V1_2)[version]
}

eval_history <- function(json_string, df, all_data, backend = FALSE) {
  print_err_in_eval_history <- print_err
  if (backend) { # Make it testable
    print_err_in_eval_history <- print
  }
  e <- try({
    l <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
    version = l[[1]]
    stopifnot(version$type == "Version")
    stopifnot(version$Nr %in% get_available_versions())
    l <- l[-1] # Remove version step as it is not evaluated
    eval_entry <- get_correct_eval(version$Nr)[[1]]
    result_state <- get_correct_result_state(version$Nr)[[1]]$new(all_data)
    result_state$bgp$in_backend <- TRUE
    data_model_state <- get_correct_data_model_state(version$Nr)[[1]]$new(df)
    data_wrangling_state <- get_correct_data_wrangling_state(version$Nr)[[1]]$new(data_model_state)
    get_result <- get_correct_get_result_fct(version$Nr)[[1]]
    for (i in seq_along(l)) {
      inner_e <- try({
        if (l[[i]]$type == "ModelSummary") {
          next
        }
        eval_entry(l[[i]], data_model_state,
          data_wrangling_state, result_state, get_result)
      })
      if (inherits(inner_e, "try-error")) {
        err <- conditionMessage(attr(inner_e, "condition"))
        print_err_in_eval_history(sprintf("Error in step: %s", i + 1))
        stop(err)
      }
    }
  }, silent = TRUE)
  if (inherits(e, "try-error")) {
    err <- conditionMessage(attr(e, "condition"))
    print_err_in_eval_history(err)
    return()
  }

  return(
    structure(
      list(
        ResultsState = result_state,
        DataModelState = data_model_state,
        DataWranglingState = data_wrangling_state
      ),
      class = "HistoryResult"
    )
  )
}
