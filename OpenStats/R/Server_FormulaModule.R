FormulaEditorServer <- function(id, DataModelState, ResultsState) {
  shiny::moduleServer(id, function(input, output, session) {

    # Per-model-type builder config (NULL = keep the operator-button palette).
    builder_config <- shiny::reactive({
      df <- DataModelState$df
      if (is.null(input$model_type)) return()
      switch(input$model_type,

        "Linear" = list(
          operator_set = linear_formula_operator_set(), docs = linear_formula_docs(),
          specs = linear_formula_specs(), palette = linear_formula_palette(),
          variables = names(df), check_variables = TRUE
        ),

        "Generalised Linear Model" = list(
          operator_set = generalized_linear_formula_operator_set(), docs = generalized_linear_formula_docs(),
          specs = generalized_linear_formula_specs(), palette = generalized_linear_formula_palette(),
          variables = names(df), check_variables = TRUE
        ),

        "Linear Mixed Model" = list(
          operator_set = linear_mixed_formula_operator_set(), docs = linear_mixed_formula_docs(),
          specs = linear_mixed_formula_specs(), palette = linear_mixed_formula_palette(),
          variables = names(df), check_variables = TRUE
        ),

        "Optimization Model" = if (isTRUE(input$PredefinedModels == "free")) list(
          operator_set = optim_formula_operator_set(), docs = optim_formula_docs(),
          specs = optim_formula_specs(), palette = optim_formula_palette(),
          variables = names(df)[sapply(df, is.numeric)], check_variables = FALSE
        )
        else NULL,
        NULL
      )
    })

    # Push the payload when the binding (re)mounts or the data changes.
    send_builder <- function() {
      cfg <- builder_config()
      if (is.null(cfg)) return(invisible())
      expr_send_payload(
        session, DataModelState, check_variables = cfg$check_variables,
        operator_set = cfg$operator_set, docs = cfg$docs, specs = cfg$specs,
        variables = cfg$variables
      )
    }
    shiny::observeEvent(input[["expr-ready"]], send_builder(), ignoreNULL = TRUE)
    shiny::observeEvent(DataModelState$df, send_builder(), ignoreNULL = TRUE)

    # Render the builder for the active model type. cfg is NULL when there is no
    # builder (predefined optimization models use the structured UI below).
    output[["buttons"]] <- shiny::renderUI({
      shiny::req(is.data.frame(DataModelState$df))
      cfg <- builder_config()
      if (is.null(cfg)) return(NULL)
      expr_builder_ui(id, cfg$palette, cfg$variables)
    })

    # Create colnames dropdown
    output[["colnames_dropdown"]] <- shiny::renderUI({
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      if (input$model_type == "Optimization Model") {
        shiny::req(input$PredefinedModels)
        if(input$PredefinedModels != "free") return(NULL)
      }
      colnames <- ""
      if (input$model_type %in% c("Linear", "Generalised Linear Model", "Linear Mixed Model")) {
        colnames <- names(DataModelState$df)
      } else if (input$model_type == "Optimization Model") {
        indices <- sapply(DataModelState$df, is.numeric) |> which()
        colnames <- names(DataModelState$df)[indices]
      }
      tooltip <- "Select the dependent variable for your statistical model. This is the outcome you want to predict based on the independent variables."
      htmltools::div(
        class = "boxed-output",
        shiny::tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        shiny::selectInput(
          inputId = paste0("FO-colnames-dropdown_", DataModelState$counter_id),
          label = "Dependent Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    # Predefined models
    output[["optim_predefined_equations"]] <- shiny::renderUI({
      if (input$model_type == "Optimization Model") {
        shiny::selectInput(inputId = "FO-PredefinedModels", "Available functions",
          choices = c(
            "Linear" = "linear",
            "Log-linear" = "log_linear",
            "Michaelis-Menten" = "michaelis_menten",
            "One-site binding" = "one_site_binding",
            "Two-hot binding kinetics" = "two_hot_binding",
            "Free formula (custom)" = "free"
          ),
          selectize = TRUE
        )
      }
    })

    # Create predefined model UIs
    output[["predefined_modelsUI"]] <- shiny::renderUI({
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      if (input$model_type == "Optimization Model") {
        shiny::req(input$PredefinedModels)
        if (input$PredefinedModels == "free") return()
        indices <- sapply(DataModelState$df, is.numeric) |> which()
        colnames <- names(DataModelState$df)[indices]
        element_list <- list()
        if (input$PredefinedModels == "linear") {
          element_list[[length(element_list) + 1]] <- htmltools::div(
            style = "padding: 10px; border-radius: 8px; display: flex; flex-direction: column; align-items: flex-start;",
            htmltools::div(
              style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
              htmltools::span("y = Slope * x + Intercept")
            ),
            htmltools::div(
              style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
              shiny::selectInput("FO-linear_lhs_var", label = "y", choices = colnames, width = "125px"),
              shiny::textInput("FO-linear_slope", label = "Slope", value = "Slope", width = "100px"),
              shiny::selectInput("FO-linear_x", label = "x", choices = colnames, width = "125px"),
              shiny::textInput("FO-linear_intercept", label = "Intercept", value = "Intercept", width = "125px")
            )
          )
        } else if (input$PredefinedModels == "log_linear") {
          element_list[[length(element_list) + 1]] <- htmltools::div(
            style = "padding: 10px; border-radius: 8px; display: flex; flex-direction: column; align-items: flex-start;",
            htmltools::div(
              style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
              htmltools::span("y = Slope * log(x) + Intercept")
            ),
            htmltools::div(
              style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
              shiny::selectInput("FO-log_linear_lhs_var", label = "y", choices = colnames, width = "125px"),
              shiny::textInput("FO-log_linear_slope", label = "Slope", value = "Slope", width = "100px"),
              shiny::selectInput("FO-log_linear_x", label = "x", choices = colnames, width = "125px"),
              shiny::textInput("FO-log_linear_intercept", label = "Intercept", value = "Intercept", width = "125px")
            )
          )
        } else if (input$PredefinedModels == "michaelis_menten") {
          element_list[[length(element_list) + 1]] <- htmltools::div(
            style = "padding: 10px; border-radius: 8px; display: flex; flex-direction: column; align-items: flex-start;",
            htmltools::div(
              style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
              htmltools::span("y = (Vmax * s) / (Km + s)")
            ),
            htmltools::div(
              style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
              shiny::selectInput("FO-mm_lhs_var", label = "y", choices = colnames, width = "125px"),
              shiny::textInput("FO-mm_vmax", label = "Vmax", value = "Vmax", width = "100px"),
              shiny::selectInput("FO-mm_x", label = "conc. of (s)", choices = colnames, width = "125px"),
              shiny::textInput("FO-mm_km", label = "Km", value = "Km", width = "100px")
            )
          )
        } else if (input$PredefinedModels == "one_site_binding") {
          element_list[[length(element_list) + 1]] <- htmltools::div(
            style = "padding: 10px; border-radius: 8px; display: flex; flex-direction: column; align-items: flex-start;",
            htmltools::div(
              style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
              htmltools::span("y = (Bmax * conc) / (Kd + conc)")
            ),
            htmltools::div(
              style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
              shiny::selectInput("FO-binding_lhs_var", label = "y", choices = colnames, width = "125px"),
              shiny::textInput("FO-binding_bmax", label = "Bmax", value = "Bmax", width = "100px"),
              shiny::selectInput("FO-binding_x", label = "conc", choices = colnames, width = "125px"),
              shiny::textInput("FO-binding_kd", label = "Kd", value = "Kd", width = "100px")
            )
          )
        }  else if (input$PredefinedModels == "two_hot_binding") {
          element_list[[length(element_list) + 1]] <- htmltools::div(
            style = "padding: 10px; border-radius: 8px; display: flex; flex-direction: column; align-items: flex-start;",
            htmltools::div(
              style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
              htmltools::span("y = (conc * 1e-9) / (conc * 1e-9 + Koff / Kon) * Bmax * (1 - exp(-(Kon * conc * 1e-9 + Koff) * Time))")
            ),
            htmltools::div(
              style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
              shiny::selectInput("FO-hotbind_lhs_var", label = "y", choices = colnames, width = "125px"),
              shiny::selectInput("FO-hotbind_conc", label = "conc", choices = colnames, width = "125px"),
              shiny::textInput("FO-hotbind_koff", label = "Koff", value = "koff", width = "80px"),
              shiny::textInput("FO-hotbind_kon", label = "Kon", value = "kon", width = "80px"),
              shiny::textInput("FO-hotbind_bmax", label = "Bmax", value = "Bmax", width = "100px"),
              shiny::selectInput("FO-hotbind_time", label = "Time", choices = colnames, width = "125px")
            )
          )
        }
        htmltools::div(
          do.call(htmltools::tagList, element_list),
          class = "boxed-output"
        )
      }
    })


    # If glm is choosen create family
    output[["glm_family_dropdown"]] <- shiny::renderUI({
      if (input$model_type == "Generalised Linear Model") {
        shiny::selectInput(inputId = "FO-Family", "The distribution family which describes the residuals",
          c(
            "binomial" = "binomial",
            "gaussian" = "gaussian",
            "Gamma" = "Gamma",
            "inverse.gaussian" = "inverse.gaussian",
            "poisson" = "poisson",
            "quasi" = "quasi",
            "quasibinomial" = "quasibinomial",
            "quasipoisson" = "quasipoisson"
          ),
          selectize = TRUE
        )
      }
    })
    # If glm is choosen create link function
    output[["glm_link_fct_dropdown"]] <- shiny::renderUI({
      shiny::req(input$Family)
      if (input$model_type == "Generalised Linear Model") {
        if (input[["Family"]] == "binomial") {
          shiny::selectInput("FO-Link_function", "The link function",
            c(
              "logit" = "logit",
              "probit" = "probit",
              "cauchit" = "cauchit"
            ),
            selectize = TRUE
          )
        } else if (input[["Family"]] %in% c("gaussian", "Gamma")) {
          shiny::selectInput("FO-Link_function", "The link function",
            c(
              "identity" = "identity",
              "log" = "log",
              "inverse" = "inverse"
            ),
            selectize = TRUE
          )
        } else if (input[["Family"]] == "inverse.gaussian") {
          shiny::selectInput("FO-Link_function", "The link function",
            c(
              "identity" = "identity",
              "log" = "log",
              "inverse" = "inverse",
              "1/mu^2" = "1/mu^2"
            ),
            selectize = TRUE
          )
        } else if (input[["Family"]] == "poisson") {
          shiny::selectInput("FO-Link_function", "The link function",
            c(
              "identity" = "identity",
              "log" = "log",
              "sqrt" = "sqrt"
            ),
            selectize = TRUE
          )
        } else if (input[["Family"]] %in% c("quasi", "quasibinomial", "quasipoisson")) {
          shiny::selectInput("FO-Link_function", "The link function",
            c(
              "identity" = "identity",
              "inverse" = "inverse",
              "log" = "log",
              "cloglog" = "cloglog",
              "logit" = "logit",
              "probit" = "probit",
              "1/mu^2" = "1/mu^2",
              "sqrt" = "sqrt"
            ),
            selectize = TRUE
          )
        }
      }
    })
    # Optim UI
    output[["optim_boundaries_and_method"]] <- shiny::renderUI({
      if (input$model_type %in% c("Linear", "Generalised Linear Model", "Linear Mixed Model")) {
        NULL
      } else if (input$model_type == "Optimization Model") {
        htmltools::div(
          shiny::selectInput(
            "FO-optim_method", "Optimization method",
            c(
              "general purpose optimization" = "general purpose optimization",
              "nonlinear least squares" = "nonlinear least squares"
            ),
            selectize = TRUE
          ),
          shiny::numericInput("FO-LowerBoundary", "Lower boundary of parameters", value = 0),
          shiny::numericInput("FO-UpperBoundary", "Upper boundary of parameters", value = 100),
          shiny::numericInput("FO-Seed", "Seed (start value for random number generation)", value = sample(1:10^6, 1))
        )
      }
    })

    # React to create formula
    shiny::observeEvent(input$create_formula, {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      tryCatch({
        withCallingHandlers(
          expr = {
            response_var <- NULL
            right_site <- NULL
            if (input$model_type == "Optimization Model") {
              if (input$PredefinedModels == "linear") {
                response_var <- input[["linear_lhs_var"]]
                right_site <- paste0(input[["linear_slope"]], "*", input[["linear_x"]], "+", input[["linear_intercept"]])
              } else if (input$PredefinedModels == "log_linear") {
                response_var <- input[["log_linear_lhs_var"]]
                right_site <- paste0(input[["log_linear_slope"]], "*log(", input[["log_linear_x"]], ")+", input[["log_linear_intercept"]])
              } else if (input$PredefinedModels == "michaelis_menten") {
                response_var <- input[["mm_lhs_var"]]
                right_site <- paste0( "(", input[["mm_vmax"]], "*", input[["mm_x"]], ") / (", input[["mm_km"]], "+", input[["mm_x"]], ")")
              } else if (input$PredefinedModels == "one_site_binding") {
                response_var <- input[["binding_lhs_var"]]
                right_site <- paste0( "(", input[["binding_bmax"]], "*", input[["binding_x"]], ") / (", input[["binding_kd"]],
                  "+", input[["binding_x"]], ")")
              } else if (input$PredefinedModels == "two_hot_binding") {
                # TODO: is only one conc correct?
                response_var <- input[["hotbind_lhs_var"]]
                conc <- input[["hotbind_conc"]]
                koff <- input[["hotbind_koff"]]
                kon <- input[["hotbind_kon"]]
                bmax <- input[["hotbind_bmax"]]
                time <- input[["hotbind_time"]]
                first_term <- sprintf("((%s * 1e-9) / (%s * 1e-9 + %s / %s))", conc, conc, koff, kon)
                second_term <- sprintf("(1 - exp(-(%s * %s * 1e-9 + %s) * %s))", kon, conc, koff, time)
                right_site <- sprintf("%s * %s * %s", first_term, bmax, second_term)
              } else if (input$PredefinedModels == "free") {
                response_var <- input[[paste0("colnames-dropdown_", DataModelState$counter_id)]]
                background <- !getOption("OpenStats.background", TRUE)
                right_site <- if (background) DataModelState$rhs_string else input$expr$text
              }
            } else {
              response_var <- input[[paste0("colnames-dropdown_", DataModelState$counter_id)]]
              background <- !getOption("OpenStats.background", TRUE)
              right_site <- if (background) DataModelState$rhs_string else input$expr$text
            }
            cf <- get_create_formula()$new(response_var, right_site, DataModelState$df)
            cf$validate()
            model_latex <- NULL
            if (input$model_type == "Linear") {
              model_latex <- cf$eval(ResultsState, DataModelState, input$model_type)
            } else if (input$model_type == "Generalised Linear Model") {
              model_latex <- cf$eval(ResultsState, DataModelState, input$model_type, input$Family, input$`Link_function`)
            } else if (input$model_type == "Optimization Model") {
              model_latex <- cf$eval(
                ResultsState, DataModelState, input$model_type,
                input$optim_method ,input$LowerBoundary, input$UpperBoundary, input$Seed
              )
            } else if (input$model_type == "Linear Mixed Model") {
              model_latex <- cf$eval(ResultsState, DataModelState, input$model_type)
            }
            output$model <- shiny::renderUI({
              if (is.null(model_latex)) return(NULL)
              shiny::withMathJax(htmltools::HTML(paste0("$$", model_latex, "$$")))
            })
          },
          warning = function(warn) {
            print_warn(warn$message)
            invokeRestart("muffleWarning")
          }
        )},
        error = function(err){
          print_err("Invalid formula")
          print_err(err$message)
        }
      )
    })

  })
}
