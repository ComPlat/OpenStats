FormulaEditorUI <- function(id) {
  ui <- fluidPage(
    fluidRow(
      uiOutput(NS(id, "model")),
      uiOutput(NS(id, "predefined_modelsUI")),
      column(
        width = 6,
        uiOutput(NS(id, "colnames_dropdown")),
        div(
          selectInput(NS(id, "model_type"), "Change the model type (optional)",
            c(
              "Linear" = "Linear",
              "Generalised Linear Model" = "Generalised Linear Model",
              "Optimization Model" = "Optimization Model"
            ),
            selectize = FALSE
          ),
          uiOutput(NS(id, "glm_family_dropdown")),
          uiOutput(NS(id, "glm_link_fct_dropdown")),
          uiOutput(NS(id, "optim_predefined_equations")),
          uiOutput(NS(id, "optim_boundaries")),
          class = "boxed-output"
        )
      ),
      column(
        width = 6,
        div(
          uiOutput(NS(id, "colnames_list")),
          uiOutput(NS(id, "buttons")),
          uiOutput(NS(id, "rhs")),
          actionButton(NS(id, "create_formula"), "Create statistical model", class = "create_button"),
          class = "boxed-output"
        )
      )
    )
  )
}

FormulaEditorServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {

    # Create buttons
    output[["buttons"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if (input$model_type == "Optimization Model") {
        req(input$PredefinedModels)
        if(input$PredefinedModels != "free") return(NULL)
      }
      button_list <- list(
        actionButton("FO-add", "+",
          class = "add-button",
          title = "Include an additional predictor variable in the model"
        ),
        actionButton("FO-minus", "-",
          class = "add-button",
          title = "Removes an additional predictor variable in the model"
        ),
        actionButton("FO-mul", "*",
          class = "add-button",
          title = "Multiply variables to assess interactions in the model"
        )
      )
      if (input$model_type == "Linear" || input$model_type == "Generalised Linear Model") {
        button_list[[length(button_list) + 1]] <- actionButton("FO-colon", ":",
          class = "add-button",
          title = "Includes the interaction between two variables in the model"
        )
      } else if (input$model_type == "Optimization Model") {
        button_list[[length(button_list) + 1]]  <- actionButton("FO-div", "/",
          class = "add-button",
          title = "Includes nested effects (both variable levels) in the model"
        )
      }
      div(
        div(
          hr(),
          do.call(tagList, button_list)
        )
      )
    })

    # Create colnames buttons
    output[["colnames_list"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if (input$model_type == "Optimization Model") {
        req(input$PredefinedModels)
        if(input$PredefinedModels != "free") return(NULL)
      }
      colnames <- ""
      if (input$model_type == "Linear" || input$model_type == "Generalised Linear Model") {
        colnames <- names(DataModelState$df)
      } else if (input$model_type == "Optimization Model") {
        indices <- sapply(DataModelState$df, is.numeric) |> which()
        colnames <- names(DataModelState$df)[indices]
      }
      button_list <- lapply(colnames[1:length(colnames)], function(i) {
        actionButton(
          inputId = paste0("FO-colnames_", i, "_", DataModelState$counter_id),
          label = paste(i),
          class = "add-button",
          title = paste("Select variable", i, "as a predictor for the model")
        )
      })
      div(
        div(
          do.call(tagList, button_list),
          br()
        )
      )
    })

    # Create colnames dropdown
    output[["colnames_dropdown"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if (input$model_type == "Optimization Model") {
        req(input$PredefinedModels)
        if(input$PredefinedModels != "free") return(NULL)
      }
      colnames <- ""
      if (input$model_type == "Linear" || input$model_type == "Generalised Linear Model") {
        colnames <- names(DataModelState$df)
      } else if (input$model_type == "Optimization Model") {
        indices <- sapply(DataModelState$df, is.numeric) |> which()
        colnames <- names(DataModelState$df)[indices]
      }
      tooltip <- "Select the dependent variable for your statistical model. This is the outcome you want to predict based on the independent variables."
      div(
        class = "boxed-output",
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("FO-colnames-dropdown_", DataModelState$counter_id),
          label = "Dependent Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    # Predefined models
    output[["optim_predefined_equations"]] <- renderUI({
      if (input$model_type == "Optimization Model") {
        selectInput(inputId = "FO-PredefinedModels", "Available functions",
          choices = c(
            "Linear" = "linear",
            "Log-linear" = "log_linear",
            "Michaelis-Menten" = "michaelis_menten",
            "One-site binding" = "one_site_binding",
            "Two-hot binding kinetics" = "two_hot_binding",
            "Free formula (custom)" = "free"
          ),
          selectize = FALSE
        )
      }
    })

    # Create right site
    output[["rhs"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if (input$model_type == "Linear" || input$model_type == "Generalised Linear Model") {
        div(
          hr(),
          textAreaInput("FO-editable_code", "Formula terms:", value = "", rows = 12)
        )
      } else if (input$model_type == "Optimization Model") {
        req(input$PredefinedModels)
        if(input$PredefinedModels == "free") {
          div(
            hr(),
            textAreaInput("FO-editable_code", "formula terms:", value = "", rows = 12)
          )
        }
      }
    })
    # Create predefined model UIs
    output[["predefined_modelsUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if (input$model_type == "Optimization Model") {
        req(input$PredefinedModels)
        if (input$PredefinedModels == "free") return()
        indices <- sapply(DataModelState$df, is.numeric) |> which()
        colnames <- names(DataModelState$df)[indices]
        element_list <- list()
        if (input$PredefinedModels == "linear") {
          element_list[[length(element_list) + 1]] <- div(
            style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
            selectInput("FO-linear_lhs_var", label = NULL, choices = colnames, width = "125px"),
            span(" = "),
            textInput("FO-linear_slope", label = NULL, value = "Slope", width = "100px"),
            span(" × "),
            selectInput("FO-linear_x", label = NULL, choices = colnames, width = "125px"),
            span(" + "),
            textInput("FO-linear_intercept", label = NULL, value = "Intercept", width = "125px")
          )
        } else if (input$PredefinedModels == "log_linear") {
          element_list[[length(element_list) + 1]] <- div(
            style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
            selectInput("FO-log_linear_lhs_var", label = NULL, choices = colnames, width = "125px"),
            span(" = "),
            textInput("FO-log_linear_slope", label = NULL, value = "Slope", width = "100px"),
            span(" × log("),
            selectInput("FO-log_linear_x", label = NULL, choices = colnames, width = "125px"),
            span(") + "),
            textInput("FO-log_linear_intercept", label = NULL, value = "Intercept", width = "125px")
          )
        } else if (input$PredefinedModels == "michaelis_menten") {
          element_list[[length(element_list) + 1]] <- div(
            style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
            selectInput("FO-mm_lhs_var", label = NULL, choices = colnames, width = "125px"),
            span(" = "),
            textInput("FO-mm_vmax", label = NULL, value = "Vmax", width = "100px"),
            span(" × "),
            selectInput("FO-mm_x", label = NULL, choices = colnames, width = "125px"),
            span(" / ("),
            textInput("FO-mm_km", label = NULL, value = "Km", width = "100px"),
            span(" + "),
            selectInput("FO-mm_x2", label = NULL, choices = colnames, width = "125px"),
            span(")")
          )
        } else if (input$PredefinedModels == "one_site_binding") {
          element_list[[length(element_list) + 1]] <- div(
            style = "padding: 10px; border-radius: 8px; display: flex; align-items: center;",
            selectInput("FO-binding_lhs_var", label = NULL, choices = colnames, width = "125px"),
            span(" = "),
            textInput("FO-binding_bmax", label = NULL, value = "Bmax", width = "100px"),
            span(" × "),
            selectInput("FO-binding_x", label = NULL, choices = colnames, width = "125px"),
            span(" / ("),
            textInput("FO-binding_kd", label = NULL, value = "Kd", width = "100px"),
            span(" + "),
            selectInput("FO-binding_x2", label = NULL, choices = colnames, width = "125px"),
            span(")")
          )
        }  else if (input$PredefinedModels == "two_hot_binding") {
          element_list[[length(element_list) + 1]] <- div(
            style = "padding: 10px; border-radius: 8px; display: flex; flex-direction: column; gap: 8px;",
            div(
              style = "display: flex; align-items: center; gap: 12px;",
              selectInput("FO-hotbind_lhs_var", label = NULL, choices = colnames, width = "125px"),
              span(" = ("),
              selectInput("FO-hotbind_conc", label = NULL, choices = colnames, width = "125px"),
              span(" × 1e-9 ) / ("),
              selectInput("FO-hotbind_conc2", label = NULL, choices = colnames, width = "125px"),
              span(" × 1e-9 + "),
              textInput("FO-hotbind_koff", label = NULL, value = "koff", width = "80px"),
              span(" / "),
              textInput("FO-hotbind_kon", label = NULL, value = "kon", width = "80px"),
              span(")")
            ),
            div(
              style = "display: flex; align-items: center; gap: 12px;",
              span(" × "),
              textInput("FO-hotbind_bmax", label = NULL, value = "Bmax", width = "100px"),
              span(" × (1 - exp(-("),
              textInput("FO-hotbind_kon2", label = NULL, value = "kon", width = "80px"),
              span(" × "),
              selectInput("FO-hotbind_conc3", label = NULL, choices = colnames, width = "125px"),
              span(" × 1e-9 + "),
              textInput("FO-hotbind_koff2", label = NULL, value = "koff", width = "80px"),
              span(") × "),
              selectInput("FO-hotbind_time", label = NULL, choices = colnames, width = "125px"),
              span("))")
            )
          )
        }
        div(
          do.call(tagList, element_list),
          class = "boxed-output"
        )
      }
    })


    # If glm is choosen create family
    output[["glm_family_dropdown"]] <- renderUI({
      if (input$model_type == "Generalised Linear Model") {
        selectInput(inputId = "FO-Family", "The distribution family which describes the residuals",
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
          selectize = FALSE
        )
      }
    })
    # If glm is choosen create link function
    output[["glm_link_fct_dropdown"]] <- renderUI({
      req(input$Family)
      if (input$model_type == "Generalised Linear Model") {
        if (input[["Family"]] == "binomial") {
          selectInput("FO-Link_function", "The link function",
            c(
              "logit" = "logit",
              "probit" = "probit",
              "cauchit" = "cauchit"
            ),
            selectize = FALSE
          )
        } else if (input[["Family"]] %in% c("gaussian", "Gamma")) {
          selectInput("FO-Link_function", "The link function",
            c(
              "identity" = "identity",
              "log" = "log",
              "inverse" = "inverse"
            ),
            selectize = FALSE
          )
        } else if (input[["Family"]] == "inverse.gaussian") {
          selectInput("FO-Link_function", "The link function",
            c(
              "identity" = "identity",
              "log" = "log",
              "inverse" = "inverse",
              "1/mu^2" = "1/mu^2"
            ),
            selectize = FALSE
          )
        } else if (input[["Family"]] == "poisson") {
          selectInput("FO-Link_function", "The link function",
            c(
              "identity" = "identity",
              "log" = "log",
              "sqrt" = "sqrt"
            ),
            selectize = FALSE
          )
        } else if (input[["Family"]] %in% c("quasi", "quasibinomial", "quasipoisson")) {
          selectInput("FO-Link_function", "The link function",
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
            selectize = FALSE
          )
        }
      }
    })
    # Optim UI
    output[["optim_boundaries"]] <- renderUI({
      if (input$model_type == "Linear" || input$model_type == "Generalised Linear Model") {
        NULL
      } else if (input$model_type == "Optimization Model") {
        div(
          numericInput("FO-LowerBoundary", "Lower boundary of parameters", value = 0),
          numericInput("FO-UpperBoundary", "Upper boundary of parameters", value = 100),
          numericInput("FO-Seed", "Seed (start value for random number generation)", value = sample(1:10^6, 1))
        )
      }
    })

    # React to colnames buttons
    observe({
      req(DataModelState$df)
      colnames <- names(DataModelState$df)
      lapply(colnames, function(col) {
        observeEvent(input[[paste0("colnames_", col, "_", DataModelState$counter_id)]], {
          current_text <- input[["editable_code"]]
          updated_text <- paste(current_text, col, sep = " ")
          updateTextAreaInput(session, "editable_code", value = updated_text)
        })
      })
    })

    observeEvent(input$add, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "+", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$mul, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "*", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$minus, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "-", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$colon, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ":", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$div, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "/", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$nested, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "%in%", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$interaction_level, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "^", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$I, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "I(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    # React to create formula
    observeEvent(input$create_formula, {
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
                right_site <- paste0( "(", input[["mm_vmax"]], "*", input[["mm_x"]], ") / (", input[["mm_km"]], "+", input[["mm_x2"]], ")")
              } else if (input$PredefinedModels == "one_site_binding") {
                response_var <- input[["binding_lhs_var"]]
                right_site <- paste0( "(", input[["binding_bmax"]], "*", input[["binding_x"]], ") / (", input[["binding_kd"]], "+", input[["binding_x2"]], ")")
              } else if (input$PredefinedModels == "two_hot_binding") {
                response_var <- input[["hotbind_lhs_var"]]
                conc <- input[["hotbind_conc"]]
                conc2 <- input[["hotbind_conc2"]]
                conc3 <- input[["hotbind_conc3"]]
                koff <- input[["hotbind_koff"]]
                kon <- input[["hotbind_kon"]]
                bmax <- input[["hotbind_bmax"]]
                time <- input[["hotbind_time"]]
                first_term <- sprintf("((%s * 1e-9) / (%s * 1e-9 + %s / %s))", conc, conc2, koff, kon)
                second_term <- sprintf("(1 - exp(-(%s * %s * 1e-9 + %s) * %s))", kon, conc3, koff, time)
                right_site <- sprintf("%s * %s * %s", first_term, bmax, second_term)
              } else if (input$PredefinedModels == "free") {
                response_var <- input[[paste0("colnames-dropdown_", DataModelState$counter_id)]]
                right_site <- input[["editable_code"]]
              }
            } else {
              response_var <- input[[paste0("colnames-dropdown_", DataModelState$counter_id)]]
              right_site <- input[["editable_code"]]
            }
            cf <- create_formula_V1_2$new(response_var, right_site, DataModelState$df)
            cf$validate()
            model_latex <- NULL
            if (input$model_type == "Linear") {
              model_latex <- cf$eval(ResultsState, DataModelState, input$model_type)
            } else if (input$model_type == "Generalised Linear Model") {
              model_latex <- cf$eval(ResultsState, DataModelState, input$model_type, input$Family, input$`Link_function`)
            } else if (input$model_type == "Optimization Model") {
              model_latex <- cf$eval(
                ResultsState, DataModelState, input$model_type,
                input$LowerBoundary, input$UpperBoundary, input$Seed
              )
            }
            output$model <- renderUI({
              withMathJax(HTML(paste0("$$", model_latex, "$$")))
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
