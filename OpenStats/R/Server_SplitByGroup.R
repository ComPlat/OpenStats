SplitByGroupServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {
    # Reactive values
    SplitByGroupState <- reactiveValues(
      df = NULL,
      is_filtered = FALSE
    )

    observe({
      SplitByGroupState$df <- DataModelState$df
    })

    # Create colnames dropdown
    output[["colnames_dropdown"]] <- renderUI({
      req(!is.null(SplitByGroupState$df))
      req(is.data.frame(SplitByGroupState$df))
      colnames <- names(SplitByGroupState$df)
      tooltip <- "Select the column by name which you want to split by"
      div(
        tags$label(
          "Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("SG-colnames-dropdown_"),
          label = "Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL,
          multiple = TRUE
        )
      )
    })

    # Show levels based on column which is choosen
    output[["levels_dropdown"]] <- renderUI({
      req(!is.null(SplitByGroupState$df))
      req(is.data.frame(SplitByGroupState$df))
      selected_col <- input[[paste0("colnames-dropdown_")]]
      if (is.null(selected_col)) {
        return(NULL)
      }
      vals <- unique(SplitByGroupState$df[, selected_col])
      tooltip <- "Select the level (group) by name which you want to use"
      div(
        tags$label(
          "Variable levels",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("SG-levels-dropdown_"),
          label = "Variable levels",
          choices = vals[1:length(vals)],
          selected = NULL,
          multiple = TRUE
        )
      )
    })

    # React to split data
    observeEvent(input$split_data, {
      print_req(is.data.frame(SplitByGroupState$df), "The dataset is missing")
      selected_cols <- input[[paste0("colnames-dropdown_")]]
      selected_groups <- input[[paste0("levels-dropdown_")]]
      af <- get_apply_filter()$new(selected_cols, selected_groups)
      e <- try(
        {
          af$validate()
          af$eval(DataModelState, ResultsState)
        },
        silent = TRUE
      )
      if (inherits(e, "try-error")) {
        print_err(e)
      }
    })
  })
}
