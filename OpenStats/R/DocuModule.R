# nocov start ui-scaffold
docu_path <- function(file) {
  if (Sys.getenv("RUN_MODE") == "LOCAL") {
    return(system.file("www", file, package = "OpenStats"))
  } else if (Sys.getenv("RUN_MODE") != "SERVER") {
    return(file.path("./www", file))
  } else {
    return(system.file("www", file, package = "OpenStats"))
  }
}

get_docu <- function(panel) {
  path <- ""
  title <- ""

  if (panel == "Data") {
    path <- docu_path("data.html")
    title <- "Example Dataframe"
  } else if (panel == "DataWrangling") {
    path <- docu_path("operations.html")
    title <- "Data wrangling"
  } else if (panel == "Visualisation") {
    path1 <- docu_path("visualization1.html")
    path2 <- docu_path("visualization2.html")
    plot_path <- docu_path("DocuPlot.jpg")
    title <- "Visualization"
    return(list(path1, path2, plot_path, title))
  } else if (panel == "Assumption") {
    path <- docu_path("assumptions.html")
    title <- "Testing assumptions"
  } else if (panel == "Correlation") {
    path <- docu_path("correlation.html")
    title <- "Correlation"
  } else if (panel == "Tests") {
    path <- docu_path("tests.html")
    title <- "Statistical tests"
  } else if (panel == "Dose Response analysis") {
    path <- docu_path("doseresponse.html")
    title <- "Doseresponse analysis"
  } else if (panel == "History") {
    path <- docu_path("history.html")
    title <- "History"
  } else if (panel == "LinearFormula") {
    path <- docu_path("linear_formula.html")
    title <- "Defining the formula"
  } else if (panel == "Generalised Linear ModelFormula") {
    path <- docu_path("generalised_linear_formula.html")
    title <- "Defining the formula"
  } else if (panel == "Optimization ModelFormula") {
    path <- docu_path("optim_formula.html")
    title <- "Defining the formula"
  } else if (panel == "Split") {
    path <- docu_path("SplitData.html")
    title <- "Subsetting the dataset"
  }

  return(list(path, title))
}

show_docu <- function(input) {
  obs_main <- observeEvent(input[["docu"]], {
    path_list <- get_docu(input$conditionedPanels)
    if (length(path_list) == 4) {
      path1 <- path_list[[1]]
      path2 <- path_list[[2]]
      plot_path <- path_list[[3]]
      title <- path_list[[4]]
      showModal(modalDialog(
        title = title,
        includeHTML(path1),
        br(),
        renderImage(
          {
            list(
              src = plot_path,
              contentType = "image/jpg",
              width = 650,
              height = 500,
              alt = "Basic Plot"
            )
          },
          deleteFile = FALSE
        ),
        br(),
        br(),
        br(),
        br(),
        br(),
        includeHTML(path2),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
    } else {
      path <- path_list[[1]]
      title <- path_list[[2]]
      showModal(modalDialog(
        title = title,
        includeHTML(path),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  # docu formula editor
  obs_formula <- observeEvent(input[["FO-formula_docu"]], {
    type <- input[["FO-model_type"]]
    path_list <- get_docu(paste0(type, "Formula"))
    showModal(modalDialog(
      title = path_list[[2]],
      includeHTML(path_list[[1]]),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })

  # docu split by group
  obs_split <- observeEvent(input[["SG-split_docu"]], {
    path_list <- get_docu("Split")
    showModal(modalDialog(
      title = path_list[[2]],
      includeHTML(path_list[[1]]),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })

  invisible(list(
    main   = obs_main,
    formula = obs_formula,
    split   = obs_split
  ))
}

# nocov end ui-scaffold
