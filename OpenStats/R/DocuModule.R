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
