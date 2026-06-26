install.packages("OpenStats", type = "source", repos = NULL)
tinytest::test_package("OpenStats")
arnings()
tinytest::run_test_file("./OpenStats/inst/tinytest/test_Backend_TestHistory.R")
tinytest::run_test_file("./OpenStats/inst/tinytest/test_Server_Assumptions.R")

files <- list.files("OpenStats/R", full.names = TRUE)
res <- lapply(files, tools::showNonASCII)
vapply(res, function(e) identical(e, character(0L)), logical(1L)) |> all()
