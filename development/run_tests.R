install.packages("OpenStats", type = "source", repos = NULL)
tinytest::test_package("OpenStats")
tinytest::run_test_file("./OpenStats/inst/tinytest/test_Server_Rendering.R")

files <- list.files("OpenStats/R", full.names = TRUE)
res <- lapply(files, tools::showNonASCII)
vapply(res, function(e) identical(e, character(0L)), logical(1L)) |> all()
