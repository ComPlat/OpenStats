install.packages("OpenStats", type = "source", repos = NULL)
tinytest::test_package("OpenStats")
tinytest::run_test_file("./OpenStats/inst/tinytest/test_Parser.R")
tinytest::run_test_file("./OpenStats/inst/tinytest/test_Server_TestFormulaUI.R")
