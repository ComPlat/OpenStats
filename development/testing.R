install.packages("OpenStats", type = "source", repos = NULL)
tinytest::test_package("OpenStats")
tinytest::run_test_file("OpenStats/inst/tinytest/test_Backend_TestHistory.R")
