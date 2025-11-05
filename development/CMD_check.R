Sys.setenv(R_COVR = "TESTIT")
setwd("./OpenStats")

tinytest::test_package("OpenStats")

tinytest::run_test_file("OpenStats/inst/tinytest/test_DataWrangling.R")
tinytest::run_test_file("OpenStats/inst/tinytest/test_Operations.R")

cov <- covr::package_coverage(
  type = "none", code = 'tinytest::test_package("OpenStats")')

cov
Sys.unsetenv("R_COVR")

# OpenStats Coverage: 76.73%
# R/ReplayHistory.R: 27.27%
# R/StatisticalTests.R: 42.51%
# R/FormulaModule.R: 46.17%
# R/MainApp.R: 47.32%
# R/SplitByGroup.R: 57.75%
# R/CheckFunctions.R: 74.51%
# R/Visualisation.R: 74.81%
# R/Engine_V1_2.R: 82.28%
# R/LC50_V1_2.r: 90.34%
# R/History_V1_2.R: 92.79%
# R/OperationsModule.R: 94.40%
# R/Assumption.R: 94.63%
# R/Correlation.R: 97.01%
# R/Optimizing_V1_2.R: 97.90%
# R/Import.R: 98.78%
# R/DoseResponse.R: 100.00%
