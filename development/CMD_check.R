Sys.setenv(R_COVR = "TESTIT")

tinytest::test_package("OpenStats")

tinytest::run_test_file("OpenStats/inst/tinytest/test_Operations.R")
tinytest::run_test_file("OpenStats/inst/tinytest/test_Operations.R")
tinytest::run_test_file("OpenStats/inst/tinytest/test_TestFormulaUI.R")

cov <- covr::package_coverage(
  path = "./OpenStats",
  type = "none", code = 'tinytest::test_package("OpenStats")'
)

# cov <- covr::package_coverage(
#   path         = "./OpenStats",
#   type         = "none",
#   code         = 'tinytest::run_test_file("./OpenStats/inst/tinytest/test_ModelVisualsation.R")',
#   quiet        = TRUE,
#   clean        = FALSE,
#   pre_clean    = FALSE,
#   use_load_all = TRUE
# )

cov

Sys.unsetenv("R_COVR")

# OpenStats Coverage: 83.26%
# R/Server_MainApp.R: 49.46%
# R/Server_SplitByGroup.R: 57.75%
# R/Server_CheckFunctions.R: 76.47%
# R/Server_ReplayHistory.R: 81.82%
# R/Server_StatisticalTests.R: 84.81%
# R/Server_Visualization.R: 85.21%
# R/Backend_V1_2_Engine.R: 85.47%
# R/Server_FormulaModule.R: 89.66%
# R/Server_OperationsModule.R: 92.64%
# R/Backend_V1_2_History.R: 92.79%
# R/Server_Assumption.R: 94.16%
# R/Server_Correlation.R: 96.77%
# R/Backend_V1_2_LC50.R: 100.00%
# R/Server_DoseResponse.R: 100.00%
# R/UI_Assumption.R: 100.00%
# R/UI_DoseResponse.R: 100.00%
# R/UI_OperationsModule.R: 100.00%
# R/UI_ReplayHistory.R: 100.00%
# R/UI_StatisticalTests.R: 100.00%
