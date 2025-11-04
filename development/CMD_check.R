# TODO: replace tinytest::
Sys.setenv(R_COVR = "TESTIT")
setwd("./OpenStats")

cov <- covr::package_coverage(
  type = "none", code = 'tinytest::test_package("OpenStats")')


cov <- covr::packge_coverage(
  path         = ".",
  type         = "none",
  code         = 'tinytest::run_test_file("./inst/tinytest/test_Assumptions2.R")',
  quiet        = TRUE,
  clean        = FALSE,
  pre_clean    = FALSE,
  use_load_all = TRUE
)

cov
Sys.unsetenv("R_COVR")

# Requires slightly more tests:
# R/Engine_V1_2.R
# R/SummarisingModel.R

# Requires more tests:
# R/utils.R
# R/CheckFunctions.R
# R/SplitByGroup.R
# R/MainApp.R
# R/FormulaModule.R

# OpenStats Coverage: 75.56%
# R/ReplayHistory.R: 27.27%
# R/statisticalTests.R: 42.51%
# R/FormulaModule.R: 46.17%
# R/MainApp.R: 47.32%
# R/SplitByGroup.R: 57.75%
# R/utils.R: 61.54%
# R/SummarisingModel.R: 65.56%
# R/CheckFunctions.R: 74.51%
# R/visualisation.R: 74.81%
# R/plottingInternally.R: 81.90%
# R/Engine_V1_2.R: 82.28%
# R/DiagnosticPlots.R: 86.46%
# R/lc50.r: 90.34%
# R/History.R: 92.79%
# R/check_ast.R: 94.12%
# R/OperationsModule.R: 94.40%
# R/assumption.R: 94.63%
# R/correlation.R: 97.01%
# R/Optimizing.R: 97.90%
# R/Import.R: 98.78%
# R/DoseResponse.R: 100.00%
