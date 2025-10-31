Sys.setenv(R_COVR = "TESTIT")
setwd("./OpenStats")

cov <- covr::package_coverage(
        type = "none", code = 'tinytest::test_package("OpenStats")')

cov
Sys.unsetenv("R_COVR")

# OpenStats Coverage: 70.27%
# R/DocuModule.R: 0.00%
# R/OpenStats.R: 0.00%
# R/FormulaModule.R: 17.20%
# R/statisticalTests.R: 26.95%
# R/MainApp.R: 40.89%
# R/SplitByGroup.R: 46.07%
# R/ReplayHistory.R: 55.56%
# R/SummarisingModel.R: 62.66%
# R/utils.R: 63.92%
# R/CheckFunctions.R: 66.67%
# R/visualisation.R: 74.04%
# R/DoseResponse.R: 81.69%
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
