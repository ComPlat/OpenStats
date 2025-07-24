Sys.setenv(SHINYTEST2_APP_DRIVER_TEST_ON_CRAN = 1)
setwd("~/Documents/OpenStats")
files <- list.files("./OpenStats/inst/tinytest", full.names = TRUE)

tinytest::run_test_file(files[1]) # Assumptions
tinytest::run_test_file(files[2]) # Correlation
tinytest::run_test_file(files[3]) # Data DataWrangling --> Failed if run as script. But not in interactive mode
tinytest::run_test_file(files[4]) # DoseResponse
tinytest::run_test_file(files[5]) # DoseResponseBackend
tinytest::run_test_file(files[6]) # GLMStatisticalTests
tinytest::run_test_file(files[7]) # ImportTests
tinytest::run_test_file(files[8]) # ImportUITests
tinytest::run_test_file(files[9]) # ModelVisualsation
tinytest::run_test_file(files[10]) # StatisticalTests
tinytest::run_test_file(files[11]) # TestEngine
tinytest::run_test_file(files[12]) # TestHistory
tinytest::run_test_file(files[13]) # TestOptimization
tinytest::run_test_file(files[14]) # TestUIOptimization
tinytest::run_test_file(files[15]) # TTest
tinytest::run_test_file(files[16]) # UtilsTests
tinytest::run_test_file(files[17]) # Visualisation

Sys.unsetenv("SHINYTEST2_APP_DRIVER_TEST_ON_CRAN")
