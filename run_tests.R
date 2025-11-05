Sys.setenv(SHINYTEST2_APP_DRIVER_TEST_ON_CRAN = 1)
setwd("~/Documents/OpenStats")
files <- list.files("./OpenStats/inst/tinytest", full.names = TRUE)

 run_test_file(files[1]) # Assumptions
 run_test_file(files[2]) # Correlation
 run_test_file(files[3]) # Data DataWrangling
 run_test_file(files[4]) # DoseResponse
 run_test_file(files[5]) # DoseResponseBackend
 run_test_file(files[6]) # GLMStatisticalTests
 run_test_file(files[7]) # ImportTests
 run_test_file(files[8]) # ImportUITests
 run_test_file(files[9]) # ModelVisualsation
 run_test_file(files[10]) # StatisticalTests
 run_test_file(files[11]) # TestEngine
 run_test_file(files[12]) # TestHistory
 run_test_file(files[13]) # TestOptimization
 run_test_file(files[14]) # TestUIOptimization
 run_test_file(files[15]) # TTest
 run_test_file(files[16]) # UtilsTests
 run_test_file(files[17]) # Visualisation

Sys.unsetenv("SHINYTEST2_APP_DRIVER_TEST_ON_CRAN")
