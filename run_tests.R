Sys.setenv(SHINYTEST2_APP_DRIVER_TEST_ON_CRAN = 1)
setwd("~/Documents/OpenStats")
files <- list.files("./OpenStats/inst/tinytest", full.names = TRUE)
results <- lapply(1:length(files), function(idx) {
  tinytest::run_test_file(files[idx])
})
results
Sys.unsetenv("SHINYTEST2_APP_DRIVER_TEST_ON_CRAN")
