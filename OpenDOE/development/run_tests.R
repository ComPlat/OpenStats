install.packages("OpenDOE", type = "source", repos = NULL)
tinytest::test_package("OpenDOE")

path <- "./OpenDOE/inst/tinytest/"
tinytest::run_test_file(paste0(path, "test_Backend_Engine.R"))
tinytest::run_test_file(paste0(path, "test_Server_Data.R"))
tinytest::run_test_file(paste0(path, "test_Server_Design.R"))
tinytest::run_test_file(paste0(path, "test_Server_FiniteAssign.R"))
tinytest::run_test_file(paste0(path, "test_Server_History.R"))
tinytest::run_test_file(paste0(path, "test_Server_Predictors.R"))
tinytest::run_test_file(paste0(path, "test_Server_RandomAssign.R"))
tinytest::run_test_file(paste0(path, "test_Server_SampleSize.R"))
