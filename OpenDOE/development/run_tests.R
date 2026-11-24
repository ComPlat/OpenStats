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

OpenDOE:::opendoe(4000L)

# Animal planning without ELN access
# Example: Genotype = "Cd 44" (one level),
# control "Cd44 ft/ft" -> assign it in the
# Predictors tab, Genotype gets 2 levels
eln_json <- normalizePath(
  "./OpenDOE/development/labimotion_example_ext_app.json"
)
fake_fetch <- function(session) {
  jsonlite::fromJSON(
    eln_json, simplifyVector = FALSE
  )
}
assignInNamespace(
  "fetch_eln_doe", fake_fetch, "OpenDOE"
)
app <- OpenDOE:::opendoe(4000L)
shiny::runApp(app, launch.browser = FALSE)
# http://127.0.0.1:4000/?method=SampleSizeCalculation
