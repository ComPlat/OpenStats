install.packages("OpenStats", type = "source", repos = NULL)
tinytest::test_package("OpenStats")

tools::showNonASCIIfile("./OpenStats/R/Backend_import_export_variations.R")
