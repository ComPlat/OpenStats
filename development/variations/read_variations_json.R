# TODO:
# - create a dedicated summary tab
# - this tab is only shown when the method is "VariationStatistics"
#
# Problems:
# - if the user creates a variation for example from starting material.
#   Then the default value is 0 in the ELN. But I get as information NULL.
#   The user has to change the value in order to create a real value which is
#   different from NULL.
#   * Maybe, don't handle this and instead write an issue for chemotion ELN
# - I get often # List of 1 $ error: chr "Token download permission expired"
#   * Maybe, this is just due to the fact that OpenStats fails repeatedly in
#     downloading the file. Thus, the token counter is exceeded

# id <- system2("docker", c("ps", "--filter", "ancestor=openstats", "--format", "{{.ID}}"), stdout = TRUE)
# system(sprintf("docker cp %s:/home/shiny/example_variations_new2.json .", id))
# system(sprintf("docker exec %s rm /home/shiny/example_variations_new2.json", id))

path <- "starting_material_varied.json"
# Expected:
# serin     c(8, 7, 6, 5, 0, 1, 2, 3, 4)      [g]

path <- "properties_varied.json"
# Expected:
# duration    c(0, NA, 1)    [Seconds(s)]

path <- "draged_cols.json"
# Expected:
# NH4+    c(1)    [g] <== product
# CO2     c(2)    [g] <== starting material

path <- "metadata_and_samples_varied.json"
# Expected:
# group         c(2, 1, 1, 2)
# subgroup      c(1, 1, 2, 2)
# notes         c("", "", "", "")
# NH4           c(2, 1, 1, 2)         [g]
# CO2           c(2, 2, 1, 1)         [g]
# NAD           c(12, 33, 12, 33)     [g]

path <- "all_samples_varied.json"
# Expected:
# NH4+      c(1, 2, 3)                        [g]
# NAD       c(2, 3, 4)                        [g]
# NAD       c(0.003015, 0.004522, 0.006029)   [mol]
# glycine   c(3, 4, 5)                        [g]
# NADH      c(4, 5, 6)                        [g]
# CO2       c(5, 6, 7)                        [g]
# serin     c(0, 7, 8)                        [l]

path <- "variation.json"
# file.copy("example_variations_new2.json", path, overwrite = TRUE)

files <- list.files("OpenStats/R", full.names = TRUE)
trash <- lapply(files, source)

base <- "./development/variations/"
DataModelState <- new.env(parent = emptyenv())
ResultsState <- new.env(parent = emptyenv())
read_variations(paste0(base, path), DataModelState, ResultsState)
DataModelState$df
