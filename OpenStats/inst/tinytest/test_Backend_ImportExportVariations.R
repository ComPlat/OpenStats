library(tinytest)
library(OpenStats)

fresh_states <- function() {
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list())
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(NULL)
  MethodState <- new.env(parent = emptyenv())
  MethodState$method <- "VariationStatistics"
  MethodState$storage_class <- new("MethodVariationStatistics", id = "", request_id = "", element_info = list())
  list(RS = ResultsState, DMS = DataModelState, MS = MethodState)
}

read_fixture <- function(file) {
  path <- system.file(file.path("test_data", file), package = "OpenStats")
  s <- fresh_states()
  OpenStats:::read_variations(path, s$DMS, s$RS, s$MS)
  s
}

test_read_variations_one <- function() {
  s <- read_fixture("variations_one.json")
  df <- s$DMS$df
  checks <- c()
  checks[[1]] <- expect_equal(class(df), "data.frame")
  checks[[2]] <- expect_equal(nrow(df), 1L)
  checks[[3]] <- expect_equal(colnames(df), "serin_kkr17_g")
  checks[[4]] <- expect_true(is.na(df$serin_kkr17_g))
  expect_true(all(unlist(checks)))
}
test_read_variations_one()

test_read_variations_properties <- function() {
  s <- read_fixture("variations_properties.json")
  df <- s$DMS$df
  checks <- c()
  checks[[1]] <- expect_equal(colnames(df), "duration_s")
  checks[[2]] <- expect_equal(df$duration_s, c(0, NA, 1))
  expect_true(all(unlist(checks)))
}
test_read_variations_properties()

test_read_variations_dragged_cols <- function() {
  s <- read_fixture("variations_dragged_cols.json")
  df <- s$DMS$df
  checks <- c()
  checks[[1]] <- expect_equal(colnames(df), c("H4N_kkr26_g", "CO2_kkr25_g"))
  checks[[2]] <- expect_equal(df$H4N_kkr26_g, 1)
  checks[[3]] <- expect_equal(df$CO2_kkr25_g, 2)
  expect_true(all(unlist(checks)))
}
test_read_variations_dragged_cols()

test_read_variations_metadata_samples <- function() {
  s <- read_fixture("variations_metadata_samples.json")
  df <- s$DMS$df
  checks <- c()
  checks[[1]] <- expect_equal(
    colnames(df),
    c("group", "subgroup", "notes", "H4N_kkr27_g", "CO2_reactant_g", "NAD_kkr28_g")
  )
  checks[[2]] <- expect_equal(df$group, c(2, 1, 1, 2))
  checks[[3]] <- expect_equal(df$subgroup, c(1, 1, 2, 2))
  checks[[4]] <- expect_equal(df$notes, c("", "", "", ""))
  checks[[5]] <- expect_equal(df$H4N_kkr27_g, c(2, 1, 1, 2))
  checks[[6]] <- expect_equal(df$CO2_reactant_g, c(2, 2, 1, 1))
  checks[[7]] <- expect_equal(df$NAD_kkr28_g, c(12, 33, 12, 33))
  expect_true(all(unlist(checks)))
}
test_read_variations_metadata_samples()

test_read_variations_all_samples <- function() {
  s <- read_fixture("variations_all_samples.json")
  df <- s$DMS$df
  checks <- c()
  checks[[1]] <- expect_equal(
    colnames(df),
    c(
      "H4N_kkr21_g", "NAD_kkr22_g", "NAD_kkr22_mol", "glycine_reactant_g",
      "NADH_kkr23_g", "CO2_kkr24_g", "serin_solvent_l"
    )
  )
  checks[[2]] <- expect_equal(df$H4N_kkr21_g, c(1, 2, 3))
  checks[[3]] <- expect_equal(df$NAD_kkr22_g, c(2, 3, 4))
  checks[[4]] <- expect_equal(df$NAD_kkr22_mol, c(0.003014658, 0.004521987, 0.006029317), tolerance = 1e-6)
  checks[[5]] <- expect_equal(df$glycine_reactant_g, c(3, 4, 5))
  checks[[6]] <- expect_equal(df$NADH_kkr23_g, c(4, 5, 6))
  checks[[7]] <- expect_equal(df$CO2_kkr24_g, c(5, 6, 7))
  checks[[8]] <- expect_equal(df$serin_solvent_l, c(0, 7, 8))
  expect_true(all(unlist(checks)))
}
test_read_variations_all_samples()

test_read_variations_full <- function() {
  s <- read_fixture("variations_full.json")
  df <- s$DMS$df
  checks <- c()
  checks[[1]] <- expect_equal(class(df), "data.frame")
  checks[[2]] <- expect_equal(nrow(df), 8L)
  checks[[3]] <- expect_equal(df$group, c(5, 5, 1, 2, 4, 2, 3, 1))
  checks[[4]] <- expect_equal(df$subgroup, c(2, 1, 2, 1, 1, 2, 3, 1))
  # special units cast correctly
  checks[[5]] <- expect_true("serin_reactant_equivalent" %in% colnames(df))
  checks[[6]] <- expect_true("CO2_kkr16_turnover_num" %in% colnames(df))
  checks[[7]] <- expect_true("CO2_kkr16_turnover_freq" %in% colnames(df))
  checks[[8]] <- expect_true("CO2_kkr16_degC" %in% colnames(df))
  checks[[9]] <- expect_true("CO2_kkr16_ppm" %in% colnames(df))
  checks[[10]] <- expect_true("CO2_kkr16_pct" %in% colnames(df))
  expect_true(all(unlist(checks)))
}
test_read_variations_full()

test_read_variations_registers_result <- function() {
  s <- read_fixture("variations_dragged_cols.json")
  checks <- c()
  checks[[1]] <- expect_equal(s$RS$counter, 1)
  checks[[2]] <- expect_true("df0" %in% names(s$RS$all_data))
  checks[[3]] <- expect_equal(s$RS$all_data[["df0"]], s$DM$df)
  expect_true(all(unlist(checks)))
}
test_read_variations_registers_result()
