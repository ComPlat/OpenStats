library(tinytest)
options(OpenDOE.background = FALSE)

eln_doe_mock <- function() {
  list(
    id = "42", request_id = "req-1", mode = "initial",
    doe_definition = list(
      version = 1L, initial_batch_size = 6L,
      factors = list(
        list(name = "Catalyst", type = "categorical",
             target = list(kind = "material_selection", sample_ids = list(11L, 12L), allow_none = FALSE)),
        list(name = "Additive", type = "categorical",
             target = list(kind = "material_selection", sample_ids = list(21L), allow_none = TRUE)),
        list(name = "Base loading", type = "continuous", min = 1, max = 3,
             target = list(kind = "material_amount", sample_id = 31L, unit = "equivalent")),
        list(name = "Temperature", type = "continuous", min = 40, max = 90,
             target = list(kind = "property", field = "temperature"))
      )
    ),
    samples = list(
      list(id = 11L, group = "reactants", name = "Pd(OAc)2", shortLabel = "KK-1"),
      list(id = 12L, group = "reactants", name = "Pd(PPh3)4", shortLabel = "KK-2"),
      list(id = 21L, group = "reactants", name = "Additive", shortLabel = ""),
      list(id = 31L, group = "reactants", name = "K2CO3", shortLabel = "KK-4")
    ),
    properties = list(), variations = list()
  )
}

test_eln_doe_predictors <- function() {
  conv <- OpenDOE:::eln_doe_predictors(eln_doe_mock())
  expect_equal(names(conv$predictors), c("Catalyst", "Additive", "Base loading", "Temperature"))
  expect_equal(conv$predictors$Catalyst, c("Pd(OAc)2", "Pd(PPh3)4"))
  expect_equal(conv$predictors$Additive, c("Additive", "none"))
  expect_equal(conv$predictors$Temperature, c("40", "90"))
  expect_equal(
    unname(conv$predictor_types),
    c("categorical", "categorical", "continuous", "continuous")
  )
  expect_equal(conv$eln$batch_size, 6L)
  expect_equal(conv$eln$level_ids$Catalyst, c(`Pd(OAc)2` = 11L, `Pd(PPh3)4` = 12L))
  expect_equal(conv$eln$level_ids$Additive, c(Additive = 21L, none = NA_integer_))
}
test_eln_doe_predictors()

test_eln_level_labels <- function() {
  # reaction 68: all reactants carry the short label "reactant", the product the sample label
  labels <- OpenDOE:::eln_level_labels(list(
    list(id = 746L, shortLabel = "reactant", name = "Catalyst1"),
    list(id = 745L, shortLabel = "reactant", name = "catalyst3"),
    list(id = 740L, shortLabel = "kkr-101", name = ""),
    list(id = 747L, shortLabel = "", name = NULL)
  ))
  expect_equal(labels, c("Catalyst1", "catalyst3", "kkr-101", "sample 747"))

  labels <- OpenDOE:::eln_level_labels(list(
    list(id = 1L, shortLabel = "reactant", name = "hexane"),
    list(id = 2L, shortLabel = "reactant", name = "hexane")
  ))
  expect_equal(labels, c("hexane (1)", "hexane (2)"))

  variations <- list(list(products = list(
    `743` = list(aux = list(name = "Product", shortLabel = "kkr-102")),
    `744` = list(aux = list(name = "Product", shortLabel = "kkr-103"))
  )))
  expect_equal(unname(OpenDOE:::eln_product_labels(variations)), c("Product (743)", "Product (744)"))
}
test_eln_level_labels()

test_eln_doe_invalid <- function() {
  doe <- eln_doe_mock()
  doe$doe_definition <- NULL
  expect_error(OpenDOE:::eln_doe_predictors(doe), "no design")

  doe <- eln_doe_mock()
  doe$doe_definition$factors[[1]]$target$sample_ids <- list(11L, 99L)
  expect_error(OpenDOE:::eln_doe_predictors(doe), "not in the reaction: 99")

  doe <- eln_doe_mock()
  doe$doe_definition$factors[[4]]$min <- 90
  expect_error(OpenDOE:::eln_doe_predictors(doe), "Temperature")

  doe <- eln_doe_mock()
  doe$doe_definition$version <- 2L
  expect_error(OpenDOE:::eln_doe_predictors(doe), "version")
}
test_eln_doe_invalid()

test_eln_doe_rows <- function() {
  eln <- OpenDOE:::eln_doe_predictors(eln_doe_mock())$eln
  df <- data.frame(
    Catalyst = c("Pd(OAc)2", "Pd(PPh3)4"), Additive = c("none", "Additive"),
    `Base loading` = c(1.5, 2.25), Temperature = c(40, 87.5),
    check.names = FALSE
  )
  rows <- OpenDOE:::eln_doe_rows(df, eln)
  expect_equal(length(rows), 2L)
  expect_equal(rows[[1]]$Catalyst, 11L)
  expect_true("Additive" %in% names(rows[[1]]) && is.null(rows[[1]]$Additive))
  expect_equal(rows[[2]]$Additive, 21L)
  expect_equal(rows[[2]]$`Base loading`, 2.25)

  path <- tempfile(fileext = ".json")
  OpenDOE:::write_eln_doe_batch(df, eln, path)
  json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  unlink(path)
  expect_equal(json$request_id, "req-1")
  expect_true(is.null(json$rows[[1]]$Additive) && "Additive" %in% names(json$rows[[1]]))
  expect_equal(json$rows[[2]]$Temperature, 87.5)

  df$Catalyst[1] <- "KK-9"
  expect_error(OpenDOE:::eln_doe_rows(df, eln), "unknown levels KK-9")
  expect_error(OpenDOE:::eln_doe_rows(df[c("Catalyst")], eln), "Missing factor columns")
}
test_eln_doe_rows()

# Base loading as mass in mg that may also be left out; triplicates
eln_doe_mock_absent <- function() {
  doe <- eln_doe_mock()
  doe$doe_definition$replicates <- 3L
  doe$doe_definition$factors[[3]]$min <- 5
  doe$doe_definition$factors[[3]]$max <- 20
  doe$doe_definition$factors[[3]]$target <- list(
    kind = "material_amount", sample_id = 31L, entry = "mass", unit = "mg", allow_absent = TRUE
  )
  doe
}

test_eln_absent_predictor <- function() {
  conv <- OpenDOE:::eln_doe_predictors(eln_doe_mock_absent())
  expect_equal(
    names(conv$predictors),
    c("Catalyst", "Additive", "Base loading present", "Base loading", "Temperature")
  )
  expect_equal(conv$predictors$`Base loading present`, c("yes", "no"))
  expect_equal(conv$predictor_types[["Base loading present"]], "categorical")
  expect_equal(conv$predictors$`Base loading`, c("5", "20"))
  expect_equal(conv$eln$replicates, 3L)

  df <- data.frame(
    Catalyst = c("Pd(PPh3)4", "Pd(OAc)2"), Additive = "none",
    `Base loading present` = c("yes", "no"), `Base loading` = c(7.5, 12),
    Temperature = 50, check.names = FALSE
  )
  rows <- OpenDOE:::eln_doe_rows(df, conv$eln)
  expect_equal(rows[[1]]$`Base loading`, 7.5)
  expect_true("Base loading" %in% names(rows[[2]]) && is.null(rows[[2]]$`Base loading`))
  expect_true(is.null(rows[[1]]$`Base loading present`))
}
test_eln_absent_predictor()

test_eln_replicates_preset <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) return(invisible())
  app <- OpenDOE:::app()
  checks <- logical(0)
  shiny::testServer(app$server, {
    OpenDOE:::import_eln_doe(State, eln_doe_mock_absent())
    session$setInputs(`DESIGN-method` = "lhs", `DESIGN-lhs_n` = 4, `DESIGN-lhs_seed` = 1)
    session$setInputs(`DESIGN-lhs_replicates` = State$eln$replicates, `DESIGN-build` = 1)
    df <- State$results[["1"]]@df
    checks[1] <<- nrow(df) == 12L
    checks[2] <<- State$history[["1"]]$params$replicates == 3L
    checks[3] <<- all(df$`Base loading present` %in% c("yes", "no"))
  })
  expect_true(all(checks))
}
test_eln_replicates_preset()

test_eln_convert_property <- function() {
  expect_equal(OpenDOE:::eln_convert_property(7200, "Second(s)", "Hour(s)"), 2)
  expect_equal(OpenDOE:::eln_convert_property(60, "°C", "°C"), 60)
  expect_error(OpenDOE:::eln_convert_property(300, "K", "°C"), "Unexpected unit")
}
test_eln_convert_property()

eln_material <- function(amount = 0, mass = 0, equivalent = NULL, volume = NULL) {
  m <- list(mass = list(value = mass, unit = "g"), amount = list(value = amount, unit = "mol"))
  if (!is.null(equivalent)) m$equivalent <- list(value = equivalent, unit = NULL)
  if (!is.null(volume)) m$volume <- list(value = volume, unit = "l")
  m
}

eln_variation <- function(id, cat1, cat2, additive, base_mg, temperature, yield, ton = NULL) {
  product <- list(
    yield = list(value = yield, unit = "%"), mass = list(value = 0.2, unit = "g"),
    amount = list(value = NULL, unit = NULL), aux = list(shortLabel = "P-1")
  )
  if (!is.null(ton)) product$turnoverNumber <- list(value = ton, unit = NULL)
  list(
    id = id,
    properties = list(temperature = list(value = temperature, unit = "°C")),
    startingMaterials = list(`31` = eln_material(base_mg / 1e5, base_mg / 1000, 1)),
    reactants = list(
      `11` = eln_material(cat1, cat1),
      `12` = eln_material(cat2, cat2),
      `21` = eln_material(additive)
    ),
    products = list(`51` = product),
    solvents = list()
  )
}

test_eln_doe_variations_df <- function() {
  eln <- OpenDOE:::eln_doe_predictors(eln_doe_mock_absent())$eln
  variations <- list(
    eln_variation(1L, cat1 = 0.1, cat2 = 0, additive = 0, base_mg = 12, temperature = 60, yield = 45, ton = 420),
    eln_variation(2L, cat1 = 0, cat2 = 0.1, additive = 0.5, base_mg = 0, temperature = 80, yield = NULL),
    eln_variation(3L, cat1 = 0.1, cat2 = 0.1, additive = 0, base_mg = 8, temperature = 70, yield = 10)
  )
  res <- OpenDOE:::eln_doe_variations_df(variations, eln)
  df <- res$df

  expect_equal(res$invalid_ids, "3") # both catalysts present
  expect_equal(
    names(df),
    c("Catalyst", "Additive", "Base loading present", "Base loading", "Temperature",
      "P-1 yield", "P-1 mass", "P-1 TON")
  )
  expect_equal(df$Catalyst, c("Pd(OAc)2", "Pd(PPh3)4"))
  expect_equal(df$Additive, c("none", "Additive"))
  expect_equal(df$`Base loading present`, c("yes", "no"))
  expect_equal(df$`Base loading`, c(12, 5)) # g in the ELN -> mg; absent -> lower bound
  expect_equal(df$Temperature, c(60, 80))
  expect_equal(df$`P-1 yield`, c(45, NA)) # not measured yet
  expect_equal(df$`P-1 TON`, c(420, NA))
}
test_eln_doe_variations_df()

# The ELN stores amount = 0 when it cannot derive it from the volume (no density/molarity).
test_eln_material_present_by_volume <- function() {
  expect_equal(OpenDOE:::eln_material_quantity(eln_material(volume = 0.1)), 0.1)
  expect_equal(OpenDOE:::eln_material_quantity(eln_material(volume = 0)), 0)
  expect_true(is.na(OpenDOE:::eln_material_quantity(NULL)))

  eln <- OpenDOE:::eln_doe_predictors(eln_doe_mock_absent())$eln
  catalyst <- eln$factors[[1]]
  row <- list(reactants = list(
    `11` = eln_material(volume = 0.1),
    `12` = eln_material()
  ))
  expect_equal(OpenDOE:::eln_variation_level(row, catalyst, eln$level_ids$Catalyst), "Pd(OAc)2")
}
test_eln_material_present_by_volume()

test_eln_follow_up_bo <- function() {
  for (p in c("shiny", "mlr3mbo", "mlr3pipelines", "DiceKriging")) {
    if (!requireNamespace(p, quietly = TRUE)) return(invisible())
  }
  doe <- eln_doe_mock_absent()
  doe$mode <- "follow_up"
  # 10 measured experiments + 1 planned one (no yield yet); temperatures only cover 57-77
  doe$variations <- lapply(1:11, function(i) eln_variation(
    i, cat1 = if (i %% 2 == 1) 0.1 else 0, cat2 = if (i %% 2 == 0) 0.1 else 0,
    additive = if (i %% 3 == 0) 0.5 else 0, base_mg = if (i %% 4 == 0) 0 else 5 + i,
    temperature = 55 + 2 * i, yield = if (i == 11) NULL else (i * 37) %% 90
  ))
  app <- OpenDOE:::app()
  checks <- logical(0)
  shiny::testServer(app$server, {
    OpenDOE:::import_eln_doe_variations(State, doe)
    checks[1] <<- identical(State$eln$df_source, "1")
    checks[2] <<- nrow(State$results[["1"]]@df) == 11L

    session$setInputs(`BO-df_source` = "1", `BO-predictor_cols` = names(State$predictors))
    session$setInputs(`BO-objective` = "P-1 yield", `BO-seed` = 1, `BO-n_points` = 2, `BO-suggest` = 1)
    params <- State$history[["2"]]$params
    checks[3] <<- identical(params$bounds$Temperature, c("40", "90"))
    checks[4] <<- identical(params$bounds$Catalyst, c("Pd(OAc)2", "Pd(PPh3)4"))
    new_rows <- utils::tail(State$results[["2"]]@df, 2L)
    checks[5] <<- all(new_rows$Temperature >= 40 & new_rows$Temperature <= 90)
    checks[6] <<- all(new_rows$`Base loading present` %in% c("yes", "no"))
  })
  expect_true(all(checks))
}
test_eln_follow_up_bo()

test_eln_import_presets_lhs <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) return(invisible())
  app <- OpenDOE:::app()
  checks <- logical(0)
  shiny::testServer(app$server, {
    OpenDOE:::import_eln_doe(State, eln_doe_mock())
    session$flushReact()
    checks[1] <<- length(State$predictors) == 4L
    checks[2] <<- OpenDOE:::step_unlocked(State, "Design of experiment")

    session$setInputs(`DESIGN-lhs_n` = State$eln$batch_size, `DESIGN-lhs_seed` = 1)
    session$setInputs(`DESIGN-method` = "lhs", `DESIGN-build` = 1)
    df <- State$results[["1"]]@df
    checks[3] <<- nrow(df) == 6L
    checks[4] <<- all(df$Catalyst %in% c("Pd(OAc)2", "Pd(PPh3)4"))
    checks[5] <<- all(df$Temperature >= 40 & df$Temperature <= 90)
  })
  expect_true(all(checks))
}
test_eln_import_presets_lhs()
