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
  expect_equal(conv$predictors$Catalyst, c("KK-1", "KK-2"))
  expect_equal(conv$predictors$Additive, c("Additive", "none")) # empty shortLabel falls back to name
  expect_equal(conv$predictors$Temperature, c("40", "90"))
  expect_equal(
    unname(conv$predictor_types),
    c("categorical", "categorical", "continuous", "continuous")
  )
  expect_equal(conv$eln$batch_size, 6L)
  expect_equal(conv$eln$level_ids$Catalyst, c(`KK-1` = 11L, `KK-2` = 12L))
  expect_equal(conv$eln$level_ids$Additive, c(Additive = 21L, none = NA_integer_))
}
test_eln_doe_predictors()

test_eln_level_labels_not_unique <- function() {
  # reaction 68: all reactants carry the short label "reactant"
  labels <- OpenDOE:::eln_level_labels(list(
    list(id = 741L, shortLabel = "reactant", name = "hexane"),
    list(id = 742L, shortLabel = "reactant", name = "icosane"),
    list(id = 740L, shortLabel = "kkr-101", name = "benzene")
  ))
  expect_equal(unname(labels), c("hexane", "icosane", "kkr-101"))

  labels <- OpenDOE:::eln_level_labels(list(
    list(id = 1L, shortLabel = "reactant", name = "hexane"),
    list(id = 2L, shortLabel = "reactant", name = "hexane")
  ))
  expect_equal(unname(labels), c("hexane (1)", "hexane (2)"))
}
test_eln_level_labels_not_unique()

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
    Catalyst = c("KK-1", "KK-2"), Additive = c("none", "Additive"),
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
    checks[4] <<- all(df$Catalyst %in% c("KK-1", "KK-2"))
    checks[5] <<- all(df$Temperature >= 40 & df$Temperature <= 90)
  })
  expect_true(all(checks))
}
test_eln_import_presets_lhs()
