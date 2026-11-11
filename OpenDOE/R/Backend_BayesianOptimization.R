build_bo_domain <- function(predictors, predictor_types) {
  params <- lapply(names(predictors), function(name) {
    levels <- predictors[[name]]
    switch(predictor_types[[name]],
      continuous_grid = ,
      continuous = {
        values <- as.numeric(levels)
        paradox::p_dbl(lower = min(values), upper = max(values))
      },
      discrete = ,
      categorical = paradox::p_fct(levels = levels)
    )
  })
  names(params) <- names(predictors)
  do.call(paradox::ps, params)
}

build_bo_codomain <- function(objectives) {
  params <- lapply(objectives, function(direction) paradox::p_dbl(tags = direction))
  do.call(paradox::ps, params)
}

prepare_bo_xdt <- function(data, predictors, predictor_types) {
  xdt <- lapply(names(predictors), function(name) {
    col <- data[[name]]
    if (predictor_types[[name]] %in% c("continuous_grid", "continuous")) return(as.numeric(col))
    col <- as.character(col)
    unknown <- setdiff(unique(col), predictors[[name]])
    if (length(unknown) > 0L) {
      stop("Unknown levels for predictor ", name, ": ", paste(unknown, collapse = ", "))
    }
    col
  })
  names(xdt) <- names(predictors)
  data.table::as.data.table(xdt)
}

# km needs more experiments than input dimensions; categorical/discrete predictors are one-hot encoded,
# so each of them counts with one dimension per level
bo_min_experiments <- function(predictors, predictor_types) {
  dims <- vapply(names(predictors), function(name) {
    if (predictor_types[[name]] %in% c("continuous_grid", "continuous")) 1L else length(predictors[[name]])
  }, integer(1))
  max(3L, sum(dims) + 1L)
}

build_bo_learner <- function(all_continuous, mixed_surrogate) {
  if (all_continuous) return(mlr3mbo::default_gp(noisy = TRUE))
  if (mixed_surrogate == "rf") return(mlr3mbo::default_rf(noisy = TRUE))
  # the archive stores categorical values as character, which km rejects -> factor -> one-hot dummies
  to_factor <- mlr3pipelines::po("colapply", applicator = as.factor, affect_columns = mlr3pipelines::selector_type("character"))
  encode <- mlr3pipelines::po("encode", method = "one-hot")
  graph <- mlr3pipelines::concat_graphs(mlr3pipelines::concat_graphs(to_factor, encode), mlr3mbo::default_gp(noisy = TRUE))
  learner <- mlr3::as_learner(graph)
  learner$predict_type <- "se"
  learner
}

standardize_objectives <- function(ydt) {
  data.table::as.data.table(lapply(ydt, function(y) {
    s <- stats::sd(y)
    if (is.na(s) || s == 0) s <- 1
    (y - mean(y)) / s
  }))
}

# objectives: named vector column -> "maximize"/"minimize"; NA rows (not yet measured) stay NA
pareto_optimal <- function(data, objectives) {
  y <- as.matrix(data[names(objectives)])
  y[, objectives == "maximize"] <- -y[, objectives == "maximize"]
  measured <- stats::complete.cases(y)
  res <- rep(NA, nrow(y))
  idx <- which(measured)
  res[idx] <- vapply(idx, function(i) {
    !any(vapply(idx, function(j) all(y[j, ] <= y[i, ]) && any(y[j, ] < y[i, ]), logical(1)))
  }, logical(1))
  res
}

build_bo_acquisition <- function(surrogate, n_objectives, all_continuous) {
  if (n_objectives == 1L) {
    # AEI only accounts for noise with a kriging surrogate (reads the km nugget)
    return(mlr3mbo::acqf(if (all_continuous) "aei" else "ei", surrogate = surrogate))
  }
  if (n_objectives == 2L) return(mlr3mbo::acqf("ehvi", surrogate = surrogate))
  acquisition <- mlr3mbo::acqf("smsego", surrogate = surrogate)
  # progress = remaining budget in mlr3mbo's own loop; unknown in ask/tell, 0 is the most conservative choice
  acquisition$progress <- 0
  acquisition
}

bo_suggest <- function(data, predictors, predictor_types, objectives, mixed_surrogate = "gp_onehot", n_points = 1L) {
  if (length(objectives) < 1L) stop("Select at least one measured result")
  if (is.na(n_points) || n_points < 1L) stop("Number of suggestions must be at least 1")
  missing_cols <- setdiff(c(names(predictors), names(objectives)), names(data))
  if (length(missing_cols) > 0L) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  # Rows without a measured objective are planned but not yet run experiments
  data <- data[stats::complete.cases(data[names(objectives)]), , drop = FALSE]
  n_min <- bo_min_experiments(predictors, predictor_types)
  if (nrow(data) < n_min) {
    stop("At least ", n_min, " measured experiments are required for these predictors (found ", nrow(data), ")")
  }

  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("bbotk")$set_threshold("warn")

  domain <- build_bo_domain(predictors, predictor_types)
  codomain <- build_bo_codomain(objectives)
  archive <- bbotk::ArchiveBatch$new(search_space = domain, codomain = codomain)
  # standardized, because the hypervolume reference point is "worst + 1" and would otherwise
  # weight objectives on different scales (yield 0-1 vs cost 0-1000) very differently
  ydt <- standardize_objectives(lapply(data[names(objectives)], as.numeric))
  archive$add_evals(xdt = prepare_bo_xdt(data, predictors, predictor_types), ydt = ydt)

  all_continuous <- all(predictor_types[names(predictors)] %in% c("continuous_grid", "continuous"))
  learners <- lapply(seq_along(objectives), function(i) build_bo_learner(all_continuous, mixed_surrogate))
  surrogate <- mlr3mbo::srlrn(if (length(learners) == 1L) learners[[1L]] else learners, archive = archive)
  acquisition <- build_bo_acquisition(surrogate, length(objectives), all_continuous)
  optimizer <- mlr3mbo::acqo(
    bbotk::opt("random_search", batch_size = 1000L),
    terminator = bbotk::trm("evals", n_evals = 10000L),
    acq_function = acquisition,
    # purely categorical spaces are exhausted quickly; a replicate of a measured point is a valid suggestion for noisy lab data
    skip_already_evaluated = FALSE
  )

  # Constant liar: a proposed point is added with the mean of the measured results as fake result,
  # so the refitted GP is certain there and the next proposal lands elsewhere.
  # The archive is a local copy built from data, so the lies never reach the user's data.
  lie <- data.table::as.data.table(lapply(ydt, mean))
  suggestions <- vector("list", n_points)
  for (i in seq_len(n_points)) {
    surrogate$update()
    acquisition$update()
    res <- optimizer$optimize()
    # OpenDOE doesn't import data.table, so data.table syntax (with = FALSE) silently falls back to data.frame semantics
    x <- data.table::as.data.table(as.data.frame(res)[names(predictors)])
    suggestions[[i]] <- x
    if (i < n_points) archive$add_evals(xdt = x, ydt = lie)
  }
  as.data.frame(data.table::rbindlist(suggestions))
}
