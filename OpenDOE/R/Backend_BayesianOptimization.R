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

bo_suggest <- function(data, predictors, predictor_types, objectives) {
  if (length(objectives) != 1L) stop("Only a single objective is supported so far")
  missing_cols <- setdiff(c(names(predictors), names(objectives)), names(data))
  if (length(missing_cols) > 0L) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  # Rows without a measured objective are planned but not yet run experiments
  data <- data[stats::complete.cases(data[names(objectives)]), , drop = FALSE]
  if (nrow(data) < 3L) stop("At least three measured experiments are required")

  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("bbotk")$set_threshold("warn")

  domain <- build_bo_domain(predictors, predictor_types)
  codomain <- build_bo_codomain(objectives)
  archive <- bbotk::ArchiveBatch$new(search_space = domain, codomain = codomain)
  archive$add_evals(
    xdt = prepare_bo_xdt(data, predictors, predictor_types),
    ydt = data.table::as.data.table(lapply(data[names(objectives)], as.numeric))
  )

  all_continuous <- all(predictor_types[names(predictors)] %in% c("continuous_grid", "continuous"))
  learner <- if (all_continuous) {
    mlr3mbo::default_gp(noisy = TRUE)
  } else {
    mlr3mbo::default_rf(noisy = TRUE)
  }
  surrogate <- mlr3mbo::srlrn(learner, archive = archive)
  # AEI only accounts for noise with a kriging surrogate (reads the km nugget)
  acquisition <- mlr3mbo::acqf(if (all_continuous) "aei" else "ei", surrogate = surrogate)
  optimizer <- mlr3mbo::acqo(
    bbotk::opt("random_search", batch_size = 1000L),
    terminator = bbotk::trm("evals", n_evals = 10000L),
    acq_function = acquisition
  )

  surrogate$update()
  acquisition$update()
  res <- optimizer$optimize()
  as.data.frame(res)[names(predictors)]
}
