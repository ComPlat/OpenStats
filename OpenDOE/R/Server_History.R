to_json_safe <- function(x) {
  if (is.list(x)) return(lapply(x, to_json_safe))
  if (!is.null(names(x))) return(as.list(x))
  x
}

serialize_params <- function(type, params) {
  if (identical(type, "mc_anova")) {
    params$formula <- format(params$formula)
    params$interactions <- lapply(params$interactions, function(it) {
      it$mask <- format(it$mask)
      it
    })
  }
  to_json_safe(params)
}

deserialize_params <- function(type, params) {
  switch(type,
    ttest = list(
      predictors = lapply(params$predictors, function(v) as.character(unlist(v))),
      primary_factor = as.character(params$primary_factor),
      cohens_d = as.numeric(params$cohens_d),
      sig_level = as.numeric(params$sig_level),
      desired_power = as.numeric(params$desired_power)
    ),
    anova = list(
      predictors = lapply(params$predictors, function(v) as.character(unlist(v))),
      primary_factor = as.character(params$primary_factor),
      cohens_f = as.numeric(params$cohens_f),
      sig_level = as.numeric(params$sig_level),
      desired_power = as.numeric(params$desired_power)
    ),
    predictor_table = list(
      predictors = lapply(params$predictors, function(v) as.character(unlist(v))),
      predictor_types = unlist(params$predictor_types)
    ),
    mc = list(
      means = unlist(params$means),
      sds = unlist(params$sds),
      power_target = as.numeric(params$power_target),
      alpha = as.numeric(params$alpha),
      mcc = as.character(params$mcc),
      family = as.character(params$family),
      nsim = as.numeric(params$nsim),
      n_min = as.numeric(params$n_min),
      n_max = as.numeric(params$n_max),
      seed = as.numeric(params$seed)
    ),
    mc_anova = list(
      levels = lapply(params$levels, function(v) as.character(unlist(v))),
      means = lapply(params$means, unlist),
      cv = if (is.null(params$cv)) NULL else as.numeric(params$cv),
      sd = if (is.null(params$sd)) NULL else as.numeric(params$sd),
      interactions = lapply(params$interactions, function(it) {
        list(mask = str2lang(it$mask), value = as.numeric(it$value))
      }),
      formula = stats::as.formula(params$formula),
      alphas = unlist(params$alphas),
      power_target = as.numeric(params$power_target),
      seed = as.numeric(params$seed),
      nsim = as.numeric(params$nsim),
      n_min = as.numeric(params$n_min),
      n_max = as.numeric(params$n_max)
    ),
    design = list(
      predictors = lapply(params$predictors, function(v) as.character(unlist(v))),
      n_per_level = as.integer(params$n_per_level)
    ),
    params
  )
}

replay_history_entry <- function(State, entry) {
  p <- entry$params
  switch(entry$type,
    ttest = State$bgp$start(
      fun = run_two_sample_ttest, args = p,
      on_success = function(res) {
        State$n_per_level <- res
        add_result(State, "ttest", "Power analysis: t-test (replay)", p, methods::new("sampleSizeResult", n = res))
      }
    ),
    anova = State$bgp$start(
      fun = run_one_way_anova, args = p,
      on_success = function(res) {
        State$n_per_level <- res
        add_result(State, "anova", "Power analysis: ANOVA (replay)", p, methods::new("sampleSizeResult", n = res))
      }
    ),
    predictor_table = State$bgp$start(
      fun = build_predictor_df, args = p,
      on_success = function(df) {
        add_result(State, "predictor_table", "Predictor table (replay)", p, methods::new("predictorTable", df = df))
      }
    ),
    mc = State$bgp$start(
      fun = run_estimate_sample_size, args = p,
      on_success = function(res) {
        if (is.na(res$n)) {
          print_err(res$message)
          return(invisible())
        }
        State$n_per_level <- res$n
        add_result(State, "mc", "Monte Carlo: multiple comparison (replay)", p, methods::new("sampleSizeResult", n = res$n))
      }
    ),
    mc_anova = State$bgp$start(
      fun = run_determine_sample_size, args = p,
      on_success = function(res) {
        if (is.na(res$n)) {
          print_err(res$message)
          return(invisible())
        }
        State$n_per_level <- res$n
        add_result(State, "mc_anova", "Monte Carlo: ANOVA (replay)", p, methods::new("sampleSizeResult", n = res$n))
      }
    ),
    design = State$bgp$start(
      fun = run_completely_randomised_design, args = p,
      on_success = function(res) {
        label <- paste0("Design of experiment (n=", p$n_per_level, ") (replay)")
        add_result(State, "design", label, p, methods::new("designResult", df = res))
      }
    ),
    print_warn(paste("Cannot replay this entry type:", entry$type))
  )
}

history_entries_list <- function(State) {
  stats::setNames(
    lapply(State$history, function(entry) {
      list(type = entry$type, label = entry$label, time = entry$time,
           params = serialize_params(entry$type, entry$params))
    }),
    names(State$history)
  )
}

historyServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    output$save_history <- shiny::downloadHandler(
      filename = function() paste0("opendoe_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
      content = function(file) {
        l <- State$results
        names(l) <- vapply(State$results, function(v) attr(v, "label"), character(1))

        history_json <- jsonlite::toJSON(history_entries_list(State), pretty = TRUE, auto_unbox = TRUE, null = "null")
        l <- c(l, stats::setNames(list(as.character(history_json)), "History (JSON)"))

        fn <- create_excel_file(l)
        if (is.null(fn)) return(invisible())
        file.copy(fn, file, overwrite = TRUE)
        unlink(fn)
      }
    )

    shiny::observeEvent(input$replay_json, {
      raw <- try(jsonlite::fromJSON(input$history_json, simplifyVector = FALSE), silent = TRUE)
      if (inherits(raw, "try-error") || !is.list(raw)) {
        print_err("Could not parse history JSON")
        return(invisible())
      }
      queued <- lapply(raw, function(e) {
        list(type = e$type, label = e$label, params = deserialize_params(e$type, e$params))
      })
      State$replay_queue <- c(State$replay_queue, queued)
      State$history_replay_running <- TRUE
    })

    shiny::observe({
      shiny::invalidateLater(250)
      if (isTRUE(State$bgp$running)) return()
      if (length(State$replay_queue) == 0L) {
        State$history_replay_running <- FALSE
        return()
      }
      queue <- State$replay_queue
      entry <- queue[[1]]
      State$replay_queue <- queue[-1]
      replay_history_entry(State, entry)
    })

    output$replay_status <- shiny::renderUI({
      if (isTRUE(State$history_replay_running)) {
        shiny::tags$div(
          class = "doe-box",
          shiny::tags$p("Replaying history..."),
          shiny::actionButton("HISTORY-cancel_replay", "Cancel", class = "btn-danger")
        )
      }
    })

    shiny::observeEvent(input$cancel_replay, {
      State$replay_queue <- list()
      State$bgp$cancel()
      State$history_replay_running <- FALSE
    }, ignoreInit = TRUE)
  })
}
