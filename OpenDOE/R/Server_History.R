to_json_safe <- function(x) {
  if (is.list(x)) return(lapply(x, to_json_safe))
  if (!is.null(names(x))) return(as.list(x))
  x
}

serialize_params <- function(type, params) {
  if (identical(type, "mc_anova")) {
    params$formula <- format(params$formula)
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
    # mc/mc_wilcoxon/mc_student_t share the same params shape (only the
    # underlying test differs, decided at replay time by entry$type) --
    # fallthrough.
    mc = ,
    mc_wilcoxon = ,
    mc_student_t = list(
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
      mu = as.numeric(unlist(params$mu)),
      cv = if (is.null(params$cv)) NULL else as.numeric(params$cv),
      sd = if (is.null(params$sd)) NULL else as.numeric(params$sd),
      formula = stats::as.formula(params$formula),
      alphas = unlist(params$alphas),
      target_effect = as.character(params$target_effect),
      power_target = as.numeric(params$power_target),
      seed = as.numeric(params$seed),
      nsim = as.numeric(params$nsim),
      n_min = as.numeric(params$n_min),
      n_max = as.numeric(params$n_max)
    ),
    # mc_kruskal/mc_welch_anova share the same params shape too (single
    # predictor, see Randomization::determine_sample_size_one_way) --
    # fallthrough.
    mc_kruskal = ,
    mc_welch_anova = list(
      level = as.character(unlist(params$level)),
      mu = as.numeric(unlist(params$mu)),
      cv = if (is.null(params$cv)) NULL else as.numeric(params$cv),
      sd = if (is.null(params$sd)) NULL else as.numeric(params$sd),
      alpha = as.numeric(params$alpha),
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
    random_assign = list(
      df_source = as.character(params$df_source),
      groups = as.character(unlist(params$groups)),
      ratios = as.numeric(unlist(params$ratios)),
      col = as.character(params$col),
      block_col = if (is.null(params$block_col)) NULL else as.character(params$block_col),
      strata_cols = if (is.null(params$strata_cols)) NULL else as.character(unlist(params$strata_cols)),
      randomization_method = as.character(params$randomization_method),
      seed = as.numeric(params$seed)
    ),
    random_finite_assign = list(
      groups_id = as.character(params$groups_id),
      id_col = as.character(params$id_col),
      design_id = as.character(params$design_id),
      max_iter = as.numeric(params$max_iter),
      loss_function = as.character(params$loss_function),
      seed = as.numeric(params$seed)
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
        add_result(State, "mc", "Pairwise comparisons (t-test) (replay)", p, methods::new("sampleSizeResult", n = res$n))
      }
    ),
    mc_wilcoxon = State$bgp$start(
      fun = run_estimate_sample_size_wilcoxon, args = p,
      on_success = function(res) {
        if (is.na(res$n)) {
          print_err(res$message)
          return(invisible())
        }
        State$n_per_level <- res$n
        add_result(State, "mc_wilcoxon", "Pairwise comparisons (Wilcoxon rank-sum test) (replay)", p, methods::new("sampleSizeResult", n = res$n))
      }
    ),
    mc_student_t = State$bgp$start(
      fun = run_estimate_sample_size_student_t, args = p,
      on_success = function(res) {
        if (is.na(res$n)) {
          print_err(res$message)
          return(invisible())
        }
        State$n_per_level <- res$n
        add_result(State, "mc_student_t", "Pairwise comparisons (t-Test (Student)) (replay)", p, methods::new("sampleSizeResult", n = res$n))
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
    mc_kruskal = State$bgp$start(
      fun = run_determine_sample_size_kruskal, args = p,
      on_success = function(res) {
        if (is.na(res$n)) {
          print_err(res$message)
          return(invisible())
        }
        State$n_per_level <- res$n
        add_result(State, "mc_kruskal", "Monte Carlo: Kruskal-Wallis-Test (replay)", p, methods::new("sampleSizeResult", n = res$n))
      }
    ),
    mc_welch_anova = State$bgp$start(
      fun = run_determine_sample_size_welch_anova, args = p,
      on_success = function(res) {
        if (is.na(res$n)) {
          print_err(res$message)
          return(invisible())
        }
        State$n_per_level <- res$n
        add_result(State, "mc_welch_anova", "Monte Carlo: Welch-ANOVA (replay)", p, methods::new("sampleSizeResult", n = res$n))
      }
    ),
    design = State$bgp$start(
      fun = run_completely_randomised_design, args = p,
      on_success = function(res) {
        label <- paste0("Design of experiment (n=", p$n_per_level, ") (replay)")
        add_result(State, "design", label, p, methods::new("designResult", df = res))
      }
    ),
    # random_assign/random_finite_assign reference a dataset by Results id
    # rather than storing it (see roadmap.md) -- replayable only if that id
    # still resolves in *this* session (e.g. because you uploaded/rebuilt
    # the same thing first, in the same order the original session did).
    # If not, this is a plain "cannot replay" -- there's no way to
    # reconstruct a missing dataset, only the user re-doing the upload can.
    random_assign = {
      df_source <- p$df_source
      if (is.null(State$results[[df_source]])) {
        print_warn(paste0(
          "Cannot replay random_assign: dataset '", df_source, "' doesn't exist in this session. ",
          "Upload/recreate it first (in the same order as the original session), then replay again."
        ))
      } else {
        args <- p[setdiff(names(p), "df_source")]
        args$df <- State$results[[df_source]]@df
        method_labels <- c(simple = "simple", block = "block", block_stratified = "block + stratified")
        label <- paste0("Random assignment (", method_labels[[p$randomization_method]], ") (replay)")
        State$bgp$start(
          fun = run_random_assign, args = args,
          on_success = function(res) {
            add_result(State, "random_assign", label, p, methods::new("assignmentResult", df = res))
          }
        )
      }
    },
    random_finite_assign = {
      groups_id <- p$groups_id
      design_id <- p$design_id
      if (is.null(State$results[[groups_id]]) || is.null(State$results[[design_id]])) {
        print_warn(paste0(
          "Cannot replay random_finite_assign: referenced dataset(s) don't exist in this session. ",
          "Upload/recreate them first (in the same order as the original session), then replay again."
        ))
      } else {
        groups_df_full <- State$results[[groups_id]]@df
        id_col <- p$id_col
        if (!(id_col %in% names(groups_df_full))) {
          print_warn(paste0(
            "Cannot replay random_finite_assign: ID column '", id_col, "' not found in dataset '", groups_id, "'."
          ))
        } else {
          ids <- groups_df_full[[id_col]]
          groups_df <- groups_df_full[, setdiff(names(groups_df_full), id_col), drop = FALSE]
          design_df <- State$results[[design_id]]@df
          State$bgp$start(
            fun = run_random_finite_assign,
            args = list(
              seed = p$seed, groups = groups_df, design = design_df, ids = ids,
              max_iter = p$max_iter, loss_function = p$loss_function
            ),
            on_success = function(res) {
              add_result(
                State, "random_finite_assign", "Random assignment (finite groups) (replay)", p,
                methods::new("finiteAssignmentResult", df = res$df, loss = res$loss)
              )
            }
          )
        }
      }
    },
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
