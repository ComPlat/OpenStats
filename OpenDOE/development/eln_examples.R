two <- c("Cd44 ft/ft", "Cd44 ko")
three <- c("Vehicle", "DrugA", "DrugB")

ess_mock <- function(predictor, levels) {
  list(
    eln = list(
      id = "2", request_id = "SSC-1",
      element_info = list(
        id = "140",
        name = "New Animal Study Protocol",
        short_label = "CHI-AT1",
        klass_uuid = "klass-1",
        version = "1.33"
      )
    ),
    response = "Tumor volume",
    notes = character(0),
    outcomes = list(list(
      uuid = "o1", name = "Tumor volume",
      role = "primary", type = "continuous",
      unit = "mm3"
    )),
    predictors = list(list(
      uuid = "p1", name = predictor,
      role = "primary_factor",
      type = "factor"
    )),
    controls = list(list(
      uuid = "c1", name = levels[[1]],
      type = "negative",
      predictor = predictor,
      level = levels[[1]]
    ))
  )
}

mc_pairwise <- function(levels) {
  list(
    means = setNames(
      c(10, 14, 16)[seq_along(levels)],
      levels
    ),
    sds = setNames(
      rep(3, length(levels)), levels
    ),
    power_target = 0.8, alpha = 0.05,
    mcc = "holm", family = "any",
    nsim = 1000, n_min = 2, n_max = 50,
    seed = 42
  )
}

mc_one_way <- function() {
  list(
    level = three, mu = c(10, 14, 16),
    cv = 0.25, alpha = 0.05,
    power_target = 0.8, seed = 42,
    nsim = 1000, n_min = 2, n_max = 50
  )
}

power <- function(n, p) {
  OpenDOE:::sample_size_power(list(
    n = n, power = p, power_se = 0.01
  ))
}

cases <- list(
  ttest = list(
    predictor = "Genotype", levels = two,
    params = list(
      predictors = list(Genotype = two),
      primary_factor = "Genotype",
      cohens_d = 0.8, sig_level = 0.05,
      desired_power = 0.8
    ),
    n = 26, power = list()
  ),
  anova = list(
    predictor = "Treatment", levels = three,
    params = list(
      predictors = list(Treatment = three),
      primary_factor = "Treatment",
      cohens_f = 0.4, sig_level = 0.05,
      desired_power = 0.8
    ),
    n = 21, power = list()
  ),
  mc_student_t = list(
    predictor = "Genotype", levels = two,
    params = mc_pairwise(two),
    n = 11, power = power(11, 0.82)
  ),
  mc = list(
    predictor = "Genotype", levels = two,
    params = mc_pairwise(two),
    n = 11, power = power(11, 0.81)
  ),
  mc_wilcoxon = list(
    predictor = "Genotype", levels = two,
    params = mc_pairwise(two),
    n = 12, power = power(12, 0.81)
  ),
  mc_student_t_multiple = list(
    type = "mc_student_t",
    predictor = "Treatment", levels = three,
    params = mc_pairwise(three),
    n = 7, power = power(7, 0.84)
  ),
  mc_multiple = list(
    type = "mc",
    predictor = "Treatment", levels = three,
    params = mc_pairwise(three),
    n = 7, power = power(7, 0.83)
  ),
  mc_wilcoxon_multiple = list(
    type = "mc_wilcoxon",
    predictor = "Treatment", levels = three,
    params = mc_pairwise(three),
    n = 8, power = power(8, 0.88)
  ),
  mc_anova = list(
    predictor = "Treatment", levels = three,
    params = list(
      levels = list(Treatment = three),
      mu = c(10, 14, 16), cv = 0.25,
      formula = values ~ Treatment,
      alphas = c(Treatment = 0.05),
      target_effect = "Treatment",
      power_target = 0.8, seed = 42,
      nsim = 1000, n_min = 2, n_max = 50
    ),
    n = 6,
    power = OpenDOE:::sample_size_power(list(
      n = 6, power = 0.86, power_se = 0.01,
      power_by_effect = c(Treatment = 0.86)
    ))
  ),
  mc_welch_anova = list(
    predictor = "Treatment", levels = three,
    params = mc_one_way(),
    n = 7, power = power(7, 0.83)
  ),
  mc_kruskal = list(
    predictor = "Treatment", levels = three,
    params = mc_one_way(),
    n = 8, power = power(8, 0.82)
  )
)

out_dir <- "OpenDOE/development/eln_examples"
dir.create(out_dir, showWarnings = FALSE)

for (name in names(cases)) {
  case <- cases[[name]]
  type <- if (is.null(case$type)) name else
    case$type
  ess <- ess_mock(case$predictor, case$levels)
  result <- methods::new(
    "sampleSizeResult",
    n = case$n, power = case$power
  )
  current <- setNames(
    list(case$levels), case$predictor
  )
  out <- OpenDOE:::eln_sample_size_output(
    ess, result,
    list(type = type, params = case$params),
    current
  )
  writeLines(
    OpenDOE:::eln_sample_size_json(out),
    file.path(
      out_dir,
      paste0("eln_example_", name, ".json")
    )
  )
}
