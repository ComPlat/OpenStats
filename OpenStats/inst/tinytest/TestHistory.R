library(tinytest)
test_data_dir <- system.file("test_data", package = "OpenStats")
files <- list.files(test_data_dir, pattern = "\\.json$", full.names = TRUE)

load_and_eval_history <- function(file, df) {
  json <- readLines(file, n = -1)
  OpenStats:::eval_history(json, df, list(df0 = df), TRUE)
}

# Test glm history
# ========================================================================================
CO2 <- read.csv(paste0(test_data_dir, "/CO2.csv"))
result <- load_and_eval_history(files[7], CO2)
result <- result$ResultsState$all_data
expect_true(
  inherits(result[[2]]@p, "plot"),
  "Summary model 1"
)
expect_true(
  identical(
    broom::tidy(glm(uptake ~ Treatment, data = CO2)), result[[2]]@summary
  ),
  "Summary model 1"
)

expect_true(
  identical(
    {
      family <- "gaussian"
      link_fct <- "identity"
      family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))
      model <- glm(uptake ~ Treatment, data = CO2, family = eval(family))
      fit <- broom::tidy(anova(model, test = "Chisq"))
    },
    result[[4]]
  ),
  "anova"
)

adjustment_methods <- c(
  "tukey", "sidak", "bonferroni", "scheffe", "none", "fdr", "holm", "hochberg", "hommel"
)

run_posthoc_glm <- function(method) {
  family <- "gaussian"
  link_fct <- "identity"
  family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))
  formula <- uptake ~ Treatment
  f_split <- OpenStats:::split_formula(formula)
  rhs_vars <- OpenStats:::vars_rhs(f_split$right_site)
  df_temp <- OpenStats:::num_to_factor(CO2, rhs_vars)
  if (any(apply(CO2, 2, is.numeric))) {
    warning(paste0("Found numeric predictors and converted them to factors"))
  }
  model <- glm(formula, data = CO2, family = eval(family))
  emm <- emmeans::emmeans(model, rhs_vars)
  fit <- pairs(emm, adjust = method)
  as.data.frame(fit)
}

checks <- c()
for (i in seq_along(adjustment_methods)) {
  checks <- c(checks,
    expect_true(
      identical(
        {
          run_posthoc_glm(adjustment_methods[i])
        },
        result[[i + 4]]
      ),
      adjustment_methods[i]
    )
  )
}
expect_true(all(checks), "All PostHoc tests GLM")

# Test entire analysis
# ========================================================================================
CO2 <- read.csv(paste0(test_data_dir, "/CO2.csv"))
result <- load_and_eval_history(files[6], CO2)
result <- result$ResultsState$all_data

expect_true(
  inherits(result[[2]]@p, "plot"),
  "Summary model 1"
)
expect_true(
  identical(
    broom::tidy(lm(uptake ~ conc, data = CO2)), result[[2]]@summary
  ),
  "Summary model 1"
)
expect_true(
  identical(
    AIC(lm(uptake ~ conc, data = CO2)), result[[2]]@information_criterions[,1]
  ),
  "Summary model 1"
)
expect_equal(
  result[[4]], data.frame(mean_conc = mean(CO2$conc)),
  info = "max_conc <- Max(conc)"
)
expect_equal(
  result[[5]],
  {
    CO2$conc_norm <-  CO2$conc / mean(CO2$conc)
    CO2
  },
  info = "CO2$conc_norm <-  CO2$conc / max(CO2$conc)"
)
expect_true(
  inherits(result[[6]], "plot"),
  info = "Plot model 1"
)
expect_true(
  inherits(result[[7]], "plot"),
  info = "plot uptake against conc_norm"
)
expect_true(
  inherits(result[[8]], "data.frame"),
  info = "Shapiro on data"
)
expect_equal(
  result[[9]],
  {
    fit <- lm(uptake ~ conc, data = CO2)
    r <- resid(fit)
    res <- broom::tidy(shapiro.test(r))
    res$`Residuals normal distributed` <- res$p.value > 0.05
    res
  },
  info = "CO2$conc_norm <-  CO2$conc / max(CO2$conc)"
)
expect_true(
  inherits(result[[10]], "plot"),
  info = "Diagnostic plots"
)
expect_true(
  inherits(result[[11]]@p, "plot"),
  "Summary model 2"
)
expect_true(
  identical(
    broom::tidy(lm(uptake ~ Type, data = CO2)), result[[11]]@summary
  ),
  "Summary model 2"
)
expect_true(
  identical(
    AIC(lm(uptake ~ Type, data = CO2)), result[[11]]@information_criterions[,1]
  ),
  "Summary model 2"
)
expect_equal(
  result[[13]],
  {
    res <- broom::tidy(
      car::leveneTest(uptake ~ Type, data = CO2, center = "mean")
    )
    res$`Variance homogenity` <- res$p.value > 0.05
    res
  },
  info = "Levene test on model 2"
)
expect_true(
  inherits(result[[14]]@p, "plot"),
  "Summary model 3"
)
expect_true(
  identical(
    broom::tidy(lm(uptake ~ conc, data = CO2)), result[[14]]@summary
  ),
  "Summary model 3"
)
expect_true(
  identical(
    AIC(lm(uptake ~ conc, data = CO2)), result[[14]]@information_criterions[,1]
  ),
  "Summary model 3"
)
expect_equal(
  result[[16]],
  {
    res <- broom::tidy(
      cor.test(
        CO2$uptake, CO2$conc,
        method = "pearson",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
    res
  },
  info = "pearson"
)
expect_equal(
  result[[17]],
  {
    res <- broom::tidy(
      cor.test(
        CO2$uptake, CO2$conc,
        method = "spearman",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
    res
  },
  info = "spearman"
)
expect_equal(
  result[[18]],
  {
    res <- broom::tidy(
      cor.test(
        CO2$uptake, CO2$conc,
        method = "kendall",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
    res
  },
  info = "kendall"
)
expect_true(
  inherits(result[[19]]@p, "plot"),
  "Summary model 4"
)
expect_true(
  identical(
    broom::tidy(lm(uptake ~ Treatment, data = CO2)), result[[19]]@summary
  ),
  "Summary model 4"
)
expect_true(
  identical(
    AIC(lm(uptake ~ Treatment, data = CO2)), result[[19]]@information_criterions[,1]
  ),
  "Summary model 4"
)
expect_equal(
  broom::tidy(t.test(uptake ~ Treatment,
    data = CO2, conf.level = 0.95,
    alternative = "two.sided", var.equal = TRUE
  )),
  result[[21]],
  info = "Ttest"
)
expect_equal(
  {
    fit <- broom::tidy(aov(uptake ~ Treatment, data = CO2))
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("Treatment", collapse = ".")
    fit
  },
  result[[22]],
  info = "ANOVA"
)
expect_equal(
  {
    fit <- broom::tidy(kruskal.test(uptake ~ Treatment, data = CO2))
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("Treatment", collapse = ".")
    fit
  },
  result[[23]],
  info = "Kruskal Wallis test"
)
expect_equal(
  {
    aov_res <- aov(uptake ~ Treatment, data = CO2)
    bal <- "Balanced"
    if (bal == "Balanced") {
      bal <- TRUE
    } else {
      bal <- FALSE
    }
    fit <- agricolae::HSD.test(aov_res,
      trt = "Treatment",
      alpha = 0.05, group = TRUE, unbalanced = bal
    )$groups
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("Treatment", collapse = ".")
    fit
  },
  result[[24]],
  info = "TukeyHSD"
)
expect_equal(
  {
    fit <- with(CO2, agricolae::kruskal(CO2[, "uptake"], CO2[, "Treatment"]),
      alpha = 0.05, p.adj = "Holm", group = TRUE
    )$groups
    names(fit)[1] <- "uptake"
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("Treatment", collapse = ".")
    fit
  },
  result[[25]],
  info = "Kruskal Wallis PostHoc test"
)
expect_equal(
  {
    aov_res <- aov(uptake ~ Treatment, data = CO2)
    fit <- agricolae::LSD.test(aov_res,
      trt = "Treatment",
      alpha = 0.05, p.adj = "holm", group = TRUE
    )$groups
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("Treatment", collapse = ".")
    fit
  },
  result[[26]],
  info = "LSD"
)
expect_equal(
  {
    aov_res <- aov(uptake ~ Treatment, data = CO2)
    fit <- agricolae::scheffe.test(aov_res,
      trt = "Treatment",
      alpha = 0.05, group = TRUE
    )$groups
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("Treatment", collapse = ".")
    fit
  },
  result[[27]],
  info = "scheffe"
)
expect_true(
  inherits(result[[28]]@p, "plot"),
  "Summary model 5"
)
expect_true(
  identical(
    broom::tidy(lm(uptake ~ conc_norm, data = CO2)), result[[29]]@summary
  ),
  "Summary model 5"
)
expect_true(
  identical(
    AIC(lm(uptake ~ conc_norm, data = CO2)), result[[29]]@information_criterions[,1]
  ),
  "Summary model 5"
)
expect_equal(
  {
    aov_res <- aov(uptake ~ conc_norm, data = CO2)
    fit <- agricolae::REGW.test(aov_res,
      trt = "conc",
      alpha = 0.05, group = TRUE
    )$groups
    fit <- cbind(fit, row.names(fit))
    names(fit)[ncol(fit)] <- paste0("conc_norm", collapse = ".")
    fit
  },
  result[[30]],
  info = "REGW"
)

# Applying and removing filters
# ========================================================================================
CO2 <- read.csv(paste0(test_data_dir, "/CO2.csv"))
result <- load_and_eval_history(files[4], CO2)
result <- result$ResultsState$all_data

expect_true(
  length(result) == 3,
  info = "Apply filter"
)
expect_true(
  inherits(result[[2]], "plot"),
  info = "Apply filter"
)
expect_true(
  inherits(result[[3]], "plot"),
  info = "Apply filter"
)

# Wrong stuff
# ========================================================================================
expect_null(
  load_and_eval_history(files[1], CO2),
  info = "Invalid text BlaBla"
)
expect_null(
  load_and_eval_history(files[2], CO2),
  info = "Invalid type (RocketLaunch)"
)

# Dose response
# ========================================================================================
dose_response_V1_2 <- read.csv(paste0(test_data_dir, "/DoseResponse.csv"))
result <- load_and_eval_history(files[3], dose_response_V1_2)
result <- result$ResultsState$all_data
expect_true(
  inherits(result[[2]], "summaryModel"),
  info = "summary of a model"
)
expect_equal(
  broom::tidy(lm(abs ~ conc, data = dose_response_V1_2)),
  result[[2]]@summary,
  info = "Result summary"
)
expect_true(
  inherits(result[[4]], "doseResponse"),
  info = "Dose response result"
)
expect_true(
  inherits(result[[5]], "doseResponse"),
  info = "Dose response result"
)
expect_true(
  inherits(result[[6]], "doseResponse"),
  info = "Dose response result"
)

# Test formula
# ========================================================================================
CO2 <- read.csv(paste0(test_data_dir, "/CO2.csv"))
result <- load_and_eval_history(files[5], CO2)
result <- result$ResultsState$all_data
expect_true(
  length(result) == 5,
  info = "Formula editor"
)
expect_true(
  inherits(result[[2]]@p, "plot"),
  info = "Formula plot 1"
)
expect_true(
  identical(
    broom::tidy(lm(uptake ~ conc, data = CO2)), result[[2]]@summary
  ),
  "Summary model 1"
)
expect_true(
  inherits(result[[2]]@p, "plot"),
  info = "Formula plot 2"
)
expect_true(
  identical(
    broom::tidy(lm(uptake ~ conc*Type, data = CO2)), result[[4]]@summary
  ),
  "Summary model 2"
)
