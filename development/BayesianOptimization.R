library(mlr3mbo)
library(mlr3learners)
lgr::get_logger("bbotk")$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

pl <- function() {
  grid <- seq(from = 0, to = 5, length.out = 200)
  preds <- surrogate$predict(data.table(x = grid))
  preds <- as.data.frame(preds)
  preds$lower95 <- preds$mean - 1.96 * preds$se
  preds$upper95 <- preds$mean + 1.96 * preds$se

  plot(
    NA,
    xlim = range(grid),
    ylim = range(preds$lower95, preds$upper95),
    xlab = "x", ylab = "y"
  )
  polygon(
    c(grid, rev(grid)),
    c(preds$lower95, rev(preds$upper95)),
    col = adjustcolor("steelblue", 0.3),
    border = NA
  )
  lines(grid, preds$mean, col = "steelblue", lwd = 2)
  curve(sin(x), add = TRUE, lty = 2)
  points(archive$data$x, archive$data$y, pch = 19)
}

test_function <- function(x) {
  sin(x) + rnorm(length(x), sd = 0.1)
}

domain <- paradox::ps(x = paradox::p_dbl(lower = 0, upper = 5))
codomain <- paradox::ps(y = paradox::p_dbl(tags = "maximize"))
archive <- bbotk::ArchiveBatch$new(
  search_space = domain,
  codomain = codomain
)

x_start <- runif(3, 0, 5)
y_start <- test_function(x_start)
archive$add_evals(
  xdt = data.table(x = x_start), ydt = data.table(y = y_start)
)

surrogate <- srlrn(
  lrn(
    "regr.km", nugget.estim = TRUE,
    control = list(trace = FALSE)
  ),
  archive = archive,
)
surrogate$update()

aquistion <- acqf("aei", surrogate = surrogate)
aquistion$update()

optimizer <- acqo(
  bbotk::opt("random_search", batch_size = 1000),
  terminator = bbotk::trm("evals", n_evals = 1000),
  acq_function = aquistion
)

n <- 100L
pb <- utils::txtProgressBar(min = 1, max = n, style = 3)
for (i in seq_len(n)) {
  res <- optimizer$optimize()
  y_new <- test_function(res$x)
  archive$add_evals(
    xdt = data.table(x = res$x), ydt = data.table(y = y_new)
  )
  surrogate$update()
  aquistion$update()
  utils::setTxtProgressBar(pb, i)
}
close(pb)
archive$best()
pl()

