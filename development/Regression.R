library(ggplot2)

files <- list.files("./OpenStats/R", full.names = TRUE)
trash <- lapply(files, source)

# Simulate calibration data
set.seed(42)
true_a <- 5
true_b <- 2
conc <- seq(0.1, 10, length.out = 30)
response <- true_a * conc / (true_b + conc) + rnorm(length(conc), sd = 0.2)
df <- data.frame(conc = conc, response = response)
write.csv(df, "./test_data/calibration.csv", quote = FALSE, row.names = FALSE)
formula <- response ~ a * conc / (b + conc)
formula_optim <- create_formula_optim(formula, df, 0, 100, 1234)
res <- optimize(formula_optim, df)
res@parameter
p <- plot_model_optim(formula_optim, res)
p
assumptions_optim(res)


summary_model_optim(formula_optim, res)
information_criterion_optim(res)
