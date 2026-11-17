simulate <- function(name, slope, true_ic50) {
  # Define true parameters
  b  <- slope          # slope
  c  <- 0.05          # lower limit
  d  <- 1.1           # upper limit
  e  <- true_ic50     # IC50

  set.seed(1)
  conc_levels <- c(0.5, seq(2.5, 26, by = 2.5))
  conc <- rep(conc_levels, each = 5)

  # Generate response
  logistic_response <- function(conc, b, c, d, e) {
    c + (d - c) / (1 + (conc / e)^b)
  }
  abs <- logistic_response(conc, b, c, d, e) + rnorm(length(conc), sd = 0.05)

  # positive control
  abs_pos <- rnorm(5, mean = 0.05, sd = 0.01)
  conc_pos <- rep("pos", 5)
  # Add negative control
  abs_neg <- rnorm(5, mean = 1.1, sd = 0.01)
  conc_neg <- rep("neg", 5)

  data.frame(
    substance = factor(c(rep(name, length(conc)), conc_pos, conc_neg), levels = c(name, "pos", "neg")),
    conc = factor(c(conc, conc_pos, conc_neg), levels = c(as.character(conc_levels), "pos", "neg")),
    abs = c(abs, abs_pos, abs_neg)
  )
}

slopes <- rep(7, 12)
names <- paste0("Substance", 1:12)
l1 <- simulate(names[1], slopes[1], 1)
l2 <- simulate(names[2], slopes[2], 2.5)
l3 <- simulate(names[3], slopes[3], 6)
l4 <- simulate(names[4], slopes[4], 7.5)
l5 <- simulate(names[5], slopes[5], 9)
l6 <- simulate(names[6], slopes[6], 10.75)
l7 <- simulate(names[7], slopes[7], 12.5)
l8 <- simulate(names[8], slopes[8], 15.3)
l9 <- simulate(names[9], slopes[9], 17.5)
l10 <- simulate(names[10], slopes[10], 19.3)
l11 <- simulate(names[11], slopes[11], 22.2)
l12 <- simulate(names[12], slopes[12], 24.5)

df <- do.call(rbind, list(l1, l2, l3, l4, l5, l6, l7, l8,l9, l10, l11, l12))
write.csv(df, "./Paper/DoseResponseData.csv", quote = FALSE, row.names = FALSE)
