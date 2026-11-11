# see: https://statsdavew.github.io/math3014-6027-notes/fractional-factorial.html
size <- data.frame(1:15, 2^(1:15))
knitr::kable(
  size,
  col.names = c("No. factors", "No. of trts"),
  caption = "Number of treatments in a $2^f$ factorial designs for different numbers, $f$, of factors."
)

factor.name <- c(
  "Quench temperature (F)",
  "Heat temperature (F)",
  "Heating time (s)",
  "Transfer time (s)",
  "Hold-down time (s)"
)
low.level <- c("130-150", 1840, 23, 10, 2)
high.level <- c("150-170", 1880, 25, 12, 3)
spring.factors <- data.frame(
  factor = factor.name,
  low = low.level,
  high = high.level
)
row.names(spring.factors) <- LETTERS[1:5]
knitr::kable(
  spring.factors,
  col.names = c("Factor", "Low level", "High level"),
  align = c("l", "r", "r"),
  caption = "Spring experiment: factors and levels"
)
spring <- FrF2::FrF2(
  nruns = 16, nfactors = 5,
  generators = "BCD",
  randomize = FALSE
)
spring$height <- c(7.54, 7.20, 7.69, 7.63, 7.94, 7.40, 7.95, 7.62, 7.52, 7.52, 7.63, 7.65, 7.79, 7.29, 8.07, 7.73)
knitr::kable(
  spring,
  caption = "Spring experiment: 16 run design.",
  align = rep("r", 6)
)
fac_to_numeric <- function(x) as.numeric(as.character(x))
BCDE <- fac_to_numeric(spring$B) *
  fac_to_numeric(spring$C) *
  fac_to_numeric(spring$D) *
  fac_to_numeric(spring$E)
BCDE
BC <- fac_to_numeric(spring$B) * fac_to_numeric(spring$C)
DE <- fac_to_numeric(spring$D) * fac_to_numeric(spring$E)
BC
DE
all.equal(BC, DE)
B <- fac_to_numeric(spring$B)
CDE <- fac_to_numeric(spring$C) * fac_to_numeric(spring$D) * fac_to_numeric(spring$E)
B
CDE
all.equal(B, CDE)

spring.lm <- lm(height ~ (.)^5, data = spring)
FrF2::aliases(spring.lm)
