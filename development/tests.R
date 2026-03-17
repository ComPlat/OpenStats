set.seed(1234)
n <- 3000
dat <- data.frame(
  y = rnorm(n),
  A = factor(sample(letters[1:4], n, TRUE)),
  B = factor(sample(letters[1:3], n, TRUE)),
  C = factor(sample(letters[1:2], n, TRUE)),
  D = factor(sample(letters[1:5], n, TRUE))
)
formula <- y  ~ A * B * C * D

res <- summary(
  permuco::aovperm(
    formula, data = dat,np = 5000
  )
)
res
str(res)
as.data.frame(res)

set.seed(42)
P <- permuco::Pmat(np = 5000, n = nrow(CO2))
fit <- permuco::aovperm(
  uptake ~ conc * Treatment * Type,
  data = CO2,
  P = P
)
summary(fit)
