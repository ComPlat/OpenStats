library(perm)

set.seed(1234)
n <- 18000
dat <- data.frame(
  y = rnorm(n),
  A = factor(sample(letters[1:4], n, TRUE)),
  B = factor(sample(letters[1:3], n, TRUE)),
  C = factor(sample(letters[1:2], n, TRUE)),
  D = factor(sample(letters[1:5], n, TRUE))
)

# Collapse all factor combinations into one group
dat$grp <- interaction(dat$A, dat$B, dat$C, dat$D, drop = TRUE)

nlevels(dat$grp)
# 4 * 3 * 2 * 5 = 120 groups

res <- permKS(
  y ~ grp,
  data = dat,
  method = "pclt",
  control = permControl(nmc = 9999)
)

res
