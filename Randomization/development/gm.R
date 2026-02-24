n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 27L))
groups <- data.frame(blood = c(rep(4000, 27L), rep(3500, 27L), rep(300, 27L)))
random_finite_assign(42, groups, n_blocks)
