set.seed(1234)
# ---------- core model ----------
binding_one <- function(conc_nM, time_s, kon, koff, Bmax) {
  cM <- conc_nM * 1e-9
  frac <- cM / (cM + koff / kon)
  frac * Bmax * (1 - exp(-(kon * cM + koff) * time_s))
}

binding_two <- function(conc_nM, time_s,
                        kon1, koff1, Bmax1,
                        kon2, koff2, Bmax2,
                        baseline = 0) {
  binding_one(conc_nM, time_s, kon1, koff1, Bmax1) +
    binding_one(conc_nM, time_s, kon2, koff2, Bmax2) +
    baseline
}

# ---------- simulator ----------
simulate_two_hot <- function(
  conc_nM = c(0, 5, 10, 25, 50, 100, 250, 500, 1000),
  time_s  = seq(0, 600, by = 5),
  reps    = 3,
  # "true" parameters (edit freely)
  kon1 = 8e5,  koff1 = 2e-3,  Bmax1 = 120,
  kon2 = 5e4,  koff2 = 3e-4,  Bmax2 = 60,
  baseline = 2,
  # noise model
  sigma_add = 1.5,     # additive SD
  sigma_prop = 0.03    # proportional SD (relative)
) {
  # full grid + replicate id
  df <- expand.grid(
    conc_nM = conc_nM,
    time_s  = time_s,
    rep     = seq_len(reps)
  )
  df <- df[order(df$conc_nM, df$rep, df$time_s), , drop = FALSE]

  # noiseless mean
  mu <- binding_two(
    conc_nM = df$conc_nM, time_s = df$time_s,
    kon1 = kon1, koff1 = koff1, Bmax1 = Bmax1,
    kon2 = kon2, koff2 = koff2, Bmax2 = Bmax2,
    baseline = baseline
  )

  # heteroscedastic noise: SD = sqrt(sigma_add^2 + (sigma_prop * mu)^2)
  sd <- sqrt(sigma_add^2 + (sigma_prop * mu)^2)
  y_obs <- mu + rnorm(length(mu), mean = 0, sd = sd)

  df$y_true <- mu
  df$y <- y_obs

  attr(df, "true_params") <- list(
    kon1 = kon1, koff1 = koff1, Bmax1 = Bmax1,
    kon2 = kon2, koff2 = koff2, Bmax2 = Bmax2,
    baseline = baseline,
    sigma_add = sigma_add, sigma_prop = sigma_prop
  )

  df
}

# ---------- run + write ----------
df <- simulate_two_hot(
  conc_nM = c(0, 2, 5, 10, 25, 50, 100, 250, 500),
  time_s  = seq(0, 900, by = 10),
  reps    = 4,
  kon1 = 7e5,  koff1 = 1.5e-3, Bmax1 = 100,
  kon2 = 4e4,  koff2 = 4e-4,   Bmax2 = 55,
  baseline = 1.0,
  sigma_add = 1.2,
  sigma_prop = 0.04
)

write.csv(df, "./test_data/two_hot_binding_sim.csv", row.names = FALSE)
