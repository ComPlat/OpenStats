env_MIC_V1_2 <- new.env(parent = getNamespace("OpenStats"))

mic_internal <- function(df, conc_col, response_col, threshold) {
  stopifnot(
    is.data.frame(df),
    conc_col %in% names(df),
    response_col %in% names(df)
  )
  if (any(!df[[response_col]] %in% c(0, 1))) {
    stop("Response must be binary (0 = no growth, 1 = growth).")
  }

  concs <- sort(unique(df[[conc_col]]))
  means <- vapply(concs, function(cc) {
    mean(df[df[[conc_col]] == cc, response_col])
  }, numeric(1L))

  idx <- which(means <= threshold)[1L]
  if (is.na(idx)) {
    return(Inf)
  }
  concs[idx]
}
env_MIC_V1_2$mic_internal <- mic_internal

mic <- function(df, formula, threshold = 0) {
  f <- as.character(formula)
  response_col <- f[[2L]]
  conc_col <- f[[3L]]

  val <- env_MIC_V1_2$mic_internal(df, conc_col, response_col, threshold)
  res <- data.frame(MIC = val)
  names(res) <- paste0("MIC_", threshold)
  res
}
env_MIC_V1_2$mic <- mic

# Example 1: one sample, single observation per concentration.
df_one <- data.frame(
  conc = c(0.5, 1, 2, 4, 8, 16),
  response = c(1, 1, 1, 0, 0, 0)
)
print(env_MIC_V1_2$mic(df_one, response ~ conc))

# Example 2: one sample with technical replicates per concentration.
df_tech <- data.frame(
  conc = rep(c(0.5, 1, 2, 4, 8, 16), each = 3),
  response = c(
    1, 1, 1,
    1, 1, 1,
    1, 1, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0
  )
)
print(env_MIC_V1_2$mic(df_tech, response ~ conc, threshold = 0))

# Example 3: no concentration fully inhibits -> MIC = Inf.
df_no_inhib <- data.frame(
  conc = c(0.5, 1, 2, 4),
  response = c(1, 1, 1, 1)
)
print(env_MIC_V1_2$mic(df_no_inhib, response ~ conc))
