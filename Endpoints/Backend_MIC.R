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

  agg <- Reduce(rbind, tapply(df, df[[conc_col]], function(block) {
    m <- mean(block[[response_col]])
    c <- unique(block[[conc_col]])
    data.frame(conc = c, response = m)  
  }))
  agg <- agg[order(agg$conc), , drop = FALSE]
  idx <- which(agg$response <= threshold)[1L]
  if (is.na(idx)) {
    return(Inf)
  }
  MIC = agg$conc[idx]
}
env_MIC_V1_2$mic_internal <- mic_internal

mic <- function(df, formula, threshold) {
  f <- as.character(formula)
  response_col <- f[[2L]]
  conc_col <- f[[3L]]

  val <- env_MIC_V1_2$mic_internal(df, conc_col, response_col, threshold)
  res <- data.frame(MIC = val)
  names(res) <- paste0("MIC_", threshold)
  return(res)
}
env_MIC_V1_2$mic<- mic

set.seed(42)
df <- data.frame(
  conc = rep(c(0.5, 1, 2, 4, 8, 16), each = 5),
  response = c(
    rbinom(5, 1, 1.0),  # full growth
    rbinom(5, 1, 0.9),
    rbinom(5, 1, 0.7),
    rbinom(5, 1, 0.4),
    rbinom(5, 1, 0.2),
    rbinom(5, 1, 0.0)   # full inhibition
  )
)
env_MIC_V1_2$mic(df, response ~ conc, 0.5)
