library(tinytest)

mock_ggplot <- ggplot(data = CO2, aes(x = uptake, y = conc)) +
  geom_point() +
  geom_smooth()
test_annotateDF <- function() {
  df <- annotateDF(mock_ggplot, method = "lm")
  expect_equal(nrow(df), 84)
  expect_equal(ncol(df), 14)
}
test_annotateDF()

test_calcParams <- function() {
  df <- data.frame(x = 1:10, y = 1:10)
  model <- calcParams(df, formula = y ~ x, method = "lm")
  a <- model$annotation
  r2 <- strsplit(a, split = " ")[[1]][2]
  expect_equal(r2, "1")
}
test_calcParams()

test_dir()

