df <- read.csv("./test_data/DoseResponseData.csv", sep = ",")
head(df)
name_col <- "substance"
neg_control_name <- "neg"
dep <- "abs"
fit <- lm(abs ~ substance, data = df)
emm <- emmeans::emmeans(fit, "substance")
res <- emmeans::contrast(emm, method = "trt.vs.ctrl", ref = neg_control_name)
broom::tidy(res)
