VERSION <- 1.2
library(lmerTest)
library(ggplot2)

files <- list.files("./OpenStats/R", full.names = TRUE)
trash <- lapply(files, source)

response_var <- "uptake"
right_site <- "conc*Treatment*Type + (Treatment | Type)"
df <- CO2
cf <- get_create_formula()$new(response_var, right_site, df)
cf$validate()
rs <- OpenStats:::backend_result_state_V1_2$new(list(df = df))
ds <- OpenStats:::backend_data_model_state_V1_2$new(df)
model_latex <- cf$eval(rs, ds, "Linear Mixed Model")
