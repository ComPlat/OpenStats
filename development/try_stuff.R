extract_names_from_operations <- function(operation) {
  lang <- str2lang(operation)
  ga <- function(code) {
    if(!is.call(code)) return(code)
    code <- as.list(code)
    lapply(code, ga)
  }
  ast <- ga(lang)
  ast

}
operation1 <- "DataFrame(Mean(conc), SD(conc))"
operation2 <- "DataFrame(Mean(conc), SD(conc), Mean(uptake))"
operation3 <- "DataFrame(Mean(conc), DataFrame(C(1, 2, 3)))"
eval_env <- OpenStats:::create_run_env()
list2env(CO2, envir = eval_env)
eval_env[["df"]] <- "df"
new <- eval(parse(text = operation1), envir = eval_env)
new
new <- eval(parse(text = operation2), envir = eval_env)
new
new <- eval(parse(text = operation3), envir = eval_env)
new


library(tinyplot)

tinyplot(
  uptake ~ conc | Type, data = CO2,
  type = "boxplot", grid = TRUE,
  axis = "l", palette = "dark2",
  facet = ~Treatment, facet.args = list(free = TRUE),
  legend = list("top!", title = NULL),
  xlab = expression(CO[2]~"[ppm]"),
  ylab = expression(CO[2]~"uptake"~"["~µmol~"/("~cm^2~"sec)]"),
  xlim = c(0.5, 7.5),
  theme = tinytheme("default", mgp = c(2, 0.7, 0))
)

tinyplot(
  uptake ~ conc | Type, 
  facet = ~ Treatment,
  data = CO2,
  axis = "l", palette = "dark2",
  legend = list("top!", title = NULL),
  xlab = expression(CO[2]~"[ppm]"),
  ylab = expression(CO[2]~"uptake"~"["~µmol~"/("~cm^2~"sec)]"),
  xlim = c(0.5, 7.5),
  type = type_boxplot(boxwex = 0.3, staplewex = 0, outline = TRUE)
)
plt_add(type = "j")
