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
