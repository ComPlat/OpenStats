.onLoad <- function(libname, pkgname) {
  VERSION <<- 1.2
  env_utils <<- get_env_utils(VERSION)
  env_operations <<- get_env_operations(VERSION)
  env_check_ast <<- get_env_check_ast(VERSION)
  env_diagnostic_plots <<- get_env_diagnostic_plots(VERSION)
  env_optim <<- get_env_optim(VERSION)
  env_plotting <<- get_env_plotting(VERSION)
  env_summarising_model <<- get_env_summarising_model(VERSION)
}
