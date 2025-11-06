# nocov start getter
# getter for environments
# -----------------------------------------------------
get_env_check_ast <- function(version = VERSION) {
  list(
    `1.2` = env_check_ast_V1_2
  )[[version]]
}
get_env_diagnostic_plots <- function(version = VERSION) {
  list(
    `1.2` = env_diagnostic_plots_V1_2
  )[[version]]
}
get_env_import <- function(version = VERSION) {
  list(
    `1.2` = env_import_V1_2
  )[[version]]
}
get_env_lc50 <- function(version = VERSION) {
  list(
    `1.2` = env_lc_V1_2
  )[[version]]
}
get_env_operations <- function(version = VERSION) {
  list(
    `1.2` = env_operations_V1_2
  )[[version]]
}
get_env_optim <- function(version = VERSION) {
  list(
    `1.2` = env_optim_V1_2
  )[[version]]
}
get_env_plotting <- function(version = VERSION) {
  list(
    `1.2` = env_plotting_V1_2
  )[[version]]
}
get_env_summarising_model <- function(version = VERSION) {
  list(
    `1.2` = env_summarising_model_V1_2
  )[[version]]
}
get_env_utils <- function(version = VERSION) {
  list(
    `1.2` = env_utils_V1_2
  )[[version]]
}

# getter for engine classes
# -----------------------------------------------------
get_bg_process <- function(version = VERSION) {
  list(
    `1.2` = bg_process_V1_2
  )[[version]]
}
get_backend_result_state <- function(version = VERSION) {
  list(
    `1.2` = backend_result_state_V1_2
  )[[version]]
}
get_backend_data_model_state <- function(version = VERSION) {
  list(
    `1.2` = backend_data_model_state_V1_2
  )[[version]]
}
get_backend_data_wrangling_state <- function(version = VERSION) {
  list(
    `1.2` = backend_data_wrangling_state_V1_2
  )[[version]]
}
get_communicator <- function(version = VERSION) {
  list(
    `1.2` = communicator_V1_2
  )[[version]]
}
get_backend_communicator <- function(version = VERSION) {
  list(
    `1.2` = backend_communicator_V1_2
  )[[version]]
}
get_correlation <- function(version = VERSION) {
  list(
    `1.2` = correlation_V1_2
  )[[version]]
}
get_visualisation <- function(version = VERSION) {
  list(
    `1.2` = visualisation_V1_2
  )[[version]]
}
get_visualisation_model <- function(version = VERSION) {
  list(
    `1.2` = visualisation_model_V1_2
  )[[version]]
}
get_apply_filter <- function(version = VERSION) {
  list(
    `1.2` = apply_filter_V1_2
  )[[version]]
}
get_remove_filter <- function(version = VERSION) {
  list(
    `1.2` = remove_filter_V1_2
  )[[version]]
}
get_create_intermediate_var <- function(version = VERSION) {
  list(
    `1.2` = create_intermediate_var_V1_2
  )[[version]]
}
get_remove_intermediate_var <- function(version = VERSION) {
  list(
    `1.2` = remove_intermediate_var_V1_2
  )[[version]]
}
get_create_new_col <- function(version = VERSION) {
  list(
    `1.2` = create_new_col_V1_2
  )[[version]]
}
get_summarise_model <- function(version = VERSION) {
  list(
    `1.2` = summarise_model_V1_2
  )[[version]]
}
get_create_formula <- function(version = VERSION) {
  list(
    `1.2` = create_formula_V1_2
  )[[version]]
}
get_shapiro_on_data <- function(version = VERSION) {
  list(
    `1.2` = shapiro_on_data_V1_2
  )[[version]]
}
get_shapiro_on_residuals <- function(version = VERSION) {
  list(
    `1.2` = shapiro_on_residuals_V1_2
  )[[version]]
}
get_levene <- function(version = VERSION) {
  list(
    `1.2` = levene_V1_2
  )[[version]]
}
get_diagnostic_plot <- function(version = VERSION) {
  list(
    `1.2` = diagnostic_plots_V1_2
  )[[version]]
}
get_dose_response <- function(version = VERSION) {
  list(
    `1.2` = dose_response_V1_2
  )[[version]]
}
get_ttest <- function(version = VERSION) {
  list(
    `1.2` = t_test_V1_2
  )[[version]]
}
get_statistical_tests <- function(version = VERSION) {
  list(
    `1.2` = statistical_tests_V1_2
  )[[version]]
}
get_remove_results <- function(version = VERSION) {
  list(
    `1.2` = remove_result_V1_2
  )[[version]]
}
get_set_active_table <- function(version = VERSION) {
  list(
    `1.2` = set_active_table_V1_2
  )[[version]]
}
get_replay_history <- function(version = VERSION) {
  list(
    `1.2` = replay_history_V1_2
  )[[version]]
}
# nocov end getter
