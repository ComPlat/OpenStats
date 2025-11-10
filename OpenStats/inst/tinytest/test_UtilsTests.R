library(OpenStats)
library(ggplot2)
library(tinytest)
library(readxl)

# ---- Test env_utils_V1_2$split_formula ----
f <- y ~ x1 + x2
res <- OpenStats:::split_formula(f)
expect_identical(res$response, quote(y))
expect_identical(res$right_site, quote(x1 + x2))
# ---- Test env_utils_V1_2$vars_rhs ----
rhs_vars <- OpenStats:::vars_rhs(res$right_site)
expect_identical(rhs_vars, c("x1", "x2"))

# correct names
# =======================================================================================
test_correctName_true_when_column_exists <- function() {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_true(OpenStats:::correct_name("a", df))
  expect_true(OpenStats:::correct_name("b", df))
}
test_correctName_true_when_column_exists()

test_correctName_false_when_column_missing <- function() {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_false(OpenStats:::correct_name("c", df))
  expect_false(OpenStats:::correct_name("", df))
}
test_correctName_false_when_column_missing()

test_correctName_empty_df <- function() {
  df <- data.frame()
  expect_false(OpenStats:::correct_name("a", df))
}
test_correctName_empty_df()

# change char input
# =======================================================================================
test_changeCharInput_strips_spaces_and_splits <- function() {
  x <- "a, b ,  c"
  out <- OpenStats:::change_char_input(x)
  expect_equal(out, c("a", "b", "c"))
}
test_changeCharInput_strips_spaces_and_splits()

test_changeCharInput_single_value <- function() {
  x <- "column1"
  out <- OpenStats:::change_char_input(x)
  expect_equal(out, "column1")
}
test_changeCharInput_single_value()

# test env_utils_V1_2$combine
# =======================================================================================
test_combine_single_existing_column <- function() {
  df <- data.frame(a = c("x", "y"), b = c("u", "v"), stringsAsFactors = TRUE)
  res <- OpenStats:::combine(new = NULL, vec = c("a"), df = df, first = TRUE)
  expect_true(is.factor(res) || is.character(res))
  expect_equal(as.character(res), as.character(df$a))
}
test_combine_single_existing_column()

test_combine_two_columns_order_from_end <- function() {
  df <- data.frame(
    a = c("x", "y"),
    b = c("u", "v"),
    stringsAsFactors = TRUE
  )
  res <- OpenStats:::combine(new = NULL, vec = c("a", "b"), df = df, first = TRUE)

  expect_true(is.factor(res))
  expect_equal(length(res), nrow(df))

  expect_equal(as.character(res)[1], "u.x")
  expect_equal(as.character(res)[2], "v.y")
}
test_combine_two_columns_order_from_end()

test_combine_skips_missing_columns <- function() {
  df <- data.frame(
    a = c("x", "y"),
    b = c("u", "v"),
    stringsAsFactors = TRUE
  )
  res <- OpenStats:::combine(new = NULL, vec = c("a", "c"), df = df, first = TRUE)

  expect_equal(as.character(res), as.character(df$a))
}
test_combine_skips_missing_columns()

test_combine_empty_vec_returns_new <- function() {
  df <- data.frame(a = 1:3)
  res <- OpenStats:::combine(new = "start", vec = character(), df = df, first = TRUE)
  expect_equal(res, "start")
}
test_combine_empty_vec_returns_new()

# conversions
# =======================================================================================
test_num_to_factor_converts_only_numeric <- function() {
  df <- data.frame(
    a = 1:3,
    b = c("x", "y", "z"),
    c = c(TRUE, FALSE, TRUE)
  )

  out <- OpenStats:::num_to_factor(df, cols = c("a", "b", "c"))

  expect_true(is.factor(out$a))
  expect_equal(as.numeric(as.character(out$a)), df$a)

  expect_true(is.character(out$b))
  expect_identical(out$b, df$b)

  expect_true(is.logical(out$c))
  expect_identical(out$c, df$c)
}
test_num_to_factor_converts_only_numeric()

test_num_to_factor_noop_when_no_numeric <- function() {
  df <- data.frame(x = c("1","2"), y = c("a","b"))
  out <- OpenStats:::num_to_factor(df, cols = c("x","y"))
  expect_identical(out, df)
}
test_num_to_factor_noop_when_no_numeric()

test_char_to_orig_type_numeric_strings_to_numeric <- function() {
  x <- c("1", "2", "3")
  out <- suppressWarnings(OpenStats:::char_to_orig_type(x))
  expect_true(is.numeric(out))
  expect_equal(out, c(1, 2, 3))
}
test_char_to_orig_type_numeric_strings_to_numeric()

test_char_to_orig_type_mixed_strings_remain_character <- function() {
  x <- c("1", "two", "3")
  out <- suppressWarnings(OpenStats:::char_to_orig_type(x))
  expect_true(is.character(out))
  expect_identical(out, x)
}
test_char_to_orig_type_mixed_strings_remain_character()

test_char_to_orig_type_handles_list_input <- function() {
  x <- list("4", "5", "6")
  out <- suppressWarnings(OpenStats:::char_to_orig_type(x))
  expect_true(is.numeric(out))
  expect_equal(out, c(4, 5, 6))
}
test_char_to_orig_type_handles_list_input()

test_char_to_orig_type_list_with_non_numeric_stays_char <- function() {
  x <- list("7", "x")
  out <- suppressWarnings(OpenStats:::char_to_orig_type(x))
  expect_true(is.character(out))
  expect_identical(out, c("7", "x"))
}
test_char_to_orig_type_list_with_non_numeric_stays_char()

test_firstup_basic <- function() {
  x <- c("alpha", "bETA")
  out <- OpenStats:::firstup(x)
  expect_equal(out, c("Alpha", "BETA"))  # only first letter changes
}
test_firstup_basic()

test_firstup_empty_and_single_char <- function() {
  x <- c("", "q")
  out <- OpenStats:::firstup(x)
  expect_equal(out, c("", "Q"))
}
test_firstup_empty_and_single_char()

# Test elongate col
# =======================================================================================
test_elongate_col <- function() {
  # Case 1: l is a multiple of the length of col
  col <- c(1, 2, 3)
  l <- 6
  expected_result <- c(1, 2, 3, 1, 2, 3)
  result <- OpenStats:::elongate_col(col, l)
  expect_equal(result, expected_result, 
               info = "Should repeat col exactly 2 times when l = 6"
  )
  # Case 2: l is not a multiple of the length of col
  col <- c(1, 2, 3)
  l <- 8
  expected_result <- c(1, 2, 3, 1, 2, 3, 1, 2)
  result <- OpenStats:::elongate_col(col, l)
  expect_equal(result, expected_result, 
               info = "Should repeat col 2 times and append first 2 elements when l = 8"
  )
  # Case 3: l is smaller than the length of col
  col <- c(1, 2, 3)
  l <- 2
  expected_result <- c(1, 2)
  result <- OpenStats:::elongate_col(col, l)
  expect_equal(result, expected_result, 
               info = "Should truncate col to fit length l when l = 2"
  )
  # Case 4: l is equal to the length of col
  col <- c(1, 2, 3)
  l <- 3
  expected_result <- c(1, 2, 3)
  result <- OpenStats:::elongate_col(col, l)
  expect_equal(result, expected_result, 
               info = "Should return col as is when l equals length of col"
  )
  # Case 5: l is a very large number compared to length of col
  col <- c(1, 2)
  l <- 10001
  expected_result <- rep(c(1, 2), length.out = 10001)
  result <- OpenStats:::elongate_col(col, l)
  expect_equal(
    result, expected_result, 
               info = "Should repeat col enough times to match the requested length (10001)"
  )
}
test_elongate_col()

# Test check result list size = rls
# =======================================================================================
# Create a dummy object to represent a "new object" to be checked
create_dummy_object <- function(size_in_mb) {
  return(1:ceiling(size_in_mb * 1024^2 / 8))
}

# Test for env_utils_V1_2$check_rls function
test_check_rls <- function() {
  # Case 1: List of results exceeds 1000 entries
  list_results_1001 <- lapply(1:1001, function(x) {rep(100, x)})
  new_obj <- 1:100
  expect_error(OpenStats:::check_rls(list_results_1001, new_obj), 
               "You can only store 1000 results. Consider removing some results",
               info = "Should stop when there are more than 1000 results"
  )
  # Case 2: The total size exceeds 500MB
  list_results_large <- list(create_dummy_object(500), create_dummy_object(250))
  new_obj_large <- create_dummy_object(300)
  object.size(list_results_large)
  expect_error(OpenStats:::check_rls(list_results_large, new_obj_large),
               "Memory limit exceeded for user results. Consider removing some results.",
               info = "Should stop when total memory exceeds 500MB"
  )
  # Case 3: The total size does not exceed 500MB and number of results is below 1000
  list_results_valid <- list(create_dummy_object(100), create_dummy_object(100))
  new_obj_valid <- create_dummy_object(100)
  expect_silent(OpenStats:::check_rls(list_results_valid, new_obj_valid), 
                info = "Should pass when results are below 1000 and total size is within 500MB"
  )
  # Case 4: Exactly 1000 results and total size within limit
  list_results_1000 <- rep(list(1:100), 10)
  new_obj_1000 <- 1:100
  expect_silent(OpenStats:::check_rls(list_results_1000, new_obj_1000),
                info = "Should pass with exactly 1000 results and total size within limit"
  )
}
test_check_rls()
# Test for env_utils_V1_2$create_plot_pages function
# =======================================================================================
test_create_plot_pages <- function() {
  # Create mock plot list (using a simple empty plot for testing)
  plot_list <- list(ggplot2::ggplot() + ggplot2::geom_point())
  # Test: fewer than 9 plots (e.g., 1 plot)
  result <- OpenStats:::create_plot_pages(plot_list)
  expect_equal(
    length(result), 1, info = "Should return 1 page for a single plot"
  )
  # Test: exactly 9 plots
  plot_list_9 <- rep(plot_list, 9)
  result <- OpenStats:::create_plot_pages(plot_list_9)
  expect_equal(
    length(result), 2, info = "Should return 2 page for exactly 9 plots"
  )
  # Test: exactly 18 plots
  plot_list_18 <- rep(plot_list, 18)
  result <- OpenStats:::create_plot_pages(plot_list_18)
  expect_equal(
    length(result), 3, info = "Should return 3 pages for exactly 18 plots"
  )
  # Test: more than 9 but not an exact multiple (e.g., 10 plots)
  plot_list_10 <- rep(plot_list, 10)
  result <- OpenStats:::create_plot_pages(plot_list_10)
  expect_equal(
    length(result), 2, info = "Should return 2 pages for 10 plots (1st with 9, 2nd with 1)"
  )
  # Test: edge case for 17 plots (last page should have 8 plots)
  plot_list_17 <- rep(plot_list, 17)
  result <- OpenStats:::create_plot_pages(plot_list_17)
  expect_equal(
    length(result), 2, info = "Should return 2 pages for 17 plots (1st with 9, 2nd with 8)"
  )
  # Test: no plots (empty list)
  plot_list_empty <- list()
  result <- OpenStats:::create_plot_pages(plot_list_empty)
  expect_equal(
    length(result), 1, info = "Should return 1 page for an empty plot list (empty grid)"
  )
  # Test: number of plots is exactly a multiple of 9 (e.g., 27)
  plot_list_27 <- rep(plot_list, 27)
  result <- OpenStats:::create_plot_pages(plot_list_27)
  expect_equal(
    length(result), 4, info = "Should return 4 pages for 27 plots (9 plots per page)"
  )
}
test_create_plot_pages()

# Test checks for filename
# =======================================================================================
test_is_valid_filename <- function() {
  # Valid filename
  expect_true(
    OpenStats:::is_valid_filename("valid_filename.txt"), info = "Should return TRUE for valid filename"
  )
  # Filename with spaces
  expect_false(
    OpenStats:::is_valid_filename("invalid filename.txt"), info = "Should return FALSE for filename with spaces"
  )
  # Filename with invalid characters
  expect_false(
    OpenStats:::is_valid_filename("invalid|filename.txt"), info = "Should return FALSE for filename with invalid characters"
  )
  # Empty filename
  expect_false(
    OpenStats:::is_valid_filename(""), info = "Should return FALSE for empty filename"
  )
  # Filename too long
  expect_false(
    OpenStats:::is_valid_filename(strrep("a", 101)), info = "Should return FALSE for filename longer than 100 characters"
  )
  # Filename with no extension
  expect_false(
    OpenStats:::is_valid_filename("file_without_extension"), info = "Should return FALSE for filename with no extension"
  )
}
test_is_valid_filename()

test_why_filename_invalid <- function() {
  # Valid filename
  expect_equal(
    OpenStats:::why_filename_invalid("valid_filename.txt"), "", info = "Should return empty string for valid filename"
  )
  # Filename with spaces
  expect_equal(
    OpenStats:::why_filename_invalid("invalid filename.txt"), "Found spaces in filename", info = "Should return error message for spaces in filename"
  )
  # Filename with invalid characters
  expect_equal(
    OpenStats:::why_filename_invalid("invalid|filename.txt"), "Found invalid chars in filename: [<>:\"\\|?*", info = "Should return error message for invalid characters in filename"
  )
  # Empty filename
  expect_equal(
    OpenStats:::why_filename_invalid(""), "Filename is empty", info = "Should return error message for empty filename"
  )
  # Filename too long
  expect_equal(
    OpenStats:::why_filename_invalid(strrep("a", 101)), "Filename is too long (> 100 characters)", info = "Should return error message for filename longer than 100 characters"
  )
  # Filename with no extension
  expect_equal(
    OpenStats:::why_filename_invalid("file_without_extension"), "Filename extension is missing", info = "Should return error message for filename with no extension"
  )
}
test_why_filename_invalid()

test_check_filename_for_server <- function() {
  # Valid xlsx file
  expect_true(
    OpenStats:::check_filename_for_server("data/file.xlsx"), info = "Should return TRUE for filename with .xlsx extension"
  )
  # Invalid file extension
  expect_false(
    OpenStats:::check_filename_for_server("data/file.csv"), info = "Should return FALSE for filename with .csv extension"
  )
  # No extension
  expect_false(
    OpenStats:::check_filename_for_server("data/file"), info = "Should return FALSE for filename with no extension"
  )
  # Invalid extension
  expect_false(
    OpenStats:::check_filename_for_server("data/file.txt"), info = "Should return FALSE for filename with .txt extension"
  )
}
test_check_filename_for_server()

test_check_filename_for_serverless <- function() {
  # Valid zip file
  expect_true(
    OpenStats:::check_filename_for_serverless("data/file.zip"), info = "Should return TRUE for filename with .zip extension"
  )
  # Invalid file extension
  expect_false(
    OpenStats:::check_filename_for_serverless("data/file.xlsx"), info = "Should return FALSE for filename with .xlsx extension"
  )
  # No extension
  expect_false(
    OpenStats:::check_filename_for_serverless("data/file"), info = "Should return FALSE for filename with no extension"
  )
  # Invalid extension
  expect_false(
    OpenStats:::check_filename_for_serverless("data/file.tar"), info = "Should return FALSE for filename with .tar extension"
  )
}
test_check_filename_for_serverless()

# Test extract_extension
# =======================================================================================
test_extract_extension <- function() {
  # Test with a filename that has a valid extension
  expect_equal(
    OpenStats:::extract_extension("data/file.txt"), "txt", info = "Should return 'txt' for 'file.txt'"
  )
  # Test with a filename that has multiple extensions
  expect_equal(
    OpenStats:::extract_extension("archive.tar.gz"), "gz", info = "Should return 'gz' for 'archive.tar.gz'"
  )
  # Test with a filename that has no extension
  expect_equal(
    OpenStats:::extract_extension("file_without_extension"), "file_without_extension", info = "Should return the full name if there is no extension"
  )
  # Test with a filename that has a hidden file extension (e.g., dot before the file name)
  expect_equal(
    OpenStats:::extract_extension(".hiddenfile"), "hiddenfile", info = "Should return 'hiddenfile' for a filename starting with a dot"
  )
}
test_extract_extension()

# Test check formula
# =======================================================================================
test_check_formula <- function() {
  # Test with a valid formula: response ~ predictor
  valid_formula <- as.formula("y ~ x")
  expect_true(OpenStats:::check_formula(valid_formula), 
              info = "OpenStats:::check_formula should return TRUE for a valid formula")

  # Test with a formula with more than two terms
  invalid_formula_3_terms <- as.formula("y ~ x + z")
  expect_error(OpenStats:::check_formula(invalid_formula_3_terms), 
               "Formula must have exactly two terms: response ~ predictor", 
               info = "OpenStats:::check_formula should throw an error for a formula with more than two terms")

  # Test with a formula with fewer than two terms
  invalid_formula_1_term <- as.formula("y ~ 1")
  expect_error(OpenStats:::check_formula(invalid_formula_1_term), 
               "Formula must have exactly two terms: response ~ predictor", 
               info = "OpenStats:::check_formula should throw an error for a formula with fewer than two terms")

  # Test with a non-formula input (e.g., a character vector)
  invalid_non_formula <- "y ~ x"
  expect_error(OpenStats:::check_formula(invalid_non_formula), 
               "Input must be a formula of the type response ~ predictor", 
               info = "OpenStats:::check_formula should throw an error for a non-formula input")
}
test_check_formula()

# Test check length code
# =======================================================================================
test_check_length_code <- function() {
  # Test with a valid code length (less than 4000 characters)
  valid_code <- paste(rep("a", 3999), collapse = "")
  expect_null(OpenStats:::check_length_code(valid_code), 
              info = "OpenStats:::check_length_code should return NULL for code with less than 4000 characters")

  # Test with a code length exactly 4000 characters
  valid_code_4000 <- paste(rep("a", 4000), collapse = "")
  expect_null(OpenStats:::check_length_code(valid_code_4000), 
              info = "OpenStats:::check_length_code should return NULL for code with exactly 4000 characters")

  # Test with a code length greater than 4000 characters
  invalid_code <- paste(rep("a", 4001), collapse = "")
  expect_error(OpenStats:::check_length_code(invalid_code), 
               "The code is too long to be evaluated", 
               info = "OpenStats:::check_length_code should throw an error for code longer than 4000 characters")
  
  # Test with an empty code (length 0)
  empty_code <- ""
  expect_null(OpenStats:::check_length_code(empty_code), 
              info = "OpenStats:::check_length_code should return NULL for an empty code")

}
test_check_length_code()

# Test check type result
# =======================================================================================
test_check_type_res <- function() {
  # Test with allowed types
  # Numeric type
  expect_null(OpenStats:::check_type_res(1),
              info = "OpenStats:::check_type_res should return NULL for numeric input")

  # Integer type
  expect_null(OpenStats:::check_type_res(1L),
              info = "OpenStats:::check_type_res should return NULL for integer input")

  # Factor type
  expect_null(OpenStats:::check_type_res(factor("a")),
              info = "OpenStats:::check_type_res should return NULL for factor input")

  # Logical type
  expect_null(OpenStats:::check_type_res(TRUE),
              info = "OpenStats:::check_type_res should return NULL for logical input")

  # Character type
  expect_null(OpenStats:::check_type_res("text"),
              info = "OpenStats:::check_type_res should return NULL for character input")

  # Data frame type
  expect_null(OpenStats:::check_type_res(data.frame(a = 1, b = 2)),
              info = "OpenStats:::check_type_res should return NULL for data.frame input")

  # Test with disallowed type
  # List type (not allowed)
  expect_error(OpenStats:::check_type_res(list(1, 2, 3)),
               "Found result with unallowed type: list", 
               info = "OpenStats:::check_type_res should throw error for list type")

  # Function type (not allowed)
  expect_error(OpenStats:::check_type_res(function(x) x),
               "Found result with unallowed type: function", 
               info = "OpenStats:::check_type_res should throw error for function type")

  # Date type (not allowed)
  expect_error(OpenStats:::check_type_res(as.Date("2024-01-01")), 
               "Found result with unallowed type: Date", 
               info = "check_type_res should throw error for Date type")
}
test_check_type_res()

# Test check axis limits
# =======================================================================================
test_check_axis_limits <- function() {
  # Test with valid numeric axis limits
  col_numeric <- c(1, 2, 3, 4, 5)
  expect_null(OpenStats:::check_axis_limits(col_numeric, 1, 4),
              info = "OpenStats:::check_axis_limits should return NULL for valid numeric limits")

  # Test with invalid numeric axis limits (max <= min)
  expect_error(OpenStats:::check_axis_limits(col_numeric, 4, 1),
               "Found invalid axis limits: max <= min",
               info = "OpenStats:::check_axis_limits should throw error when max <= min")

  # Test with invalid numeric axis limits (non-numeric min or max)
  expect_error(OpenStats:::check_axis_limits(col_numeric, "a", 4),
               "Found invalid axis limits",
               info = "OpenStats:::check_axis_limits should throw error for non-numeric min")

  expect_error(OpenStats:::check_axis_limits(col_numeric, 1, "b"),
               "Found invalid axis limits",
               info = "OpenStats:::check_axis_limits should throw error for non-numeric max")

  # Test with valid categorical axis limits
  col_factor <- factor(c("low", "medium", "high"))
  expect_null(OpenStats:::check_axis_limits(col_factor, "low", "medium"),
              info = "OpenStats:::check_axis_limits should return NULL for valid factor levels")

  # Test with invalid categorical axis limits (min or max not in factor levels)
  expect_error(OpenStats:::check_axis_limits(col_factor, "low", "very_high"),
               "Found invalid axis limits",
               info = "OpenStats:::check_axis_limits should throw error when min or max not in factor levels")

  # Test with invalid categorical axis limits (max appears before min)
  expect_error(OpenStats:::check_axis_limits(col_factor, "medium", "low"),
               "Found invalid axis limits. The max value is found before the min value",
               info = "OpenStats:::check_axis_limits should throw error when max appears before min in factor levels")
 
  # Test with a single value in the column
  col_single <- factor("only_one_value")
  expect_null(OpenStats:::check_axis_limits(col_single, "only_one_value", "only_one_value"),
              info = "OpenStats:::check_axis_limits should return NULL for a single value column when min = max")
}
test_check_axis_limits()

# Test split
# =======================================================================================
test_split <- function() {
  # Test a dataframe with multiple columns
  df <- data.frame(
    group = c("A", "B", "A", "C", "B"),
    category = c("X", "Y", "X", "Z", "Y"),
    value = c(1, 2, 3, 4, 5)
  )
 
  # Test case where splitting on one column works correctly
  result <- OpenStats:::split_groups(df, cols = c("group"), levels = c("A", "B"))
  expect_equal(
    result$group,
    c("A", "B", "A", "B"),
    info = "split should work for a single column correctly"
  )

  # Test case where splitting on two columns works correctly
  result <- OpenStats:::split_groups(df, cols = c("group"), levels = c("A", "B"))
  expect_equal(
    result$category,
    c("X", "Y", "X", "Y"),
    info = "split should work for two columns correctly"
  )

  # Test when the dataframe has no matching rows
  result <- tryCatch({
    OpenStats:::split_groups(df, cols = c("group"), levels = c("D"))
  }, error = function(e) e)
  expect_true(
    inherits(result, "error"),
    info = "split should throw an error if no rows match the levels"
  )

  # Test case where all rows are included (no subset)
  result <- OpenStats:::split_groups(df, cols = c("group", "category"), levels = c("A", "B", "C", "X", "Y", "Z"))
  expect_equal(
    nrow(result),
    5,
    info = "split should return all rows if all levels are included"
  )

  # Edge case with empty dataframe
  df_empty <- data.frame(group = character(0), category = character(0), value = numeric(0))
  result <- tryCatch({
    OpenStats:::split_groups(df_empty, cols = c("group"), levels = c("A"))
  }, error = function(e) e)
  expect_true(
    inherits(result, "error"),
    info = "split should throw an error if the dataframe is empty"
  )

  # Edge case where no level is set
  result <- tryCatch({
    OpenStats:::split_groups(df, cols = c("group", "category"), levels = c())
  }, error = function(e) e)
  expect_true(
    inherits(result, "error"),
    info = "split should throw an error if the dataframe is empty"
  )
}
test_split()

# Test create r names
# =======================================================================================
test_create_r_names <- function() {
  # Test valid input with no invalid names
  df <- data.frame(`valid_name` = 1:5, `another_valid_name` = 6:10)
  result <- OpenStats:::create_r_names(df)
  expect_equal(
    names(result),
    c("valid_name", "another_valid_name"),
    info = "OpenStats:::create_r_names should leave valid names unchanged"
  )

  # Test input with spaces in column names
  df <- data.frame(`invalid name` = 1:5, `another name` = 6:10)
  result <- OpenStats:::create_r_names(df)
  expect_equal(
    names(result),
    c("invalid.name", "another.name"),
    info = "OpenStats:::create_r_names should replace spaces with dots"
  )

  # Test input with special characters
  df <- data.frame(`$pecial@name` = 1:5, `123start_with_number` = 6:10)
  result <- OpenStats:::create_r_names(df)
  expect_equal(
    names(result),
    c("X.pecial.name", "X123start_with_number"),
    info = "OpenStats:::create_r_names should sanitize names with special characters and numeric starts"
  )

  # Test input with reserved R keywords
  df <- data.frame(`if` = 1:5, `else` = 6:10, `TRUE` = 11:15)
  result <- OpenStats:::create_r_names(df)
  expect_equal(
    names(result),
    c("if.", "else.", "TRUE."),
    info = "OpenStats:::create_r_names should leave reserved keywords unchanged"
  )

  # Edge case: empty data frame
  df <- data.frame()
  result <- OpenStats:::create_r_names(df)
  expect_equal(
    names(result),
    character(0),
    info = "OpenStats:::create_r_names should handle empty data frames without error"
  )
}
test_create_r_names()

# Test create df names
# =======================================================================================
test_create_df_name <- function() {
  # Test valid cases
  column_names <- c("df1", "df2", "data")
 
  # Test when the name is already unique
  result_unique <- OpenStats:::create_df_name("df3", column_names)
  expect_equal(
    result_unique,
    "df3",
    info = "OpenStats:::create_df_name should return the name as is when it is not in column_names"
  )

  # Test when the name conflicts and needs adjustment
  result_conflict <- OpenStats:::create_df_name("df1", column_names)
  expect_equal(
    result_conflict,
    "df11",
    info = "OpenStats:::create_df_name should return a modified name when the input conflicts"
  )

  result_conflict_multiple <- OpenStats:::create_df_name("df", c("df", "df1", "df12", "df123"))
  expect_equal(
    result_conflict_multiple,
    "df1234",
    info = "OpenStats:::create_df_name should return a name that avoids all conflicts"
  )

  # Edge cases
  result_empty <- OpenStats:::create_df_name("new_df", character(0))
  expect_equal(
    result_empty,
    "new_df",
    info = "OpenStats:::create_df_name should return the name as is when column_names is empty"
  )

  # Test a case with a very large number of conflicts
  result_large_conflict <- OpenStats:::create_df_name(
    "test",
    c("test", paste0("test", 1:1000))
  )
  expect_equal(
    result_large_conflict,
    "test1234",
    info = "OpenStats:::create_df_name should correctly handle a large number of conflicts"
  )
}
test_create_df_name()

# Test env_utils_V1_2$split_data
# =======================================================================================
test_splitData <- function() {
  # Test data
  df <- data.frame(
    value = c(10, 20, 30, 40, 50),
    group1 = c("A", "A", "B", "B", "C"),
    group2 = c("X", "Y", "X", "Y", "X")
  )

  # Basic case
  result <- OpenStats:::split_data(df, value ~ group1 + group2)

  # Check the structure of the result
  expect_equal(
    colnames(result),
    c("value", "interaction"),
    info = "Result should have columns: 'value' and 'interaction'"
  )
  expect_equal(
    nrow(result),
    nrow(df),
    info = "Result should have the same number of rows as the input data frame"
  )
  expect_equal(
    ncol(result),
    2,
    info = "Two columns are expected"
  )

  # Check the interaction column
  model_frame <- model.frame(value ~ group1 + group2, data = df)
  expected_interaction <- interaction(model_frame$group1, model_frame$group2)
  expect_equal(
    result$interaction,
    expected_interaction,
    info = "Interaction column should correctly reflect the interaction of group1 and group2"
  )

  # Check the value column
  expect_equal(
    result$value,
    df$value,
    info = "Value column should be identical to the response variable in the formula"
  )

  # Edge case: Single predictor variable
  result_single <- OpenStats:::split_data(df, value ~ group1)
  expected_interaction_single <- interaction(df$group1)
  expect_equal(
    result_single$interaction,
    expected_interaction_single,
    info = "Interaction column should work correctly for a single predictor variable"
  )

  # Edge case: Invalid formula
  expect_error(
    OpenStats:::split_data(df, value ~ non_existent_column),
    info = "Function should throw an error for a formula with non-existent variables"
  )

  # Edge case: Less than 2 columns after model.frame
  df_min <- data.frame(value = c(1, 2, 3))
  expect_error(
    OpenStats:::split_data(df_min, value ~ .),
    info = "Function should throw an error when there are fewer than 2 columns in the formula output"
  )

  # Edge case: 
  df_min <- data.frame(value = runif(6), rep(c("A", "B"), each = 3))
  expect_error(
    OpenStats:::split_data(df_min, value ~ 1),
    info = "Function should throw an error when there are fewer than 2 columns in the formula output"
  )
}
test_splitData()

# Test stack and unstack DF
# =======================================================================================
test_stack_unstackDF <- function() {
  # Test data
  df <- data.frame(
    ID = c(1, 2, 3),
    A = c(10, 20, 30),
    B = c(40, 50, 60)
  )

  # Test env_utils_V1_2$stack_df
  stacked <- OpenStats:::stack_df(df, keepCol = "ID")

  # Check structure of stacked data frame
  expect_equal(
    colnames(stacked),
    c("ID", "name", "value"),
    info = "stackDF should produce columns: ID, name, value"
  )
  expect_equal(
    nrow(stacked),
    6,
    info = "stackDF should have rows equal to original non-ID columns * number of rows"
  )
  expect_equal(
    unique(stacked$name),
    c("A", "B"),
    info = "stackDF should properly stack column names into 'name' column"
  )
  expect_equal(
    stacked$value[stacked$name == "A"],
    df$A,
    info = "stackDF should retain values correctly for column A"
  )

  # Test unenv_utils_V1_2$stack_df
  unstacked <- OpenStats:::unstack_df(stacked, name = "name", value = "value")

  # Check structure of unstacked data frame
  expect_equal(
    colnames(unstacked),
    colnames(df),
    info = "unstackDF should reconstruct the original column names"
  )
  expect_equal(
    unstacked$A,
    df$A,
    info = "unstackDF should correctly reconstruct column A"
  )
  expect_equal(
    unstacked$B,
    df$B,
    info = "unstackDF should correctly reconstruct column B"
  )
  expect_equal(
    unstacked$ID,
    df$ID,
    info = "unstackDF should preserve the ID column"
  )
}
test_stack_unstackDF()

# Test env_utils_V1_2$create_js_string
# =======================================================================================
test_create_js_string <- function() {
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  if (!identical(Sys.getenv("RUN_UI_TESTS"), "true")) return()
  # 1. Create a plot object
  p <- ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
    geom_boxplot()
  plot_obj <- new("plot", p = p, width = 10, height = 10, resolution = 600)

  # 2. Create a diagnostic plot object
  diag_fn <- tempfile(fileext = ".png")
  ggsave(plot = p, filename = diag_fn, width = 10, height = 10, dpi = 600)
  diag_obj <- new("diagnosticPlot", p = diag_fn)

  # 3. Create a dose response object
  dose_obj <- new("doseResponse", df = iris, p = list(p, p))

  # 4. Data frame and character
  df <- iris
  char_obj <- "This is a test string"

  # Combine into list
  l <- list(plot_obj, diag_obj, dose_obj, df, char_obj)

  # Call the function
  result <- OpenStats:::create_js_string(l)

  # Validate the result
  # Check structure and length
  expect_equal(
    length(result[[1]]), 7,
    info = "Result should include encoded strings for each element"
  )

  # Check for base64-encoded strings
  expect_true(
    grepl("^data:image/png;base64,", result[[1]][[1]]),
    info = "First element should be a base64-encoded image"
  )
  expect_true(
    grepl("^data:image/png;base64,", result[[1]][[2]]),
    info = "Second element should be a base64-encoded diagnostic plot"
  )
  expect_true(
    grepl("^data:image/png;base64,", result[[1]][[3]]),
    info = "Dose response plot should be base64-encoded"
  )
  expect_true(
    grepl("^Sepal.Length", result[[1]][[5]]),
    info = "Data frame should be converted to string format"
  )
  expect_equal(
    result[[1]][[7]], char_obj,
    info = "Character string should remain as is"
  )

  # Cleanup
  unlink(diag_fn)
}
test_create_js_string()

# Test env_utils_V1_2$create_excel_file
# =======================================================================================
test_create_excel_file <- function() {
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  if (!identical(Sys.getenv("RUN_UI_TESTS"), "true")) return()
  p <- ggplot(
    data = iris,
    aes(x = Species, y = Sepal.Length)
  ) +
    geom_boxplot()
  p <- new("plot", p = p, width = 10, height = 10, resolution = 600)
  l <- list(p, iris)
  file <- OpenStats:::create_excel_file(l)

  # File existence
  expect_true(
    !is.null(file), "File should not be NULL"
  )

  # Check workbook structure
  wb <- openxlsx::loadWorkbook(file)
  sheets <- openxlsx::getSheetNames(file)
  expect_true(
    "Results" %in% sheets, "Sheet 'Results' should exist"
  )

  # Check data content
  data <- openxlsx::read.xlsx(file, sheet = "Results", colNames = TRUE, startRow = 21)
  expect_equal(
    colnames(data), colnames(iris),
    info = "Data column names should match"
  )

  # Check plot presence
  temp_files <- dir(tempdir(), pattern = "\\.png$")
  expect_true(
    length(temp_files) > 0, "At least one temporary plot file should exist"
  )

  # Cleanup
  file.remove(file)
  unlink(temp_files)
}
test_create_excel_file()

# Test env_utils_V1_2$df_2_string
# =======================================================================================
test_df_2_string <- function() {
  # Normal data frame
  df <- data.frame(Col1 = 1:5, Col2 = letters[1:5])
  str <- OpenStats:::df_2_string(df)
  f <- tempfile(fileext = ".txt")
  writeLines(str, f)
  read.csv(f, sep = "\t") |>
    expect_equal(df)

  # Not a dataframe
  e <- try(OpenStats:::df_2_string("Invalid"), silent = TRUE)
  expect_true(inherits(e, "try-error"))

  # EMPTY data frame
  df <- data.frame()
  str <- OpenStats:::df_2_string(df)
  expect_equal(str, "\n")

  # Dataframe with one column
  df <- data.frame(Col1 = 1:5)
  str <- OpenStats:::df_2_string(df)
  f <- tempfile(fileext = ".txt")
  writeLines(str, f)
  read.csv(f, sep = "\t") |>
    expect_equal(df)

  # Dataframe with one column and one row
  df <- data.frame(Col1 = 1)
  str <- OpenStats:::df_2_string(df)
  f <- tempfile(fileext = ".txt")
  writeLines(str, f)
  read.csv(f, sep = "\t") |>
    expect_equal(df)

  # Dataframe with multiple column and one row
  df <- data.frame(Col1 = 1, Col2 = "a", Col3 = 1.5)
  str <- OpenStats:::df_2_string(df)
  f <- tempfile(fileext = ".txt")
  writeLines(str, f)
  read.csv(f, sep = "\t") |>
    expect_equal(df)
}
test_df_2_string()
