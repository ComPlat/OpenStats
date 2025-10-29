library(OpenStats)
library(ggplot2)
library(tinytest)
library(readxl)

# Test rng stuff
# =======================================================================================
test_Rnorm <- function() {
  # Test 1: Basic functionality for Rnorm
  result <- OpenStats:::Rnorm(10)
  expect_equal(
    length(result), 10, info = "OpenStats:::Rnorm should generate 10 random values"
  ) |> print()
  # Test 2: Large sequence
  expect_error(OpenStats:::Rnorm(10^9), "The size of the sequence is too large", 
               info = "OpenStats:::Rnorm should throw an error if the sequence exceeds the size limit"
  ) |> print()
  # Test 3: Non-integer `n`
  result <- OpenStats:::Rnorm(10.7)
  expect_equal(length(result), 10, info = "OpenStats:::Rnorm should round down non-integer `n` values to integers"
  ) |> print()
  # Test 4: Edge case: `n` is length of input
  expect_error(
    OpenStats:::Rnorm(c(1, 2, 3, 4, 5)),
    info = "OpenStats:::Rnorm should throw an error if a vector with length > 1 is used for n"
  ) |> print()
  # Test 5: Edge case: `n` is a dataframe
  expect_error(
    OpenStats:::Rnorm(data.frame(x = 1)),
    info = "OpenStats:::Rnorm should throw an error if a vector with length > 1 is used for n"
  ) |> print()
}
test_Rbinom <- function() {
  # Test 1: Basic functionality for Rbinom
  result <- OpenStats:::Rbinom(10, 1, 0.1)
  expect_equal(
    length(result), 10, info = "OpenStats:::Rbinom should generate 10 random values"
  ) |> print()
  # Test 2: Large sequence
  expect_error(OpenStats:::Rbinom(10^9, 1, 0.1), "The size of the sequence is too large", 
               info = "OpenStats:::Rbinom should throw an error if the sequence exceeds the size limit"
  ) |> print()
  # Test 3: Non-integer `n`
  result <- OpenStats:::Rbinom(10.7, 1, 0.1)
  expect_equal(length(result), 10, info = "OpenStats:::Rbinom should round down non-integer `n` values to integers"
  ) |> print()
  # Test 4: Edge case: `n` is length of input
  expect_error(
    OpenStats:::Rbinom(c(1, 2, 3, 4, 5), 1, 0.1),
    info = "OpenStats:::Rbinom should throw an error if a vector with length > 1 is used for n"
  ) |> print()
  # Test 5: Edge case: `n` is a dataframe
  expect_error(
    OpenStats:::Rbinom(data.frame(x = 1)),
    info = "OpenStats:::Rbinom should throw an error if a vector with length > 1 is used for n"
  ) |> print()
}
test_Rpois <- function() {
  # Test 1: Basic functionality for Rpois
  result <- OpenStats:::Rpois(10, 1)
  expect_equal(
    length(result), 10, info = "OpenStats:::Rpois should generate 10 random values"
  ) |> print()
  # Test 2: Large sequence
  expect_error(OpenStats:::Rpois(10^9, 1), "The size of the sequence is too large", 
               info = "OpenStats:::Rpois should throw an error if the sequence exceeds the size limit"
  ) |> print()
  # Test 3: Non-integer `n`
  result <- OpenStats:::Rpois(10.7, 1)
  expect_equal(length(result), 10, info = "OpenStats:::Rpois should round down non-integer `n` values to integers"
  ) |> print()
  # Test 4: Edge case: `n` is length of input
  expect_error(
    OpenStats:::Rpois(c(1, 2, 3, 4, 5), 1),
    info = "OpenStats:::Rpois should throw an error if a vector with length > 1 is used for n"
  ) |> print()
  # Test 5: Edge case: `n` is a dataframe
  expect_error(
    OpenStats:::Rpois(data.frame(x = 1)),
    info = "OpenStats:::Rpois should throw an error if a vector with length > 1 is used for n"
  ) |> print()
}
test_Runif <- function() {
  # Test 1: Basic functionality for Runif
  result <- OpenStats:::Runif(10)
  expect_equal(
    length(result), 10, info = "OpenStats:::Runif should generate 10 random values"
  ) |> print()
  # Test 2: Large sequence
  expect_error(OpenStats:::Runif(10^9), "The size of the sequence is too large", 
               info = "OpenStats:::Runif should throw an error if the sequence exceeds the size limit"
  ) |> print()
  # Test 3: Non-integer `n`
  result <- OpenStats:::Runif(10.7)
  expect_equal(length(result), 10, info = "OpenStats:::Runif should round down non-integer `n` values to integers"
  ) |> print()
  # Test 4: Edge case: `n` is length of input
  expect_error(
    OpenStats:::Runif(c(1, 2, 3, 4, 5)),
    info = "OpenStats:::Runif should throw an error if a vector with length > 1 is used for n"
  ) |> print()
  # Test 5: Edge case: `n` is a dataframe
  expect_error(
    OpenStats:::Runif(data.frame(x = 1)),
    info = "OpenStats:::Runif should throw an error if a vector with length > 1 is used for n"
  ) |> print()
}

# Run the tests
test_Rnorm()
test_Rbinom()
test_Rpois()
test_Runif()

# Test Seq
# =======================================================================================
test_Seq <- function() {
  # Test 1: Basic functionality
  start <- 1
  end <- 10
  by <- 2
  expected_result <- seq(1, 10, by = 2)
  result <- OpenStats:::Seq(start, end, by)
  expect_equal(
    result, expected_result, info = "OpenStats:::Seq should generate a sequence from start to end with the given step size"
  ) |> print()
  # Test 2: Large sequence
  # Here, we'll make a sequence that exceeds the 100MB limit
  expect_error(
    OpenStats:::Seq(1, 10^8, 1), "The size of the sequence is too large", 
               info = "OpenStats:::Seq should throw an error if the sequence exceeds the size limit"
  ) |> print()
  # Test 3: Negative step size
  start <- 10
  end <- 1
  by <- -2
  expected_result <- seq(10, 1, by = -2)
  result <- OpenStats:::Seq(start, end, by)
  expect_equal(
    result, expected_result, info = "OpenStats:::Seq should handle negative step sizes correctly"
  ) |> print()
  # Test 4: OpenStats:::Sequence length calculation
  start <- 1
  end <- 10
  by <- 2
  expected_length <- 5  # The sequence should have 5 elements: 1, 3, 5, 7, 9
  result <- OpenStats:::Seq(start, end, by)
  expect_equal(
    length(result), expected_length, info = "OpenStats:::Seq should generate the correct number of elements"
  ) |> print()
  # Test 5: Edge case: start equals end
  start <- 5
  end <- 5
  by <- 1
  expected_result <- seq(5, 5, by = 1)  # The sequence will just be [5]
  result <- OpenStats:::Seq(start, end, by)
  expect_equal(
    result, expected_result, info = "OpenStats:::Seq should handle the case where start equals end"
  ) |> print()
}
test_Seq()

# Test DataFrame
# =======================================================================================
test_DataFrame <- function() {
  # Test 1: Basic functionality
  col1 <- c(1, 2, 3)
  col2 <- c("A", "B", "C")
  expected_result <- data.frame(col1 = as.character(c(1, 2, 3)), col2 = c("A", "B", "C"))
  result <- OpenStats:::DataFrame(col1, col2)
  expect_equal(
    result, expected_result, info = "OpenStats:::DataFrame should create a data frame from two vectors"
  ) |> print()
  # Test 2: Empty column test
  col1 <- c("1", "2", "3")
  col2 <- c()
  expect_error(
    OpenStats:::DataFrame(col1, col2), "Found empty column", 
               info = "OpenStats:::DataFrame should throw an error if a column is empty"
  ) |> print()
  # Test 3: OpenStats:::Data frame size limit test
  large_col <- rep(1, 10^8)
  expect_error(
    OpenStats:::DataFrame(large_col, large_col), "The total size of the data frame is too large",
               info = "OpenStats:::DataFrame should throw an error if the total size exceeds 100MB"
  ) |> print()
  # Test 4: Column name handling
  col1 <- c(1, 2, 3)
  col2 <- c("A", "B", "C")
  expected_names <- c("col1", "col2")
  result <- OpenStats:::DataFrame(col1, col2)
  expect_equal(
    names(result), expected_names, info = "OpenStats:::DataFrame should use the variable names as column names"
  ) |> print()
  # Test 5: Column length mismatch, elongation
  col1 <- c(1, 2)
  col2 <- c("A", "B", "C")
  expected_result <- data.frame(col1 = c("1", "2", "1"), col2 = c("A", "B", "C"))
  result <- OpenStats:::DataFrame(col1, col2)
  expect_equal(
    result, expected_result, info = "OpenStats:::DataFrame should elongate shorter columns to match the longest column"
  ) |> print()
}
test_DataFrame()

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
  ) |> print()
  # Case 2: l is not a multiple of the length of col
  col <- c(1, 2, 3)
  l <- 8
  expected_result <- c(1, 2, 3, 1, 2, 3, 1, 2)
  result <- OpenStats:::elongate_col(col, l)
  expect_equal(result, expected_result, 
               info = "Should repeat col 2 times and append first 2 elements when l = 8"
  ) |> print()
  # Case 3: l is smaller than the length of col
  col <- c(1, 2, 3)
  l <- 2
  expected_result <- c(1, 2)
  result <- OpenStats:::elongate_col(col, l)
  expect_equal(result, expected_result, 
               info = "Should truncate col to fit length l when l = 2"
  ) |> print()
  # Case 4: l is equal to the length of col
  col <- c(1, 2, 3)
  l <- 3
  expected_result <- c(1, 2, 3)
  result <- OpenStats:::elongate_col(col, l)
  expect_equal(result, expected_result, 
               info = "Should return col as is when l equals length of col"
  ) |> print()
  # Case 5: l is a very large number compared to length of col
  col <- c(1, 2)
  l <- 10001
  expected_result <- rep(c(1, 2), length.out = 10001)
  result <- OpenStats:::elongate_col(col, l)
  expect_equal(
    result, expected_result, 
               info = "Should repeat col enough times to match the requested length (10001)"
  ) |> print()
}
test_elongate_col()

# Test check result list size = rls
# =======================================================================================
# Create a dummy object to represent a "new object" to be checked
create_dummy_object <- function(size_in_mb) {
  return(1:ceiling(size_in_mb * 1024^2 / 8))
}

# Test for check_rls function
test_check_rls <- function() {
  # Case 1: List of results exceeds 1000 entries
  list_results_1001 <- lapply(1:1001, function(x) {rep(100, x)})
  new_obj <- 1:100
  expect_error(OpenStats:::check_rls(list_results_1001, new_obj), 
               "You can only store 1000 results. Consider removing some results",
               info = "Should stop when there are more than 1000 results"
  ) |> print()
  # Case 2: The total size exceeds 500MB
  list_results_large <- list(create_dummy_object(500), create_dummy_object(250))
  new_obj_large <- create_dummy_object(300)
  object.size(list_results_large)
  expect_error(OpenStats:::check_rls(list_results_large, new_obj_large),
               "Memory limit exceeded for user results. Consider removing some results.",
               info = "Should stop when total memory exceeds 500MB"
  ) |> print()
  # Case 3: The total size does not exceed 500MB and number of results is below 1000
  list_results_valid <- list(create_dummy_object(100), create_dummy_object(100))
  new_obj_valid <- create_dummy_object(100)
  expect_silent(OpenStats:::check_rls(list_results_valid, new_obj_valid), 
                info = "Should pass when results are below 1000 and total size is within 500MB"
  ) |> print()
  # Case 4: Exactly 1000 results and total size within limit
  list_results_1000 <- rep(list(1:100), 10)
  new_obj_1000 <- 1:100
  expect_silent(OpenStats:::check_rls(list_results_1000, new_obj_1000),
                info = "Should pass with exactly 1000 results and total size within limit"
  ) |> print()
}
test_check_rls()
# Test for create_plot_pages function
# =======================================================================================
test_create_plot_pages <- function() {
  # Create mock plot list (using a simple empty plot for testing)
  plot_list <- list(ggplot2::ggplot() + ggplot2::geom_point())
  # Test: fewer than 9 plots (e.g., 1 plot)
  result <- OpenStats:::create_plot_pages(plot_list)
  expect_equal(
    length(result), 1, info = "Should return 1 page for a single plot"
  ) |> print()
  # Test: exactly 9 plots
  plot_list_9 <- rep(plot_list, 9)
  result <- OpenStats:::create_plot_pages(plot_list_9)
  expect_equal(
    length(result), 2, info = "Should return 2 page for exactly 9 plots"
  ) |> print()
  # Test: exactly 18 plots
  plot_list_18 <- rep(plot_list, 18)
  result <- OpenStats:::create_plot_pages(plot_list_18)
  expect_equal(
    length(result), 3, info = "Should return 3 pages for exactly 18 plots"
  ) |> print()
  # Test: more than 9 but not an exact multiple (e.g., 10 plots)
  plot_list_10 <- rep(plot_list, 10)
  result <- OpenStats:::create_plot_pages(plot_list_10)
  expect_equal(
    length(result), 2, info = "Should return 2 pages for 10 plots (1st with 9, 2nd with 1)"
  ) |> print()
  # Test: edge case for 17 plots (last page should have 8 plots)
  plot_list_17 <- rep(plot_list, 17)
  result <- OpenStats:::create_plot_pages(plot_list_17)
  expect_equal(
    length(result), 2, info = "Should return 2 pages for 17 plots (1st with 9, 2nd with 8)"
  ) |> print()
  # Test: no plots (empty list)
  plot_list_empty <- list()
  result <- OpenStats:::create_plot_pages(plot_list_empty)
  expect_equal(
    length(result), 1, info = "Should return 1 page for an empty plot list (empty grid)"
  ) |> print()
  # Test: number of plots is exactly a multiple of 9 (e.g., 27)
  plot_list_27 <- rep(plot_list, 27)
  result <- OpenStats:::create_plot_pages(plot_list_27)
  expect_equal(
    length(result), 4, info = "Should return 4 pages for 27 plots (9 plots per page)"
  ) |> print()
}
test_create_plot_pages()

# Test checks for filename
# =======================================================================================
test_is_valid_filename <- function() {
  # Valid filename
  expect_true(
    OpenStats:::is_valid_filename("valid_filename.txt"), info = "Should return TRUE for valid filename"
  ) |> print()
  # Filename with spaces
  expect_false(
    OpenStats:::is_valid_filename("invalid filename.txt"), info = "Should return FALSE for filename with spaces"
  ) |> print()
  # Filename with invalid characters
  expect_false(
    OpenStats:::is_valid_filename("invalid|filename.txt"), info = "Should return FALSE for filename with invalid characters"
  ) |> print()
  # Empty filename
  expect_false(
    OpenStats:::is_valid_filename(""), info = "Should return FALSE for empty filename"
  ) |> print()
  # Filename too long
  expect_false(
    OpenStats:::is_valid_filename(strrep("a", 101)), info = "Should return FALSE for filename longer than 100 characters"
  ) |> print()
  # Filename with no extension
  expect_false(
    OpenStats:::is_valid_filename("file_without_extension"), info = "Should return FALSE for filename with no extension"
  ) |> print()
}
test_is_valid_filename()

test_why_filename_invalid <- function() {
  # Valid filename
  expect_equal(
    OpenStats:::why_filename_invalid("valid_filename.txt"), "", info = "Should return empty string for valid filename"
  ) |> print()
  # Filename with spaces
  expect_equal(
    OpenStats:::why_filename_invalid("invalid filename.txt"), "Found spaces in filename", info = "Should return error message for spaces in filename"
  ) |> print()
  # Filename with invalid characters
  expect_equal(
    OpenStats:::why_filename_invalid("invalid|filename.txt"), "Found invalid chars in filename: [<>:\"\\|?*", info = "Should return error message for invalid characters in filename"
  ) |> print()
  # Empty filename
  expect_equal(
    OpenStats:::why_filename_invalid(""), "Filename is empty", info = "Should return error message for empty filename"
  ) |> print()
  # Filename too long
  expect_equal(
    OpenStats:::why_filename_invalid(strrep("a", 101)), "Filename is too long (> 100 characters)", info = "Should return error message for filename longer than 100 characters"
  ) |> print()
  # Filename with no extension
  expect_equal(
    OpenStats:::why_filename_invalid("file_without_extension"), "Filename extension is missing", info = "Should return error message for filename with no extension"
  ) |> print()
}
test_why_filename_invalid()

test_check_filename_for_server <- function() {
  # Valid xlsx file
  expect_true(
    OpenStats:::check_filename_for_server("data/file.xlsx"), info = "Should return TRUE for filename with .xlsx extension"
  ) |> print()
  # Invalid file extension
  expect_false(
    OpenStats:::check_filename_for_server("data/file.csv"), info = "Should return FALSE for filename with .csv extension"
  ) |> print()
  # No extension
  expect_false(
    OpenStats:::check_filename_for_server("data/file"), info = "Should return FALSE for filename with no extension"
  ) |> print()
  # Invalid extension
  expect_false(
    OpenStats:::check_filename_for_server("data/file.txt"), info = "Should return FALSE for filename with .txt extension"
  ) |> print()
}
test_check_filename_for_server()

test_check_filename_for_serverless <- function() {
  # Valid zip file
  expect_true(
    OpenStats:::check_filename_for_serverless("data/file.zip"), info = "Should return TRUE for filename with .zip extension"
  ) |> print()
  # Invalid file extension
  expect_false(
    OpenStats:::check_filename_for_serverless("data/file.xlsx"), info = "Should return FALSE for filename with .xlsx extension"
  ) |> print()
  # No extension
  expect_false(
    OpenStats:::check_filename_for_serverless("data/file"), info = "Should return FALSE for filename with no extension"
  ) |> print()
  # Invalid extension
  expect_false(
    OpenStats:::check_filename_for_serverless("data/file.tar"), info = "Should return FALSE for filename with .tar extension"
  ) |> print()
}
test_check_filename_for_serverless()

# Test extract_extension
# =======================================================================================
test_extract_extension <- function() {
  # Test with a filename that has a valid extension
  expect_equal(
    OpenStats:::extract_extension("data/file.txt"), "txt", info = "Should return 'txt' for 'file.txt'"
  ) |> print()
  # Test with a filename that has multiple extensions
  expect_equal(
    OpenStats:::extract_extension("archive.tar.gz"), "gz", info = "Should return 'gz' for 'archive.tar.gz'"
  ) |> print()
  # Test with a filename that has no extension
  expect_equal(
    OpenStats:::extract_extension("file_without_extension"), "file_without_extension", info = "Should return the full name if there is no extension"
  ) |> print()
  # Test with a filename that has a hidden file extension (e.g., dot before the file name)
  expect_equal(
    OpenStats:::extract_extension(".hiddenfile"), "hiddenfile", info = "Should return 'hiddenfile' for a filename starting with a dot"
  ) |> print()
}
test_extract_extension()

# Test Mean, Max, sd, sum, max and min
# =======================================================================================
test_Mean <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::Mean(c(1, 2, 3, 4, 5)), 3, info = "OpenStats:::Mean should return 3 for the vector c(1, 2, 3, 4, 5)"
  ) |> print()
  # Test with non-numeric input
  expect_equal(
    OpenStats:::Mean(c("1", "2", "3", "4", "5")) , 3, info = "OpenStats:::Mean should convert character input to numeric and return 3"
  ) |> print()
  # Test with NA values
  expect_equal(
    OpenStats:::Mean(c(1, 2, 3, NA, 5)), 2.75, info = "OpenStats:::Mean should return 2.75 when NA values are present"
  ) |> print()
}
test_Mean()

test_Median <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::Median(c(1, 2, 3, 4, 5)), 3, info = "Median should return 3 for the vector c(1, 2, 3, 4, 5)"
  ) |> print()
  # Test with non-numeric input
  expect_equal(
    OpenStats:::Median(c("1", "2", "3", "4", "5")) , 3, info = "Median should convert character input to numeric and return 3"
  ) |> print()
  # Test with NA values
  expect_equal(
    OpenStats:::Median(c(1, 2, 3, NA, 5)), 2.5, info = "Median should return 2.5 when NA values are present"
  ) |> print()
}
test_Median()

test_SD <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::SD(c(1, 2, 3, 4, 5)), sd(1:5), info = "SD should return the correct standard deviation for the vector c(1, 2, 3, 4, 5)"
  ) |> print()
  # Test with non-numeric input
  expect_equal(
    OpenStats:::SD(c("1", "2", "3", "4", "5")) , sd(1:5), info = "SD should convert character input to numeric and return the correct SD"
  ) |> print()
  # Test with NA values
  expect_equal(
    OpenStats:::SD(c(1, 2, 3, NA, 5)), sd(c(1, 2, 3, 5)), info = "SD should return the correct SD when NA values are present"
  ) |> print()
}
test_SD()

test_Sum <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::Sum(c(1, 2, 3, 4, 5)), 15, info = "Sum should return 15 for the vector c(1, 2, 3, 4, 5)"
  ) |> print()
  # Test with non-numeric input
  expect_equal(
    OpenStats:::Sum(c("1", "2", "3", "4", "5")) , 15, info = "Sum should convert character input to numeric and return 15"
  ) |> print()
  # Test with NA values
  expect_equal(
    OpenStats:::Sum(c(1, 2, 3, NA, 5)), 11, info = "Sum should return 11 when NA values are present"
  ) |> print()
}
test_Sum()

test_Min <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::Min(c(1, 2, 3, 4, 5)), 1, info = "Min should return 1 for the vector c(1, 2, 3, 4, 5)"
  ) |> print()
  # Test with non-numeric input
  expect_equal(
    OpenStats:::Min(c("1", "2", "3", "4", "5")) , 1, info = "Min should convert character input to numeric and return 1"
  ) |> print()
  # Test with NA values
  expect_equal(
    OpenStats:::Min(c(1, 2, 3, NA, 5)), 1, info = "Min should return 1 when NA values are present"
  ) |> print()
}
test_Min()

test_Max <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::Max(c(1, 2, 3, 4, 5)), 5, info = "Max should return 5 for the vector c(1, 2, 3, 4, 5)"
  ) |> print()
  # Test with non-numeric input
  expect_equal(
    OpenStats:::Max(c("1", "2", "3", "4", "5")) , 5, info = "Max should convert character input to numeric and return 5"
  ) |> print()
  # Test with NA values
  expect_equal(
    OpenStats:::Max(c(1, 2, 3, NA, 5)), 5, info = "Max should return 5 when NA values are present"
  ) |> print()
}
test_Max()

# Test check formula
# =======================================================================================
test_check_formula <- function() {
  # Test with a valid formula: response ~ predictor
  valid_formula <- as.formula("y ~ x")
  expect_true(OpenStats:::check_formula(valid_formula), 
              info = "OpenStats:::check_formula should return TRUE for a valid formula") |> print()

  # Test with a formula with more than two terms
  invalid_formula_3_terms <- as.formula("y ~ x + z")
  expect_error(OpenStats:::check_formula(invalid_formula_3_terms), 
               "Formula must have exactly two terms: response ~ predictor", 
               info = "OpenStats:::check_formula should throw an error for a formula with more than two terms") |> print()

  # Test with a formula with fewer than two terms
  invalid_formula_1_term <- as.formula("y ~ 1")
  expect_error(OpenStats:::check_formula(invalid_formula_1_term), 
               "Formula must have exactly two terms: response ~ predictor", 
               info = "OpenStats:::check_formula should throw an error for a formula with fewer than two terms") |> print()

  # Test with a non-formula input (e.g., a character vector)
  invalid_non_formula <- "y ~ x"
  expect_error(OpenStats:::check_formula(invalid_non_formula), 
               "Input must be a formula of the type response ~ predictor", 
               info = "OpenStats:::check_formula should throw an error for a non-formula input") |> print()
}
test_check_formula()

# Test check length code
# =======================================================================================
test_check_length_code <- function() {
  # Test with a valid code length (less than 4000 characters)
  valid_code <- paste(rep("a", 3999), collapse = "")
  expect_null(OpenStats:::check_length_code(valid_code), 
              info = "OpenStats:::check_length_code should return NULL for code with less than 4000 characters") |> print()

  # Test with a code length exactly 4000 characters
  valid_code_4000 <- paste(rep("a", 4000), collapse = "")
  expect_null(OpenStats:::check_length_code(valid_code_4000), 
              info = "OpenStats:::check_length_code should return NULL for code with exactly 4000 characters") |> print()

  # Test with a code length greater than 4000 characters
  invalid_code <- paste(rep("a", 4001), collapse = "")
  expect_error(OpenStats:::check_length_code(invalid_code), 
               "The code is too long to be evaluated", 
               info = "OpenStats:::check_length_code should throw an error for code longer than 4000 characters") |> print()
  
  # Test with an empty code (length 0)
  empty_code <- ""
  expect_null(OpenStats:::check_length_code(empty_code), 
              info = "OpenStats:::check_length_code should return NULL for an empty code") |> print()

}
test_check_length_code()

# Test check type result
# =======================================================================================
test_check_type_res <- function() {
  # Test with allowed types
  # Numeric type
  expect_null(OpenStats:::check_type_res(1),
              info = "OpenStats:::check_type_res should return NULL for numeric input") |> print()

  # Integer type
  expect_null(OpenStats:::check_type_res(1L),
              info = "OpenStats:::check_type_res should return NULL for integer input") |> print()

  # Factor type
  expect_null(OpenStats:::check_type_res(factor("a")),
              info = "OpenStats:::check_type_res should return NULL for factor input") |> print()

  # Logical type
  expect_null(OpenStats:::check_type_res(TRUE),
              info = "OpenStats:::check_type_res should return NULL for logical input") |> print()

  # Character type
  expect_null(OpenStats:::check_type_res("text"),
              info = "OpenStats:::check_type_res should return NULL for character input") |> print()

  # Data frame type
  expect_null(OpenStats:::check_type_res(data.frame(a = 1, b = 2)),
              info = "OpenStats:::check_type_res should return NULL for data.frame input") |> print()

  # Test with disallowed type
  # List type (not allowed)
  expect_error(OpenStats:::check_type_res(list(1, 2, 3)),
               "Found result with unallowed type: list", 
               info = "OpenStats:::check_type_res should throw error for list type") |> print()

  # Function type (not allowed)
  expect_error(OpenStats:::check_type_res(function(x) x),
               "Found result with unallowed type: function", 
               info = "OpenStats:::check_type_res should throw error for function type") |> print()

  # Date type (not allowed)
  expect_error(OpenStats:::check_type_res(as.Date("2024-01-01")), 
               "Found result with unallowed type: Date", 
               info = "check_type_res should throw error for Date type") |> print()
}
test_check_type_res()

# Test check axis limits
# =======================================================================================
test_check_axis_limits <- function() {
  # Test with valid numeric axis limits
  col_numeric <- c(1, 2, 3, 4, 5)
  expect_null(OpenStats:::check_axis_limits(col_numeric, 1, 4),
              info = "OpenStats:::check_axis_limits should return NULL for valid numeric limits") |> print()

  # Test with invalid numeric axis limits (max <= min)
  expect_error(OpenStats:::check_axis_limits(col_numeric, 4, 1),
               "Found invalid axis limits: max <= min",
               info = "OpenStats:::check_axis_limits should throw error when max <= min") |> print()

  # Test with invalid numeric axis limits (non-numeric min or max)
  expect_error(OpenStats:::check_axis_limits(col_numeric, "a", 4),
               "Found invalid axis limits",
               info = "OpenStats:::check_axis_limits should throw error for non-numeric min") |> print()

  expect_error(OpenStats:::check_axis_limits(col_numeric, 1, "b"),
               "Found invalid axis limits",
               info = "OpenStats:::check_axis_limits should throw error for non-numeric max") |> print()

  # Test with valid categorical axis limits
  col_factor <- factor(c("low", "medium", "high"))
  expect_null(OpenStats:::check_axis_limits(col_factor, "low", "medium"),
              info = "OpenStats:::check_axis_limits should return NULL for valid factor levels") |> print()

  # Test with invalid categorical axis limits (min or max not in factor levels)
  expect_error(OpenStats:::check_axis_limits(col_factor, "low", "very_high"),
               "Found invalid axis limits",
               info = "OpenStats:::check_axis_limits should throw error when min or max not in factor levels") |> print()

  # Test with invalid categorical axis limits (max appears before min)
  expect_error(OpenStats:::check_axis_limits(col_factor, "medium", "low"),
               "Found invalid axis limits. The max value is found before the min value",
               info = "OpenStats:::check_axis_limits should throw error when max appears before min in factor levels") |> print()
 
  # Test with a single value in the column
  col_single <- factor("only_one_value")
  expect_null(OpenStats:::check_axis_limits(col_single, "only_one_value", "only_one_value"),
              info = "OpenStats:::check_axis_limits should return NULL for a single value column when min = max") |> print()
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
  ) |> print()

  # Test case where splitting on two columns works correctly
  result <- OpenStats:::split_groups(df, cols = c("group"), levels = c("A", "B"))
  expect_equal(
    result$category,
    c("X", "Y", "X", "Y"),
    info = "split should work for two columns correctly"
  ) |> print()

  # Test when the dataframe has no matching rows
  result <- tryCatch({
    OpenStats:::split_groups(df, cols = c("group"), levels = c("D"))
  }, error = function(e) e)
  expect_true(
    inherits(result, "error"),
    info = "split should throw an error if no rows match the levels"
  ) |> print()

  # Test case where all rows are included (no subset)
  result <- OpenStats:::split_groups(df, cols = c("group", "category"), levels = c("A", "B", "C", "X", "Y", "Z"))
  expect_equal(
    nrow(result),
    5,
    info = "split should return all rows if all levels are included"
  ) |> print()

  # Edge case with empty dataframe
  df_empty <- data.frame(group = character(0), category = character(0), value = numeric(0))
  result <- tryCatch({
    OpenStats:::split_groups(df_empty, cols = c("group"), levels = c("A"))
  }, error = function(e) e)
  expect_true(
    inherits(result, "error"),
    info = "split should throw an error if the dataframe is empty"
  ) |> print()

  # Edge case where no level is set
  result <- tryCatch({
    OpenStats:::split_groups(df, cols = c("group", "category"), levels = c())
  }, error = function(e) e)
  expect_true(
    inherits(result, "error"),
    info = "split should throw an error if the dataframe is empty"
  ) |> print()
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
  ) |> print()

  # Test input with spaces in column names
  df <- data.frame(`invalid name` = 1:5, `another name` = 6:10)
  result <- OpenStats:::create_r_names(df)
  expect_equal(
    names(result),
    c("invalid.name", "another.name"),
    info = "OpenStats:::create_r_names should replace spaces with dots"
  ) |> print()

  # Test input with special characters
  df <- data.frame(`$pecial@name` = 1:5, `123start_with_number` = 6:10)
  result <- OpenStats:::create_r_names(df)
  expect_equal(
    names(result),
    c("X.pecial.name", "X123start_with_number"),
    info = "OpenStats:::create_r_names should sanitize names with special characters and numeric starts"
  ) |> print()

  # Test input with reserved R keywords
  df <- data.frame(`if` = 1:5, `else` = 6:10, `TRUE` = 11:15)
  result <- OpenStats:::create_r_names(df)
  expect_equal(
    names(result),
    c("if.", "else.", "TRUE."),
    info = "OpenStats:::create_r_names should leave reserved keywords unchanged"
  ) |> print()

  # Edge case: empty data frame
  df <- data.frame()
  result <- OpenStats:::create_r_names(df)
  expect_equal(
    names(result),
    character(0),
    info = "OpenStats:::create_r_names should handle empty data frames without error"
  ) |> print()
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
  ) |> print()

  # Test when the name conflicts and needs adjustment
  result_conflict <- OpenStats:::create_df_name("df1", column_names)
  expect_equal(
    result_conflict,
    "df11",
    info = "OpenStats:::create_df_name should return a modified name when the input conflicts"
  ) |> print()

  result_conflict_multiple <- OpenStats:::create_df_name("df", c("df", "df1", "df12", "df123"))
  expect_equal(
    result_conflict_multiple,
    "df1234",
    info = "OpenStats:::create_df_name should return a name that avoids all conflicts"
  ) |> print()

  # Edge cases
  result_empty <- OpenStats:::create_df_name("new_df", character(0))
  expect_equal(
    result_empty,
    "new_df",
    info = "OpenStats:::create_df_name should return the name as is when column_names is empty"
  ) |> print()

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

# Test get rows
# =======================================================================================
test_get_rows <- function() {
  # Test data
  df <- data.frame(
    A = c(1, 2, 3, 4),
    B = c(5, 6, 7, 8),
    C = c("x", "y", "z", "x")
  )

  # Test valid cases
  result <- OpenStats:::get_rows(df, df$A > 2)
  expect_equal(
    result,
    subset(df, A > 2),
    info = "OpenStats:::get_rows should correctly filter rows where A > 2"
  ) |> print()

  result_mult_cond <- OpenStats:::get_rows(df, df$A > 2 & df$B < 8)
  expect_equal(
    result_mult_cond,
    subset(df, A > 2 & B < 8),
    info = "OpenStats:::get_rows should correctly filter rows with multiple conditions"
  ) |> print()

  result_char <- OpenStats:::get_rows(df, df$C == "x")
  expect_equal(
    result_char,
    subset(df, C == "x"),
    info = "OpenStats:::get_rows should correctly filter rows with character comparisons"
  ) |> print()

  # Test empty result
  result_empty <- OpenStats:::get_rows(df, df$A > 10)
  expect_equal(
    result_empty,
    subset(df, A > 10),
    info = "OpenStats:::get_rows should return an empty data frame if no rows match"
  ) |> print()

  # Edge cases
  empty_df <- data.frame(A = numeric(), B = numeric())
  result_empty_df <- OpenStats:::get_rows(empty_df, empty_df$A > 0)
  expect_equal(
    result_empty_df,
    subset(empty_df, A > 0),
    info = "OpenStats:::get_rows should return an empty data frame for an empty input data frame"
  ) |> print()

  # Test error cases
  expect_error(
    OpenStats:::get_rows(42, dfA > 2),
    info = "OpenStats:::get_rows should throw an error when input is not a data frame"
  ) |> print()
}
test_get_rows()

# Test get cols
# =======================================================================================
test_get_cols <- function() {
  # Test data
  df <- data.frame(
    A = c(1, 2, 3),
    B = c(4, 5, 6),
    C = c(7, 8, 9)
  )

  # Test valid cases
  result <- OpenStats:::get_cols(df, A, B)
  expect_equal(
    result,
    df[, c("A", "B")],
    info = "OpenStats:::get_cols should return the correct subset of columns"
  ) |> print()
 
  result_single <- OpenStats:::get_cols(df, C)
  expect_equal(
    result_single,
    df[, "C", drop = TRUE],
    info = "OpenStats:::get_cols should work for a single column selection"
  ) |> print()

  # Test with all columns
  result_all <- OpenStats:::get_cols(df, A, B, C)
  expect_equal(
    result_all,
    df,
    info = "OpenStats:::get_cols should return the entire data frame when all columns are selected"
  ) |> print()

  # Test with reordered columns
  result_reordered <- OpenStats:::get_cols(df, C, A)
  expect_equal(
    result_reordered,
    df[, c("C", "A")],
    info = "OpenStats:::get_cols should respect the order of columns provided in the arguments"
  ) |> print()

  # Test error cases
  expect_error(
    OpenStats:::get_cols(df, D),
    info = "OpenStats:::get_cols should throw an error when a non-existent column is requested"
  ) |> print()
  expect_error(
    OpenStats:::get_cols(df),
    info = "OpenStats:::get_cols should throw an error when no columns are specified"
  ) |> print()
  expect_error(
    OpenStats:::get_cols(42, A),
    info = "OpenStats:::get_cols should throw an error when the input is not a data frame"
  ) |> print()

  # Edge cases
  empty_df <- data.frame()
  expect_error(
    OpenStats:::get_cols(empty_df, A),
    info = "OpenStats:::get_cols should throw an error when accessing a column in an empty data frame"
  ) |> print()

  single_col_df <- data.frame(A = c(1, 2, 3))
  result_single_col <- OpenStats:::get_cols(single_col_df, A)
  expect_equal(
    result_single_col,
    single_col_df[, 1],
    info = "OpenStats:::get_cols should work correctly for a single-column data frame"
  ) |> print()
}
test_get_cols()


# Test get element
# =======================================================================================
test_get_elem <- function() {
  # Test data
  df <- data.frame(
    A = c(1, 2, 3),
    B = c(4, 5, 6)
  )
  vec <- c(10, 20, 30, 40)

  # Test valid cases for data frame
  expect_equal(
    OpenStats:::get_elem(df, 2, 1),
    df[2, 1],
    info = "get_elem should return the correct element for data frame with row and column indices"
  ) |> print()
  expect_equal(
    OpenStats:::get_elem(df, 3, 2),
    df[3, 2],
    info = "get_elem should return the correct element for another valid data frame case"
  ) |> print()

  # Test valid cases for vector
  expect_equal(
    OpenStats:::get_elem(vec, 3),
    vec[3],
    info = "get_elem should return the correct element for vector with a single index"
  ) |> print()
  expect_equal(
    OpenStats:::get_elem(vec, 1),
    vec[1],
    info = "get_elem should work with another valid index for vector"
  ) |> print()

  # Test error cases
  expect_error(
    OpenStats:::get_elem(df, 1),
    info = "get_elem should throw an error when only one index is provided for a data frame"
  ) |> print()
  expect_error(
    OpenStats:::get_elem(vec, 1, 2),
    info = "get_elem should throw an error when two indices are provided for a vector"
  ) |> print()
  expect_error(
    OpenStats:::get_elem(df, "row", "col"),
    info = "OpenStats:::get_elem should throw an error when non-numeric indices are provided for a data frame"
  ) |> print()
  expect_error(
    OpenStats:::get_elem(vec, "index"),
    info = "OpenStats:::get_elem should throw an error when non-numeric index is provided for a vector"
  ) |> print()
  expect_error(
    OpenStats:::get_elem(df, 2, 1, 3),
    info = "OpenStats:::get_elem should throw an error when more than two indices are provided"
  ) |> print()
  expect_error(
    OpenStats:::get_elem(vec),
    info = "OpenStats:::get_elem should throw an error when no index is provided"
  ) |> print()

  # Test edge cases
  empty_df <- data.frame()
  expect_error(
    OpenStats:::get_elem(empty_df, 1, 1),
    info = "OpenStats:::get_elem should throw an error when accessing an element in an empty data frame"
  ) |> print()

  empty_vec <- numeric()
  expect_error(
    OpenStats:::get_elem(empty_vec, 1),
    info = "OpenStats:::get_elem should throw an error when accessing an element in an empty vector"
  )
}
test_get_elem()

# Test splitData
# =======================================================================================
test_splitData <- function() {
  # Test data
  df <- data.frame(
    value = c(10, 20, 30, 40, 50),
    group1 = c("A", "A", "B", "B", "C"),
    group2 = c("X", "Y", "X", "Y", "X")
  )

  # Basic case
  result <- OpenStats:::splitData(df, value ~ group1 + group2)

  # Check the structure of the result
  expect_equal(
    colnames(result),
    c("value", "interaction"),
    info = "Result should have columns: 'value' and 'interaction'"
  ) |> print()
  expect_equal(
    nrow(result),
    nrow(df),
    info = "Result should have the same number of rows as the input data frame"
  ) |> print()
  expect_equal(
    ncol(result),
    2,
    info = "Two columns are expected"
  ) |> print()

  # Check the interaction column
  model_frame <- model.frame(value ~ group1 + group2, data = df)
  expected_interaction <- interaction(model_frame$group1, model_frame$group2)
  expect_equal(
    result$interaction,
    expected_interaction,
    info = "Interaction column should correctly reflect the interaction of group1 and group2"
  ) |> print()

  # Check the value column
  expect_equal(
    result$value,
    df$value,
    info = "Value column should be identical to the response variable in the formula"
  ) |> print()

  # Edge case: Single predictor variable
  result_single <- OpenStats:::splitData(df, value ~ group1)
  expected_interaction_single <- interaction(df$group1)
  expect_equal(
    result_single$interaction,
    expected_interaction_single,
    info = "Interaction column should work correctly for a single predictor variable"
  ) |> print()

  # Edge case: Invalid formula
  expect_error(
    OpenStats:::splitData(df, value ~ non_existent_column),
    info = "Function should throw an error for a formula with non-existent variables"
  ) |> print()

  # Edge case: Less than 2 columns after model.frame
  df_min <- data.frame(value = c(1, 2, 3))
  expect_error(
    OpenStats:::splitData(df_min, value ~ .),
    info = "Function should throw an error when there are fewer than 2 columns in the formula output"
  ) |> print()

  # Edge case: 
  df_min <- data.frame(value = runif(6), rep(c("A", "B"), each = 3))
  expect_error(
    OpenStats:::splitData(df_min, value ~ 1),
    info = "Function should throw an error when there are fewer than 2 columns in the formula output"
  ) |> print()
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

  # Test stackDF
  stacked <- OpenStats:::stackDF(df, keepCol = "ID")

  # Check structure of stacked data frame
  expect_equal(
    colnames(stacked),
    c("ID", "name", "value"),
    info = "stackDF should produce columns: ID, name, value"
  ) |> print()
  expect_equal(
    nrow(stacked),
    6,
    info = "stackDF should have rows equal to original non-ID columns * number of rows"
  ) |> print()
  expect_equal(
    unique(stacked$name),
    c("A", "B"),
    info = "stackDF should properly stack column names into 'name' column"
  ) |> print()
  expect_equal(
    stacked$value[stacked$name == "A"],
    df$A,
    info = "stackDF should retain values correctly for column A"
  ) |> print()

  # Test unstackDF
  unstacked <- OpenStats:::unstackDF(stacked, name = "name", value = "value")

  # Check structure of unstacked data frame
  expect_equal(
    colnames(unstacked),
    colnames(df),
    info = "unstackDF should reconstruct the original column names"
  ) |> print()
  expect_equal(
    unstacked$A,
    df$A,
    info = "unstackDF should correctly reconstruct column A"
  ) |> print()
  expect_equal(
    unstacked$B,
    df$B,
    info = "unstackDF should correctly reconstruct column B"
  ) |> print()
  expect_equal(
    unstacked$ID,
    df$ID,
    info = "unstackDF should preserve the ID column"
  ) |> print()
}
test_stack_unstackDF()

# Test createJSString
# =======================================================================================
# TODO: update test
test_createJSString <- function() {
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
  result <- OpenStats:::createJSString(l)

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
  ) |> print()
  expect_true(
    grepl("^data:image/png;base64,", result[[1]][[2]]),
    info = "Second element should be a base64-encoded diagnostic plot"
  ) |> print()
  expect_true(
    grepl("^data:image/png;base64,", result[[1]][[3]]),
    info = "Dose response plot should be base64-encoded"
  ) |> print()
  expect_true(
    grepl("^Sepal.Length", result[[1]][[5]]),
    info = "Data frame should be converted to string format"
  ) |> print()
  expect_equal(
    result[[1]][[7]], char_obj,
    info = "Character string should remain as is"
  ) |> print()

  # Cleanup
  unlink(diag_fn)
}
test_createJSString()

# Test createExcelFile
# =======================================================================================
test_createExcelFile <- function() {
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  if (!identical(Sys.getenv("RUN_UI_TESTS"), "true")) return()
  p <- ggplot(
    data = iris,
    aes(x = Species, y = Sepal.Length)
  ) +
    geom_boxplot()
  p <- new("plot", p = p, width = 10, height = 10, resolution = 600)
  l <- list(p, iris)
  file <- OpenStats:::createExcelFile(l)

  # File existence
  expect_true(
    !is.null(file), "File should not be NULL"
  ) |> print()

  # Check workbook structure
  wb <- openxlsx::loadWorkbook(file)
  sheets <- openxlsx::getSheetNames(file)
  expect_true(
    "Results" %in% sheets, "Sheet 'Results' should exist"
  ) |> print()

  # Check data content
  data <- openxlsx::read.xlsx(file, sheet = "Results", colNames = TRUE, startRow = 21)
  expect_equal(
    colnames(data), colnames(iris),
    info = "Data column names should match"
  ) |> print()

  # Check plot presence
  temp_files <- dir(tempdir(), pattern = "\\.png$")
  expect_true(
    length(temp_files) > 0, "At least one temporary plot file should exist"
  ) |> print()

  # Cleanup
  file.remove(file)
  unlink(temp_files)
}
test_createExcelFile()

# Test DF2String
# =======================================================================================
test_DF2String <- function() {
  # Normal data frame
  df <- data.frame(Col1 = 1:5, Col2 = letters[1:5])
  str <- OpenStats:::DF2String(df)
  f <- tempfile(fileext = ".txt")
  writeLines(str, f)
  read.csv(f, sep = "\t") |>
    expect_equal(df) |>
    print()

  # Not a dataframe
  OpenStats:::DF2String("Invalid") |>
    expect_error("Input to DF2String is not of type DataFrame") |>
    print()

  # EMPTY data frame
  df <- data.frame()
  str <- OpenStats:::DF2String(df)
  expect_equal(str, "\n") |> print()

  # Dataframe with one column
  df <- data.frame(Col1 = 1:5)
  str <- OpenStats:::DF2String(df)
  f <- tempfile(fileext = ".txt")
  writeLines(str, f)
  read.csv(f, sep = "\t") |>
    expect_equal(df) |>
    print()

  # Dataframe with one column and one row
  df <- data.frame(Col1 = 1)
  str <- OpenStats:::DF2String(df)
  f <- tempfile(fileext = ".txt")
  writeLines(str, f)
  read.csv(f, sep = "\t") |>
    expect_equal(df) |>
    print()

  # Dataframe with multiple column and one row
  df <- data.frame(Col1 = 1, Col2 = "a", Col3 = 1.5)
  str <- OpenStats:::DF2String(df)
  f <- tempfile(fileext = ".txt")
  writeLines(str, f)
  read.csv(f, sep = "\t") |>
    expect_equal(df) |>
    print()
}
test_DF2String()


# Test readData
# =======================================================================================
# TODO: update test
test_readData <- function() {

  # Test 1: Valid input with an Excel file
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(NULL)
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(NULL)
  test_file <- tempfile(fileext = ".xlsx")
  write.csv(data.frame(a = 1:5, b = letters[1:5]), test_file, row.names = FALSE)
  writexl::write_xlsx(read.csv(test_file), test_file)
  OpenStats:::readData(test_file, DataModelState, ResultsState)
  expect_equal(class(DataModelState$df), "data.frame") |> print()
  expect_equal(nrow(DataModelState$df), 5) |> print()
  expect_equal(ncol(DataModelState$df), 2) |> print()

  # Test 2: Valid input with a CSV file (comma-separated)
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(NULL)
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(NULL)
  test_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:5, b = letters[1:5]), test_file, row.names = FALSE)
  OpenStats:::readData(test_file, DataModelState, ResultsState)
  expect_equal(class(DataModelState$df), "data.frame") |> print()
  expect_equal(nrow(DataModelState$df), 5) |> print()
  expect_equal(ncol(DataModelState$df), 2) |> print()

  # Test 3: File exceeds size limit
  test_file <- tempfile()
  write.csv(data.frame(a = 1:(50 * 1024^2 / 2)), test_file, row.names = FALSE)
  expect_error(OpenStats:::readData(test_file), "File size exceeds the 50 MB limit.")

  # Test 4: File with unknown separator
  test_file <- tempfile()
  writeLines("a|b|c\n1|2|3", test_file)
  expect_error(
    OpenStats:::readData(test_file),
    "Could not identify the separator. Please upload a file with a known separator."
  )

  # Test 5: File with semicolon separator
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(NULL)
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(NULL)
  test_file <- tempfile()
  writeLines("a;b;c\n1;2;3", test_file)
  result <- OpenStats:::readData(test_file, DataModelState, ResultsState)
  expect_equal(class(DataModelState$df), "data.frame") |> print()
  expect_equal(nrow(DataModelState$df), 2) |> print()
  expect_equal(ncol(DataModelState$df), 3) |> print()

  # Test 6: File with tab separator
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(NULL)
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(NULL)
  test_file <- tempfile()
  writeLines("a\tb\tc\n1\t2\t3", test_file)
  result <- OpenStats:::readData(test_file, DataModelState, ResultsState)
  expect_equal(class(DataModelState$df), "data.frame") |> print()
  expect_equal(nrow(DataModelState$df), 2) |> print()
  expect_equal(ncol(DataModelState$df), 3) |> print()

  # Test 7: File with invalid path
  expect_error(OpenStats:::readData("nonexistent_file.csv"), "File does not exists") |> print()

  # Test 8: Data exceeds row or column limits
  test_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(matrix(1, nrow = 1e6 + 1, ncol = 2)), test_file, row.names = FALSE)
  expect_error(OpenStats:::readData(test_file, DataModelState, ResultsState), "Data exceeds the limit of") |> print()

  write.csv(data.frame(matrix(1, nrow = 10, ncol = 1001)), test_file, row.names = FALSE)
  expect_error(OpenStats:::readData(test_file, DataModelState, ResultsState), "Data exceeds the limit of") |> print()

  # Test 9: Empty file
  test_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(), test_file, row.names = FALSE)
  expect_error(
    OpenStats:::readData(test_file, DataModelState, ResultsState),
    "Could not identify the separator. Please upload a file with a known separator."
  ) |> print()

  # Test 10: Non-character input for path
  expect_error(OpenStats:::readData(123), "is.character") |> print()
}
test_readData()
