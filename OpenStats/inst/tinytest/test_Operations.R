library(OpenStats)

# Test Mean, env_operations_V1_2$Max, sd, sum, max and min
# =======================================================================================
test_Mean <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$Mean(c(1, 2, 3, 4, 5)), 3, info = "OpenStats:::env_operations_V1_2$Mean should return 3 for the vector c(1, 2, 3, 4, 5)"
  )
  # Test with non-numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$Mean(c("1", "2", "3", "4", "5")) , 3, info = "OpenStats:::env_operations_V1_2$Mean should convert character input to numeric and return 3"
  )
  # Test with NA values
  expect_equal(
    OpenStats:::env_operations_V1_2$Mean(c(1, 2, 3, NA, 5)), 2.75, info = "OpenStats:::Mean should return 2.75 when NA values are present"
  )
}
test_Mean()

test_Median <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$Median(c(1, 2, 3, 4, 5)), 3, info = "env_operations_V1_2$Median should return 3 for the vector c(1, 2, 3, 4, 5)"
  )
  # Test with non-numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$Median(c("1", "2", "3", "4", "5")) , 3, info = "env_operations_V1_2$Median should convert character input to numeric and return 3"
  )
  # Test with NA values
  expect_equal(
    OpenStats:::env_operations_V1_2$Median(c(1, 2, 3, NA, 5)), 2.5, info = "env_operations_V1_2$Median should return 2.5 when NA values are present"
  )
}
test_Median()

test_SD <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$SD(c(1, 2, 3, 4, 5)), sd(1:5), info = "env_operations_V1_2$SD should return the correct standard deviation for the vector c(1, 2, 3, 4, 5)"
  )
  # Test with non-numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$SD(c("1", "2", "3", "4", "5")) , sd(1:5), info = "env_operations_V1_2$SD should convert character input to numeric and return the correct env_operations_V1_2$SD"
  )
  # Test with NA values
  expect_equal(
    OpenStats:::env_operations_V1_2$SD(c(1, 2, 3, NA, 5)), sd(c(1, 2, 3, 5)), info = "env_operations_V1_2$SD should return the correct env_operations_V1_2$SD when NA values are present"
  )
}
test_SD()

test_Sum <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$Sum(c(1, 2, 3, 4, 5)), 15, info = "env_operations_V1_2$Sum should return 15 for the vector c(1, 2, 3, 4, 5)"
  )
  # Test with non-numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$Sum(c("1", "2", "3", "4", "5")) , 15, info = "env_operations_V1_2$Sum should convert character input to numeric and return 15"
  )
  # Test with NA values
  expect_equal(
    OpenStats:::env_operations_V1_2$Sum(c(1, 2, 3, NA, 5)), 11, info = "env_operations_V1_2$Sum should return 11 when NA values are present"
  )
}
test_Sum()

test_Min <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$Min(c(1, 2, 3, 4, 5)), 1, info = "env_operations_V1_2$Min should return 1 for the vector c(1, 2, 3, 4, 5)"
  )
  # Test with non-numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$Min(c("1", "2", "3", "4", "5")) , 1, info = "env_operations_V1_2$Min should convert character input to numeric and return 1"
  )
  # Test with NA values
  expect_equal(
    OpenStats:::env_operations_V1_2$Min(c(1, 2, 3, NA, 5)), 1, info = "env_operations_V1_2$Min should return 1 when NA values are present"
  )
}
test_Min()

test_Max <- function() {
  # Test with numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$Max(c(1, 2, 3, 4, 5)), 5, info = "env_operations_V1_2$Max should return 5 for the vector c(1, 2, 3, 4, 5)"
  )
  # Test with non-numeric input
  expect_equal(
    OpenStats:::env_operations_V1_2$Max(c("1", "2", "3", "4", "5")) , 5, info = "env_operations_V1_2$Max should convert character input to numeric and return 5"
  )
  # Test with NA values
  expect_equal(
    OpenStats:::env_operations_V1_2$Max(c(1, 2, 3, NA, 5)), 5, info = "env_operations_V1_2$Max should return 5 when NA values are present"
  )
}
test_Max()
# Test rng stuff
# =======================================================================================
test_Rnorm <- function() {
  # Test 1: Basic functionality for env_operations_V1_2$Rnorm
  result <- OpenStats:::env_operations_V1_2$Rnorm(10)
  expect_equal(
    length(result), 10, info = "OpenStats:::env_operations_V1_2$Rnorm should generate 10 random values"
  )
  # Test 2: Large sequence
  expect_error(OpenStats:::env_operations_V1_2$Rnorm(10^9), "The size of the sequence is too large", 
               info = "OpenStats:::env_operations_V1_2$Rnorm should throw an error if the sequence exceeds the size limit"
  )
  # Test 3: Non-integer `n`
  result <- OpenStats:::env_operations_V1_2$Rnorm(10.7)
  expect_equal(length(result), 10, info = "OpenStats:::env_operations_V1_2$Rnorm should round down non-integer `n` values to integers"
  )
  # Test 4: Edge case: `n` is length of input
  expect_error(
    OpenStats:::env_operations_V1_2$Rnorm(c(1, 2, 3, 4, 5)),
    info = "OpenStats:::env_operations_V1_2$Rnorm should throw an error if a vector with length > 1 is used for n"
  )
  # Test 5: Edge case: `n` is a dataframe
  expect_error(
    OpenStats:::env_operations_V1_2$Rnorm(data.frame(x = 1)),
    info = "OpenStats:::env_operations_V1_2$Rnorm should throw an error if a vector with length > 1 is used for n"
  )
}
test_Rbinom <- function() {
  # Test 1: Basic functionality for env_operations_V1_2$Rbinom
  result <- OpenStats:::env_operations_V1_2$Rbinom(10, 1, 0.1)
  expect_equal(
    length(result), 10, info = "OpenStats:::env_operations_V1_2$Rbinom should generate 10 random values"
  )
  # Test 2: Large sequence
  expect_error(OpenStats:::env_operations_V1_2$Rbinom(10^9, 1, 0.1), "The size of the sequence is too large", 
               info = "OpenStats:::env_operations_V1_2$Rbinom should throw an error if the sequence exceeds the size limit"
  )
  # Test 3: Non-integer `n`
  result <- OpenStats:::env_operations_V1_2$Rbinom(10.7, 1, 0.1)
  expect_equal(length(result), 10, info = "OpenStats:::env_operations_V1_2$Rbinom should round down non-integer `n` values to integers"
  )
  # Test 4: Edge case: `n` is length of input
  expect_error(
    OpenStats:::env_operations_V1_2$Rbinom(c(1, 2, 3, 4, 5), 1, 0.1),
    info = "OpenStats:::env_operations_V1_2$Rbinom should throw an error if a vector with length > 1 is used for n"
  )
  # Test 5: Edge case: `n` is a dataframe
  expect_error(
    OpenStats:::env_operations_V1_2$Rbinom(data.frame(x = 1)),
    info = "OpenStats:::env_operations_V1_2$Rbinom should throw an error if a vector with length > 1 is used for n"
  )
}
test_Rpois <- function() {
  # Test 1: Basic functionality for Rpois
  result <- OpenStats:::env_operations_V1_2$Rpois(10, 1)
  expect_equal(
    length(result), 10, info = "OpenStats:::env_operations_V1_2$Rpois should generate 10 random values"
  )
  # Test 2: Large sequence
  expect_error(OpenStats:::env_operations_V1_2$Rpois(10^9, 1), "The size of the sequence is too large", 
               info = "OpenStats:::env_operations_V1_2$Rpois should throw an error if the sequence exceeds the size limit"
  )
  # Test 3: Non-integer `n`
  result <- OpenStats:::env_operations_V1_2$Rpois(10.7, 1)
  expect_equal(length(result), 10, info = "OpenStats:::env_operations_V1_2$Rpois should round down non-integer `n` values to integers"
  )
  # Test 4: Edge case: `n` is length of input
  expect_error(
    OpenStats:::env_operations_V1_2$Rpois(c(1, 2, 3, 4, 5), 1),
    info = "OpenStats:::env_operations_V1_2$Rpois should throw an error if a vector with length > 1 is used for n"
  )
  # Test 5: Edge case: `n` is a dataframe
  expect_error(
    OpenStats:::env_operations_V1_2$Rpois(data.frame(x = 1)),
    info = "OpenStats:::env_operations_V1_2$Rpois should throw an error if a vector with length > 1 is used for n"
  )
}
test_Runif <- function() {
  # Test 1: Basic functionality for env_operations_V1_2$Runif
  result <- OpenStats:::env_operations_V1_2$Runif(10)
  expect_equal(
    length(result), 10, info = "OpenStats:::env_operations_V1_2$Runif should generate 10 random values"
  )
  # Test 2: Large sequence
  expect_error(OpenStats:::env_operations_V1_2$Runif(10^9), "The size of the sequence is too large", 
               info = "OpenStats:::env_operations_V1_2$Runif should throw an error if the sequence exceeds the size limit"
  )
  # Test 3: Non-integer `n`
  result <- OpenStats:::env_operations_V1_2$Runif(10.7)
  expect_equal(length(result), 10, info = "OpenStats:::env_operations_V1_2$Runif should round down non-integer `n` values to integers"
  )
  # Test 4: Edge case: `n` is length of input
  expect_error(
    OpenStats:::env_operations_V1_2$Runif(c(1, 2, 3, 4, 5)),
    info = "OpenStats:::env_operations_V1_2$Runif should throw an error if a vector with length > 1 is used for n"
  )
  # Test 5: Edge case: `n` is a dataframe
  expect_error(
    OpenStats:::env_operations_V1_2$Runif(data.frame(x = 1)),
    info = "OpenStats:::env_operations_V1_2$Runif should throw an error if a vector with length > 1 is used for n"
  )
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
  result <- OpenStats:::env_operations_V1_2$Seq(start, end, by)
  expect_equal(
    result, expected_result, info = "OpenStats:::env_operations_V1_2$Seq should generate a sequence from start to end with the given step size"
  )
  # Test 2: Large sequence
  # Here, we'll make a sequence that exceeds the 100MB limit
  expect_error(
    OpenStats:::env_operations_V1_2$Seq(1, 10^8, 1), "The size of the sequence is too large", 
               info = "OpenStats:::env_operations_V1_2$Seq should throw an error if the sequence exceeds the size limit"
  )
  # Test 3: Negative step size
  start <- 10
  end <- 1
  by <- -2
  expected_result <- seq(10, 1, by = -2)
  result <- OpenStats:::env_operations_V1_2$Seq(start, end, by)
  expect_equal(
    result, expected_result, info = "OpenStats:::env_operations_V1_2$Seq should handle negative step sizes correctly"
  )
  # Test 4: OpenStats:::Sequence length calculation
  start <- 1
  end <- 10
  by <- 2
  expected_length <- 5  # The sequence should have 5 elements: 1, 3, 5, 7, 9
  result <- OpenStats:::env_operations_V1_2$Seq(start, end, by)
  expect_equal(
    length(result), expected_length, info = "OpenStats:::env_operations_V1_2$Seq should generate the correct number of elements"
  )
  # Test 5: Edge case: start equals end
  start <- 5
  end <- 5
  by <- 1
  expected_result <- seq(5, 5, by = 1)  # The sequence will just be [5]
  result <- OpenStats:::env_operations_V1_2$Seq(start, end, by)
  expect_equal(
    result, expected_result, info = "OpenStats:::env_operations_V1_2$Seq should handle the case where start equals end"
  )
}
test_Seq()

# Test env_operations_V1_2$DataFrame
# =======================================================================================
test_DataFrame <- function() {
  # Test 1: Basic functionality
  col1 <- c(1, 2, 3)
  col2 <- c("A", "B", "C")
  expected_result <- data.frame(col1 = as.character(c(1, 2, 3)), col2 = c("A", "B", "C"))
  result <- OpenStats:::env_operations_V1_2$DataFrame(col1, col2)
  expect_equal(
    result, expected_result, info = "OpenStats:::DataFrame should create a data frame from two vectors"
  )
  # Test 2: Empty column test
  col1 <- c("1", "2", "3")
  col2 <- c()
  expect_error(
    OpenStats:::env_operations_V1_2$DataFrame(col1, col2), "Found empty column", 
               info = "OpenStats:::env_operations_V1_2$DataFrame should throw an error if a column is empty"
  )
  # Test 3: OpenStats:::Data frame size limit test
  large_col <- rep(1, 10^8)
  expect_error(
    OpenStats:::env_operations_V1_2$DataFrame(large_col, large_col), "The total size of the data frame is too large",
               info = "OpenStats:::env_operations_V1_2$DataFrame should throw an error if the total size exceeds 100MB"
  )
  # Test 4: Column name handling
  col1 <- c(1, 2, 3)
  col2 <- c("A", "B", "C")
  expected_names <- c("col1", "col2")
  result <- OpenStats:::env_operations_V1_2$DataFrame(col1, col2)
  expect_equal(
    names(result), expected_names, info = "OpenStats:::env_operations_V1_2$DataFrame should use the variable names as column names"
  )
  # Test 5: Column length mismatch, elongation
  col1 <- c(1, 2)
  col2 <- c("A", "B", "C")
  expected_result <- data.frame(col1 = c("1", "2", "1"), col2 = c("A", "B", "C"))
  result <- OpenStats:::env_operations_V1_2$DataFrame(col1, col2)
  expect_equal(
    result, expected_result, info = "OpenStats:::env_operations_V1_2$DataFrame should elongate shorter columns to match the longest column"
  )
}
test_DataFrame()

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
  result <- OpenStats:::env_operations_V1_2$get_rows(df, df$A > 2)
  expect_equal(
    result,
    subset(df, A > 2),
    info = "OpenStats:::get_rows should correctly filter rows where A > 2"
  )

  result_mult_cond <- OpenStats:::env_operations_V1_2$get_rows(df, df$A > 2 & df$B < 8)
  expect_equal(
    result_mult_cond,
    subset(df, A > 2 & B < 8),
    info = "OpenStats:::get_rows should correctly filter rows with multiple conditions"
  )

  result_char <- OpenStats:::env_operations_V1_2$get_rows(df, df$C == "x")
  expect_equal(
    result_char,
    subset(df, C == "x"),
    info = "OpenStats:::get_rows should correctly filter rows with character comparisons"
  )

  # Test empty result
  result_empty <- OpenStats:::env_operations_V1_2$get_rows(df, df$A > 10)
  expect_equal(
    result_empty,
    subset(df, A > 10),
    info = "OpenStats:::get_rows should return an empty data frame if no rows match"
  )

  # Edge cases
  empty_df <- data.frame(A = numeric(), B = numeric())
  result_empty_df <- OpenStats:::env_operations_V1_2$get_rows(empty_df, empty_df$A > 0)
  expect_equal(
    result_empty_df,
    subset(empty_df, A > 0),
    info = "OpenStats:::get_rows should return an empty data frame for an empty input data frame"
  )

  # Test error cases
  expect_error(
    OpenStats:::env_operations_V1_2$get_rows(42, dfA > 2),
    info = "OpenStats:::get_rows should throw an error when input is not a data frame"
  )
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
  result <- OpenStats:::env_operations_V1_2$get_cols(df, A, B)
  expect_equal(
    result,
    df[, c("A", "B")],
    info = "get_cols should return the correct subset of columns"
  )
 
  result_single <- OpenStats:::env_operations_V1_2$get_cols(df, C)
  expect_equal(
    result_single,
    df[, "C", drop = TRUE],
    info = "get_cols should work for a single column selection"
  )

  # Test with all columns
  result_all <- OpenStats:::env_operations_V1_2$get_cols(df, A, B, C)
  expect_equal(
    result_all,
    df,
    info = "get_cols should return the entire data frame when all columns are selected"
  )

  # Test with reordered columns
  result_reordered <- OpenStats:::env_operations_V1_2$get_cols(df, C, A)
  expect_equal(
    result_reordered,
    df[, c("C", "A")],
    info = "get_cols should respect the order of columns provided in the arguments"
  )

  # Test error cases
  expect_error(
    OpenStats:::env_operations_V1_2$get_cols(df, D),
    info = "get_cols should throw an error when a non-existent column is requested"
  )
  expect_error(
    OpenStats:::env_operations_V1_2$get_cols(df),
    info = "get_cols should throw an error when no columns are specified"
  )
  expect_error(
    OpenStats:::env_operations_V1_2$get_cols(42, A),
    info = "get_cols should throw an error when the input is not a data frame"
  )

  # Edge cases
  empty_df <- data.frame()
  expect_error(
    OpenStats:::env_operations_V1_2$get_cols(empty_df, A),
    info = "get_cols should throw an error when accessing a column in an empty data frame"
  )

  single_col_df <- data.frame(A = c(1, 2, 3))
  result_single_col <- OpenStats:::env_operations_V1_2$get_cols(single_col_df, A)
  expect_equal(
    result_single_col,
    single_col_df[, 1],
    info = "get_cols should work correctly for a single-column data frame"
  )
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
    OpenStats:::env_operations_V1_2$get_elem(df, 2, 1),
    df[2, 1],
    info = "get_elem should return the correct element for data frame with row and column indices"
  )
  expect_equal(
    OpenStats:::env_operations_V1_2$get_elem(df, 3, 2),
    df[3, 2],
    info = "get_elem should return the correct element for another valid data frame case"
  )

  # Test valid cases for vector
  expect_equal(
    OpenStats:::env_operations_V1_2$get_elem(vec, 3),
    vec[3],
    info = "get_elem should return the correct element for vector with a single index"
  )
  expect_equal(
    OpenStats:::env_operations_V1_2$get_elem(vec, 1),
    vec[1],
    info = "get_elem should work with another valid index for vector"
  )

  # Test error cases
  expect_error(
    OpenStats:::env_operations_V1_2$get_elem(df, 1),
    info = "get_elem should throw an error when only one index is provided for a data frame"
  )
  expect_error(
    OpenStats:::env_operations_V1_2$get_elem(vec, 1, 2),
    info = "get_elem should throw an error when two indices are provided for a vector"
  )
  expect_error(
    OpenStats:::env_operations_V1_2$get_elem(df, "row", "col"),
    info = "get_elem should throw an error when non-numeric indices are provided for a data frame"
  )
  expect_error(
    OpenStats:::env_operations_V1_2$get_elem(vec, "index"),
    info = "get_elem should throw an error when non-numeric index is provided for a vector"
  )
  expect_error(
    OpenStats:::env_operations_V1_2$get_elem(df, 2, 1, 3),
    info = "get_elem should throw an error when more than two indices are provided"
  )
  expect_error(
    OpenStats:::env_operations_V1_2$get_elem(vec),
    info = "get_elem should throw an error when no index is provided"
  )

  # Test edge cases
  empty_df <- data.frame()
  expect_error(
    OpenStats:::env_operations_V1_2$get_elem(empty_df, 1, 1),
    info = "get_elem should throw an error when accessing an element in an empty data frame"
  )

  empty_vec <- numeric()
  expect_error(
    OpenStats:::env_operations_V1_2$get_elem(empty_vec, 1),
    info = "get_elem should throw an error when accessing an element in an empty vector"
  )
}
test_get_elem()

