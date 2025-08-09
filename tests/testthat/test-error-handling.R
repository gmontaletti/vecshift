# Test error handling and data validation in vecshift function

test_that("vecshift throws error for non-data.table input", {
  # Arrange
  invalid_data <- generate_invalid_data("not_datatable")
  
  # Act & Assert
  expect_error(
    vecshift(invalid_data),
    "Input 'dt' must be a data.table object"
  )
})

test_that("vecshift throws error for missing required columns", {
  # Arrange
  invalid_data <- generate_invalid_data("missing_columns")
  
  # Act & Assert
  expect_error(
    vecshift(invalid_data),
    "Missing required columns"
  )
})

test_that("vecshift throws error for wrong column types", {
  # Arrange - Test invalid INIZIO type
  invalid_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = "not-a-date",  # Invalid type
    FINE = as.Date("2023-12-31"),
    prior = 1L
  )
  
  # Act & Assert
  expect_error(
    vecshift(invalid_data),
    "Column 'INIZIO' must be numeric or Date type"
  )
})

test_that("vecshift throws error for invalid FINE column type", {
  # Arrange
  invalid_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2023-01-01"),
    FINE = "not-a-date",  # Invalid type
    prior = 1L
  )
  
  # Act & Assert
  expect_error(
    vecshift(invalid_data),
    "Column 'FINE' must be numeric or Date type"
  )
})

test_that("vecshift throws error for non-numeric prior column", {
  # Arrange
  invalid_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2023-01-01"),
    FINE = as.Date("2023-12-31"),
    prior = "not-numeric"  # Invalid type
  )
  
  # Act & Assert
  expect_error(
    vecshift(invalid_data),
    "Column 'prior' must be numeric"
  )
})

test_that("vecshift warns about invalid date ranges", {
  # Arrange
  invalid_data <- generate_invalid_data("invalid_dates")
  
  # Act & Assert
  expect_warning(
    vecshift(invalid_data),
    "Some records have FINE < INIZIO"
  )
})

test_that("vecshift handles NA values appropriately", {
  # Arrange
  test_data <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON002"),
    INIZIO = as.Date(c("2023-01-01", NA)),
    FINE = as.Date(c("2023-12-31", "2023-12-31")),
    prior = c(1L, NA_integer_)
  )
  
  # Act - Should not throw error, but may produce unexpected results
  result <- expect_silent(vecshift(test_data))
  
  # Assert - Check that function completes without error
  expect_s3_class(result, "data.table")
  # Note: The behavior with NA values may vary, but function should not crash
})

test_that("vecshift handles empty required column names", {
  # Arrange - Create data.table with wrong column names
  invalid_data <- data.table::data.table(
    wrong_id = 1L,
    wrong_cf = "PERSON001",
    wrong_start = as.Date("2023-01-01"),
    wrong_end = as.Date("2023-12-31"),
    wrong_prior = 1L
  )
  
  # Act & Assert
  expect_error(
    vecshift(invalid_data),
    "Missing required columns"
  )
})

test_that("vecshift handles numeric dates correctly", {
  # Arrange - Use numeric dates (common in some systems)
  numeric_date_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.numeric(as.Date("2023-01-01")),  # Numeric date
    FINE = as.numeric(as.Date("2023-12-31")),    # Numeric date
    prior = 1L
  )
  
  # Act - Should not throw error as numeric dates are allowed
  result <- expect_silent(vecshift(numeric_date_data))
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 0)
})

test_that("vecshift input validation covers all required columns", {
  # Test that all required columns are actually checked
  required_columns <- c("id", "cf", "INIZIO", "FINE", "prior")
  
  for (col in required_columns) {
    # Arrange - Create data missing just this column
    complete_data <- data.table::data.table(
      id = 1L,
      cf = "PERSON001", 
      INIZIO = as.Date("2023-01-01"),
      FINE = as.Date("2023-12-31"),
      prior = 1L
    )
    
    # Remove the specific column
    complete_data[[col]] <- NULL
    
    # Act & Assert
    expect_error(
      vecshift(complete_data),
      paste("Missing required columns.*", col),
      info = paste("Failed to detect missing column:", col)
    )
  }
})

test_that("vecshift handles mixed valid and invalid dates", {
  # Arrange - Some valid dates, some with end before start
  mixed_data <- data.table::data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-06-01", "2023-12-01")),
    FINE = as.Date(c("2023-05-31", "2023-03-01", "2023-12-31")),  # Second one is invalid
    prior = c(1L, 1L, 1L)
  )
  
  # Act & Assert - Should warn but not error
  expect_warning(
    result <- vecshift(mixed_data),
    "Some records have FINE < INIZIO"
  )
  
  # Function should still complete
  expect_s3_class(result, "data.table")
})