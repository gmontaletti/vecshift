test_that("analyze_employment_transitions handles basic transitions", {
  library(data.table)
  
  # Create sample data with clear transitions
  sample_data <- data.table(
    id = 1:6,
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON002"),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-08-01", 
                       "2023-02-01", "2023-06-01", "2023-10-01")),
    FINE = as.Date(c("2023-02-28", "2023-05-31", "2023-12-31", 
                     "2023-04-30", "2023-08-31", "2023-12-31")),
    prior = c(1, 0, 1, 1, 1, 0),
    company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD", "CompanyE", "CompanyF"),
    salary = c(50000, 25000, 60000, 55000, 65000, 30000)
  )
  
  # Process through pipeline (simulate pipeline result)
  pipeline_result <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON002"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-08-01",
                       "2023-02-01", "2023-05-01", "2023-10-01")),
    fine = as.Date(c("2023-02-28", "2023-07-31", "2023-12-31",
                     "2023-04-30", "2023-09-30", "2023-12-31")),
    arco = c(1, 0, 1, 1, 1, 1),
    durata = c(58, 152, 152, 88, 152, 92),
    company = c("CompanyA", NA, "CompanyC", "CompanyD", "CompanyE", "CompanyF"),
    salary = c(50000, NA, 60000, 55000, 65000, 30000)
  )
  
  # Set merged_columns attribute
  setattr(pipeline_result, "merged_columns", c("company", "salary"))
  
  # Test basic functionality
  result <- analyze_employment_transitions(pipeline_result)
  
  # Should return a data.table
  expect_s3_class(result, "data.table")
  
  # Should have expected columns
  expected_cols <- c("variable", "from", "to", "weight", "transition_duration")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("analyze_employment_transitions handles missing transition_columns", {
  library(data.table)
  
  # Create minimal pipeline result without merged_columns attribute
  pipeline_result <- data.table(
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-04-01")),
    fine = as.Date(c("2023-03-31", "2023-06-30")),
    arco = c(1, 0),
    durata = c(90, 91)
  )
  
  # Should warn and return empty data.table
  expect_warning(
    result <- analyze_employment_transitions(pipeline_result),
    "No transition_columns specified"
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("analyze_employment_transitions validates input parameters", {
  library(data.table)
  
  # Invalid pipeline_result
  expect_error(
    analyze_employment_transitions("not_a_data_table"),
    "must be a data.table object"
  )
  
  # Missing required columns
  invalid_dt <- data.table(x = 1:3)
  expect_error(
    analyze_employment_transitions(invalid_dt),
    "Missing required columns"
  )
  
  # Valid minimal data for further tests
  valid_dt <- data.table(
    cf = "PERSON001",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-12-31"),
    arco = 1,
    durata = 365,
    test_col = "value"
  )
  
  # Invalid transition_columns
  expect_error(
    analyze_employment_transitions(valid_dt, transition_columns = 123),
    "must be a non-empty character vector"
  )
  
  # Non-existent columns
  expect_error(
    analyze_employment_transitions(valid_dt, transition_columns = "nonexistent"),
    "not found in pipeline_result"
  )
  
  # Invalid min_unemployment_duration
  expect_error(
    analyze_employment_transitions(valid_dt, 
                                 transition_columns = "test_col",
                                 min_unemployment_duration = -1),
    "must be a non-negative numeric value"
  )
})

test_that("analyze_employment_transitions return_list parameter works", {
  library(data.table)
  
  # Create data with transitions
  pipeline_result <- data.table(
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")),
    fine = as.Date(c("2023-02-28", "2023-05-31", "2023-12-31")),
    arco = c(1, 0, 1),
    durata = c(58, 92, 214),
    company = c("CompanyA", NA, "CompanyB")
  )
  
  # Test return_list = TRUE
  result_list <- analyze_employment_transitions(
    pipeline_result, 
    transition_columns = "company",
    return_list = TRUE
  )
  
  expect_type(result_list, "list")
  expect_true("company" %in% names(result_list))
  
  # Test return_list = FALSE (default)
  result_combined <- analyze_employment_transitions(
    pipeline_result,
    transition_columns = "company",
    return_list = FALSE
  )
  
  expect_s3_class(result_combined, "data.table")
  if (nrow(result_combined) > 0) {
    expect_true("variable" %in% names(result_combined))
  }
})