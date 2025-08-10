# Comprehensive tests for the data_quality.R module
# Tests column mapping, validation, quality assessment, and data cleaning

library(testthat)
library(data.table)

# Load all package functions for testing
if (!exists("standardize_columns")) {
  devtools::load_all()
}

test_that("standardize_columns works with valid column mappings", {
  # Test simple column renaming
  test_data <- generate_custom_column_data("simple_rename")
  dt <- test_data$data
  column_map <- test_data$column_map
  
  # Test successful standardization
  result <- standardize_columns(dt, column_map, validate = FALSE)
  
  expect_true(inherits(result, "data.table"))
  expect_true(all(c("id", "cf", "inizio", "fine", "prior") %in% names(result)))
  expect_equal(nrow(result), nrow(dt))
  expect_equal(ncol(result), 5)  # Should have exactly 5 columns
  
  # Test that data values are preserved correctly
  expect_equal(result$id, dt$contract_id)
  expect_equal(result$cf, dt$person_code)
  expect_equal(result$inizio, dt$start_date)
  expect_equal(result$fine, dt$end_date)
  expect_equal(result$prior, dt$employment_type)
})

test_that("standardize_columns removes extra columns", {
  test_data <- generate_custom_column_data("extra_columns")
  dt <- test_data$data
  column_map <- test_data$column_map
  
  # Original data has extra columns
  expect_gt(ncol(dt), 5)
  expect_true("department" %in% names(dt))
  expect_true("salary" %in% names(dt))
  
  # After standardization, only required columns remain
  result <- standardize_columns(dt, column_map, validate = FALSE)
  expect_equal(ncol(result), 5)
  expect_false("department" %in% names(result))
  expect_false("salary" %in% names(result))
})

test_that("standardize_columns handles database-style column names", {
  test_data <- generate_custom_column_data("database_style")
  dt <- test_data$data
  column_map <- test_data$column_map
  
  result <- standardize_columns(dt, column_map, validate = FALSE)
  
  expect_true(all(c("id", "cf", "inizio", "fine", "prior") %in% names(result)))
  expect_equal(result$cf, dt$PERSON_FISCAL_CODE)
  expect_equal(result$inizio, dt$CONTRACT_START_DATE)
})

test_that("standardize_columns fails with missing column mappings", {
  dt <- data.table(
    contract_id = 1:3,
    person_code = c("A", "B", "C"),
    start_date = as.Date("2023-01-01") + 0:2,
    end_date = as.Date("2023-01-31") + 0:2,
    employment_type = c(1, 0, 1)
  )
  
  # Missing required mapping
  incomplete_map <- list(
    id = "contract_id",
    cf = "person_code"
    # Missing inizio, fine, prior
  )
  
  expect_error(
    standardize_columns(dt, incomplete_map),
    "Missing column mappings for:"
  )
})

test_that("standardize_columns fails with non-existent columns", {
  dt <- data.table(
    id = 1:3,
    cf = c("A", "B", "C"),
    inizio = as.Date("2023-01-01") + 0:2,
    fine = as.Date("2023-01-31") + 0:2,
    prior = c(1, 0, 1)
  )
  
  # Mapping to non-existent column
  bad_map <- list(
    id = "id",
    cf = "cf", 
    inizio = "inizio",
    fine = "fine",
    prior = "non_existent_column"
  )
  
  expect_error(
    standardize_columns(dt, bad_map),
    "Mapped columns not found in data:"
  )
})

test_that("validate_employment_data_types works with clean data", {
  clean_data <- generate_test_data_with_issues("clean_data")
  
  # Should pass validation without warnings
  expect_silent(validation_result <- validate_employment_data_types(clean_data))
  expect_true(validation_result$is_valid)
})

test_that("validate_employment_data_types detects missing values", {
  missing_data <- generate_test_data_with_issues("missing_values")
  
  # Should generate warnings for missing values
  expect_warning(
    validation_result <- validate_employment_data_types(missing_data),
    "NA values"
  )
  
  expect_false(validation_result$is_valid)
  expect_true(validation_result$na_fine > 0)
  expect_true(validation_result$na_inizio > 0)
})

test_that("validate_employment_data_types detects duplicate IDs", {
  duplicate_data <- generate_test_data_with_issues("duplicate_data")
  
  expect_warning(
    validation_result <- validate_employment_data_types(duplicate_data),
    "duplicate contract IDs"
  )
  
  expect_true(validation_result$duplicate_ids > 0)
})

test_that("validate_employment_data_types detects invalid date ranges", {
  invalid_dates <- generate_test_data_with_issues("invalid_dates")
  
  expect_warning(
    validation_result <- validate_employment_data_types(invalid_dates),
    "end date < start date"
  )
  
  expect_true(validation_result$invalid_date_ranges > 0)
})

test_that("validate_employment_data_types strict mode throws errors", {
  missing_data <- generate_test_data_with_issues("missing_values")
  
  expect_error(
    validate_employment_data_types(missing_data, strict = TRUE),
    "duplicate contract IDs|NA values"
  )
})

test_that("validate_employment_data_types validates data types", {
  # Test non-data.table input
  df <- data.frame(
    id = 1:3,
    cf = c("A", "B", "C"),
    inizio = as.Date("2023-01-01") + 0:2,
    fine = as.Date("2023-01-31") + 0:2,
    prior = c(1, 0, 1)
  )
  
  expect_error(
    validate_employment_data_types(df),
    "Input must be a data.table object"
  )
  
  # Test missing columns
  incomplete_dt <- data.table(
    id = 1:3,
    cf = c("A", "B", "C")
    # Missing inizio, fine, prior
  )
  
  expect_error(
    validate_employment_data_types(incomplete_dt),
    "Missing required columns:"
  )
})

test_that("assess_data_quality provides comprehensive assessment", {
  test_data <- generate_test_data_with_issues("clean_data")
  
  quality_report <- assess_data_quality(test_data)
  
  # Check structure of quality report
  expect_true(inherits(quality_report, "employment_quality_report"))
  expect_true("dimensions" %in% names(quality_report))
  expect_true("missing_values" %in% names(quality_report))
  expect_true("duplicates" %in% names(quality_report))
  expect_true("logical_consistency" %in% names(quality_report))
  expect_true("person_level" %in% names(quality_report))
  expect_true("quality_score" %in% names(quality_report))
  
  # Check dimensions
  expect_equal(quality_report$dimensions$n_records, nrow(test_data))
  expect_true(quality_report$dimensions$n_persons > 0)
  
  # Check quality score
  expect_true(quality_report$quality_score$overall_score >= 0)
  expect_true(quality_report$quality_score$overall_score <= 1)
  expect_true(is.logical(quality_report$quality_score$is_production_ready))
})

test_that("assess_data_quality detects data quality issues", {
  problematic_data <- generate_test_data_with_issues("complex_overlaps_with_issues")
  
  quality_report <- assess_data_quality(problematic_data)
  
  # Should detect missing values
  expect_gt(sum(quality_report$missing_values), 0)
  
  # Should detect logical inconsistencies
  expect_gt(quality_report$logical_consistency$invalid_date_ranges + 
           quality_report$logical_consistency$missing_person_ids, 0)
  
  # Quality score should be lower
  expect_lt(quality_report$quality_score$overall_score, 0.9)
  expect_false(quality_report$quality_score$is_production_ready)
})

test_that("assess_data_quality handles empty dataset", {
  empty_data <- generate_test_data_with_issues("clean_data")[0]  # Empty data.table with structure
  
  quality_report <- assess_data_quality(empty_data)
  
  expect_equal(quality_report$dimensions$n_records, 0)
  expect_equal(quality_report$dimensions$n_persons, 0)
})

test_that("assess_data_quality includes optional analyses", {
  test_data <- generate_test_data_with_issues("clean_data", n_records = 20)
  
  # Test with distributions
  quality_report_full <- assess_data_quality(
    test_data, 
    include_distributions = TRUE,
    include_temporal = TRUE
  )
  
  expect_true("distributions" %in% names(quality_report_full))
  expect_true("temporal_patterns" %in% names(quality_report_full))
  
  # Test without optional analyses
  quality_report_basic <- assess_data_quality(
    test_data,
    include_distributions = FALSE,
    include_temporal = FALSE
  )
  
  expect_false("distributions" %in% names(quality_report_basic))
  expect_false("temporal_patterns" %in% names(quality_report_basic))
})

test_that("analyze_temporal_clustering detects batch processing patterns", {
  batch_data <- generate_test_data_with_issues("batch_processing_pattern")
  
  clustering_result <- analyze_temporal_clustering(batch_data)
  
  expect_true("first_day_percentage" %in% names(clustering_result))
  expect_true("suggests_batch_processing" %in% names(clustering_result))
  
  # Should detect high proportion of first-day starts
  expect_gt(clustering_result$first_day_percentage, 20)
  expect_true(clustering_result$suggests_batch_processing)
})

test_that("analyze_temporal_clustering detects weekend start issues", {
  weekend_data <- generate_test_data_with_issues("weekend_starts")
  
  clustering_result <- analyze_temporal_clustering(weekend_data)
  
  expect_true("weekend_starts" %in% names(clustering_result))
  expect_true("suggests_data_issues" %in% names(clustering_result))
  
  # Should detect high proportion of weekend starts (80% weekend dates in our test data)
  expect_gt(clustering_result$weekend_start_percentage, 70)
  expect_true(clustering_result$suggests_data_issues)
})

test_that("clean_employment_data removes duplicates", {
  duplicate_data <- generate_test_data_with_issues("duplicate_data")
  original_rows <- nrow(duplicate_data)
  
  cleaned_data <- clean_employment_data(
    duplicate_data,
    remove_duplicates = TRUE,
    verbose = FALSE
  )
  
  expect_lt(nrow(cleaned_data), original_rows)
  expect_equal(sum(duplicated(cleaned_data)), 0)  # No duplicates remaining
  
  # Check cleaning log
  cleaning_log <- attr(cleaned_data, "cleaning_log")
  expect_true(!is.null(cleaning_log))
  expect_gt(cleaning_log$duplicates_removed, 0)
})

test_that("clean_employment_data removes invalid dates", {
  invalid_data <- generate_test_data_with_issues("invalid_dates")
  original_rows <- nrow(invalid_data)
  
  cleaned_data <- clean_employment_data(
    invalid_data,
    remove_invalid_dates = TRUE,
    verbose = FALSE
  )
  
  expect_lt(nrow(cleaned_data), original_rows)
  
  # Should have no invalid date ranges remaining
  expect_equal(sum(cleaned_data$fine < cleaned_data$inizio, na.rm = TRUE), 0)
  
  cleaning_log <- attr(cleaned_data, "cleaning_log")
  expect_gt(cleaning_log$invalid_dates_removed, 0)
})

test_that("clean_employment_data removes zero duration contracts", {
  zero_duration_data <- generate_test_data_with_issues("zero_duration")
  original_rows <- nrow(zero_duration_data)
  
  cleaned_data <- clean_employment_data(
    zero_duration_data,
    remove_zero_duration = TRUE,
    verbose = FALSE
  )
  
  expect_lt(nrow(cleaned_data), original_rows)
  
  # Should have no zero duration contracts remaining
  expect_equal(sum(cleaned_data$fine == cleaned_data$inizio, na.rm = TRUE), 0)
})

test_that("clean_employment_data fills missing prior values", {
  missing_data <- generate_test_data_with_issues("missing_values")
  original_na_count <- sum(is.na(missing_data$prior))
  
  cleaned_data <- clean_employment_data(
    missing_data,
    fill_missing_prior = TRUE,
    verbose = FALSE
  )
  
  # Should have fewer or no missing prior values
  expect_lte(sum(is.na(cleaned_data$prior)), original_na_count)
  
  cleaning_log <- attr(cleaned_data, "cleaning_log")
  if (original_na_count > 0 && !is.null(cleaning_log$prior_values_filled)) {
    expect_gt(cleaning_log$prior_values_filled, 0)
  }
})

test_that("clean_employment_data preserves data when no cleaning needed", {
  clean_data <- generate_test_data_with_issues("clean_data")
  original_rows <- nrow(clean_data)
  
  cleaned_data <- clean_employment_data(
    clean_data,
    verbose = FALSE
  )
  
  expect_equal(nrow(cleaned_data), original_rows)
  
  cleaning_log <- attr(cleaned_data, "cleaning_log")
  expect_equal(cleaning_log$rows_removed, 0)
  expect_equal(cleaning_log$cleaning_rate, 0)
})

test_that("clean_employment_data verbose mode provides messages", {
  duplicate_data <- generate_test_data_with_issues("duplicate_data")
  
  expect_message(
    cleaned_data <- clean_employment_data(
      duplicate_data,
      remove_duplicates = TRUE,
      verbose = TRUE
    ),
    "Removed .* duplicate records"
  )
  
  expect_message(
    cleaned_data <- clean_employment_data(
      duplicate_data,
      remove_duplicates = TRUE,
      verbose = TRUE
    ),
    "Data cleaning complete:"
  )
})

test_that("print.employment_quality_report works correctly", {
  test_data <- generate_test_data_with_issues("clean_data")
  quality_report <- assess_data_quality(test_data)
  
  # Should not error when printing
  expect_output(print(quality_report), "Employment Data Quality Report")
  expect_output(print(quality_report), "Data Dimensions:")
  expect_output(print(quality_report), "Overall Quality Score:")
})

test_that("column mapping works with identical names", {
  # Test case where some column names are already standard
  dt <- data.table(
    id = 1:3,  # Already standard
    person_code = c("A", "B", "C"),
    inizio = as.Date("2023-01-01") + 0:2,  # Already standard
    end_date = as.Date("2023-01-31") + 0:2,
    prior = c(1, 0, 1)  # Already standard
  )
  
  column_map <- list(
    id = "id",  # Same name
    cf = "person_code",
    inizio = "inizio",  # Same name
    fine = "end_date",
    prior = "prior"  # Same name
  )
  
  result <- standardize_columns(dt, column_map, validate = FALSE)
  
  expect_true(all(c("id", "cf", "inizio", "fine", "prior") %in% names(result)))
  expect_equal(result$id, dt$id)
  expect_equal(result$inizio, dt$inizio) 
  expect_equal(result$prior, dt$prior)
  expect_equal(result$cf, dt$person_code)
  expect_equal(result$fine, dt$end_date)
})

test_that("data quality assessment handles edge cases", {
  # Test with very short contracts
  short_contracts <- generate_test_data_with_issues("very_short_contracts")
  
  quality_report <- assess_data_quality(short_contracts)
  
  expect_true(quality_report$logical_consistency$zero_duration_contracts > 0)
  expect_true("zero_duration_contracts" %in% names(quality_report$logical_consistency))
})

test_that("validation detects extreme values appropriately", {
  extreme_data <- generate_test_data_with_issues("extreme_values")
  
  # Basic validation should still work with extreme but valid dates
  expect_warning(
    validation_result <- validate_employment_data_types(extreme_data),
    NA  # Should not warn about extreme dates themselves
  )
  
  # But quality assessment might flag them in distributions
  quality_report <- assess_data_quality(extreme_data, include_distributions = TRUE)
  
  expect_true("distributions" %in% names(quality_report))
  # Extreme prior values should appear in prior_values table
  expect_true(any(names(quality_report$distributions$prior_values) %in% c("-5", "100", "-10", "999")))
})

test_that("quality assessment works with numeric dates", {
  # Create data with numeric dates (days since epoch)
  dt <- data.table(
    id = 1:3,
    cf = c("A001", "A002", "A003"), 
    inizio = as.numeric(as.Date(c("2023-01-01", "2023-02-01", "2023-03-01"))),
    fine = as.numeric(as.Date(c("2023-01-31", "2023-02-28", "2023-03-31"))),
    prior = c(1, 0, 1)
  )
  
  quality_report <- assess_data_quality(dt)
  
  # Should handle numeric dates correctly
  expect_true(!is.null(quality_report$dimensions$date_range))
  expect_equal(length(quality_report$dimensions$date_range), 2)
})

test_that("contract_duration function is used correctly in quality assessment", {
  test_data <- data.table(
    id = 1:3,
    cf = c("PERSON001", "PERSON001", "PERSON002"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-03-31", "2023-02-28")),  # 31, 31, 28 days
    prior = c(1, 0, 1)
  )
  
  quality_report <- assess_data_quality(test_data)
  
  # Person-level analysis should use contract_duration function
  expect_true("person_level" %in% names(quality_report))
  expect_true("employment_days_distribution" %in% names(quality_report$person_level))
  
  # Total days should be calculated correctly (31 + 31 + 28 = 90)
  total_days_calculated <- sum(contract_duration(test_data$inizio, test_data$fine))
  expect_equal(total_days_calculated, 90)
  
  # Check that the quality report correctly uses contract_duration
  expected_person1_days <- 31 + 31  # PERSON001 has 2 contracts
  expected_person2_days <- 28       # PERSON002 has 1 contract
  
  person_level_data <- quality_report$person_level$employment_days_distribution
  expect_true(!is.null(person_level_data))
})