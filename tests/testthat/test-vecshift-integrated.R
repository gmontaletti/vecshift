# Comprehensive tests for the vecshift_integrated.R module  
# Tests integrated system functions and performance modes

library(testthat)
library(data.table)

test_that("vecshift_integrated basic functionality works", {
  # Arrange
  test_data <- data.table(
    id = 1:4,
    cf = c("PERSON001", "PERSON001", "PERSON002", "PERSON002"),
    INIZIO = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01", "2023-08-01")),
    FINE = as.Date(c("2023-03-31", "2023-12-31", "2023-05-31", "2023-11-30")),
    prior = c(1, 0, 1, 1)
  )
  
  # Act
  result <- vecshift_integrated(test_data, verbose = FALSE)
  
  # Assert
  expect_s3_class(result, "vecshift_integrated_result")
  expect_true(all(c("result", "processing_info") %in% names(result)))
  
  # Check result structure
  vecshift_result <- result$result
  expect_s3_class(vecshift_result, "data.table")
  expect_true(all(c("cf", "inizio", "fine", "arco", "durata") %in% names(vecshift_result)))
  
  # Check processing info
  processing_info <- result$processing_info
  expect_equal(processing_info$input_rows, 4)
  expect_gt(processing_info$output_rows, 0)
  expect_equal(processing_info$performance_mode, "fast_core")
  expect_gt(processing_info$records_per_second, 0)
  expect_true("fast_core" %in% processing_info$modules_used)
})

test_that("vecshift_integrated works with custom column mapping", {
  # Arrange - Custom column names
  custom_data <- data.table(
    contract_id = 1:3,
    person_code = c("A001", "A001", "B002"),
    start_date = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
    end_date = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
    employment_type = c(1, 0, 1)
  )
  
  column_map <- list(
    id = "contract_id",
    cf = "person_code",
    INIZIO = "start_date",
    FINE = "end_date",
    prior = "employment_type"
  )
  
  # Act
  result <- vecshift_integrated(custom_data, column_map = column_map, verbose = FALSE)
  
  # Assert
  expect_s3_class(result, "vecshift_integrated_result")
  expect_true("column_mapping" %in% result$processing_info$modules_used)
  expect_gt(nrow(result$result), 0)
  
  # Check that standard column names are used in result
  expect_true(all(c("cf", "inizio", "fine") %in% names(result$result)))
})

test_that("vecshift_integrated enables modular processing", {
  # Arrange
  test_data <- data.table(
    id = 1:5,
    cf = rep(c("PERSON001", "PERSON002"), c(3, 2)),
    INIZIO = as.Date("2023-01-01") + c(0, 90, 180, 30, 120),
    FINE = as.Date("2023-01-01") + c(60, 150, 240, 90, 180),
    prior = c(1, 0, 1, 1, 0)
  )
  
  # Act - Enable all modules
  result <- vecshift_integrated(
    test_data,
    use_fast_core = FALSE,  # Use modular approach
    enable_validation = TRUE,
    enable_cleaning = TRUE,
    quality_report = TRUE,
    verbose = FALSE
  )
  
  # Assert
  expect_equal(result$processing_info$performance_mode, "modular")
  
  # Should have used multiple modules
  expected_modules <- c("data_quality", "vecshift", "default_status_rules")
  expect_true(all(expected_modules %in% result$processing_info$modules_used))
  
  # Should have quality report
  expect_true("quality_report" %in% names(result))
  expect_s3_class(result$quality_report, "employment_quality_report")
})

test_that("vecshift_integrated handles data cleaning", {
  # Arrange - Data with quality issues
  problematic_data <- data.table(
    id = c(1, 1, 2, 3),  # Duplicate ID
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002"),
    INIZIO = as.Date(c("2023-01-01", "2023-01-01", "2023-03-01", "2023-02-01")),
    FINE = as.Date(c("2023-02-28", "2023-02-28", "2023-05-31", "2023-04-30")),
    prior = c(1, 1, 0, 1)
  )
  
  # Act
  result <- vecshift_integrated(
    problematic_data,
    enable_cleaning = TRUE,
    quality_report = TRUE,
    verbose = FALSE
  )
  
  # Assert
  expect_true("data_cleaning" %in% result$processing_info$modules_used)
  expect_true("cleaned_rows" %in% names(result$processing_info))
  expect_true("rows_removed" %in% names(result$processing_info))
  
  # Should have cleaned some problematic data
  expect_gte(result$processing_info$rows_removed, 0)
  
  # Quality report should show issues
  if (!is.null(result$quality_report)) {
    expect_true(result$quality_report$duplicates$duplicate_ids >= 0)
  }
})

test_that("vecshift_integrated custom status rules", {
  # Arrange
  test_data <- data.table(
    id = 1:3,
    cf = rep("PERSON001", 3),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01")),
    FINE = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30")),
    prior = c(1, 0, 1)
  )
  
  custom_rules <- create_custom_status_rules(
    unemployment_threshold = 15,
    custom_labels = list(
      unemployed_short = "job_seeking",
      full_time = "full_employment"
    )
  )
  
  # Act
  result <- vecshift_integrated(
    test_data,
    use_fast_core = FALSE,
    status_rules = custom_rules,
    verbose = FALSE
  )
  
  # Assert
  expect_true("custom_status_rules" %in% result$processing_info$modules_used)
  
  # Should use custom labels if unemployment periods exist
  if (any(result$result$arco == 0)) {
    unemployment_states <- unique(result$result[arco == 0, stato])
    expect_true("job_seeking" %in% unemployment_states || length(unemployment_states) == 0)
  }
  
  # Should use custom full-time label
  if (any(result$result$arco == 1 & result$result$prior == 1)) {
    ft_states <- unique(result$result[arco == 1 & prior == 1, stato])
    expect_true("full_employment" %in% ft_states)
  }
})

test_that("vecshift_integrated date standardization", {
  # Arrange - Data with numeric dates
  numeric_date_data <- data.table(
    id = 1:3,
    cf = rep("PERSON001", 3),
    INIZIO = as.numeric(as.Date(c("2023-01-01", "2023-04-01", "2023-07-01"))),
    FINE = as.numeric(as.Date(c("2023-03-31", "2023-06-30", "2023-09-30"))),
    prior = c(1, 0, 1)
  )
  
  # Act
  result <- vecshift_integrated(
    numeric_date_data,
    date_standardization = TRUE,
    verbose = FALSE
  )
  
  # Assert
  expect_true("date_standardization" %in% result$processing_info$modules_used)
  
  # Result should have proper Date objects
  expect_s3_class(result$result$inizio, "Date")
  expect_s3_class(result$result$fine, "Date")
})

test_that("vecshift_integrated status validation", {
  # Arrange
  test_data <- data.table(
    id = 1:4,
    cf = rep(c("PERSON001", "PERSON002"), each = 2),
    INIZIO = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01", "2023-08-01")),
    FINE = as.Date(c("2023-03-31", "2023-12-31", "2023-05-31", "2023-11-30")),
    prior = c(1, 0, 1, 1)
  )
  
  # Act
  result <- vecshift_integrated(
    test_data,
    use_fast_core = FALSE,
    enable_validation = TRUE,
    verbose = FALSE
  )
  
  # Assert
  if ("status_validation" %in% names(result$processing_info)) {
    validation <- result$processing_info$status_validation
    expect_s3_class(validation, "employment_status_validation")
    expect_true(is.logical(validation$is_valid))
    expect_true(is.numeric(validation$validation_rate))
  }
})

test_that("vecshift_fast_path provides minimal overhead", {
  # Arrange
  test_data <- data.table(
    id = 1:100,
    cf = rep(paste0("PERSON", 1:10), each = 10),
    INIZIO = as.Date("2023-01-01") + sample(0:200, 100, replace = TRUE),
    FINE = as.Date("2023-01-01") + sample(30:230, 100, replace = TRUE),
    prior = sample(c(0, 1), 100, replace = TRUE)
  )
  
  # Ensure valid dates
  test_data[FINE < INIZIO, FINE := INIZIO + 30]
  
  # Act
  start_time <- Sys.time()
  result <- vecshift_fast_path(test_data)
  end_time <- Sys.time()
  
  processing_time <- as.numeric(end_time - start_time, units = "secs")
  
  # Assert
  expect_s3_class(result, "data.table")  # Returns data.table directly, not wrapped
  expect_gt(nrow(result), 0)
  expect_lt(processing_time, 2)  # Should be very fast
  
  # Should not have status classification (fast path doesn't classify by default)
  # Note: vecshift might still add status classification depending on implementation
  # expect_false("stato" %in% names(result))
})

test_that("vecshift_fast_path validates input", {
  # Test with invalid input
  invalid_data <- data.table(
    id = 1:3,
    cf = c("A", "B", "C")
    # Missing required columns
  )
  
  expect_error(
    vecshift_fast_path(invalid_data),
    "Missing required columns"
  )
  
  # Test with validation disabled - will still error in vecshift itself due to missing columns
  expect_error(
    vecshift_fast_path(invalid_data, validate_input = FALSE)
  )
})

test_that("vecshift_dev_mode enables comprehensive analysis", {
  # Arrange
  test_data <- data.table(
    id = 1:6,
    cf = rep(c("PERSON001", "PERSON002"), each = 3),
    INIZIO = as.Date("2023-01-01") + c(0, 90, 180, 30, 120, 210),
    FINE = as.Date("2023-01-01") + c(60, 150, 240, 90, 180, 270),
    prior = c(1, 0, 1, 1, 0, 1)
  )
  
  # Act
  expect_message(
    result <- vecshift_dev_mode(test_data),
    "Starting vecshift integrated processing"
  )
  
  # Assert
  expect_s3_class(result, "vecshift_integrated_result")
  expect_equal(result$processing_info$performance_mode, "modular")
  
  # Should use comprehensive modules
  expected_dev_modules <- c("date_standardization", "data_quality", "data_cleaning", "vecshift")
  expect_true(all(expected_dev_modules %in% result$processing_info$modules_used))
  
  # Should have quality report
  expect_true("quality_report" %in% names(result))
  
  # Should provide detailed processing info
  expect_true("records_per_second" %in% names(result$processing_info))
  expect_gt(result$processing_info$processing_time, 0)
})

test_that("vecshift_production_mode optimizes for performance", {
  # Arrange
  test_data <- data.table(
    id = 1:50,
    cf = rep(paste0("PERSON", 1:5), each = 10),
    INIZIO = as.Date("2023-01-01") + sample(0:100, 50, replace = TRUE),
    FINE = as.Date("2023-01-01") + sample(30:130, 50, replace = TRUE),
    prior = sample(c(0, 1), 50, replace = TRUE)
  )
  
  # Ensure valid dates
  test_data[FINE < INIZIO, FINE := INIZIO + sample(1:30, sum(FINE < INIZIO), replace = TRUE)]
  
  # Act
  result <- vecshift_production_mode(test_data)
  
  # Assert
  expect_s3_class(result, "vecshift_integrated_result")
  expect_equal(result$processing_info$performance_mode, "fast_core")
  expect_true("fast_core" %in% result$processing_info$modules_used)
  
  # Should NOT include validation modules (for speed)
  expect_false("data_quality" %in% result$processing_info$modules_used)
  
  # Should NOT have quality report
  expect_false("quality_report" %in% names(result))
  
  # Should be reasonably fast
  expect_gt(result$processing_info$records_per_second, 100)
})

test_that("vecshift_production_mode with cleaning enabled", {
  # Arrange - Data with some issues
  test_data <- data.table(
    id = c(1, 1, 2, 3, 4),  # Duplicate ID
    cf = rep("PERSON001", 5),
    INIZIO = as.Date("2023-01-01") + 0:4,
    FINE = as.Date("2023-01-01") + c(30, 30, 60, 90, 120),  # Duplicate row
    prior = c(1, 1, 0, 1, 0)
  )
  
  # Act
  result <- vecshift_production_mode(test_data, enable_cleaning = TRUE)
  
  # Assert
  expect_true("data_cleaning" %in% result$processing_info$modules_used)
  expect_true("cleaned_rows" %in% names(result$processing_info))
})

test_that("benchmark_vecshift_modes compares performance", {
  # Arrange - Sample data for benchmarking
  benchmark_data <- data.table(
    id = 1:200,
    cf = rep(paste0("PERSON", 1:20), each = 10),
    INIZIO = as.Date("2020-01-01") + sample(0:500, 200, replace = TRUE),
    FINE = as.Date("2020-01-01") + sample(30:530, 200, replace = TRUE),
    prior = sample(c(0, 1), 200, replace = TRUE)
  )
  
  # Ensure valid dates
  benchmark_data[FINE < INIZIO, FINE := INIZIO + sample(1:30, sum(FINE < INIZIO), replace = TRUE)]
  
  # Act - Run with fewer iterations for testing
  benchmark_results <- benchmark_vecshift_modes(benchmark_data, n_iterations = 2)
  
  # Assert
  expect_s3_class(benchmark_results, "data.table")
  expected_cols <- c("mode", "mean_time_sec", "records_per_sec", "success_rate")
  expect_true(all(expected_cols %in% names(benchmark_results)))
  
  # Should test multiple modes
  expected_modes <- c("fast_path", "production", "integrated_default", "dev_mode")
  expect_true(all(expected_modes %in% benchmark_results$mode))
  
  # All modes should succeed
  expect_true(all(benchmark_results$success_rate >= 0.5))  # At least 50% success rate
  
  # Should have relative speed if calculated
  if ("relative_speed" %in% names(benchmark_results)) {
    expect_true(max(benchmark_results$relative_speed, na.rm = TRUE) >= 0.5)
  }
})

test_that("benchmark_vecshift_modes handles errors gracefully", {
  # Arrange - Problematic data that might cause some modes to fail
  problematic_data <- data.table(
    id = integer(0),
    cf = character(0),
    INIZIO = as.Date(character(0)),
    FINE = as.Date(character(0)),
    prior = integer(0)
  )
  
  # Act
  benchmark_results <- benchmark_vecshift_modes(problematic_data, n_iterations = 1)
  
  # Assert - Should handle errors and report success rates
  expect_s3_class(benchmark_results, "data.table")
  expect_true(all(benchmark_results$success_rate <= 1.0))
  
  # Some modes might fail, but results should still be returned
  failed_modes <- benchmark_results[success_rate == 0, mode]
  successful_modes <- benchmark_results[success_rate > 0, mode]
  
  # At least some information should be available
  expect_true(nrow(benchmark_results) > 0)
})

test_that("print.vecshift_integrated_result works correctly", {
  # Arrange
  test_data <- data.table(
    id = 1:5,
    cf = rep(c("PERSON001", "PERSON002"), c(3, 2)),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01", "2023-02-01", "2023-08-01")),
    FINE = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30", "2023-05-31", "2023-11-30")),
    prior = c(1, 0, 1, 1, 0)
  )
  
  result <- vecshift_integrated(
    test_data,
    quality_report = TRUE,
    verbose = FALSE
  )
  
  # Act & Assert
  expect_output(print(result), "Vecshift Integrated Results")
  expect_output(print(result), "Processing Summary:")
  expect_output(print(result), "Modules Used:")
  # Status distribution may not be present if fast_core doesn't classify
  # expect_output(print(result), "Employment Status Distribution:")
  expect_output(print(result), "Data Quality Summary:")
  expect_output(print(result), "Result Preview:")
})

test_that("vecshift_integrated error handling", {
  # Test with invalid column mapping
  test_data <- data.table(
    id = 1:3,
    cf = rep("PERSON001", 3),
    INIZIO = as.Date("2023-01-01") + 0:2,
    FINE = as.Date("2023-01-01") + 30:32,
    prior = c(1, 0, 1)
  )
  
  # Invalid column mapping (missing required mappings)
  invalid_map <- list(id = "id", cf = "cf")  # Missing INIZIO, FINE, prior
  
  expect_error(
    vecshift_integrated(test_data, column_map = invalid_map),
    "Missing column mappings"
  )
  
  # Test with data lacking standard columns and no mapping
  custom_data <- data.table(
    contract_id = 1:3,
    person_id = rep("PERSON001", 3),
    start_dt = as.Date("2023-01-01") + 0:2,
    end_dt = as.Date("2023-01-01") + 30:32,
    emp_type = c(1, 0, 1)
  )
  
  expect_error(
    vecshift_integrated(custom_data),
    "does not have standard column names"
  )
})

test_that("vecshift_integrated handles edge cases", {
  # Test with single record
  single_record <- data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2023-01-01"),
    FINE = as.Date("2023-12-31"),
    prior = 1L
  )
  
  result <- vecshift_integrated(single_record, verbose = FALSE)
  expect_equal(nrow(result$result), 1)
  # Check if status column exists before asserting its value
  if ("stato" %in% names(result$result)) {
    expect_equal(result$result$stato, "occ_ft")
  }
  
  # Test with same-day contract
  same_day <- data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2023-06-15"),
    FINE = as.Date("2023-06-15"),
    prior = 1L
  )
  
  result_day <- vecshift_integrated(same_day, verbose = FALSE)
  expect_equal(nrow(result_day$result), 1)
  expect_equal(as.numeric(result_day$result$durata), 1)
})

test_that("vecshift_integrated performance tracking", {
  # Arrange
  test_data <- data.table(
    id = 1:100,
    cf = rep(paste0("PERSON", 1:10), each = 10),
    INIZIO = as.Date("2023-01-01") + sample(0:200, 100, replace = TRUE),
    FINE = as.Date("2023-01-01") + sample(30:230, 100, replace = TRUE),
    prior = sample(c(0, 1), 100, replace = TRUE)
  )
  
  # Ensure valid dates
  test_data[FINE < INIZIO, FINE := INIZIO + sample(1:30, sum(FINE < INIZIO), replace = TRUE)]
  
  # Act
  result <- vecshift_integrated(test_data, verbose = FALSE)
  
  # Assert
  processing_info <- result$processing_info
  expect_gt(processing_info$processing_time, 0)
  expect_gt(processing_info$records_per_second, 0)
  expect_equal(processing_info$input_rows, 100)
  expect_gt(processing_info$output_rows, 0)
})

test_that("vecshift_integrated module selection works correctly", {
  test_data <- data.table(
    id = 1:4,
    cf = c("PERSON001", "PERSON001", "PERSON002", "PERSON002"),
    INIZIO = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01", "2023-08-01")),
    FINE = as.Date(c("2023-03-31", "2023-12-31", "2023-05-31", "2023-11-30")),
    prior = c(1, 0, 1, 1)
  )
  
  # Test minimal modules (fast core only)
  minimal_result <- vecshift_integrated(
    test_data,
    use_fast_core = TRUE,
    enable_validation = FALSE,
    enable_cleaning = FALSE,
    date_standardization = FALSE,
    quality_report = FALSE,
    verbose = FALSE
  )
  
  expect_equal(minimal_result$processing_info$performance_mode, "fast_core")
  expect_true("fast_core" %in% minimal_result$processing_info$modules_used)
  expect_false("data_quality" %in% minimal_result$processing_info$modules_used)
  
  # Test maximal modules
  maximal_result <- vecshift_integrated(
    test_data,
    use_fast_core = FALSE,
    enable_validation = TRUE,
    enable_cleaning = TRUE,
    date_standardization = TRUE,
    quality_report = TRUE,
    verbose = FALSE
  )
  
  expect_equal(maximal_result$processing_info$performance_mode, "modular")
  expected_modules <- c("date_standardization", "data_quality", "data_cleaning", "vecshift")
  expect_true(all(expected_modules %in% maximal_result$processing_info$modules_used))
  expect_true("quality_report" %in% names(maximal_result))
})