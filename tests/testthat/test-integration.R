# Integration tests for the vecshift package
# Tests the full pipeline with different module combinations and performance modes

library(testthat)
library(data.table)

# Load all package functions for testing
if (!exists("vecshift_integrated")) {
  devtools::load_all()
}

test_that("vecshift_integrated works with standard column names and default settings", {
  # Test basic integration with clean data
  test_data <- generate_test_data_with_issues("clean_data", n_records = 20, n_persons = 5)
  
  result <- vecshift_integrated(test_data)
  
  # Check structure of integrated result
  expect_true(inherits(result, "vecshift_integrated_result"))
  expect_true("result" %in% names(result))
  expect_true("processing_info" %in% names(result))
  
  # Check that result has expected structure
  vecshift_result <- result$result
  expect_true(inherits(vecshift_result, "data.table"))
  expect_true(all(c("cf", "inizio", "fine", "arco", "durata", "stato") %in% names(vecshift_result)))
  
  # Check processing info
  processing_info <- result$processing_info
  expect_equal(processing_info$input_rows, nrow(test_data))
  expect_gt(processing_info$output_rows, 0)
  expect_gt(processing_info$records_per_second, 0)
  expect_equal(processing_info$performance_mode, "fast_core")
})

test_that("vecshift_integrated works with custom column mapping", {
  # Test with custom column names
  test_data_custom <- generate_custom_column_data("simple_rename")
  dt <- test_data_custom$data
  column_map <- test_data_custom$column_map
  
  result <- vecshift_integrated(
    dt,
    column_map = column_map,
    verbose = FALSE
  )
  
  expect_true("column_mapping" %in% result$processing_info$modules_used)
  expect_gt(nrow(result$result), 0)
  
  # Verify that data was processed correctly despite different column names
  expect_true(all(c("cf", "inizio", "fine", "arco", "durata", "stato") %in% names(result$result)))
})

test_that("vecshift_integrated handles data quality assessment and cleaning", {
  # Test with problematic data that needs cleaning
  problematic_data <- generate_test_data_with_issues("duplicate_data")
  
  result <- vecshift_integrated(
    problematic_data,
    enable_validation = TRUE,
    enable_cleaning = TRUE,
    quality_report = TRUE,
    verbose = FALSE
  )
  
  # Should include data quality modules
  expected_modules <- c("data_quality", "data_cleaning")
  expect_true(all(expected_modules %in% result$processing_info$modules_used))
  
  # Should have quality report
  expect_true("quality_report" %in% names(result))
  expect_true(inherits(result$quality_report, "employment_quality_report"))
  
  # Should have cleaned some data
  expect_true("cleaned_rows" %in% names(result$processing_info))
  expect_true("rows_removed" %in% names(result$processing_info))
})

test_that("vecshift_fast_path provides minimal overhead processing", {
  test_data <- generate_test_data_with_issues("clean_data", n_records = 100)
  
  # Time the fast path processing
  start_time <- Sys.time()
  result <- vecshift_fast_path(test_data)
  end_time <- Sys.time()
  
  processing_time <- as.numeric(end_time - start_time, units = "secs")
  
  # Should return data.table directly (not wrapped)
  expect_true(inherits(result, "data.table"))
  expect_true(all(c("cf", "inizio", "fine", "arco", "durata", "stato") %in% names(result)))
  
  # Should be very fast for small datasets
  expect_lt(processing_time, 1.0)  # Should complete in less than 1 second
})

test_that("vecshift_dev_mode enables all validation and reporting", {
  test_data <- generate_test_data_with_issues("clean_data", n_records = 50)
  
  # Capture messages from verbose mode
  expect_message(
    result <- vecshift_dev_mode(test_data),
    "Starting vecshift integrated processing"
  )
  
  # Should use modular approach (slower but more debuggable)
  expect_equal(result$processing_info$performance_mode, "modular")
  
  # Should enable comprehensive modules
  expected_dev_modules <- c("date_standardization", "data_quality", "data_cleaning", 
                           "modular_core", "default_status_rules")
  expect_true(all(expected_dev_modules %in% result$processing_info$modules_used))
  
  # Should have quality report
  expect_true("quality_report" %in% names(result))
})

test_that("vecshift_production_mode optimizes for performance", {
  test_data <- generate_test_data_with_issues("clean_data", n_records = 100)
  
  result <- vecshift_production_mode(test_data)
  
  # Should use fast core
  expect_equal(result$processing_info$performance_mode, "fast_core")
  expect_true("fast_core" %in% result$processing_info$modules_used)
  
  # Should NOT include validation modules (for speed)
  expect_false("data_quality" %in% result$processing_info$modules_used)
  
  # Should NOT have quality report (unless explicitly requested)
  expect_false("quality_report" %in% names(result))
  
  # Should be fast
  expect_gt(result$processing_info$records_per_second, 500)  # Should process > 500 records/sec
})

test_that("integration handles messy real-world data scenarios", {
  # Create a complex scenario with multiple data quality issues
  messy_data <- rbind(
    generate_test_data_with_issues("missing_values")[1:3],
    generate_test_data_with_issues("duplicate_data")[1:5], 
    generate_test_data_with_issues("invalid_dates")[1:3],
    generate_test_data_with_issues("zero_duration")[1:2]
  )
  
  # Should handle this gracefully with cleaning enabled
  result <- vecshift_integrated(
    messy_data,
    enable_validation = TRUE,
    enable_cleaning = TRUE,
    quality_report = TRUE,
    verbose = FALSE
  )
  
  # Should produce some result despite data quality issues
  expect_gt(nrow(result$result), 0)
  
  # Should report data quality issues
  expect_lt(result$quality_report$quality_score$overall_score, 0.9)
  expect_false(result$quality_report$quality_score$is_production_ready)
  
  # Should have cleaned some problematic records
  expect_gt(result$processing_info$rows_removed, 0)
})

test_that("integration with custom column mapping and complex data", {
  # Create complex custom-named data
  complex_custom_data <- generate_custom_column_data("database_style")
  dt <- complex_custom_data$data
  column_map <- complex_custom_data$column_map
  
  # Add some data quality issues to the custom data
  dt[1, CONTRACT_END_DATE := CONTRACT_START_DATE - 1]  # Invalid date range
  dt <- rbind(dt, dt[1])  # Add duplicate
  
  result <- vecshift_integrated(
    dt,
    column_map = column_map,
    enable_validation = TRUE,
    enable_cleaning = TRUE,
    verbose = FALSE
  )
  
  # Should handle both column mapping and data cleaning
  expected_modules <- c("column_mapping", "data_quality", "data_cleaning")
  expect_true(all(expected_modules %in% result$processing_info$modules_used))
  
  # Should still produce valid results
  expect_gt(nrow(result$result), 0)
  expect_true(all(c("cf", "inizio", "fine", "arco", "durata", "stato") %in% names(result$result)))
})

test_that("integration validates status classifications correctly", {
  test_data <- generate_test_data_with_issues("clean_data", n_records = 30, n_persons = 5)
  
  result <- vecshift_integrated(
    test_data,
    enable_validation = TRUE,
    use_fast_core = FALSE,  # Use modular for status validation
    verbose = FALSE
  )
  
  # Should include status validation in processing info
  expect_true("status_validation" %in% names(result$processing_info))
  
  status_validation <- result$processing_info$status_validation
  expect_true(inherits(status_validation, "employment_status_validation"))
  
  # With clean data, status validation should pass
  expect_true(status_validation$is_valid)
  expect_equal(status_validation$missing_labels, 0)
  expect_equal(status_validation$total_impossible, 0)
})

test_that("integration handles date standardization correctly", {
  # Create data with mixed date formats
  mixed_date_data <- generate_date_test_data("mixed_date_formats")
  
  result <- vecshift_integrated(
    mixed_date_data,
    date_standardization = TRUE,
    verbose = FALSE
  )
  
  expect_true("date_standardization" %in% result$processing_info$modules_used)
  
  # Should standardize all dates to Date objects
  expect_true(inherits(result$result$inizio, "Date"))
  expect_true(inherits(result$result$fine, "Date"))
})

test_that("benchmark_vecshift_modes compares performance correctly", {
  # Create sample data for benchmarking
  benchmark_data <- generate_test_data_with_issues("clean_data", n_records = 200, n_persons = 10)
  
  # Run benchmark with fewer iterations for testing speed
  benchmark_results <- benchmark_vecshift_modes(benchmark_data, n_iterations = 3)
  
  expect_true(inherits(benchmark_results, "data.table"))
  expect_true(all(c("mode", "mean_time_sec", "records_per_sec", "success_rate") %in% names(benchmark_results)))
  
  # All modes should succeed
  expect_true(all(benchmark_results$success_rate == 1.0))
  
  # Fast path should be fastest (or tied for fastest)
  fast_path_time <- benchmark_results[mode == "fast_path", mean_time_sec]
  expect_true(fast_path_time <= min(benchmark_results$mean_time_sec))
  
  # Should have relative speed calculations
  if ("relative_speed" %in% names(benchmark_results)) {
    expect_true(max(benchmark_results$relative_speed, na.rm = TRUE) == 1.0)
  }
})

test_that("integration error handling works correctly", {
  # Test with data missing required columns
  incomplete_data <- data.table(
    id = 1:5,
    cf = paste0("PERSON", 1:5)
    # Missing inizio, fine, prior
  )
  
  # Should fail gracefully
  expect_error(
    vecshift_integrated(incomplete_data),
    "does not have standard column names"
  )
  
  # Test fast path validation
  expect_error(
    vecshift_fast_path(incomplete_data),
    "Missing required columns"
  )
  
  # Test with invalid column mapping
  test_data <- generate_test_data_with_issues("clean_data", n_records = 5)
  invalid_map <- list(id = "id", cf = "cf")  # Incomplete mapping
  
  expect_error(
    vecshift_integrated(test_data, column_map = invalid_map),
    "Missing column mappings"
  )
})

test_that("integration preserves data consistency through pipeline", {
  # Create known test case
  test_data <- data.table(
    id = 1:3,
    cf = rep("PERSON001", 3),
    inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01")),
    fine = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30")),
    prior = c(1, 0, 1)
  )
  
  result <- vecshift_integrated(test_data, verbose = FALSE)
  
  # Should preserve person identity
  expect_true(all(result$result$cf == "PERSON001"))
  
  # Should have correct number of segments (3 employment + 2 unemployment)
  expect_equal(nrow(result$result), 5)
  
  # Should have correct employment states
  employment_segments <- result$result[arco >= 1]
  expect_equal(nrow(employment_segments), 3)
  
  unemployment_segments <- result$result[arco == 0]  
  expect_equal(nrow(unemployment_segments), 2)
  
  # Total duration should make sense
  total_days <- sum(result$result$durata)
  observation_span <- as.integer(max(result$result$fine) - min(result$result$inizio))
  expect_equal(total_days, observation_span)
})

test_that("integration handles overlapping employment correctly", {
  # Create overlapping employment scenario
  overlapping_data <- data.table(
    id = 1:3,
    cf = rep("PERSON001", 3), 
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")),
    fine = as.Date(c("2023-08-31", "2023-07-31", "2023-12-31")),  # Overlapping periods
    prior = c(1, 0, 1)
  )
  
  result <- vecshift_integrated(overlapping_data, verbose = FALSE)
  
  # Should detect and label overlapping periods
  overlap_segments <- result$result[arco > 1]
  expect_gt(nrow(overlap_segments), 0)
  
  # Should have overlap status labels
  expect_true(any(grepl("^over_", result$result$stato)))
  
  # Maximum arco should reflect maximum overlap
  max_arco <- max(result$result$arco)
  expect_gte(max_arco, 2)  # At least double employment
})

test_that("print method for integrated results works correctly", {
  test_data <- generate_test_data_with_issues("clean_data", n_records = 20)
  
  result <- vecshift_integrated(
    test_data,
    quality_report = TRUE,
    verbose = FALSE
  )
  
  # Should print without errors
  expect_output(print(result), "Vecshift Integrated Results")
  expect_output(print(result), "Processing Summary:")
  expect_output(print(result), "Modules Used:")
  expect_output(print(result), "Employment Status Distribution:")
  expect_output(print(result), "Data Quality Summary:")
  expect_output(print(result), "Result Preview:")
})

test_that("integration respects processing mode preferences", {
  test_data <- generate_test_data_with_issues("clean_data", n_records = 100)
  
  # Test explicit fast core mode
  fast_result <- vecshift_integrated(test_data, use_fast_core = TRUE, verbose = FALSE)
  expect_equal(fast_result$processing_info$performance_mode, "fast_core")
  expect_true("fast_core" %in% fast_result$processing_info$modules_used)
  
  # Test explicit modular mode  
  modular_result <- vecshift_integrated(test_data, use_fast_core = FALSE, verbose = FALSE)
  expect_equal(modular_result$processing_info$performance_mode, "modular")
  expect_true("modular_core" %in% modular_result$processing_info$modules_used)
  
  # Results should be equivalent despite different processing paths
  expect_equal(nrow(fast_result$result), nrow(modular_result$result))
  
  # Status distributions should be similar (may have minor differences due to implementation)
  fast_status_table <- table(fast_result$result$stato)
  modular_status_table <- table(modular_result$result$stato)
  expect_true(length(intersect(names(fast_status_table), names(modular_status_table))) > 0)
})

test_that("integration handles edge cases gracefully", {
  # Test with single record
  single_record <- data.table(
    id = 1L,
    cf = "PERSON001",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-12-31"),
    prior = 1L
  )
  
  result <- vecshift_integrated(single_record, verbose = FALSE)
  expect_equal(nrow(result$result), 1)
  expect_equal(result$result$arco, 1)
  expect_equal(result$result$stato, "occ_ft")
  
  # Test with single day contract
  single_day <- data.table(
    id = 1L,
    cf = "PERSON001", 
    inizio = as.Date("2023-06-15"),
    fine = as.Date("2023-06-15"),
    prior = 1L
  )
  
  result_day <- vecshift_integrated(single_day, verbose = FALSE)
  expect_equal(nrow(result_day$result), 1)
  expect_equal(result_day$result$durata, 1)
})

test_that("integration performance scales appropriately", {
  # Test with increasing data sizes to verify scaling
  small_data <- generate_test_data_with_issues("clean_data", n_records = 50, n_persons = 5)
  medium_data <- generate_test_data_with_issues("clean_data", n_records = 200, n_persons = 20)
  
  # Time small dataset
  start_small <- Sys.time()
  result_small <- vecshift_fast_path(small_data)
  time_small <- as.numeric(Sys.time() - start_small, units = "secs")
  
  # Time medium dataset  
  start_medium <- Sys.time()
  result_medium <- vecshift_fast_path(medium_data)
  time_medium <- as.numeric(Sys.time() - start_medium, units = "secs")
  
  # Scaling should be reasonable (not exponential)
  scaling_ratio <- time_medium / time_small
  data_ratio <- nrow(medium_data) / nrow(small_data)
  
  # Processing time should scale sub-linearly or linearly (not worse than quadratic)
  expect_lt(scaling_ratio, data_ratio * data_ratio)
  
  # Both should complete in reasonable time
  expect_lt(time_small, 1.0)
  expect_lt(time_medium, 5.0)
})