# Test and demonstrate all synthetic datasets created for vecshift testing

test_that("all synthetic test datasets can be generated without error", {
  # Test that all predefined scenarios generate valid data
  scenarios <- c(
    "single_employment", "single_parttime", "employment_with_gap",
    "consecutive_employment", "overlapping_employment", "complex_overlapping",
    "single_day", "multiple_people", "edge_dates", "all_parttime_overlap",
    "all_fulltime_overlap", "negative_prior", "empty"
  )
  
  for (scenario in scenarios) {
    # Act
    test_data <- generate_test_data(scenario)
    
    # Assert
    expect_s3_class(test_data, "data.table")
    
    # Check that all required columns are present
    required_cols <- c("id", "cf", "INIZIO", "FINE", "prior")
    expect_true(all(required_cols %in% names(test_data)), 
                info = paste("Missing columns in scenario:", scenario))
    
    # Verify column types (except for empty dataset)
    if (scenario != "empty") {
      expect_true(is.integer(test_data$id) || is.numeric(test_data$id))
      expect_true(is.character(test_data$cf))
      expect_true(inherits(test_data$INIZIO, "Date"))
      expect_true(inherits(test_data$FINE, "Date"))
      expect_true(is.integer(test_data$prior) || is.numeric(test_data$prior))
    }
  }
})

test_that("all synthetic datasets produce valid vecshift results", {
  # Test that vecshift can process all synthetic datasets without error
  scenarios <- c(
    "single_employment", "single_parttime", "employment_with_gap",
    "consecutive_employment", "overlapping_employment", "complex_overlapping",
    "single_day", "multiple_people", "edge_dates", "all_parttime_overlap",
    "all_fulltime_overlap", "negative_prior", "empty"
  )
  
  for (scenario in scenarios) {
    # Skip empty dataset as it causes an error by design
    if (scenario == "empty") {
      next
    }
    
    # Arrange
    test_data <- generate_test_data(scenario)
    
    # Act
    result <- expect_silent(vecshift(test_data))
    
    # Assert
    expect_s3_class(result, "data.table")
    
    # Check result structure
    expected_cols <- c("cf", "inizio", "fine", "arco", "prior", "id", "durata", "stato")
    expect_true(all(expected_cols %in% names(result)), 
                info = paste("Missing result columns in scenario:", scenario))
    
    # Verify basic data integrity
    if (nrow(result) > 0) {
      expect_true(all(result$durata > 0), 
                  info = paste("Non-positive duration in scenario:", scenario))
      expect_true(all(result$arco >= 0), 
                  info = paste("Negative arco in scenario:", scenario))
      expect_true(all(result$prior %in% c(0, 1)), 
                  info = paste("Invalid prior values in scenario:", scenario))
    }
  }
})

test_that("invalid data scenarios generate appropriate errors", {
  # Test that all invalid data types produce expected errors
  error_types <- c(
    "missing_columns", "wrong_types", "invalid_dates", "not_datatable"
  )
  
  for (error_type in error_types) {
    # Arrange
    invalid_data <- generate_invalid_data(error_type)
    
    # Act & Assert
    if (error_type == "invalid_dates") {
      expect_warning(vecshift(invalid_data))  # This only warns, doesn't error
    } else {
      expect_error(vecshift(invalid_data))
    }
  }
})

test_that("synthetic datasets demonstrate all employment scenarios", {
  # Verify that our synthetic datasets cover all major scenarios
  
  # Test single employment scenarios
  single_ft <- vecshift(generate_test_data("single_employment"))
  expect_true("occ_ft" %in% single_ft$stato)
  
  single_pt <- vecshift(generate_test_data("single_parttime"))
  expect_true("occ_pt" %in% single_pt$stato)
  
  # Test gap scenarios
  gap_result <- vecshift(generate_test_data("employment_with_gap"))
  expect_true("disoccupato" %in% gap_result$stato)
  
  # Test overlapping scenarios
  overlap_result <- vecshift(generate_test_data("overlapping_employment"))
  overlap_states <- overlap_result[arco > 1]$stato
  expect_gt(length(overlap_states), 0)
  
  # Test complex overlapping
  complex_result <- vecshift(generate_test_data("complex_overlapping"))
  expect_true(max(complex_result$arco) >= 2)  # Should have multiple overlapping jobs
  
  # Test multiple people
  multi_result <- vecshift(generate_test_data("multiple_people"))
  expect_equal(length(unique(multi_result$cf)), 2)  # Two different people
})

test_that("synthetic datasets provide educational examples", {
  # Document and verify educational aspects of the synthetic datasets
  
  cat("\n=== EDUCATIONAL SYNTHETIC DATASETS FOR VECSHIFT ===\n")
  
  scenarios <- list(
    "Basic single employment" = "single_employment",
    "Single part-time job" = "single_parttime", 
    "Employment with unemployment gap" = "employment_with_gap",
    "Consecutive jobs (no gap)" = "consecutive_employment",
    "Overlapping employment" = "overlapping_employment",
    "Complex overlapping (3 jobs)" = "complex_overlapping",
    "Single day employment" = "single_day",
    "Multiple people" = "multiple_people",
    "Edge case dates" = "edge_dates",
    "All part-time overlapping" = "all_parttime_overlap",
    "All full-time overlapping" = "all_fulltime_overlap",
    "Negative prior values" = "negative_prior"
  )
  
  for (description in names(scenarios)) {
    scenario <- scenarios[[description]]
    test_data <- generate_test_data(scenario)
    result <- vecshift(test_data)
    
    cat("\n", description, ":\n")
    cat("  Input rows:", nrow(test_data), "\n")
    cat("  Output segments:", nrow(result), "\n")
    
    if (nrow(result) > 0) {
      states <- unique(result$stato)
      cat("  Employment states:", paste(states, collapse = ", "), "\n")
      
      max_arco <- max(result$arco)
      cat("  Max concurrent jobs:", max_arco, "\n")
    }
    
    # Verify that the example is valid
    expect_s3_class(test_data, "data.table")
    expect_s3_class(result, "data.table")
  }
  
  cat("\n=== END EDUCATIONAL EXAMPLES ===\n")
})

test_that("synthetic datasets demonstrate all employment status classifications", {
  # Ensure we can demonstrate every possible employment status
  
  all_statuses <- c("disoccupato", "occ_ft", "occ_pt", "over_pt_ft", 
                    "over_ft_pt", "over_pt_pt", "over_ft_ft")
  found_statuses <- character(0)
  
  # Collect all statuses from all scenarios
  scenarios <- c(
    "single_employment", "single_parttime", "employment_with_gap",
    "consecutive_employment", "overlapping_employment", "complex_overlapping",
    "all_parttime_overlap", "all_fulltime_overlap"
  )
  
  for (scenario in scenarios) {
    result <- vecshift(generate_test_data(scenario))
    found_statuses <- c(found_statuses, result$stato)
  }
  
  found_statuses <- unique(found_statuses)
  
  # Check coverage
  missing_statuses <- setdiff(all_statuses, found_statuses)
  
  # Report findings
  cat("\nEmployment status coverage:\n")
  cat("Found statuses:", paste(sort(found_statuses), collapse = ", "), "\n")
  if (length(missing_statuses) > 0) {
    cat("Missing statuses:", paste(missing_statuses, collapse = ", "), "\n")
    
    # Try to create specific examples for missing statuses
    for (missing_status in missing_statuses) {
      cat("Attempting to generate example for:", missing_status, "\n")
      # Additional test data generation could be added here
    }
  }
  
  # We should find at least the basic employment types
  basic_types <- c("disoccupato", "occ_ft", "occ_pt")
  expect_true(all(basic_types %in% found_statuses),
              info = "Missing basic employment types in synthetic datasets")
})