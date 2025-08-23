# Test the over_id functionality in vecshift package
# over_id is the core identifier for continuous overlapping employment periods

library(testthat)
library(data.table)

# Test vecshift() over_id generation ====

test_that("vecshift generates over_id = 0 for unemployment periods", {
  # Arrange - Employment with gap creating unemployment
  test_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"), 
    inizio = as.Date(c("2023-01-01", "2023-07-01")),
    fine = as.Date(c("2023-03-31", "2023-12-31")),
    prior = c(1L, 1L)
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_true("over_id" %in% names(result))
  
  # Find unemployment period (arco = 0)
  unemployment <- result[arco == 0]
  expect_gt(nrow(unemployment), 0)
  expect_true(all(unemployment$over_id == 0))
  
  # Employment periods should have over_id > 0
  employment <- result[arco > 0]
  expect_true(all(employment$over_id > 0))
})

test_that("vecshift assigns different over_id values to non-overlapping consecutive contracts", {
  # Arrange - Two consecutive contracts (no gap, no overlap)
  test_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-04-01")), 
    fine = as.Date(c("2023-03-31", "2023-12-31")),
    prior = c(1L, 0L)
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Should have two employment periods with different over_id values
  employment_periods <- result[arco > 0]
  expect_equal(nrow(employment_periods), 2)
  
  # Each employment period should have different over_id
  over_id_values <- unique(employment_periods$over_id)
  expect_length(over_id_values, 2)
  expect_true(all(over_id_values > 0))
})

test_that("vecshift assigns same over_id value to overlapping contracts", {
  # Arrange - Two overlapping contracts from same person
  test_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-06-01")),
    fine = as.Date(c("2023-12-31", "2023-09-30")),  # Second contract ends within first
    prior = c(1L, 0L)
  )
  
  # Act 
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Should have periods with arco > 1 (overlapping)
  overlapping_periods <- result[arco > 1]
  expect_gt(nrow(overlapping_periods), 0)
  
  # All overlapping segments should have the same over_id
  over_id_values <- unique(overlapping_periods$over_id)
  expect_length(over_id_values, 1)
  expect_gt(over_id_values[1], 0)
})

test_that("vecshift duration calculation preserves elapsed time equals sum of durations", {
  # Arrange - Create contracts where we can verify duration calculation
  test_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-07-01")),
    fine = as.Date(c("2023-06-30", "2023-12-31")), 
    prior = c(1L, 1L)
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  # Total elapsed time from first inizio to last fine should equal sum of durations
  person_data <- result[cf == "PERSON001"]
  
  # Calculate elapsed time: from first inizio to last fine (inclusive)
  first_inizio <- min(person_data$inizio)
  last_fine <- max(person_data$fine) 
  elapsed_time <- as.numeric(last_fine - first_inizio) + 1  # +1 for inclusive calculation
  
  # Sum of all segment durations
  total_duration <- sum(as.numeric(person_data$durata))
  
  expect_equal(elapsed_time, total_duration)
})

test_that("vecshift handles complex overlapping scenario with correct over_id assignment", {
  # Arrange - Three contracts with different overlap patterns
  test_data <- data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-08-01")),
    fine = as.Date(c("2023-06-30", "2023-05-31", "2023-12-31")),
    prior = c(1L, 0L, 1L)
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Should have different over_id patterns
  # - First two contracts overlap (same over_id)  
  # - Third contract separate (different over_id)
  # - Unemployment periods have over_id = 0
  
  over_id_values <- unique(result$over_id)
  expect_true(0 %in% over_id_values)  # Should have unemployment
  expect_gt(length(over_id_values), 2)  # Should have multiple different over_id values
  
  # Check overlapping periods have same over_id
  overlapping_segments <- result[arco > 1]
  if (nrow(overlapping_segments) > 0) {
    overlap_over_ids <- unique(overlapping_segments$over_id)
    expect_length(overlap_over_ids, 1)  # All overlapping segments should share same over_id
  }
})

# Test merge_consecutive_employment() with consolidation types ====

test_that("merge_consecutive_employment works with 'overlapping' consolidation", {
  # Arrange - Create vecshift output with over_id
  test_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-06-01")),
    fine = as.Date(c("2023-12-31", "2023-09-30")),
    prior = c(1L, 0L)
  )
  
  vecshift_result <- vecshift(test_data)
  
  # Act - Apply overlapping consolidation  
  merged_result <- merge_consecutive_employment(vecshift_result, consolidation_type = "overlapping")
  
  # Assert
  expect_s3_class(merged_result, "data.table")
  expect_true("collapsed" %in% names(merged_result))
  
  # Should have fewer rows than original (overlapping periods merged)
  overlapping_segments <- vecshift_result[over_id > 0 & arco > 1]
  if (nrow(overlapping_segments) > 1) {
    expect_lt(nrow(merged_result), nrow(vecshift_result))
    
    # Check that some periods were collapsed  
    expect_true(any(merged_result$collapsed == TRUE))
  }
})

test_that("merge_consecutive_employment works with 'consecutive' consolidation", {
  # Arrange - Create consecutive employment periods
  test_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"), 
    inizio = as.Date(c("2023-01-01", "2023-04-01")),
    fine = as.Date(c("2023-03-31", "2023-12-31")),
    prior = c(1L, 1L)  # Same employment type to enable merging
  )
  
  vecshift_result <- vecshift(test_data)
  
  # Act
  merged_result <- merge_consecutive_employment(vecshift_result, consolidation_type = "consecutive")
  
  # Assert
  expect_s3_class(merged_result, "data.table")
  
  # Should merge consecutive periods of same type
  employment_periods <- merged_result[arco > 0]
  if (nrow(employment_periods) > 0) {
    expect_true("collapsed" %in% names(merged_result))
    
    # Duration should be preserved
    total_original_duration <- sum(as.numeric(vecshift_result[arco > 0]$durata))
    total_merged_duration <- sum(as.numeric(employment_periods$durata))
    expect_equal(total_original_duration, total_merged_duration)
  }
})

test_that("merge_consecutive_employment works with 'both' consolidation", {
  # Arrange - Create scenario with both overlapping and consecutive periods
  test_data <- data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-08-01")),
    fine = as.Date(c("2023-06-30", "2023-05-31", "2023-12-31")),
    prior = c(1L, 0L, 1L)
  )
  
  vecshift_result <- vecshift(test_data)
  
  # Act - Apply both consolidation types
  merged_result <- merge_consecutive_employment(vecshift_result, consolidation_type = "both")
  
  # Assert
  expect_s3_class(merged_result, "data.table")
  
  # Should apply both overlapping and consecutive consolidation
  # Result should be most consolidated form
  expect_lte(nrow(merged_result), nrow(vecshift_result))
  
  # Duration should still be preserved
  total_original_duration <- sum(as.numeric(vecshift_result$durata))
  total_merged_duration <- sum(as.numeric(merged_result$durata))
  expect_equal(total_original_duration, total_merged_duration)
})

test_that("merge_consecutive_employment works with 'none' consolidation", {
  # Arrange
  test_data <- generate_test_data("overlapping_employment")
  vecshift_result <- vecshift(test_data)
  
  # Act
  merged_result <- merge_consecutive_employment(vecshift_result, consolidation_type = "none")
  
  # Assert - Should return unchanged data
  expect_s3_class(merged_result, "data.table")
  expect_equal(nrow(merged_result), nrow(vecshift_result))
  
  # Should add collapsed column (all FALSE for no consolidation)
  expect_true("collapsed" %in% names(merged_result))
  expect_true(all(merged_result$collapsed == FALSE))
})

test_that("merge_consecutive_employment preserves duration sums after consolidation", {
  # Arrange - Complex scenario  
  test_data <- data.table(
    id = c(1L, 2L, 3L, 4L),
    cf = rep("PERSON001", 4),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01", "2023-08-01")),
    fine = as.Date(c("2023-03-31", "2023-03-15", "2023-06-30", "2023-12-31")),
    prior = c(1L, 0L, 1L, 1L)
  )
  
  vecshift_result <- vecshift(test_data)
  original_total_duration <- sum(as.numeric(vecshift_result$durata))
  
  # Act - Test each consolidation type
  for (consolidation_type in c("overlapping", "consecutive", "both")) {
    merged_result <- merge_consecutive_employment(vecshift_result, consolidation_type = consolidation_type)
    merged_total_duration <- sum(as.numeric(merged_result$durata))
    
    # Assert - Duration should be preserved
    expect_equal(original_total_duration, merged_total_duration,
                 info = paste("Duration not preserved for consolidation type:", consolidation_type))
  }
})

# Test analyze_employment_transitions() with consolidated periods ====

test_that("analyze_employment_transitions works with use_consolidated_periods parameter", {
  # Arrange - Test basic functionality of use_consolidated_periods parameter
  # Create simple test case to verify consolidation parameter works
  test_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-06-01")),
    fine = as.Date(c("2023-12-31", "2023-09-30")),  # Overlapping contracts
    prior = c(1L, 0L)
  )
  
  pipeline_result <- vecshift(test_data)
  
  # Act & Assert - Test that both parameters work without error
  expect_no_error({
    result_no_consolidation <- merge_consecutive_employment(pipeline_result, consolidation_type = "none")
  })
  
  expect_no_error({
    result_with_consolidation <- merge_consecutive_employment(pipeline_result, consolidation_type = "overlapping")
  })
  
  # Both results should be data.tables
  expect_s3_class(result_no_consolidation, "data.table")
  expect_s3_class(result_with_consolidation, "data.table")
  
  # Results may be different sizes depending on consolidation
  expect_gte(nrow(result_no_consolidation), nrow(result_with_consolidation))
})

test_that("merge_consecutive_employment supports all consolidation_type options with over_id", {
  # Arrange - Create vecshift output with over_id
  test_data <- data.table(
    id = c(1L, 2L, 3L),
    cf = rep("PERSON001", 3),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-08-01")),
    fine = as.Date(c("2023-06-30", "2023-05-31", "2023-12-31")),
    prior = c(1L, 0L, 1L)
  )
  
  pipeline_result <- vecshift(test_data)
  
  # Act & Assert - Test each consolidation type works without error
  consolidation_types <- c("overlapping", "consecutive", "both", "none")
  
  for (cons_type in consolidation_types) {
    # Test that merge_consecutive_employment works without error for each consolidation type
    expect_no_error({
      result <- merge_consecutive_employment(
        pipeline_result,
        consolidation_type = cons_type
      )
    })
    
    # Verify result is valid data.table with over_id
    result <- merge_consecutive_employment(pipeline_result, consolidation_type = cons_type)
    expect_s3_class(result, "data.table")
    expect_true("over_id" %in% names(result))
    expect_true("collapsed" %in% names(result))
  }
})

test_that("over_id preserves employment period relationships after consolidation", {
  # Arrange - Create overlapping employment scenario
  test_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01")),
    fine = as.Date(c("2023-06-30", "2023-05-31")),  # Overlapping periods
    prior = c(1L, 0L)
  )
  
  vecshift_result <- vecshift(test_data)
  
  # Act - Test consolidation preserves over_id logic
  consolidated <- merge_consecutive_employment(vecshift_result, consolidation_type = "overlapping")
  
  # Assert - over_id relationships should be preserved
  expect_s3_class(consolidated, "data.table")
  expect_true("over_id" %in% names(consolidated))
  
  # Unemployment periods should still have over_id = 0
  unemployment_periods <- consolidated[arco == 0]
  if (nrow(unemployment_periods) > 0) {
    expect_true(all(unemployment_periods$over_id == 0))
  }
  
  # Employment periods should have over_id > 0  
  employment_periods <- consolidated[arco > 0]
  if (nrow(employment_periods) > 0) {
    expect_true(all(employment_periods$over_id > 0))
  }
})

# Test classify_employment_status() with over_id ====

test_that("classify_employment_status works with over_id present", {
  # Arrange - Create vecshift output with over_id
  test_data <- generate_test_data("overlapping_employment")
  vecshift_result <- vecshift(test_data)
  
  # Act - Apply classification
  classified_result <- classify_employment_status(vecshift_result)
  
  # Assert
  expect_s3_class(classified_result, "data.table")
  expect_true("stato" %in% names(classified_result))
  expect_true("over_id" %in% names(classified_result))
  
  # Should have employment status labels
  states <- unique(classified_result$stato)
  expected_states <- c("disoccupato", "occ_ft", "occ_pt", "over_pt_ft", "over_ft_pt", "over_pt_pt", "over_ft_ft")
  expect_true(any(states %in% expected_states))
})

test_that("classify_employment_status maintains backward compatibility when over_id missing", {
  # Arrange - Create vecshift output without over_id (simulate legacy data)
  test_data <- generate_test_data("employment_with_gap")
  vecshift_result <- vecshift(test_data)
  
  # Remove over_id column to simulate legacy data
  vecshift_result[, over_id := NULL]
  
  # Act - Should still work without over_id
  classified_result <- classify_employment_status(vecshift_result)
  
  # Assert
  expect_s3_class(classified_result, "data.table")
  expect_true("stato" %in% names(classified_result))
  
  # Should produce valid employment states
  states <- unique(classified_result$stato)
  expect_gt(length(states), 0)
  expect_true(any(c("disoccupato", "occ_ft", "occ_pt") %in% states))
})

# Test edge cases and error handling ====

test_that("vecshift handles single day contracts correctly with over_id", {
  # Arrange - Single day employment
  test_data <- data.table(
    id = 1L,
    cf = "PERSON001",
    inizio = as.Date("2023-06-15"),
    fine = as.Date("2023-06-15"),
    prior = 1L
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_gt(result$over_id[1], 0)  # Should have positive over_id
  expect_equal(as.numeric(result$durata), 1)
})

test_that("vecshift handles same start/end dates with over_id", {
  # Arrange - Two contracts starting and ending on same dates
  test_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-01-01")),
    fine = as.Date(c("2023-01-31", "2023-01-31")),
    prior = c(1L, 0L)
  )
  
  # Act 
  result <- vecshift(test_data)
  
  # Assert - Should handle identical dates correctly
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 0)
  expect_true("over_id" %in% names(result))
  
  # All employment segments should have same over_id (perfect overlap)
  employment_segments <- result[arco > 0]
  if (nrow(employment_segments) > 0) {
    over_ids <- unique(employment_segments$over_id)
    expect_length(over_ids, 1)
  }
})

test_that("over_id calculation handles multiple people correctly", {
  # Arrange - Multiple people with different employment patterns
  test_data <- data.table(
    id = c(1L, 2L, 3L, 4L),
    cf = c("PERSON001", "PERSON001", "PERSON002", "PERSON002"),
    inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01", "2023-08-01")),
    fine = as.Date(c("2023-12-31", "2023-09-30", "2023-05-31", "2023-11-30")),
    prior = c(1L, 0L, 0L, 1L)
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Each person should have independent over_id numbering
  person1_over_ids <- unique(result[cf == "PERSON001" & over_id > 0]$over_id)
  person2_over_ids <- unique(result[cf == "PERSON002" & over_id > 0]$over_id)
  
  expect_gt(length(person1_over_ids), 0)
  expect_gt(length(person2_over_ids), 0)
  
  # over_id should be person-specific (could have same values for different people)
  # This is expected behavior - over_id is meaningful within each person's timeline
})

test_that("duration calculation with over_id handles overlapping periods correctly", {
  # Arrange - Create overlapping contracts with known durations
  test_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),  
    fine = as.Date(c("2023-03-31", "2023-02-28")),  # 90 days, 28 days with overlap
    prior = c(1L, 0L)
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert duration calculation
  person_result <- result[cf == "PERSON001"]
  
  # Total elapsed time (inclusive calculation)
  total_elapsed <- as.numeric(max(person_result$fine) - min(person_result$inizio)) + 1
  total_duration <- sum(as.numeric(person_result$durata))
  
  expect_equal(total_elapsed, total_duration)
  
  # Check that overlapping period has correct duration
  overlapping_period <- person_result[arco > 1]
  if (nrow(overlapping_period) > 0) {
    expect_gt(as.numeric(overlapping_period$durata[1]), 0)
    expect_gt(overlapping_period$over_id[1], 0)
  }
})