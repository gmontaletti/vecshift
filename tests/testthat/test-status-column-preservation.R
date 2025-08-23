# Test that status column is preserved through pipeline processing
# This test addresses the bug where the stato column was lost during consolidation

library(testthat)
library(data.table)

# Create test data
test_data <- data.table(
  id = 1:4,
  cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002"),
  inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01", "2023-02-01")),
  fine = as.Date(c("2023-03-31", "2023-06-30", "2023-12-31", "2023-11-30")),
  prior = c(1, 1, 0, 1)
)

test_that("process_employment_pipeline preserves stato column when classify_status = TRUE", {
  result <- process_employment_pipeline(test_data, show_progress = FALSE)
  
  expect_true("stato" %in% names(result))
  expect_true(all(!is.na(result$stato)))
  expect_true(all(result$stato %in% c("occ_ft", "occ_pt", "over_ft_ft", "over_pt_pt", "over_ft_pt", "over_pt_ft", "disoccupato")))
})

test_that("process_employment_pipeline preserves stato column without consolidation", {
  result <- process_employment_pipeline(
    original_data = test_data,
    classify_status = TRUE,
    collapse_consecutive = FALSE,
    show_progress = FALSE
  )
  
  expect_true("stato" %in% names(result))
  expect_true(all(!is.na(result$stato)))
})

test_that("process_employment_pipeline preserves stato column with consolidate_periods = TRUE", {
  result <- process_employment_pipeline(
    original_data = test_data,
    classify_status = TRUE,
    collapse_consecutive = TRUE,
    consolidate_periods = TRUE,
    consolidation_type = "both",
    show_progress = FALSE
  )
  
  expect_true("stato" %in% names(result))
  expect_true(all(!is.na(result$stato)))
})

test_that("process_employment_pipeline preserves stato column with consolidate_periods = FALSE (fast path)", {
  result <- process_employment_pipeline(
    original_data = test_data,
    classify_status = TRUE,
    collapse_consecutive = TRUE,
    consolidate_periods = FALSE,  # This uses merge_consecutive_employment_fast
    show_progress = FALSE
  )
  
  expect_true("stato" %in% names(result))
  expect_true(all(!is.na(result$stato)))
})

test_that("merge_consecutive_employment preserves stato column for all consolidation types", {
  # First get vecshift output with stato
  vecshift_result <- vecshift(test_data)
  
  consolidation_types <- c("overlapping", "consecutive", "both")
  
  for (consolidation_type in consolidation_types) {
    result <- merge_consecutive_employment(vecshift_result, consolidation_type = consolidation_type)
    
    expect_true("stato" %in% names(result), 
                info = paste("stato column missing for consolidation_type:", consolidation_type))
    expect_true(all(!is.na(result$stato)), 
                info = paste("NA values in stato for consolidation_type:", consolidation_type))
  }
})

test_that("merge_consecutive_employment_fast preserves stato column", {
  # First get vecshift output with stato
  vecshift_result <- vecshift(test_data)
  
  result <- merge_consecutive_employment_fast(vecshift_result)
  
  expect_true("stato" %in% names(result))
  expect_true(all(!is.na(result$stato)))
})

test_that("consolidated stato values are logical for merged periods", {
  # Create data with overlapping periods to test stato consolidation logic
  overlap_data <- data.table(
    id = 1:6,
    cf = rep("PERSON001", 6),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-05-01", "2023-06-01", "2023-08-01", "2023-10-01")),
    fine = as.Date(c("2023-04-30", "2023-06-30", "2023-05-31", "2023-07-31", "2023-09-30", "2023-11-30")),
    prior = c(1, 0, 1, 1, 0, 1)
  )
  
  # Apply pipeline with consolidation
  result <- process_employment_pipeline(
    original_data = overlap_data,
    classify_status = TRUE,
    collapse_consecutive = TRUE,
    consolidate_periods = TRUE,
    consolidation_type = "both",
    show_progress = FALSE
  )
  
  expect_true("stato" %in% names(result))
  expect_true(all(!is.na(result$stato)))
  
  # Check that collapsed periods have reasonable stato values
  if ("collapsed" %in% names(result)) {
    collapsed_rows <- result[collapsed == TRUE]
    if (nrow(collapsed_rows) > 0) {
      expect_true(all(collapsed_rows$stato %in% c("occ_ft", "occ_pt", "over_ft_ft", "over_pt_pt", "over_ft_pt", "over_pt_ft")))
    }
  }
})