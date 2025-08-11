library(testthat)
library(data.table)

test_that("merge_consecutive_employment collapses consecutive periods with duration-weighted means", {
  # Create test data with consecutive employment periods
  dt <- data.table(
    cf = c("A", "A", "A", "A", "A"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01", "2023-06-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31", "2023-04-30", "2023-06-30")),
    arco = c(1, 1, 1, 0, 1),  # Three consecutive employment, one unemployment, one employment
    prior = c(1, 0.5, 1, 0, 1),
    salary = c(3000, 2500, 3200, NA, 3500),
    contract_type = c("FT", "PT", "FT", NA, "FT")
  )
  
  result <- merge_consecutive_employment(dt)
  
  # Should have 3 periods: 1 collapsed employment, 1 unemployment, 1 single employment
  expect_equal(nrow(result), 3)
  
  # First row should be collapsed employment from Jan to Mar
  expect_equal(result$inizio[1], as.Date("2023-01-01"))
  expect_equal(result$fine[1], as.Date("2023-03-31"))
  expect_true(result$collapsed[1])
  
  # Calculate expected duration-weighted means
  # Durations: Jan (31 days), Feb (28 days), Mar (31 days)
  durations <- c(31, 28, 31)
  expected_prior <- round(weighted.mean(c(1, 0.5, 1), w = durations), 2)
  expected_salary <- round(weighted.mean(c(3000, 2500, 3200), w = durations), 2)
  
  expect_equal(result$prior[1], expected_prior)
  expect_equal(result$salary[1], expected_salary)
  expect_equal(result$contract_type[1], "FT->FT")
  
  # Second row should be unemployment (unchanged)
  expect_equal(result$arco[2], 0)
  expect_false(result$collapsed[2])
  
  # Third row should be single employment (unchanged)
  expect_equal(result$inizio[3], as.Date("2023-06-01"))
  expect_false(result$collapsed[3])
})

test_that("merge_consecutive_employment preserves unemployment periods", {
  dt <- data.table(
    cf = c("B", "B", "B"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    arco = c(1, 0, 1),
    prior = c(1, 0, 1)
  )
  
  result <- merge_consecutive_employment(dt)
  
  # Should have 3 separate periods (no collapsing across unemployment)
  expect_equal(nrow(result), 3)
  expect_equal(result$arco[2], 0)
  expect_false(any(result$collapsed[result$arco == 0]))
})

test_that("merge_consecutive_employment handles consecutive periods with different arco values", {
  # Create consecutive periods with different arco values (simulating employment with varying overlap levels)
  dt <- data.table(
    cf = c("C", "C", "C"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    arco = c(1, 2, 1),  # Different arco values but consecutive periods
    prior = c(1, 1, 0.5),
    bonus = c(100, 200, 150)
  )
  
  result <- merge_consecutive_employment(dt)
  
  # All should be collapsed into one period since they're consecutive employment
  expect_equal(nrow(result), 1)
  expect_equal(result$inizio[1], as.Date("2023-01-01"))
  expect_equal(result$fine[1], as.Date("2023-03-31"))
  expect_true(result$collapsed[1])
  expect_equal(result$bonus_direction[1], 50)  # 150 - 100
  
  # Check duration-weighted mean for arco
  durations <- c(31, 28, 31)
  expected_arco <- round(weighted.mean(c(1, 2, 1), w = durations), 2)
  expect_equal(result$arco[1], expected_arco)
})

test_that("merge_consecutive_employment handles gaps in employment", {
  dt <- data.table(
    cf = c("D", "D", "D"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-03-15")),
    fine = as.Date(c("2023-01-31", "2023-03-14", "2023-03-31")),
    arco = c(1, 1, 1),  # All employment but with gap
    prior = c(1, 1, 1)
  )
  
  result <- merge_consecutive_employment(dt)
  
  # First period should be separate due to gap
  expect_equal(nrow(result), 2)
  expect_equal(result$fine[1], as.Date("2023-01-31"))
  expect_equal(result$inizio[2], as.Date("2023-03-01"))
  expect_equal(result$fine[2], as.Date("2023-03-31"))
})

test_that("merge_consecutive_employment handles multiple persons", {
  dt <- data.table(
    cf = c("E", "E", "F", "F"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-01-31", "2023-02-28")),
    arco = c(1, 1, 1, 0),
    prior = c(1, 1, 0.5, 0)
  )
  
  result <- merge_consecutive_employment(dt)
  
  # E should have 1 collapsed period, F should have 2 separate periods
  expect_equal(nrow(result[cf == "E"]), 1)
  expect_equal(nrow(result[cf == "F"]), 2)
  expect_true(result[cf == "E"]$collapsed[1])
  expect_false(result[cf == "F"]$collapsed[2])
})

test_that("merge_consecutive_employment uses duration-weighted means correctly", {
  # Create test data with truly consecutive periods and varying durations
  dt <- data.table(
    cf = c("TEST", "TEST", "TEST"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-02-06")),  
    fine = as.Date(c("2023-01-31", "2023-02-05", "2023-03-31")),    # 31, 5, 54 days (consecutive)
    arco = c(1, 1, 1),  # All employment periods
    prior = c(1, 0, 1),  # Different values to test weighting
    salary = c(3000, 1000, 2000)  # Different values to test weighting
  )
  
  result <- merge_consecutive_employment(dt)
  
  # Should have 1 collapsed period (all consecutive)
  expect_equal(nrow(result), 1)
  expect_true(result$collapsed[1])
  
  # Calculate expected duration-weighted means manually
  durations <- c(31, 5, 54)  # Days for each period  
  expected_prior <- round(weighted.mean(c(1, 0, 1), w = durations), 2)
  expected_salary <- round(weighted.mean(c(3000, 1000, 2000), w = durations), 2)
  
  # The longer periods should have more weight than short period
  expect_equal(result$prior[1], expected_prior)
  expect_equal(result$salary[1], expected_salary)
  
  # Verify that weighted mean gives different result than simple mean
  simple_prior_mean <- round(mean(c(1, 0, 1)), 2)
  simple_salary_mean <- round(mean(c(3000, 1000, 2000)), 2)
  expect_true(result$prior[1] != simple_prior_mean)  # Should be different due to weighting
  expect_true(result$salary[1] != simple_salary_mean)  # Should be different due to weighting
})

test_that("merge_consecutive_employment handles edge cases for weighted means", {
  # Test with all zero durations (should fall back to simple mean)
  dt_zero <- data.table(
    cf = c("ZERO", "ZERO"),
    inizio = as.Date(c("2023-01-01", "2023-01-01")),
    fine = as.Date(c("2023-01-01", "2023-01-01")),  # Zero duration (same start/end)
    arco = c(1, 1),
    prior = c(1, 0),
    salary = c(1000, 2000)
  )
  
  result_zero <- merge_consecutive_employment(dt_zero)
  expect_equal(nrow(result_zero), 1)
  expect_true(result_zero$collapsed[1])
  # Should fall back to simple mean when all weights are zero/equal
  expect_equal(result_zero$prior[1], round(mean(c(1, 0)), 2))
  
  # Test with missing values in numeric columns
  dt_na <- data.table(
    cf = c("NA_TEST", "NA_TEST", "NA_TEST"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
    arco = c(1, 1, 1),
    prior = c(1, NA, 1),  # Missing value in middle
    salary = c(1000, 2000, NA)  # Missing value at end
  )
  
  result_na <- merge_consecutive_employment(dt_na)
  expect_equal(nrow(result_na), 1)
  expect_true(result_na$collapsed[1])
  # Should handle NA values correctly in weighted.mean
  expect_false(is.na(result_na$prior[1]))  # Should get valid result excluding NA
  expect_false(is.na(result_na$salary[1]))  # Should get valid result excluding NA
})

test_that("merge_consecutive_employment_fast produces same results", {
  dt <- data.table(
    cf = c("G", "G", "G", "G"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-05-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31", "2023-05-31")),
    arco = c(1, 1, 0, 1),
    prior = c(1, 0.5, 0, 1),
    value = c(100, 150, NA, 200)
  )
  
  result_standard <- merge_consecutive_employment(dt)
  result_fast <- merge_consecutive_employment_fast(dt)
  
  # Compare key columns
  expect_equal(result_standard$cf, result_fast$cf)
  expect_equal(result_standard$inizio, result_fast$inizio)
  expect_equal(result_standard$fine, result_fast$fine)
  expect_equal(result_standard$arco, result_fast$arco)
  expect_equal(result_standard$prior, result_fast$prior)
  expect_equal(result_standard$collapsed, result_fast$collapsed)
})