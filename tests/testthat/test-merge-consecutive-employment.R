library(testthat)
library(data.table)

test_that("merge_consecutive_employment collapses consecutive working periods", {
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
  expect_equal(result$prior[1], round(mean(c(1, 0.5, 1)), 2))
  expect_equal(result$salary[1], round(mean(c(3000, 2500, 3200)), 2))
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

test_that("merge_consecutive_employment handles overlapping periods", {
  dt <- data.table(
    cf = c("C", "C", "C"),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-02-01")),
    fine = as.Date(c("2023-01-31", "2023-02-15", "2023-02-28")),
    arco = c(1, 2, 1),  # Overlapping in the middle
    prior = c(1, 1, 0.5),
    bonus = c(100, 200, 150)
  )
  
  result <- merge_consecutive_employment(dt)
  
  # All should be collapsed into one period
  expect_equal(nrow(result), 1)
  expect_equal(result$inizio[1], as.Date("2023-01-01"))
  expect_equal(result$fine[1], as.Date("2023-02-28"))
  expect_true(result$collapsed[1])
  expect_equal(result$bonus_direction[1], 50)  # 150 - 100
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