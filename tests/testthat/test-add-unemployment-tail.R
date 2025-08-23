# Test add_unemployment_tail function

test_that("add_unemployment_tail works with basic employment ending before max_date", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  max_date <- as.Date("2025-12-31")
  
  # Act
  result <- add_unemployment_tail(vecshift_result, max_date)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)  # Original employment + unemployment tail
  
  # Add status classification
  result <- classify_employment_status(result)
  
  # Check original employment segment is preserved
  employment_segment <- result[arco > 0]
  expect_equal(nrow(employment_segment), 1)
  expect_equal(employment_segment$cf, "PERSON001")
  expect_equal(employment_segment$arco, 1L)
  expect_equal(employment_segment$stato, "occ_ft")
  
  # Check unemployment tail was added correctly
  unemployment_tail <- result[arco == 0]
  expect_equal(nrow(unemployment_tail), 1)
  expect_equal(unemployment_tail$cf, "PERSON001")
  expect_equal(unemployment_tail$arco, 0L)
  expect_equal(unemployment_tail$over_id, 0L)
  expect_equal(unemployment_tail$id, 0L)
  expect_equal(unemployment_tail$prior, 0L)
  expect_equal(unemployment_tail$stato, "disoccupato")
  expect_equal(unemployment_tail$inizio, as.Date("2024-01-01"))  # Last employment end + 1
  expect_equal(unemployment_tail$fine, max_date)
  expect_gt(unemployment_tail$durata, 0)
})

test_that("add_unemployment_tail handles multiple people correctly", {
  # Arrange
  test_data <- generate_test_data("multiple_people")
  vecshift_result <- vecshift(test_data)
  max_date <- as.Date("2025-06-30")
  
  # Act
  result <- add_unemployment_tail(vecshift_result, max_date)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Add status classification
  result <- classify_employment_status(result)
  
  # Check that both people get unemployment tails (both end in 2023)
  unemployment_tails <- result[arco == 0]
  expect_equal(length(unique(unemployment_tails$cf)), 2)
  expect_true(all(unemployment_tails$stato == "disoccupato"))
  
  # Check only the tail segments (those ending at max_date)
  tail_segments <- unemployment_tails[fine == max_date]
  expect_equal(nrow(tail_segments), 2)  # One tail per person
  expect_equal(length(unique(tail_segments$cf)), 2)  # Both people have tails
  expect_true(all(tail_segments$fine == max_date))
})

test_that("add_unemployment_tail doesn't add tail for employment ending at/after max_date", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  max_date <- as.Date("2023-12-31")  # Before employment ends
  
  # Act
  result <- add_unemployment_tail(vecshift_result, max_date)
  
  # Assert
  expect_equal(nrow(result), nrow(vecshift_result))  # No change
  expect_false(any(result$arco == 0))  # No unemployment periods added
})

test_that("add_unemployment_tail respects min_tail_duration parameter", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  max_date <- as.Date("2024-01-05")  # Only 5 days after employment ends
  min_tail_duration <- 10L  # Require at least 10 days
  
  # Act
  result <- add_unemployment_tail(vecshift_result, max_date, min_tail_duration)
  
  # Assert
  expect_equal(nrow(result), nrow(vecshift_result))  # No tail added due to duration constraint
  expect_false(any(result$arco == 0))  # No unemployment periods added
})

test_that("add_unemployment_tail works with min_tail_duration = 1", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  max_date <- as.Date("2024-01-02")  # Only 1 day after employment ends
  min_tail_duration <- 1L  # Allow minimum 1 day
  
  # Act
  result <- add_unemployment_tail(vecshift_result, max_date, min_tail_duration)
  
  # Assert
  expect_gt(nrow(result), nrow(vecshift_result))  # Tail should be added
  unemployment_tail <- result[arco == 0]
  expect_equal(as.numeric(unemployment_tail$durata), 2)  # 2 days duration (Jan 1-2)
})

test_that("add_unemployment_tail works with employment_with_gap scenario", {
  # Arrange
  test_data <- generate_test_data("employment_with_gap")
  vecshift_result <- vecshift(test_data)
  max_date <- as.Date("2025-12-31")
  
  # Act
  result <- add_unemployment_tail(vecshift_result, max_date)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), nrow(vecshift_result))  # Should add unemployment tail
  
  # Add status classification
  result <- classify_employment_status(result)
  
  # Check that unemployment tail is added after the last employment
  unemployment_periods <- result[arco == 0]
  expect_gte(nrow(unemployment_periods), 2)  # Original gap + new tail
  
  # Find the tail (should be the one ending at max_date)
  tail_segment <- unemployment_periods[fine == max_date]
  expect_equal(nrow(tail_segment), 1)
  expect_equal(tail_segment$cf, "PERSON001")
  expect_equal(tail_segment$stato, "disoccupato")
})

test_that("add_unemployment_tail preserves column structure and ordering", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  max_date <- as.Date("2025-12-31")
  original_cols <- names(vecshift_result)
  
  # Act
  result <- add_unemployment_tail(vecshift_result, max_date)
  
  # Assert
  expect_equal(names(result), original_cols)  # Same column structure
  # Check that data is ordered by cf and inizio
  expect_true(all(result$cf == result[order(cf, inizio)]$cf))  
  expect_true(all(result$inizio == result[order(cf, inizio)]$inizio))
})

test_that("add_unemployment_tail works without stato column (classify_status = FALSE)", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  max_date <- as.Date("2025-12-31")
  
  # Act
  result <- add_unemployment_tail(vecshift_result, max_date)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_false("stato" %in% names(result))  # No stato column
  expect_gt(nrow(result), nrow(vecshift_result))  # Tail added
  
  # Check unemployment tail properties
  unemployment_tail <- result[arco == 0]
  expect_equal(unemployment_tail$arco, 0L)
  expect_equal(unemployment_tail$over_id, 0L)
  expect_equal(unemployment_tail$id, 0L)
})

test_that("add_unemployment_tail handles numeric dates correctly", {
  # Arrange - create data with numeric dates
  numeric_data <- data.table(
    id = 1L,
    cf = "PERSON001",
    inizio = 19358,  # 2023-01-01 as numeric
    fine = 19723,    # 2023-12-31 as numeric
    prior = 1L
  )
  vecshift_result <- vecshift(numeric_data)
  max_date <- 20088  # 2025-12-31 as numeric
  
  # Act
  result <- add_unemployment_tail(vecshift_result, max_date)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), nrow(vecshift_result))
  
  unemployment_tail <- result[arco == 0]
  expect_equal(unemployment_tail$fine, max_date)
  expect_true(is.numeric(unemployment_tail$fine))
})

test_that("add_unemployment_tail returns original data for empty input", {
  # Arrange
  empty_data <- generate_test_data("empty")
  # Since empty data causes error in vecshift, create manual empty result
  empty_vecshift <- data.table(
    cf = character(0),
    inizio = as.Date(character(0)),
    fine = as.Date(character(0)),
    arco = integer(0),
    prior = integer(0),
    id = integer(0),
    over_id = integer(0),
    durata = integer(0),
    stato = character(0)
  )
  max_date <- as.Date("2025-12-31")
  
  # Act
  result <- add_unemployment_tail(empty_vecshift, max_date)
  
  # Assert
  expect_equal(nrow(result), 0L)
  expect_identical(result, empty_vecshift)
})

test_that("add_unemployment_tail validates input parameters correctly", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  
  # Test invalid vecshift_data
  expect_error(
    add_unemployment_tail(data.frame(x = 1), as.Date("2025-12-31")),
    "must be a data.table object"
  )
  
  # Test missing columns
  invalid_dt <- data.table(cf = "TEST", inizio = as.Date("2023-01-01"))
  expect_error(
    add_unemployment_tail(invalid_dt, as.Date("2025-12-31")),
    "Missing required vecshift columns"
  )
  
  # Test invalid max_date
  expect_error(
    add_unemployment_tail(vecshift_result, c(as.Date("2025-01-01"), as.Date("2025-12-31"))),
    "max_date must be a single Date or numeric value"
  )
  
  # Test invalid min_tail_duration
  expect_error(
    add_unemployment_tail(vecshift_result, as.Date("2025-12-31"), min_tail_duration = -1),
    "min_duration must be a non-negative integer"
  )
})

test_that("add_unemployment_tail handles Date/numeric conversion properly", {
  # Arrange - vecshift result has Date, max_date is numeric
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)  # Should have Date columns
  max_date_numeric <- 20088  # 2025-12-31 as numeric
  
  # Act
  result <- add_unemployment_tail(vecshift_result, max_date_numeric)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), nrow(vecshift_result))
  
  unemployment_tail <- result[arco == 0]
  expect_equal(unemployment_tail$fine, as.Date("2024-12-31"))  # Should convert to Date
  expect_true(inherits(unemployment_tail$fine, "Date"))
})