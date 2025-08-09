# Test edge cases and boundary conditions for vecshift function

test_that("vecshift handles zero-duration employment correctly", {
  # Arrange - Employment that starts and ends on same day
  test_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2023-06-15"),
    FINE = as.Date("2023-06-15"),  # Same day
    prior = 1L
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(as.numeric(result$durata), 1)  # Duration is difftime object
  expect_equal(result$stato, "occ_ft")
})

test_that("vecshift handles very short gaps between employment", {
  # Arrange - Employment periods with gap (need larger gap to see unemployment)
  test_data <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-01-05")),  # 2-day gap after first job ends
    FINE = as.Date(c("2023-01-02", "2023-01-06")),
    prior = c(1L, 1L)
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Should have unemployment period between jobs
  unemployment <- result[stato == "disoccupato"]
  expect_equal(nrow(unemployment), 1)
  expect_equal(as.numeric(unemployment$durata), 1)  # Duration is difftime object
})

test_that("vecshift handles employment starting at year boundary", {
  # Arrange - Employment crossing year boundary
  test_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2023-12-15"),
    FINE = as.Date("2024-01-15"),  # Crosses year boundary
    prior = 1L
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(as.numeric(result$durata), 32)  # Duration is difftime object
  expect_equal(result$stato, "occ_ft")
})

test_that("vecshift handles leap year dates correctly", {
  # Arrange - Employment during leap year including Feb 29
  test_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2024-02-28"),  # 2024 is leap year
    FINE = as.Date("2024-03-01"),    # Includes Feb 29
    prior = 1L
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(as.numeric(result$durata), 3)  # Duration is difftime object
})

test_that("vecshift handles numeric dates (Unix timestamps)", {
  # Arrange - Use numeric representations of dates
  start_date <- as.numeric(as.Date("2023-01-01"))
  end_date <- as.numeric(as.Date("2023-12-31"))
  
  test_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = start_date,
    FINE = end_date,
    prior = 1L
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(as.numeric(result$durata), 365)  # Duration is difftime object
  expect_equal(result$stato, "occ_ft")
})

test_that("vecshift handles very large prior values", {
  # Arrange - Test with large positive prior values (should all be full-time)
  test_data <- data.table::data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-05-01", "2023-09-01")),
    FINE = as.Date(c("2023-04-30", "2023-08-31", "2023-12-31")),
    prior = c(999L, 1000000L, 1L)  # Various large positive values
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Check that all positive prior values are normalized to 1
  employment_periods <- result[arco > 0]  # Exclude unemployment periods
  expect_true(all(employment_periods$prior %in% c(0, 1)))
  
  # Full-time employment periods should have prior = 1
  ft_periods <- employment_periods[stato == "occ_ft"]
  if (nrow(ft_periods) > 0) {
    expect_true(all(ft_periods$prior == 1))
  }
})

test_that("vecshift handles zero duration segments appropriately", {
  # This tests the durata > 0 filter at the end of the function
  # Arrange - Create a scenario that might produce zero-duration segments
  test_data <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-01-02")),
    FINE = as.Date(c("2023-01-01", "2023-01-02")),  # Consecutive single days
    prior = c(1L, 1L)
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # All segments should have positive duration
  expect_true(all(result$durata > 0))
})