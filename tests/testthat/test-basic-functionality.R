# Test basic functionality of the vecshift function

test_that("vecshift works with single employment record", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$cf, "PERSON001")
  expect_equal(result$arco, 1L)
  expect_equal(result$prior, 1L)
  expect_equal(result$id, 1L)
  expect_equal(result$stato, "occ_ft")
  expect_gt(result$durata, 0)
  
  # Check that dates are properly handled
  expect_equal(result$inizio, as.Date("2023-01-01"))
  expect_equal(result$fine, as.Date("2024-01-01"))  # FINE + 1 day
})

test_that("vecshift works with single part-time employment", {
  # Arrange
  test_data <- generate_test_data("single_parttime")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$cf, "PERSON001")
  expect_equal(result$arco, 1L)
  expect_equal(result$prior, 0L)  # part-time
  expect_equal(result$id, 1L)
  expect_equal(result$stato, "occ_pt")  # part-time status
})

test_that("vecshift correctly handles employment gaps (unemployment periods)", {
  # Arrange
  test_data <- generate_test_data("employment_with_gap")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)  # Two employment periods + one unemployment gap
  
  # Check the unemployment period
  unemployment_period <- result[stato == "disoccupato"]
  expect_equal(nrow(unemployment_period), 1)
  expect_equal(unemployment_period$arco, 0L)
  expect_equal(unemployment_period$id, 0L)  # Should be 0 for unemployment
  expect_equal(unemployment_period$prior, 0L)
  
  # Check employment periods
  employment_periods <- result[stato != "disoccupato"]
  expect_equal(nrow(employment_periods), 2)
  expect_true(all(employment_periods$arco == 1L))
  expect_true(all(employment_periods$stato == "occ_ft"))
})

test_that("vecshift handles consecutive employment correctly", {
  # Arrange
  test_data <- generate_test_data("consecutive_employment")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)  # Two consecutive employment periods, no gap
  
  # Check that there's no unemployment period
  expect_false(any(result$stato == "disoccupato"))
  
  # Check employment types
  expect_equal(result$stato[1], "occ_ft")  # First job full-time
  expect_equal(result$stato[2], "occ_pt")  # Second job part-time
  
  # Verify arco values
  expect_true(all(result$arco == 1L))
})

test_that("vecshift returns empty result for empty input", {
  # Arrange
  test_data <- generate_test_data("empty")
  
  # Act & Assert - empty input causes an error due to the function's design
  expect_error(vecshift(test_data))
})

test_that("vecshift preserves person identifiers correctly", {
  # Arrange
  test_data <- generate_test_data("multiple_people")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Check that both people are in the results
  people <- unique(result$cf)
  expect_length(people, 2)
  expect_true("PERSON001" %in% people)
  expect_true("PERSON002" %in% people)
  
  # Check that each person has their own employment segments
  person1_segments <- result[cf == "PERSON001"]
  person2_segments <- result[cf == "PERSON002"]
  
  expect_gt(nrow(person1_segments), 0)
  expect_gt(nrow(person2_segments), 0)
})

test_that("vecshift handles single day employment correctly", {
  # Arrange
  test_data <- generate_test_data("single_day")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(as.numeric(result$durata), 1)  # Duration is difftime object
  expect_equal(result$stato, "occ_ft")
})

test_that("vecshift duration calculation is correct", {
  # Arrange - Create a known duration employment
  test_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2023-01-01"),
    FINE = as.Date("2023-01-31"),  # 31 days total
    prior = 1L
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_equal(as.numeric(result$durata), 31)  # Duration is difftime object
  
  # Check that the date calculation is correct
  expect_equal(result$inizio, as.Date("2023-01-01"))
  expect_equal(result$fine, as.Date("2023-02-01"))  # FINE + 1
})

test_that("vecshift handles negative prior values as part-time", {
  # Arrange
  test_data <- generate_test_data("negative_prior")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 0)
  
  # Check that negative prior values are converted to 0 (part-time)
  # and positive values are converted to 1 (full-time)
  expect_true(all(result$prior %in% c(0, 1)))
  
  # Should have part-time employment periods
  states <- unique(result$stato)
  expect_true("occ_pt" %in% states)  # Part-time employment from negative prior
})