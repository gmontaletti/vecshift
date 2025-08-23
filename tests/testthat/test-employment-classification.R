# Test employment status classification logic in vecshift function

test_that("vecshift classifies unemployment periods correctly", {
  # Arrange - Test both short and long unemployment periods
  test_data <- data.table::data.table(
    id = c(1L, 2L, 3L, 4L),
    cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-05-01")),
    fine = as.Date(c("2023-01-15", "2023-02-15", "2023-04-01", "2023-12-31")),
    prior = c(1L, 1L, 1L, 1L)
  )
  
  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)
  
  # Assert
  unemployment_periods <- result[stato == "disoccupato"]
  expect_gt(nrow(unemployment_periods), 0)
  
  # All unemployment periods should have arco = 0 and id = 0
  expect_true(all(unemployment_periods$arco == 0))
  expect_true(all(unemployment_periods$id == 0))
})

test_that("vecshift correctly identifies full-time employment", {
  # Arrange - Various full-time prior values
  test_data <- data.table::data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-05-01", "2023-09-01")),
    fine = as.Date(c("2023-04-30", "2023-08-31", "2023-12-31")),
    prior = c(1L, 5L, 100L)  # All positive values should be full-time
  )
  
  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)
  
  # Assert
  employment_periods <- result[arco == 1]
  expect_gt(nrow(employment_periods), 0)  # Should have employment periods
  
  # All employment periods should be full-time
  ft_periods <- employment_periods[stato == "occ_ft"]
  expect_gt(nrow(ft_periods), 0)
  expect_true(all(ft_periods$prior == 1))  # Should be normalized to 1
})

test_that("vecshift correctly identifies part-time employment", {
  # Arrange - Various part-time prior values (0 and negative)
  test_data <- data.table::data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-05-01", "2023-09-01")),
    fine = as.Date(c("2023-04-30", "2023-08-31", "2023-12-31")),
    prior = c(0L, -1L, -5L)  # Zero and negative values should be part-time
  )
  
  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)
  
  # Assert
  employment_periods <- result[arco == 1]
  expect_equal(nrow(employment_periods), 3)
  expect_true(all(employment_periods$stato == "occ_pt"))
  expect_true(all(employment_periods$prior == 0))  # Should be normalized to 0
})

test_that("vecshift classifies overlapping employment transitions correctly", {
  # Test all four types of overlapping employment transitions
  
  # Test over_pt_ft: part-time job starts, then full-time overlaps
  test_pt_ft <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01")),
    fine = as.Date(c("2023-06-30", "2023-05-31")),
    prior = c(0L, 1L)  # PT first, then FT
  )
  result_pt_ft <- vecshift(test_pt_ft)
  result_pt_ft <- classify_employment_status(result_pt_ft)
  overlap_pt_ft <- result_pt_ft[arco > 1]
  expect_true("over_pt_ft" %in% overlap_pt_ft$stato)
  
  # Test over_ft_pt: full-time job starts, then part-time overlaps
  test_ft_pt <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01")),
    fine = as.Date(c("2023-06-30", "2023-05-31")),
    prior = c(1L, 0L)  # FT first, then PT
  )
  result_ft_pt <- vecshift(test_ft_pt)
  result_ft_pt <- classify_employment_status(result_ft_pt)
  overlap_ft_pt <- result_ft_pt[arco > 1]
  expect_true("over_ft_pt" %in% overlap_ft_pt$stato)
  
  # Test over_pt_pt: multiple part-time jobs overlap
  test_pt_pt <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01")),
    fine = as.Date(c("2023-06-30", "2023-05-31")),
    prior = c(0L, 0L)  # Both PT
  )
  result_pt_pt <- vecshift(test_pt_pt)
  result_pt_pt <- classify_employment_status(result_pt_pt)
  overlap_pt_pt <- result_pt_pt[arco > 1]
  expect_true("over_pt_pt" %in% overlap_pt_pt$stato)
  
  # Test over_ft_ft: multiple full-time jobs overlap
  test_ft_ft <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01")),
    fine = as.Date(c("2023-06-30", "2023-05-31")),
    prior = c(1L, 1L)  # Both FT
  )
  result_ft_ft <- vecshift(test_ft_ft)
  result_ft_ft <- classify_employment_status(result_ft_ft)
  overlap_ft_ft <- result_ft_ft[arco > 1]
  expect_true("over_ft_ft" %in% overlap_ft_ft$stato)
})

test_that("vecshift status classification uses shift() correctly for overlaps", {
  # Test that the shift logic works correctly for determining overlap types
  # This is specifically testing the logic: prior > shift(prior, type = "lag")
  
  # Create a scenario where we can verify the shift behavior
  test_data <- data.table::data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-12-31", "2023-12-31", "2023-04-30")),  # 1&2 overlap fully, 3 ends early
    prior = c(0L, 1L, 0L)  # PT -> FT -> back to PT only
  )
  
  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Find overlap periods and verify their classifications
  overlaps <- result[arco > 1]
  expect_gt(nrow(overlaps), 0)
  
  # Should include transitions from PT to FT and FT to PT
  overlap_states <- overlaps$stato
  expected_states <- c("over_pt_ft", "over_ft_pt")
  expect_true(any(overlap_states %in% expected_states))
})

test_that("vecshift handles default overlap classification", {
  # Test the default case in fcase for overlapping employment
  # This should catch any overlap scenario not explicitly handled
  
  # Create a complex overlapping scenario
  test_data <- data.table::data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-02-01")),
    fine = as.Date(c("2023-12-31", "2023-12-31", "2023-03-31")),
    prior = c(1L, 2L, 3L)  # All different positive values (all full-time)
  )
  
  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)
  
  # Assert
  overlaps <- result[arco > 1]
  if (nrow(overlaps) > 0) {
    # The default case should be "over_ft_ft" for full-time overlaps
    # when conditions don't match the specific transition cases
    expect_true("over_ft_ft" %in% overlaps$stato)
  }
})

test_that("vecshift prior normalization works correctly", {
  # Test that prior values are correctly normalized according to:
  # prior := fcase(prior <= 0, 0, default = 1)
  
  test_data <- data.table::data.table(
    id = c(1L, 2L, 3L, 4L, 5L),
    cf = rep("PERSON001", 5),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-05-01", "2023-07-01", "2023-09-01")),
    fine = as.Date(c("2023-02-28", "2023-04-30", "2023-06-30", "2023-08-31", "2023-10-31")),
    prior = c(-10L, -1L, 0L, 1L, 100L)  # Various values to test normalization
  )
  
  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)
  
  # Assert
  # Check that prior values are normalized to 0 or 1
  expect_true(all(result$prior %in% c(0L, 1L)))
  
  # Check specific employment classifications
  employment_periods <- result[arco == 1]
  
  # Negative and zero prior should become part-time (0)
  pt_periods <- employment_periods[prior == 0]
  expect_true(all(pt_periods$stato == "occ_pt"))
  
  # Positive prior should become full-time (1)
  ft_periods <- employment_periods[prior == 1]
  expect_true(all(ft_periods$stato == "occ_ft"))
})

test_that("vecshift status logic covers all possible combinations", {
  # Comprehensive test to ensure all status classifications are reachable
  
  # Single employments
  single_ft <- generate_test_data("single_employment")
  result_single_ft <- vecshift(single_ft)
  result_single_ft <- classify_employment_status(result_single_ft)
  expect_true("occ_ft" %in% result_single_ft$stato)
  
  single_pt <- generate_test_data("single_parttime")
  result_single_pt <- vecshift(single_pt)
  result_single_pt <- classify_employment_status(result_single_pt)
  expect_true("occ_pt" %in% result_single_pt$stato)
  
  # Unemployment
  gap_data <- generate_test_data("employment_with_gap")
  result_gap <- vecshift(gap_data)
  result_gap <- classify_employment_status(result_gap)
  expect_true("disoccupato" %in% result_gap$stato)
  
  # All overlap types should be testable
  overlap_types <- c("over_pt_ft", "over_ft_pt", "over_pt_pt", "over_ft_ft")
  
  for (overlap_type in overlap_types) {
    # Create test data for each overlap type
    if (overlap_type == "over_pt_ft") {
      test_data <- data.table::data.table(
        id = c(1L, 2L), cf = c("PERSON001", "PERSON001"),
        inizio = as.Date(c("2023-01-01", "2023-03-01")),
        fine = as.Date(c("2023-06-30", "2023-05-31")),
        prior = c(0L, 1L)
      )
    } else if (overlap_type == "over_ft_pt") {
      test_data <- data.table::data.table(
        id = c(1L, 2L), cf = c("PERSON001", "PERSON001"),
        inizio = as.Date(c("2023-01-01", "2023-03-01")),
        fine = as.Date(c("2023-06-30", "2023-05-31")),
        prior = c(1L, 0L)
      )
    } else if (overlap_type == "over_pt_pt") {
      test_data <- data.table::data.table(
        id = c(1L, 2L), cf = c("PERSON001", "PERSON001"),
        inizio = as.Date(c("2023-01-01", "2023-03-01")),
        fine = as.Date(c("2023-06-30", "2023-05-31")),
        prior = c(0L, 0L)
      )
    } else { # over_ft_ft
      test_data <- data.table::data.table(
        id = c(1L, 2L), cf = c("PERSON001", "PERSON001"),
        inizio = as.Date(c("2023-01-01", "2023-03-01")),
        fine = as.Date(c("2023-06-30", "2023-05-31")),
        prior = c(1L, 1L)
      )
    }
    
    result <- vecshift(test_data)
    result <- classify_employment_status(result)
    overlaps <- result[arco > 1]
    expect_true(overlap_type %in% overlaps$stato, 
                info = paste("Failed to find overlap type:", overlap_type))
  }
})