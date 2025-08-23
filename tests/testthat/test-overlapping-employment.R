# Test overlapping employment scenarios in vecshift function

test_that("vecshift handles basic overlapping employment (full-time and part-time)", {
  # Arrange
  test_data <- generate_test_data("overlapping_employment")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 1)  # Should have multiple segments
  
  # Check that we have overlapping employment status
  overlap_segments <- result[arco > 1]
  expect_gt(nrow(overlap_segments), 0)  # Should have at least one overlapping segment
  
  # Check overlap classifications
  overlap_states <- unique(overlap_segments$stato)
  expected_overlap_states <- c("over_pt_ft", "over_ft_pt", "over_pt_pt", "over_ft_ft")
  expect_true(any(overlap_states %in% expected_overlap_states))
})

test_that("vecshift correctly classifies part-time to full-time overlap", {
  # Arrange - Specific scenario where part-time starts first, then full-time overlaps
  test_data <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01")),  # PT starts first
    fine = as.Date(c("2023-06-30", "2023-05-31")),    # FT overlaps and ends first
    prior = c(0L, 1L)  # part-time then full-time
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Find overlapping segments
  overlap_segments <- result[arco > 1]
  expect_gt(nrow(overlap_segments), 0)
  
  # Should have over_pt_ft status (part-time to full-time transition)
  expect_true("over_pt_ft" %in% overlap_segments$stato)
})

test_that("vecshift correctly classifies full-time to part-time overlap", {
  # Arrange - Full-time starts first, then part-time overlaps
  test_data <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-03-01")),  # FT starts first
    fine = as.Date(c("2023-06-30", "2023-05-31")),    # PT overlaps
    prior = c(1L, 0L)  # full-time then part-time
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Find overlapping segments
  overlap_segments <- result[arco > 1]
  expect_gt(nrow(overlap_segments), 0)
  
  # Should have over_ft_pt status (full-time to part-time transition)
  expect_true("over_ft_pt" %in% overlap_segments$stato)
})

test_that("vecshift handles multiple part-time overlapping jobs", {
  # Arrange
  test_data <- generate_test_data("all_parttime_overlap")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Find overlapping segments
  overlap_segments <- result[arco > 1]
  expect_gt(nrow(overlap_segments), 0)
  
  # Should have over_pt_pt status (part-time to part-time)
  expect_true("over_pt_pt" %in% overlap_segments$stato)
})

test_that("vecshift handles multiple full-time overlapping jobs", {
  # Arrange
  test_data <- generate_test_data("all_fulltime_overlap")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Find overlapping segments
  overlap_segments <- result[arco > 1]
  expect_gt(nrow(overlap_segments), 0)
  
  # Should have over_ft_ft status (full-time to full-time)
  expect_true("over_ft_ft" %in% overlap_segments$stato)
})

test_that("vecshift handles complex overlapping scenario with three jobs", {
  # Arrange
  test_data <- generate_test_data("complex_overlapping")
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 3)  # Should have multiple segments
  
  # Check that arco values are correct
  max_arco <- max(result$arco)
  expect_gte(max_arco, 2)  # Should have at least 2 overlapping jobs at some point
  
  # Check that we have various employment states
  states <- unique(result$stato)
  expect_true(length(states) > 1)  # Should have multiple different states
  
  # Should have both single employment and overlapping employment periods
  expect_true(any(result$arco == 1))  # Single employment
  expect_true(any(result$arco > 1))   # Overlapping employment
})

test_that("vecshift calculates arco (overlap count) correctly", {
  # Arrange - Create exactly 3 overlapping jobs
  test_data <- data.table::data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-12-31", "2023-12-31", "2023-12-31")),  # All end same time
    prior = c(1L, 1L, 1L)  # All full-time
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Check maximum overlap
  max_arco <- max(result$arco)
  expect_equal(max_arco, 3L)  # Should have 3 overlapping jobs at peak
  
  # Check that arco progression makes sense
  arco_values <- sort(unique(result$arco))
  expect_true(1L %in% arco_values)  # Should start with 1 job
  expect_true(3L %in% arco_values)  # Should reach 3 jobs
})

test_that("vecshift handles partial overlaps correctly", {
  # Arrange - Jobs that partially overlap (not fully nested)
  test_data <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-06-01")),
    fine = as.Date(c("2023-09-30", "2023-12-31")),  # Overlap Jun-Sep
    prior = c(1L, 0L)
  )
  
  # Act
  result <- vecshift(test_data)
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Should have three periods: single FT, overlap, single PT
  expected_periods <- 3
  expect_equal(nrow(result), expected_periods)
  
  # Check the sequence
  expect_equal(result$arco[1], 1L)  # First period: single job
  expect_equal(result$arco[2], 2L)  # Second period: overlap
  expect_equal(result$arco[3], 1L)  # Third period: single job again
  
  # Check states
  expect_equal(result$prior[1], "occ_ft")     # Single full-time
  expect_true(result$stato[2] %in% c("over_pt_ft", "over_ft_pt"))  # Overlap
  expect_equal(result$stato[3], "occ_pt")     # Single part-time
})

test_that("vecshift overlap states match employment type transitions", {
  # Test each specific overlap transition type
  
  # Part-time to Full-time overlap
  pt_to_ft <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-06-01")),
    fine = as.Date(c("2023-12-31", "2023-09-30")),
    prior = c(0L, 1L)  # PT first, then FT
  )
  
  result_pt_ft <- vecshift(pt_to_ft)
  overlap_pt_ft <- result_pt_ft[arco > 1]
  expect_true("over_pt_ft" %in% overlap_pt_ft$stato)
  
  # Full-time to Part-time overlap
  ft_to_pt <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-06-01")),
    fine = as.Date(c("2023-12-31", "2023-09-30")),
    prior = c(1L, 0L)  # FT first, then PT
  )
  
  result_ft_pt <- vecshift(ft_to_pt)
  overlap_ft_pt <- result_ft_pt[arco > 1]
  expect_true("over_ft_pt" %in% overlap_ft_pt$stato)
})