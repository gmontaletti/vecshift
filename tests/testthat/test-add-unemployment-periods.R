# Tests for add_unemployment_periods function (enhanced version with head and tail)

test_that("add_unemployment_periods works with head unemployment only", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  # Single employment from 2023-01-01 to 2023-12-31
  vecshift_result <- vecshift(test_data)
  min_date <- as.Date("2022-01-01")  # Year before employment starts
  
  # Act - Add head unemployment only
  result <- add_unemployment_periods(
    vecshift_result, 
    min_date = min_date, 
    add_head = TRUE, 
    add_tail = FALSE
  )
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)  # Original employment + unemployment head
  
  # Add status classification
  result <- classify_employment_status(result)
  
  # Check unemployment head was added correctly
  unemployment_head <- result[arco == 0]
  expect_equal(nrow(unemployment_head), 1)
  expect_equal(unemployment_head$cf, "PERSON001")
  expect_equal(unemployment_head$arco, 0L)
  expect_equal(unemployment_head$over_id, 0L)
  expect_equal(unemployment_head$id, 0L)
  expect_equal(unemployment_head$prior, 0L)
  expect_equal(unemployment_head$stato, "disoccupato")
  expect_equal(unemployment_head$inizio, min_date)
  expect_equal(unemployment_head$fine, as.Date("2022-12-31"))  # Employment start - 1
  expect_gt(unemployment_head$durata, 0)
  
  # Check original employment segment is preserved
  employment_segment <- result[arco > 0]
  expect_equal(nrow(employment_segment), 1)
  expect_equal(employment_segment$cf, "PERSON001")
  expect_equal(employment_segment$arco, 1L)
})

test_that("add_unemployment_periods works with tail unemployment only", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  max_date <- as.Date("2025-12-31")
  
  # Act - Add tail unemployment only (default behavior)
  result <- add_unemployment_periods(
    vecshift_result, 
    max_date = max_date, 
    add_head = FALSE, 
    add_tail = TRUE
  )
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)  # Original employment + unemployment tail
  
  # Add status classification
  result <- classify_employment_status(result)
  
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

test_that("add_unemployment_periods works with both head and tail", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  min_date <- as.Date("2022-01-01")
  max_date <- as.Date("2025-12-31")
  
  # Act - Add both head and tail unemployment
  result <- add_unemployment_periods(
    vecshift_result, 
    min_date = min_date,
    max_date = max_date,
    add_head = TRUE, 
    add_tail = TRUE
  )
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)  # Head unemployment + employment + tail unemployment
  
  # Add status classification
  result <- classify_employment_status(result)
  
  # Check unemployment periods
  unemployment_periods <- result[arco == 0]
  expect_equal(nrow(unemployment_periods), 2)
  expect_true(all(unemployment_periods$stato == "disoccupato"))
  expect_true(all(unemployment_periods$over_id == 0L))
  expect_true(all(unemployment_periods$id == 0L))
  expect_true(all(unemployment_periods$prior == 0L))
  
  # Check head unemployment (should be first chronologically)
  head_unemployment <- unemployment_periods[inizio == min_date]
  expect_equal(nrow(head_unemployment), 1)
  expect_equal(head_unemployment$fine, as.Date("2022-12-31"))
  
  # Check tail unemployment (should be last chronologically)
  tail_unemployment <- unemployment_periods[fine == max_date]
  expect_equal(nrow(tail_unemployment), 1)
  expect_equal(tail_unemployment$inizio, as.Date("2024-01-01"))
  
  # Check temporal ordering is maintained
  expect_true(all(result$inizio == result[order(cf, inizio)]$inizio))
})

test_that("add_unemployment_periods doesn't add head for employment starting at/before min_date", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  min_date <- as.Date("2023-01-01")  # Same as employment start
  
  # Act
  result <- add_unemployment_periods(
    vecshift_result, 
    min_date = min_date, 
    add_head = TRUE, 
    add_tail = FALSE
  )
  
  # Assert
  expect_equal(nrow(result), nrow(vecshift_result))  # No change
  expect_false(any(result$arco == 0))  # No unemployment periods added
})

test_that("add_unemployment_periods respects min_duration for head unemployment", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  min_date <- as.Date("2022-12-28")  # Only 4 days before employment starts
  min_duration <- 10L  # Require at least 10 days
  
  # Act
  result <- add_unemployment_periods(
    vecshift_result, 
    min_date = min_date, 
    min_duration = min_duration,
    add_head = TRUE, 
    add_tail = FALSE
  )
  
  # Assert
  expect_equal(nrow(result), nrow(vecshift_result))  # No head added due to duration constraint
  expect_false(any(result$arco == 0))  # No unemployment periods added
})

test_that("add_unemployment_periods handles multiple people with head unemployment", {
  # Arrange
  test_data <- generate_test_data("multiple_people")
  vecshift_result <- vecshift(test_data)
  min_date <- as.Date("2022-01-01")  # Before both people's first employment
  
  # Act
  result <- add_unemployment_periods(
    vecshift_result, 
    min_date = min_date, 
    add_head = TRUE, 
    add_tail = FALSE
  )
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Add status classification
  result <- classify_employment_status(result)
  
  # Check that both people get unemployment heads
  unemployment_heads <- result[arco == 0 & inizio == min_date]
  expect_equal(length(unique(unemployment_heads$cf)), 2)
  expect_true(all(unemployment_heads$stato == "disoccupato"))
  
  # Check each person has exactly one head
  for (person in unique(unemployment_heads$cf)) {
    person_head <- unemployment_heads[cf == person]
    expect_equal(nrow(person_head), 1)
    expect_equal(person_head$inizio, min_date)
  }
})

test_that("add_unemployment_periods works with employment_with_gap scenario", {
  # Arrange
  test_data <- generate_test_data("employment_with_gap")
  vecshift_result <- vecshift(test_data)
  min_date <- as.Date("2022-06-01")
  max_date <- as.Date("2025-06-30")
  
  # Act
  result <- add_unemployment_periods(
    vecshift_result, 
    min_date = min_date, 
    max_date = max_date,
    add_head = TRUE, 
    add_tail = TRUE
  )
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), nrow(vecshift_result))  # Should add unemployment periods
  
  # Add status classification
  result <- classify_employment_status(result)
  
  # Check that both head and tail unemployment are added
  unemployment_periods <- result[arco == 0]
  expect_gte(nrow(unemployment_periods), 3)  # Original gap + head + tail
  
  # Find the head (should start at min_date)
  head_segment <- unemployment_periods[inizio == min_date]
  expect_equal(nrow(head_segment), 1)
  expect_equal(head_segment$cf, "PERSON001")
  expect_equal(head_segment$stato, "disoccupato")
  
  # Find the tail (should end at max_date)
  tail_segment <- unemployment_periods[fine == max_date]
  expect_equal(nrow(tail_segment), 1)
  expect_equal(tail_segment$cf, "PERSON001")
  expect_equal(tail_segment$stato, "disoccupato")
})

test_that("add_unemployment_periods validates parameters correctly", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  
  # Test require at least one of add_head or add_tail
  expect_error(
    add_unemployment_periods(vecshift_result, add_head = FALSE, add_tail = FALSE),
    "At least one of add_head or add_tail must be TRUE"
  )
  
  # Test require min_date when add_head = TRUE
  expect_error(
    add_unemployment_periods(vecshift_result, add_head = TRUE, add_tail = FALSE),
    "min_date must be specified when add_head = TRUE"
  )
  
  # Test require max_date when add_tail = TRUE  
  expect_error(
    add_unemployment_periods(vecshift_result, add_head = FALSE, add_tail = TRUE),
    "max_date must be specified when add_tail = TRUE"
  )
  
  # Test invalid min_date
  expect_error(
    add_unemployment_periods(vecshift_result, 
                           min_date = c(as.Date("2022-01-01"), as.Date("2022-12-31")), 
                           add_head = TRUE, add_tail = FALSE),
    "min_date must be a single Date or numeric value"
  )
  
  # Test invalid min_duration
  expect_error(
    add_unemployment_periods(vecshift_result, 
                           min_date = as.Date("2022-01-01"), 
                           min_duration = -1, 
                           add_head = TRUE, add_tail = FALSE),
    "min_duration must be a non-negative integer"
  )
})

test_that("add_unemployment_periods works without stato column", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  min_date <- as.Date("2022-01-01")
  max_date <- as.Date("2025-12-31")
  
  # Act
  result <- add_unemployment_periods(
    vecshift_result, 
    min_date = min_date,
    max_date = max_date,
    add_head = TRUE, 
    add_tail = TRUE
  )
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_false("stato" %in% names(result))  # No stato column
  expect_gt(nrow(result), nrow(vecshift_result))  # Both head and tail added
  
  # Check unemployment periods properties
  unemployment_periods <- result[arco == 0]
  expect_equal(nrow(unemployment_periods), 2)  # Head and tail
  expect_true(all(unemployment_periods$arco == 0L))
  expect_true(all(unemployment_periods$over_id == 0L))
  expect_true(all(unemployment_periods$id == 0L))
})

test_that("add_unemployment_periods handles numeric dates correctly", {
  # Arrange - create data with numeric dates
  numeric_data <- data.table(
    id = 1L,
    cf = "PERSON001",
    inizio = 19358,  # 2023-01-01 as numeric
    fine = 19723,    # 2023-12-31 as numeric
    prior = 1L
  )
  vecshift_result <- vecshift(numeric_data)
  min_date_numeric <- 18993  # 2022-01-01 as numeric
  max_date_numeric <- 20088  # 2025-12-31 as numeric
  
  # Act
  result <- add_unemployment_periods(
    vecshift_result, 
    min_date = min_date_numeric,
    max_date = max_date_numeric,
    add_head = TRUE, 
    add_tail = TRUE
  )
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), nrow(vecshift_result))
  
  unemployment_periods <- result[arco == 0]
  expect_equal(nrow(unemployment_periods), 2)  # Head and tail
  expect_true(all(is.numeric(unemployment_periods$inizio)))
  expect_true(all(is.numeric(unemployment_periods$fine)))
  
  # Check head unemployment
  head_unemployment <- unemployment_periods[inizio == min_date_numeric]
  expect_equal(nrow(head_unemployment), 1)
  expect_equal(head_unemployment$fine, 19357)  # 2022-12-31 as numeric
  
  # Check tail unemployment
  tail_unemployment <- unemployment_periods[fine == max_date_numeric]
  expect_equal(nrow(tail_unemployment), 1)
  expect_equal(tail_unemployment$inizio, 19724)  # 2024-01-01 as numeric
})

test_that("add_unemployment_periods returns original data when no periods need to be added", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  # Use dates that don't trigger head or tail addition
  min_date <- as.Date("2023-01-01")  # Same as employment start
  max_date <- as.Date("2023-12-31")  # Same as employment end
  
  # Act
  result <- add_unemployment_periods(
    vecshift_result, 
    min_date = min_date,
    max_date = max_date,
    add_head = TRUE, 
    add_tail = TRUE
  )
  
  # Assert
  expect_identical(result, vecshift_result)  # Should be exactly the same
  expect_false(any(result$arco == 0))  # No unemployment periods
})

test_that("add_unemployment_periods preserves column structure and ordering", {
  # Arrange
  test_data <- generate_test_data("single_employment")
  vecshift_result <- vecshift(test_data)
  min_date <- as.Date("2022-01-01")
  max_date <- as.Date("2025-12-31")
  original_cols <- names(vecshift_result)
  
  # Act
  result <- add_unemployment_periods(
    vecshift_result, 
    min_date = min_date,
    max_date = max_date,
    add_head = TRUE, 
    add_tail = TRUE
  )
  
  # Assert
  expect_equal(names(result), original_cols)  # Same column structure
  # Check that data is ordered by cf and inizio
  expect_true(all(result$cf == result[order(cf, inizio)]$cf))  
  expect_true(all(result$inizio == result[order(cf, inizio)]$inizio))
})