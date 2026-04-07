# Test edge cases and boundary conditions for vecshift function

test_that("vecshift handles zero-duration employment correctly", {
  # Arrange - Employment that starts and ends on same day
  test_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    inizio = as.Date("2023-06-15"),
    fine = as.Date("2023-06-15"), # Same day
    prior = 1L
  )

  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)

  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(as.numeric(result$durata), 1) # Duration is difftime object
  expect_equal(result$stato, "occ_ft")
})

test_that("vecshift handles very short gaps between employment", {
  # Arrange - Employment periods with gap (need larger gap to see unemployment)
  test_data <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-01-05")), # 2-day gap after first job ends
    fine = as.Date(c("2023-01-02", "2023-01-06")),
    prior = c(1L, 1L)
  )

  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)

  # Assert
  expect_s3_class(result, "data.table")

  # Should have unemployment period between jobs
  unemployment <- result[stato == "disoccupato"]
  expect_equal(nrow(unemployment), 1)
  expect_equal(as.numeric(unemployment$durata), 2) # Gap: Jan 3 + Jan 4 = 2 days
})

test_that("vecshift handles employment starting at year boundary", {
  # Arrange - Employment crossing year boundary
  test_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    inizio = as.Date("2023-12-15"),
    fine = as.Date("2024-01-15"), # Crosses year boundary
    prior = 1L
  )

  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)

  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(as.numeric(result$durata), 32) # Duration is difftime object
  expect_equal(result$stato, "occ_ft")
})

test_that("vecshift handles leap year dates correctly", {
  # Arrange - Employment during leap year including Feb 29
  test_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    inizio = as.Date("2024-02-28"), # 2024 is leap year
    fine = as.Date("2024-03-01"), # Includes Feb 29
    prior = 1L
  )

  # Act
  result <- vecshift(test_data)

  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(as.numeric(result$durata), 3) # Duration is difftime object
})

test_that("vecshift handles numeric dates (Unix timestamps)", {
  # Arrange - Use numeric representations of dates
  start_date <- as.numeric(as.Date("2023-01-01"))
  end_date <- as.numeric(as.Date("2023-12-31"))

  test_data <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    inizio = start_date,
    fine = end_date,
    prior = 1L
  )

  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)

  # Assert
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(as.numeric(result$durata), 365) # Duration is difftime object
  expect_equal(result$stato, "occ_ft")
})

test_that("vecshift handles very large prior values", {
  # Arrange - Test with large positive prior values (should all be full-time)
  test_data <- data.table::data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-05-01", "2023-09-01")),
    fine = as.Date(c("2023-04-30", "2023-08-31", "2023-12-31")),
    prior = c(999L, 1000000L, 1L) # Various large positive values
  )

  # Act
  result <- vecshift(test_data)
  result <- classify_employment_status(result)

  # Assert
  expect_s3_class(result, "data.table")

  # Check that all positive prior values are normalized to 1
  employment_periods <- result[arco > 0] # Exclude unemployment periods
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
    inizio = as.Date(c("2023-01-01", "2023-01-02")),
    fine = as.Date(c("2023-01-01", "2023-01-02")), # Consecutive single days
    prior = c(1L, 1L)
  )

  # Act
  result <- vecshift(test_data)

  # Assert
  expect_s3_class(result, "data.table")

  # All segments should have positive duration
  expect_true(all(result$durata > 0))
})

# v1.1.0: Empty-data and single-row guards -----

test_that("vecshift handles empty input data.table", {
  empty_dt <- data.table::data.table(
    id = integer(0),
    cf = character(0),
    inizio = as.Date(character(0)),
    fine = as.Date(character(0)),
    prior = numeric(0)
  )
  result <- vecshift(empty_dt)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
  expect_true(all(
    c("cf", "inizio", "fine", "arco", "prior", "id", "durata", "over_id") %in%
      names(result)
  ))
})

test_that("classify_employment_status handles empty input", {
  empty_segments <- data.table::data.table(
    cf = character(0),
    inizio = as.Date(character(0)),
    fine = as.Date(character(0)),
    arco = integer(0),
    prior = numeric(0),
    id = integer(0),
    durata = numeric(0),
    over_id = integer(0)
  )
  result <- classify_employment_status(empty_segments)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
  expect_true("stato" %in% names(result))
})

test_that("merge_consecutive_employment handles empty input", {
  empty_dt <- data.table::data.table(
    cf = character(0),
    inizio = as.Date(character(0)),
    fine = as.Date(character(0)),
    arco = integer(0),
    prior = numeric(0),
    durata = numeric(0),
    over_id = integer(0)
  )
  result <- merge_consecutive_employment(empty_dt, consolidation_type = "none")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})

test_that("vecshift handles single-row input", {
  single_dt <- data.table::data.table(
    id = 1L,
    cf = "PERSON001",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-12-31"),
    prior = 1L
  )
  result <- vecshift(single_dt)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1L)
  expect_equal(result$cf, "PERSON001")
  expect_equal(result$arco, 1)
  expect_equal(result$over_id, 1L)
})

test_that("classify_employment_status accepts unemployment_duration_threshold override", {
  # Two-job sequence with a 10-day gap (default threshold = 8 → "long")
  test_data <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-01-20", "2023-02-15")),
    prior = c(1L, 1L)
  )
  segments <- vecshift(test_data)
  # With high threshold (15), the 10-day gap should be classified differently than default
  result_high <- classify_employment_status(
    segments,
    unemployment_duration_threshold = 15
  )
  result_default <- classify_employment_status(segments)
  # Both should run successfully and produce a stato column
  expect_true("stato" %in% names(result_high))
  expect_true("stato" %in% names(result_default))
  # Validate parameter validation
  expect_error(
    classify_employment_status(segments, unemployment_duration_threshold = -1),
    "non-negative"
  )
})

# v2.0.0 ------------------------------------------------------------------

test_that("vecshift accepts custom column names via mapping parameters", {
  custom <- data.table::data.table(
    contract_id = 1:2,
    person = c("X1", "X1"),
    start_date = as.Date(c("2023-01-01", "2023-07-01")),
    end_date = as.Date(c("2023-06-30", "2023-12-31")),
    type = c(1, 0)
  )

  result <- vecshift(
    custom,
    person_col = "person",
    start_col = "start_date",
    end_col = "end_date",
    id_col = "contract_id",
    type_col = "type"
  )

  expect_s3_class(result, "vecshift_result")
  expect_true(all(c("cf", "inizio", "fine", "id", "prior") %in% names(result)))
  # Caller's data.table must not be mutated.
  expect_true("person" %in% names(custom))
  expect_false("cf" %in% names(custom))
})

test_that("vecshift returns a vecshift_result and data.table operations work", {
  dt <- data.table::data.table(
    id = 1:2,
    cf = c("A", "B"),
    inizio = as.Date(c("2023-01-01", "2023-03-01")),
    fine = as.Date(c("2023-06-30", "2023-09-30")),
    prior = c(1, 1)
  )

  result <- vecshift(dt)

  expect_s3_class(result, "vecshift_result")
  expect_s3_class(result, "data.table")
  expect_true(is_vecshift_result(result))

  # Subsetting still works.
  sub <- result[cf == "A"]
  expect_true(nrow(sub) >= 1L)

  # Aggregation still works.
  agg <- result[, .N, by = cf]
  expect_true(nrow(agg) == 2L)

  # Update by reference still works.
  result[, new_col := 1L]
  expect_true("new_col" %in% names(result))

  # Metadata attached.
  meta <- attr(result, "vecshift_metadata")
  expect_true(is.list(meta))
  expect_equal(meta$granularity, "day")
})

test_that("print and summary methods for vecshift_result produce output", {
  dt <- data.table::data.table(
    id = 1L,
    cf = "A",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-12-31"),
    prior = 1
  )
  result <- vecshift(dt)

  expect_output(print(result), "vecshift_result")

  s <- summary(result)
  expect_s3_class(s, "summary.vecshift_result")
  expect_true(s$duration_invariant)
  expect_output(print(s), "vecshift_result summary")
})

test_that("validate_vecshift_input detects common issues", {
  # Missing column.
  bad1 <- data.table::data.table(cf = "A", inizio = as.Date("2023-01-01"))
  v1 <- validate_vecshift_input(bad1)
  expect_s3_class(v1, "vecshift_validation")
  expect_true(v1$n_issues > 0L)
  expect_true(!is.null(v1$issues$missing_columns))

  # NA values.
  bad2 <- data.table::data.table(
    id = c(1L, 2L),
    cf = c("A", NA_character_),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-12-31", "2023-11-30")),
    prior = c(1, 1)
  )
  v2 <- validate_vecshift_input(bad2)
  expect_true(!is.null(v2$issues$na_counts))

  # end < start.
  bad3 <- data.table::data.table(
    id = 1L,
    cf = "A",
    inizio = as.Date("2023-12-31"),
    fine = as.Date("2023-01-01"),
    prior = 1
  )
  v3 <- validate_vecshift_input(bad3)
  expect_true(!is.null(v3$issues$end_before_start))

  # Duplicate IDs.
  bad4 <- data.table::data.table(
    id = c(1L, 1L),
    cf = c("A", "B"),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-06-30", "2023-08-31")),
    prior = c(1, 1)
  )
  v4 <- validate_vecshift_input(bad4)
  expect_equal(v4$issues$duplicate_ids, 1L)

  # Clean input -> no issues.
  good <- data.table::data.table(
    id = 1:2,
    cf = c("A", "B"),
    inizio = as.Date(c("2023-01-01", "2023-02-01")),
    fine = as.Date(c("2023-06-30", "2023-08-31")),
    prior = c(1, 1)
  )
  v_ok <- validate_vecshift_input(good)
  expect_equal(v_ok$n_issues, 0L)
  expect_output(print(v_ok), "OK")
})

test_that("vecshift errors on unimplemented granularity values", {
  dt <- data.table::data.table(
    id = 1L,
    cf = "A",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-12-31"),
    prior = 1
  )
  expect_error(vecshift(dt, granularity = "month"), "not yet implemented")
  expect_error(vecshift(dt, granularity = "hour"), "not yet implemented")
  expect_error(vecshift(dt, granularity = "year"), "Invalid 'granularity'")
})
