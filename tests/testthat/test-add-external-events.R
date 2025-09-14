test_that("add_external_events basic functionality works", {
  library(data.table)
  
  # Create sample employment data
  employment_dt <- data.table(
    id = 1:3,
    cf = c("ABC123", "ABC123", "DEF456"),
    inizio = as.Date(c("2023-01-01", "2023-07-01", "2023-03-01")),
    fine = as.Date(c("2023-03-31", "2023-12-31", "2023-06-30")),
    prior = c(1, 1, 0)
  )
  
  # Transform to vecshift format
  vecshift_result <- vecshift(employment_dt)
  
  # Create external events
  training_events <- data.table(
    cf = c("ABC123", "DEF456"),
    event_name = c("training_program", "training_program"),
    event_start = as.Date(c("2023-05-15", "2023-08-01")),
    event_end = as.Date(c("2023-05-25", "2023-08-15"))
  )
  
  # Test basic functionality with overlap matching
  result <- add_external_events(
    vecshift_data = vecshift_result,
    external_events = training_events,
    event_matching_strategy = "overlap"
  )
  
  # Check that new columns were created
  expect_true("training_program_attribute" %in% names(result))
  expect_true("training_program_distance" %in% names(result))
  expect_true("training_program_match_quality" %in% names(result))
  
  # Check that attributes are numeric/character as expected
  expect_type(result$training_program_attribute, "double")
  expect_type(result$training_program_distance, "double")
  expect_type(result$training_program_match_quality, "character")
  
  # Check that only unemployment periods can have attributes = 1
  unemployment_with_attr <- result[training_program_attribute == 1]
  expect_true(all(unemployment_with_attr$arco == 0))
})

test_that("add_external_events handles overlap matching correctly", {
  library(data.table)
  
  # Create data with known unemployment period
  employment_dt <- data.table(
    id = 1:2,
    cf = c("ABC123", "ABC123"),
    inizio = as.Date(c("2023-01-01", "2023-06-01")),
    fine = as.Date(c("2023-03-31", "2023-08-31")),
    prior = c(1, 1)
  )
  
  vecshift_result <- vecshift(employment_dt)
  
  # Create event that overlaps with unemployment period (April-May 2023)
  overlap_event <- data.table(
    cf = "ABC123",
    event_name = "training",
    event_start = as.Date("2023-04-15"),
    event_end = as.Date("2023-04-25")
  )
  
  result <- add_external_events(
    vecshift_data = vecshift_result,
    external_events = overlap_event,
    event_matching_strategy = "overlap"
  )
  
  # Check that overlap was detected
  unemployment_periods <- result[arco == 0]
  overlapping_period <- unemployment_periods[training_attribute == 1]
  
  expect_equal(nrow(overlapping_period), 1)
  expect_equal(overlapping_period$training_distance, 0)
  expect_equal(overlapping_period$training_match_quality, "overlap")
})

test_that("add_external_events handles nearest neighbor matching", {
  library(data.table)
  
  # Create employment data with unemployment periods
  employment_dt <- data.table(
    id = 1:2,
    cf = c("ABC123", "ABC123"),
    inizio = as.Date(c("2023-01-01", "2023-06-01")),
    fine = as.Date(c("2023-03-31", "2023-08-31")),
    prior = c(1, 1)
  )
  
  vecshift_result <- vecshift(employment_dt)
  
  # Create event that doesn't overlap but is near unemployment period
  near_event <- data.table(
    cf = "ABC123",
    event_name = "workshop",
    event_start = as.Date("2023-03-15"),  # During employment, near unemployment
    event_end = as.Date("2023-03-20")
  )
  
  result <- add_external_events(
    vecshift_data = vecshift_result,
    external_events = near_event,
    event_matching_strategy = "nearest"
  )
  
  # Check that nearest matching occurred
  unemployment_periods <- result[arco == 0]
  matched_period <- unemployment_periods[workshop_attribute == 1]
  
  expect_equal(nrow(matched_period), 1)
  expect_gt(matched_period$workshop_distance, 0)  # Should have positive distance
  expect_equal(matched_period$workshop_match_quality, "nearest")
})

test_that("add_external_events creates synthetic unemployment periods", {
  library(data.table)
  
  # Create employment data for one person only
  employment_dt <- data.table(
    id = 1,
    cf = "ABC123",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-03-31"),
    prior = 1
  )
  
  vecshift_result <- vecshift(employment_dt)
  
  # Create event for person not in employment data
  external_event <- data.table(
    cf = "GHI789",  # Different person
    event_name = "training",
    event_start = as.Date("2023-05-01"),
    event_end = as.Date("2023-05-10")
  )
  
  result <- add_external_events(
    vecshift_data = vecshift_result,
    external_events = external_event,
    event_matching_strategy = "overlap",
    create_synthetic_unemployment = TRUE,
    synthetic_unemployment_duration = 365L
  )
  
  # Check that synthetic unemployment was created
  synthetic_periods <- result[cf == "GHI789"]
  expect_gt(nrow(synthetic_periods), 0)
  expect_true(all(synthetic_periods$arco == 0))
  expect_true(all(synthetic_periods$over_id == 0))
  
  # Check that training attribute was applied to synthetic period
  training_periods <- synthetic_periods[training_attribute == 1]
  expect_gt(nrow(training_periods), 0)
})

test_that("add_external_events handles single-day events", {
  library(data.table)
  
  # Create employment data
  employment_dt <- data.table(
    id = 1:2,
    cf = c("ABC123", "ABC123"),
    inizio = as.Date(c("2023-01-01", "2023-06-01")),
    fine = as.Date(c("2023-03-31", "2023-08-31")),
    prior = c(1, 1)
  )
  
  vecshift_result <- vecshift(employment_dt)
  
  # Create single-day event (no end date provided)
  single_day_event <- data.table(
    cf = "ABC123",
    event_name = "interview",
    event_start = as.Date("2023-04-15")
    # No event_end column - should default to same as start
  )
  
  result <- add_external_events(
    vecshift_data = vecshift_result,
    external_events = single_day_event,
    event_matching_strategy = "overlap",
    date_columns = c(start = "event_start")  # Only start date specified
  )
  
  # Should work without errors and create attribute columns
  expect_true("interview_attribute" %in% names(result))
})

test_that("add_external_events handles multiple events per person", {
  library(data.table)
  
  # Create employment data
  employment_dt <- data.table(
    id = 1:2,
    cf = c("ABC123", "ABC123"),
    inizio = as.Date(c("2023-01-01", "2023-08-01")),
    fine = as.Date(c("2023-03-31", "2023-10-31")),
    prior = c(1, 1)
  )
  
  vecshift_result <- vecshift(employment_dt)
  
  # Create multiple events for same person
  multiple_events <- data.table(
    cf = c("ABC123", "ABC123"),
    event_name = c("training_a", "training_b"),
    event_start = as.Date(c("2023-05-01", "2023-06-01")),
    event_end = as.Date(c("2023-05-05", "2023-06-05"))
  )
  
  result <- add_external_events(
    vecshift_data = vecshift_result,
    external_events = multiple_events,
    event_matching_strategy = "overlap"
  )
  
  # Check that both event types created columns
  expect_true("training_a_attribute" %in% names(result))
  expect_true("training_b_attribute" %in% names(result))
  
  # Both events should overlap with unemployment period
  unemployment_periods <- result[arco == 0]
  expect_gt(sum(unemployment_periods$training_a_attribute), 0)
  expect_gt(sum(unemployment_periods$training_b_attribute), 0)
})

test_that("add_external_events validates inputs correctly", {
  library(data.table)
  
  # Create valid employment data
  employment_dt <- data.table(
    id = 1,
    cf = "ABC123",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-03-31"),
    prior = 1
  )
  
  vecshift_result <- vecshift(employment_dt)
  
  # Test invalid vecshift_data input
  expect_error(
    add_external_events(
      vecshift_data = data.frame(a = 1),  # Not a data.table
      external_events = data.table(cf = "ABC", event_name = "test", event_start = Sys.Date())
    ),
    "must be a data.table"
  )
  
  # Test missing columns in external_events
  expect_error(
    add_external_events(
      vecshift_data = vecshift_result,
      external_events = data.table(wrong_col = "ABC")  # Missing required columns
    ),
    "Missing required external_events columns"
  )
  
  # Test invalid event_matching_strategy
  expect_error(
    add_external_events(
      vecshift_data = vecshift_result,
      external_events = data.table(cf = "ABC", event_name = "test", event_start = Sys.Date()),
      event_matching_strategy = "invalid_strategy"
    ),
    "should be one of"
  )
})

test_that("add_external_events handles empty datasets", {
  library(data.table)
  
  # Create employment data
  employment_dt <- data.table(
    id = 1,
    cf = "ABC123",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-03-31"),
    prior = 1
  )
  
  vecshift_result <- vecshift(employment_dt)
  
  # Test with empty external_events
  empty_events <- data.table(
    cf = character(0),
    event_name = character(0),
    event_start = as.Date(character(0))
  )
  
  result <- add_external_events(
    vecshift_data = vecshift_result,
    external_events = empty_events
  )
  
  # Should return original data unchanged
  expect_equal(nrow(result), nrow(vecshift_result))
  expect_equal(names(result), names(vecshift_result))
})

test_that("add_external_events preserves existing data integrity", {
  library(data.table)

  # Create employment data
  employment_dt <- data.table(
    id = 1:3,
    cf = c("ABC123", "ABC123", "DEF456"),
    inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-03-01")),
    fine = as.Date(c("2023-03-31", "2023-08-31", "2023-05-31")),
    prior = c(1, 1, 0)
  )

  original_vecshift <- vecshift(employment_dt)

  # Create external events
  events <- data.table(
    cf = "ABC123",
    event_name = "training",
    event_start = as.Date("2023-05-01"),
    event_end = as.Date("2023-05-10")
  )

  result <- add_external_events(
    vecshift_data = original_vecshift,
    external_events = events,
    event_matching_strategy = "overlap"
  )

  # Check that original data structure is preserved
  original_cols <- names(original_vecshift)
  expect_true(all(original_cols %in% names(result)))

  # Check that original values in core columns are unchanged
  for (col in c("cf", "inizio", "fine", "arco", "prior", "id", "over_id", "durata")) {
    expect_equal(result[[col]], original_vecshift[[col]], info = paste("Column:", col))
  }

  # Check that new attribute columns were added
  expect_true("training_attribute" %in% names(result))
  expect_true("training_distance" %in% names(result))
  expect_true("training_match_quality" %in% names(result))
})

test_that("add_external_events handles multiple events to same unemployment period without duplication", {
  library(data.table)

  # Create employment data with known unemployment periods
  employment_dt <- data.table(
    id = 1:3,
    cf = c("ABC123", "ABC123", "ABC123"),
    inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-09-01")),
    fine = as.Date(c("2023-02-28", "2023-07-31", "2023-10-31")),
    prior = c(1, 1, 1)
  )

  vecshift_result <- vecshift(employment_dt)
  original_row_count <- nrow(vecshift_result)

  # Create multiple external events that overlap with same unemployment period
  multiple_events <- data.table(
    cf = c("ABC123", "ABC123", "ABC123"),
    event_name = c("training_program", "job_interview", "career_counseling"),
    event_start = as.Date(c("2023-03-15", "2023-04-10", "2023-04-20")),
    event_end = as.Date(c("2023-03-20", "2023-04-12", "2023-04-25"))
  )

  result <- add_external_events(
    vecshift_data = vecshift_result,
    external_events = multiple_events,
    event_matching_strategy = "overlap"
  )

  # CRITICAL TEST: No row duplication
  expect_equal(nrow(result), original_row_count)

  # Check that multiple event attributes exist
  expect_true("training_program_attribute" %in% names(result))
  expect_true("job_interview_attribute" %in% names(result))
  expect_true("career_counseling_attribute" %in% names(result))

  # Find the unemployment period that should have multiple events
  unemployment_periods <- result[arco == 0]

  # Check that we can have multiple events on the same unemployment period
  first_unemployment <- unemployment_periods[1]
  event_attrs <- c("training_program_attribute", "job_interview_attribute", "career_counseling_attribute")
  events_on_period <- sum(first_unemployment[, ..event_attrs] == 1, na.rm = TRUE)

  # At least one unemployment period should have multiple events
  expect_gt(events_on_period, 1)

  # Verify only unemployment periods have event attributes = 1
  employment_periods <- result[arco > 0]
  for (attr_col in event_attrs) {
    expect_true(all(employment_periods[[attr_col]] == 0))
  }

  # Verify overlap matching properties
  for (i in seq_along(event_attrs)) {
    attr_col <- event_attrs[i]
    dist_col <- gsub("_attribute$", "_distance", attr_col)
    quality_col <- gsub("_attribute$", "_match_quality", attr_col)

    matched_periods <- result[get(attr_col) == 1]
    if (nrow(matched_periods) > 0) {
      # All overlap matches should have distance = 0
      expect_true(all(matched_periods[[dist_col]] == 0))

      # All should be marked as "overlap" matches
      expect_true(all(matched_periods[[quality_col]] == "overlap"))
    }
  }
})