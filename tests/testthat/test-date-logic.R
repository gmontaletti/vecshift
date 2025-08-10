# Comprehensive tests for the date_logic.R module
# Tests date calculations, FINE+1 logic, event creation, and date validation

library(testthat)
library(data.table)

# Load all package functions for testing
if (!exists("contract_duration")) {
  devtools::load_all()
}

test_that("contract_duration calculates correctly for various durations", {
  # Test basic duration calculations
  test_data <- generate_date_test_data("basic_duration")
  
  # Test individual calculations
  durations <- contract_duration(test_data$inizio, test_data$fine)
  expected <- generate_expected_date_results("basic_duration")
  
  expect_equal(durations, expected$expected_durations)
  expect_equal(sum(durations), expected$expected_total)
  
  # Test edge cases
  same_day <- contract_duration(as.Date("2023-01-01"), as.Date("2023-01-01"))
  expect_equal(same_day, 1)  # Same day should be 1 day (inclusive)
  
  month_long <- contract_duration(as.Date("2023-01-01"), as.Date("2023-01-31"))
  expect_equal(month_long, 31)  # January has 31 days
  
  leap_year <- contract_duration(as.Date("2024-02-01"), as.Date("2024-02-29"))
  expect_equal(leap_year, 29)  # February 2024 has 29 days (leap year)
})

test_that("contract_duration warns on invalid date ranges", {
  # End date before start date should trigger warning
  expect_warning(
    duration <- contract_duration(as.Date("2023-02-01"), as.Date("2023-01-01")),
    "Some end dates are before start dates"
  )
})

test_that("contract_duration handles numeric dates", {
  # Test with numeric dates (days since epoch)
  start_numeric <- as.numeric(as.Date("2023-01-01"))
  end_numeric <- as.numeric(as.Date("2023-01-31"))
  
  duration <- contract_duration(start_numeric, end_numeric)
  expect_equal(duration, 31)
  
  # Compare with Date objects
  start_date <- as.Date("2023-01-01")
  end_date <- as.Date("2023-01-31")
  duration_date <- contract_duration(start_date, end_date)
  
  expect_equal(duration, duration_date)
})

test_that("unemployment_duration calculates gaps correctly", {
  # Test various gap scenarios
  
  # 3-day gap: Jan 31 -> Feb 5 means unemployed Feb 1, 2, 3, 4 (4 days)
  gap1 <- unemployment_duration(as.Date("2023-01-31"), as.Date("2023-02-05"))
  expect_equal(gap1, 4)
  
  # No gap: consecutive contracts
  gap2 <- unemployment_duration(as.Date("2023-01-31"), as.Date("2023-02-01"))
  expect_equal(gap2, 0)
  
  # Overlap: next contract starts before current ends
  gap3 <- unemployment_duration(as.Date("2023-02-15"), as.Date("2023-02-01"))
  expect_equal(gap3, 0)  # Should return 0 for overlaps
  
  # Single day gap
  gap4 <- unemployment_duration(as.Date("2023-01-31"), as.Date("2023-02-02"))
  expect_equal(gap4, 1)  # Unemployed on Feb 1 only
})

test_that("unemployment_duration handles edge cases", {
  # Same date scenario (impossible but should return 0)
  same_date <- unemployment_duration(as.Date("2023-01-01"), as.Date("2023-01-01"))
  expect_equal(same_date, 0)
  
  # Large gap
  large_gap <- unemployment_duration(as.Date("2023-01-31"), as.Date("2023-12-01"))
  expected_days <- as.integer(as.Date("2023-11-30") - as.Date("2023-02-01") + 1)
  expect_equal(large_gap, expected_days)
})

test_that("create_employment_events_with_dates generates correct events", {
  test_data <- generate_date_test_data("event_generation")
  
  events <- create_employment_events_with_dates(test_data)
  expected <- generate_expected_date_results("event_generation")
  
  # Check event count
  expect_equal(nrow(events), expected$expected_event_count)
  
  # Check that events are properly sorted
  expect_true(all(events[, cdata == sort(cdata)]))
  
  # Check start events
  start_events <- events[value == 1]
  expect_equal(nrow(start_events), 3)
  expect_equal(start_events$cdata, expected$expected_start_dates)
  
  # Check end events (FINE + 1 logic)
  end_events <- events[value == -1]
  expect_equal(nrow(end_events), 3) 
  expect_equal(end_events$cdata, expected$expected_end_dates)
  
  # Verify FINE + 1 logic specifically
  original_fine_dates <- test_data$fine
  expected_end_event_dates <- original_fine_dates + 1
  expect_equal(sort(end_events$cdata), sort(expected_end_event_dates))
})

test_that("create_employment_events_with_dates preserves person grouping", {
  # Multi-person test data
  multi_person_data <- data.table(
    id = 1:4,
    cf = c("PERSON001", "PERSON001", "PERSON002", "PERSON002"),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-02-01", "2023-06-01")),
    fine = as.Date(c("2023-02-28", "2023-05-31", "2023-04-30", "2023-08-31")),
    prior = c(1, 0, 1, 0)
  )
  
  events <- create_employment_events_with_dates(multi_person_data)
  
  # Should be sorted by person and date
  expect_true(all(events[, cf == sort(cf)]))
  
  # Check person-specific ordering
  person1_events <- events[cf == "PERSON001"]
  person2_events <- events[cf == "PERSON002"]
  
  expect_true(all(person1_events[, cdata == sort(cdata)]))
  expect_true(all(person2_events[, cdata == sort(cdata)]))
  
  # Each person should have equal number of start and end events
  person1_starts <- sum(person1_events$value == 1)
  person1_ends <- sum(person1_events$value == -1)
  expect_equal(person1_starts, person1_ends)
  
  person2_starts <- sum(person2_events$value == 1)
  person2_ends <- sum(person2_events$value == -1)
  expect_equal(person2_starts, person2_ends)
})

test_that("create_employment_events_with_dates handles custom column names", {
  # Create data with non-standard column names
  custom_data <- data.table(
    contract_id = 1:2,
    person_code = c("A001", "A001"),
    start_dt = as.Date(c("2023-01-01", "2023-03-01")),
    end_dt = as.Date(c("2023-02-28", "2023-05-31")),
    emp_type = c(1, 0)
  )
  
  events <- create_employment_events_with_dates(
    custom_data,
    start_col = "start_dt",
    end_col = "end_dt", 
    id_col = "contract_id",
    person_col = "person_code",
    type_col = "emp_type"
  )
  
  expect_equal(nrow(events), 4)  # 2 start + 2 end events
  expect_true(all(c("id", "cf", "cdata", "value", "prior") %in% names(events)))
  
  # Check that end events use FINE + 1
  end_events <- events[value == -1]
  expected_end_dates <- custom_data$end_dt + 1
  expect_equal(sort(end_events$cdata), sort(expected_end_dates))
})

test_that("validate_date_consistency detects invalid date ranges", {
  invalid_data <- generate_test_data_with_issues("invalid_dates")
  
  validation <- validate_date_consistency(invalid_data)
  
  expect_gt(validation$invalid_ranges, 0)
  expect_true(validation$has_date_issues)
  
  # Should match the number we expect from the test data
  expected_invalid <- sum(invalid_data$fine < invalid_data$inizio, na.rm = TRUE)
  expect_equal(validation$invalid_ranges, expected_invalid)
})

test_that("validate_date_consistency detects zero duration contracts", {
  zero_duration_data <- generate_test_data_with_issues("zero_duration")
  
  validation <- validate_date_consistency(zero_duration_data)
  
  expect_gt(validation$zero_duration_contracts, 0)
  
  # Should match the actual count
  expected_zero <- sum(zero_duration_data$fine == zero_duration_data$inizio, na.rm = TRUE)
  expect_equal(validation$zero_duration_contracts, expected_zero)
})

test_that("validate_date_consistency detects overlapping contracts", {
  overlapping_data <- data.table(
    id = 1:3,
    cf = rep("PERSON001", 3),
    inizio = as.Date(c("2023-01-01", "2023-02-15", "2023-06-01")),
    fine = as.Date(c("2023-08-31", "2023-07-15", "2023-12-31")),  # 1st and 2nd overlap
    prior = c(1, 0, 1)
  )
  
  validation <- validate_date_consistency(overlapping_data, detailed = FALSE)
  
  expect_gt(validation$total_overlapping_pairs, 0)
  expect_true(validation$has_overlaps)
})

test_that("validate_date_consistency detailed mode provides comprehensive analysis", {
  overlapping_data <- data.table(
    id = 1:4,
    cf = rep(c("PERSON001", "PERSON002"), each = 2),
    inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01", "2023-08-01")),
    fine = as.Date(c("2023-08-31", "2023-09-30", "2023-05-31", "2023-11-30")),  # Person1 has overlap
    prior = c(1, 0, 1, 0)
  )
  
  validation <- validate_date_consistency(overlapping_data, detailed = TRUE)
  
  expect_true("person_details" %in% names(validation))
  expect_true("total_persons_with_overlaps" %in% names(validation))
  
  # Check person-level details structure
  person_details <- validation$person_details
  expect_true(all(c("n_contracts", "n_overlaps", "total_employment_days") %in% names(person_details)))
  
  # Should detect overlap for PERSON001 only
  expect_equal(validation$total_persons_with_overlaps, 1)
})

test_that("validate_date_consistency works with clean data", {
  clean_data <- generate_test_data_with_issues("clean_data", n_records = 10)
  
  validation <- validate_date_consistency(clean_data)
  
  expect_equal(validation$invalid_ranges, 0)
  expect_false(validation$has_date_issues)
  expect_equal(validation$zero_duration_contracts, 0)
})

test_that("standardize_dates handles different input formats", {
  # Test Date objects (should return unchanged)
  date_input <- as.Date(c("2023-01-01", "2023-02-01", "2023-03-01"))
  result_date <- standardize_dates(date_input)
  expect_identical(result_date, date_input)
  expect_true(inherits(result_date, "Date"))
  
  # Test numeric dates
  numeric_input <- as.numeric(date_input)
  result_numeric <- standardize_dates(numeric_input)
  expect_equal(result_numeric, date_input)
  expect_true(inherits(result_numeric, "Date"))
  
  # Test character dates  
  char_input <- c("2023-01-01", "2023-02-01", "2023-03-01")
  result_char <- standardize_dates(char_input)
  expect_equal(result_char, date_input)
  expect_true(inherits(result_char, "Date"))
  
  # Test custom origin for numeric dates
  custom_origin <- "2000-01-01"
  # Days since custom origin: 1 day and 9 days
  numeric_from_custom <- c(1, 9)  # 1 day = 2000-01-02, 9 days = 2000-01-10
  result_custom <- standardize_dates(numeric_from_custom, origin = custom_origin)
  expected_custom <- as.Date(c("2000-01-02", "2000-01-10"))
  expect_equal(result_custom, expected_custom)
})

test_that("standardize_dates fails with unsupported formats", {
  # Test unsupported input type
  unsupported_input <- factor(c("2023-01-01", "2023-02-01"))
  
  expect_error(
    standardize_dates(unsupported_input),
    "Unsupported date format"
  )
})

test_that("analyze_temporal_coverage provides comprehensive coverage statistics", {
  test_data <- data.table(
    id = 1:6,
    cf = rep(c("PERSON001", "PERSON002"), each = 3),
    inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01", 
                      "2023-02-01", "2023-05-01", "2023-08-01")),
    fine = as.Date(c("2023-02-28", "2023-05-31", "2023-09-30",
                    "2023-04-30", "2023-07-31", "2023-11-30")),
    prior = c(1, 0, 1, 0, 1, 0)
  )
  
  coverage <- analyze_temporal_coverage(test_data)
  
  # Check structure
  expect_true("person_coverage" %in% names(coverage))
  expect_true("summary" %in% names(coverage))
  
  # Check person-level coverage
  person_cov <- coverage$person_coverage
  expect_equal(nrow(person_cov), 2)  # Two persons
  expect_true(all(c("observation_start", "observation_end", "observation_days", 
                   "employment_days", "employment_rate", "n_contracts") %in% names(person_cov)))
  
  # Check summary statistics
  summary_stats <- coverage$summary
  expect_true(all(c("mean_employment_rate", "median_employment_rate", 
                   "total_person_years") %in% names(summary_stats)))
  
  # Employment rates should be between 0 and 1
  expect_true(all(person_cov$employment_rate >= 0))
  expect_true(all(person_cov$employment_rate <= 1))
})

test_that("analyze_temporal_coverage calculates employment rates correctly", {
  # Create data with known employment pattern
  test_data <- data.table(
    id = 1:2,
    cf = rep("PERSON001", 2),
    inizio = as.Date(c("2023-01-01", "2023-07-01")),
    fine = as.Date(c("2023-06-30", "2023-12-31")),  # 6 months each = full year employment
    prior = c(1, 0)
  )
  
  coverage <- analyze_temporal_coverage(test_data)
  person_cov <- coverage$person_coverage
  
  # Should have 100% employment rate (full year covered by contracts)
  expect_equal(person_cov$employment_rate, 1.0)
  expect_equal(person_cov$employment_days, 365)  # 181 + 184 days
  expect_equal(person_cov$observation_days, 365)  # Full year
})

test_that("FINE+1 logic is correctly implemented in event creation", {
  # Test the critical FINE+1 logic specifically
  test_contract <- data.table(
    id = 1,
    cf = "TEST_PERSON",
    inizio = as.Date("2023-01-01"),
    fine = as.Date("2023-01-31"),  # Ends on Jan 31
    prior = 1
  )
  
  events <- create_employment_events_with_dates(test_contract)
  
  # Should have exactly 2 events
  expect_equal(nrow(events), 2)
  
  # Start event should be on contract start date
  start_event <- events[value == 1]
  expect_equal(start_event$cdata, as.Date("2023-01-01"))
  
  # End event should be on contract end date + 1 (Feb 1)
  end_event <- events[value == -1]
  expect_equal(end_event$cdata, as.Date("2023-02-01"))  # Jan 31 + 1 = Feb 1
  
  # This means unemployment would start on Feb 1, not Jan 32 (which doesn't exist)
  # or Feb 2 (which would skip a day)
})

test_that("date validation works with custom column names", {
  # Test with non-standard column names
  custom_data <- data.table(
    contract_id = 1:3,
    person_id = rep("PERSON001", 3),
    start_date = as.Date(c("2023-01-01", "2023-04-01", "2023-12-31")),
    end_date = as.Date(c("2023-03-31", "2023-01-01", "2023-01-01")),  # 2nd and 3rd invalid
    employment_type = c(1, 0, 1)
  )
  
  validation <- validate_date_consistency(
    custom_data,
    start_col = "start_date",
    end_col = "end_date", 
    person_col = "person_id"
  )
  
  expect_equal(validation$invalid_ranges, 2)  # 2nd and 3rd records
  expect_true(validation$has_date_issues)
})

test_that("contract_duration integrates correctly with quality assessment", {
  # Test that contract_duration is used correctly in data quality assessment
  test_data <- data.table(
    id = 1:3,
    cf = rep("PERSON001", 3),
    inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),  # 31, 28, 31 days
    prior = c(1, 0, 1)
  )
  
  # Manually calculate expected total
  expected_total <- sum(contract_duration(test_data$inizio, test_data$fine))
  expect_equal(expected_total, 90)  # 31 + 28 + 31 = 90
  
  # This should integrate correctly with the quality assessment
  quality_report <- assess_data_quality(test_data)
  
  # The person-level analysis should use contract_duration correctly
  person_stats <- quality_report$person_level$employment_days_distribution
  expect_true(!is.null(person_stats))
  expect_true(is.numeric(person_stats))
  
  # The person_stats is a summary object of employment days
  # For a single person with 3 contracts totaling 90 days:
  # All summary stats should be 90 (Min, Mean, Max all equal)
  expect_equal(as.numeric(person_stats["Mean"]), expected_total)
  expect_equal(as.numeric(person_stats["Max."]), expected_total)
  expect_equal(as.numeric(person_stats["Min."]), expected_total)
})

test_that("date logic handles leap years correctly", {
  # Test February in leap year vs non-leap year
  leap_year_start <- as.Date("2024-02-01")  # 2024 is leap year
  leap_year_end <- as.Date("2024-02-29")
  leap_duration <- contract_duration(leap_year_start, leap_year_end)
  expect_equal(leap_duration, 29)
  
  non_leap_start <- as.Date("2023-02-01")  # 2023 is not leap year
  non_leap_end <- as.Date("2023-02-28")
  non_leap_duration <- contract_duration(non_leap_start, non_leap_end)
  expect_equal(non_leap_duration, 28)
  
  # Test unemployment duration across leap year boundary
  unemployment_gap <- unemployment_duration(as.Date("2024-02-28"), as.Date("2024-03-01"))
  expect_equal(unemployment_gap, 1)  # Only Feb 29 (leap day)
})

test_that("event creation handles single-day contracts correctly", {
  single_day_data <- data.table(
    id = 1,
    cf = "PERSON001",
    inizio = as.Date("2023-06-15"),
    fine = as.Date("2023-06-15"),  # Same day
    prior = 1
  )
  
  events <- create_employment_events_with_dates(single_day_data)
  
  expect_equal(nrow(events), 2)
  
  start_event <- events[value == 1]
  end_event <- events[value == -1]
  
  expect_equal(start_event$cdata, as.Date("2023-06-15"))
  expect_equal(end_event$cdata, as.Date("2023-06-16"))  # Next day (FINE + 1)
  
  # Verify duration would be calculated correctly
  duration <- contract_duration(single_day_data$inizio, single_day_data$fine)
  expect_equal(duration, 1)
})

test_that("unemployment_duration handles year boundaries correctly", {
  # Test gap that crosses year boundary
  year_end <- as.Date("2023-12-31")
  year_start <- as.Date("2024-01-15")
  
  gap_days <- unemployment_duration(year_end, year_start)
  
  # Unemployed from Jan 1, 2024 to Jan 14, 2024 = 14 days
  expect_equal(gap_days, 14)
  
  # Test consecutive across year boundary
  consecutive_gap <- unemployment_duration(as.Date("2023-12-31"), as.Date("2024-01-01"))
  expect_equal(consecutive_gap, 0)
})