# Helper functions for generating test datasets with data quality issues
# Used across all test files for consistent test data generation

#' Generate Test Data with Various Data Quality Issues
#'
#' Creates employment datasets with specific data quality problems
#' for testing validation and cleaning functions.
#'
#' @param issue_type Type of data quality issue to simulate
#' @param n_records Number of records to generate
#' @param n_persons Number of different persons
#' @return data.table with the specified data quality issue
generate_test_data_with_issues <- function(issue_type, n_records = 10, n_persons = 3) {
  library(data.table)
  
  switch(issue_type,
    
    # Clean data for positive controls
    "clean_data" = {
      data.table(
        id = 1:n_records,
        cf = rep(paste0("PERSON", sprintf("%03d", 1:n_persons)), length.out = n_records),
        inizio = as.Date("2023-01-01") + sample(0:365, n_records),
        fine = as.Date("2023-01-01") + sample(30:395, n_records),
        prior = sample(c(0, 1), n_records, replace = TRUE)
      )[fine >= inizio]  # Ensure valid dates
    },
    
    # Missing values in various columns
    "missing_values" = {
      dt <- data.table(
        id = c(1:8, NA, NA),
        cf = c(rep("PERSON001", 3), rep("PERSON002", 3), rep("PERSON003", 2), NA, ""),
        inizio = c(as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")), 
                  as.Date(c("2023-02-01", "2023-05-01", "2023-08-01")),
                  as.Date(c("2023-01-15", "2023-07-01")), NA, as.Date("2023-12-01")),
        fine = c(as.Date(c("2023-02-28", "2023-05-31", "2023-09-30")),
                as.Date(c("2023-04-30", "2023-07-31", "2023-11-30")), 
                as.Date(c("2023-03-15", "2023-10-31")), as.Date("2023-06-15"), NA),
        prior = c(1, 0, 1, 0, 1, 0, 1, 0, NA, 1)
      )
      dt
    },
    
    # Duplicate records and IDs
    "duplicate_data" = {
      base_dt <- data.table(
        id = c(1, 1, 2, 3, 3, 4, 5, 5, 5),  # Duplicate IDs
        cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", 
               "PERSON002", "PERSON003", "PERSON003", "PERSON003"),
        inizio = as.Date(c("2023-01-01", "2023-01-01", "2023-03-01", "2023-02-01",
                          "2023-02-01", "2023-05-01", "2023-01-15", "2023-01-15", "2023-01-15")),
        fine = as.Date(c("2023-02-28", "2023-02-28", "2023-05-31", "2023-04-30",
                        "2023-04-30", "2023-07-31", "2023-03-15", "2023-03-15", "2023-03-15")),
        prior = c(1, 1, 0, 0, 0, 1, 1, 1, 1)
      )
      # Add some completely duplicate rows
      rbind(base_dt, base_dt[1:2])
    },
    
    # Invalid date ranges (end before start)
    "invalid_dates" = {
      data.table(
        id = 1:8,
        cf = rep(c("PERSON001", "PERSON002"), each = 4),
        inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-12-31", "2023-08-15",
                          "2023-02-01", "2023-07-01", "2023-11-30", "2023-09-15")),
        fine = as.Date(c("2023-02-28", "2023-01-01", "2023-01-01", "2023-08-01",  # Last 3 are invalid
                        "2023-04-30", "2023-05-01", "2023-01-01", "2023-09-01")), # Last 2 are invalid
        prior = c(1, 0, 1, 0, 0, 1, 0, 1)
      )
    },
    
    # Zero duration contracts
    "zero_duration" = {
      data.table(
        id = 1:6,
        cf = rep(c("PERSON001", "PERSON002"), each = 3),
        inizio = as.Date(c("2023-01-01", "2023-01-15", "2023-02-01",
                          "2023-03-01", "2023-03-15", "2023-04-01")),
        fine = as.Date(c("2023-01-31", "2023-01-15", "2023-02-01",  # 2nd and 3rd are zero duration
                        "2023-03-31", "2023-03-15", "2023-04-01")), # 2nd and 3rd are zero duration  
        prior = c(1, 0, 1, 0, 1, 0)
      )
    },
    
    # Wrong data types
    "wrong_types" = {
      data.table(
        id = c(1, 2, 3, "4", "5"),  # Mixed numeric and character
        cf = c("PERSON001", "PERSON001", "PERSON002", 123, 456),  # Mixed types
        inizio = c("2023-01-01", "2023-03-01", "2023-06-01", "not-a-date", "2023-12-01"),
        fine = c("2023-02-28", "2023-05-31", "2023-09-30", "2023-04-01", "also-not-a-date"),
        prior = c("1", "0", "1", "not-numeric", "0")  # Character instead of numeric
      )
    },
    
    # Extreme date values and negative priors
    "extreme_values" = {
      data.table(
        id = 1:8,
        cf = rep(c("PERSON001", "PERSON002"), each = 4),
        inizio = as.Date(c("1900-01-01", "2023-01-01", "2050-12-31", "2023-06-01",
                          "1800-01-01", "2023-02-01", "2100-01-01", "2023-07-01")),
        fine = as.Date(c("1900-01-02", "2023-02-28", "2051-01-01", "2023-08-31",
                        "1800-01-02", "2023-04-30", "2100-01-02", "2023-09-30")),
        prior = c(-5, 100, -1, 0, -10, 1, 999, 0)  # Extreme prior values
      )
    },
    
    # Complex overlapping scenarios with quality issues
    "complex_overlaps_with_issues" = {
      data.table(
        id = c(1, 2, NA, 4, 4, 6),  # Duplicate and missing IDs
        cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "", "PERSON002"),
        inizio = as.Date(c("2023-01-01", "2023-02-15", "2023-06-01", "2023-01-15", "2023-03-01", "2023-05-01")),
        fine = as.Date(c("2023-08-31", "2023-07-15", "2023-12-31", "2023-02-01", "2023-02-15", "2023-08-31")),  # 5th has end before start
        prior = c(1, 0, NA, 1, 0, 1)
      )
    },
    
    # High proportion of unusual patterns for clustering tests
    "batch_processing_pattern" = {
      # Most contracts start on the 1st of the month (batch processing indicator)
      start_dates <- c(
        rep(as.Date("2023-01-01"), 15),  # Many start on 1st
        rep(as.Date("2023-02-01"), 12),
        rep(as.Date("2023-03-01"), 10),
        as.Date(c("2023-01-15", "2023-02-20", "2023-03-25"))  # Few scattered dates
      )
      data.table(
        id = 1:length(start_dates),
        cf = rep(paste0("PERSON", sprintf("%03d", 1:10)), length.out = length(start_dates)),
        inizio = start_dates,
        fine = start_dates + sample(30:90, length(start_dates), replace = TRUE),
        prior = sample(c(0, 1), length(start_dates), replace = TRUE)
      )
    },
    
    # Weekend start dates (data quality issue indicator)
    "weekend_starts" = {
      # Generate dates that fall on weekends (unusual for real employment data)
      # Start with a known Saturday (2023-01-07)
      saturday_start <- as.Date("2023-01-07")  # This is a Saturday
      weekend_dates <- saturday_start + c(0, 1, 7, 8, 14, 15, 21, 22, 28, 29, 35, 36)  # Mix of Saturdays and Sundays
      regular_dates <- as.Date(c("2023-01-02", "2023-01-03"))  # Weekdays (fewer than weekend dates)
      
      data.table(
        id = 1:(length(weekend_dates) + length(regular_dates)),
        cf = rep("PERSON001", length(weekend_dates) + length(regular_dates)),
        inizio = c(weekend_dates, regular_dates),
        fine = c(weekend_dates + 30, regular_dates + 30),
        prior = sample(c(0, 1), length(weekend_dates) + length(regular_dates), replace = TRUE)
      )
    },
    
    # Very short contracts (potential data quality issue)
    "very_short_contracts" = {
      data.table(
        id = 1:10,
        cf = rep(c("PERSON001", "PERSON002"), each = 5),
        inizio = as.Date("2023-01-01") + 0:9 * 10,
        fine = as.Date("2023-01-01") + 0:9 * 10 + c(0, 0, 1, 1, 2, 0, 1, 0, 2, 1),  # Many 1-day contracts
        prior = sample(c(0, 1), 10, replace = TRUE)
      )
    },
    
    # Mixed date formats (character dates)
    "mixed_date_formats" = {
      data.table(
        id = 1:6,
        cf = rep(c("PERSON001", "PERSON002"), each = 3),
        inizio = c(as.Date("2023-01-01"), "2023-02-01", as.Date("2023-03-01"), 
                  "2023-04-01", as.Date("2023-05-01"), "2023-06-01"),
        fine = c("2023-01-31", as.Date("2023-02-28"), "2023-03-31",
                as.Date("2023-04-30"), "2023-05-31", as.Date("2023-06-30")),
        prior = c(1, 0, 1, 0, 1, 0)
      )
    },
    
    stop("Unknown issue type: ", issue_type)
  )
}

#' Generate Test Data with Custom Column Names
#'
#' Creates test data with non-standard column names for testing
#' column mapping functionality.
#'
#' @param mapping_scenario Type of mapping scenario
#' @return List with data.table and corresponding column_map
generate_custom_column_data <- function(mapping_scenario) {
  
  switch(mapping_scenario,
    
    # Simple renaming
    "simple_rename" = {
      dt <- data.table(
        contract_id = 1:5,
        person_code = rep(c("A001", "B002"), c(3, 2)),
        start_date = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01", "2023-02-01", "2023-05-01")),
        end_date = as.Date(c("2023-02-28", "2023-05-31", "2023-09-30", "2023-04-30", "2023-07-31")),
        employment_type = c(1, 0, 1, 0, 1)
      )
      
      column_map <- list(
        id = "contract_id",
        cf = "person_code",
        inizio = "start_date",
        fine = "end_date",
        prior = "employment_type"
      )
      
      list(data = dt, column_map = column_map)
    },
    
    # Database-style column names
    "database_style" = {
      dt <- data.table(
        CONTRACT_ID = 1:4,
        PERSON_FISCAL_CODE = c("CF001", "CF001", "CF002", "CF002"),
        CONTRACT_START_DATE = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01", "2023-07-01")),
        CONTRACT_END_DATE = as.Date(c("2023-05-31", "2023-12-31", "2023-06-30", "2023-11-30")),
        EMPLOYMENT_STATUS_PRIOR = c(1, 0, 0, 1)
      )
      
      column_map <- list(
        id = "CONTRACT_ID",
        cf = "PERSON_FISCAL_CODE", 
        inizio = "CONTRACT_START_DATE",
        fine = "CONTRACT_END_DATE",
        prior = "EMPLOYMENT_STATUS_PRIOR"
      )
      
      list(data = dt, column_map = column_map)
    },
    
    # Abbreviated column names
    "abbreviated" = {
      dt <- data.table(
        cid = 1:6,
        pid = rep(paste0("P", 1:2), each = 3),
        sd = as.Date("2023-01-01") + c(0, 90, 180, 30, 120, 210),
        ed = as.Date("2023-01-01") + c(89, 179, 269, 119, 209, 299),
        typ = c(1, 0, 1, 0, 1, 0)
      )
      
      column_map <- list(
        id = "cid",
        cf = "pid",
        inizio = "sd", 
        fine = "ed",
        prior = "typ"
      )
      
      list(data = dt, column_map = column_map)
    },
    
    # Extra columns that should be removed
    "extra_columns" = {
      dt <- data.table(
        record_id = 1:4,
        person_identifier = c("PERS001", "PERS001", "PERS002", "PERS002"),
        contract_start = as.Date(c("2023-01-01", "2023-04-01", "2023-02-01", "2023-06-01")),
        contract_end = as.Date(c("2023-03-31", "2023-08-31", "2023-05-31", "2023-10-31")),
        work_type = c(1, 0, 0, 1),
        # Extra columns that should be removed
        department = c("HR", "IT", "Finance", "IT"),
        salary = c(50000, 35000, 45000, 60000),
        manager = c("Smith", "Jones", "Brown", "Davis"),
        created_date = Sys.Date(),
        notes = c("Full time", "Part time", "Temporary", "Permanent")
      )
      
      column_map <- list(
        id = "record_id",
        cf = "person_identifier",
        inizio = "contract_start",
        fine = "contract_end", 
        prior = "work_type"
      )
      
      list(data = dt, column_map = column_map)
    },
    
    stop("Unknown mapping scenario: ", mapping_scenario)
  )
}

#' Generate Expected Results for Quality Assessment Tests
#'
#' Creates the expected output for data quality assessments
#' to validate test results.
#'
#' @param test_scenario The test scenario name
#' @return Expected assessment results structure
generate_expected_quality_results <- function(test_scenario) {
  
  switch(test_scenario,
    
    "clean_data" = {
      list(
        should_pass_validation = TRUE,
        expected_missing_values = c(id = 0, cf = 0, inizio = 0, fine = 0, prior = 0),
        expected_duplicates = list(duplicate_ids = 0, duplicate_records = 0),
        expected_quality_score_range = c(0.95, 1.0),  # Should be very high
        expected_issues = 0
      )
    },
    
    "missing_values" = {
      list(
        should_pass_validation = FALSE,
        expected_missing_values = c(id = 2, cf = 2, inizio = 1, fine = 1, prior = 1),
        expected_issues = 7,
        should_trigger_warnings = TRUE
      )
    },
    
    "duplicate_data" = {
      list(
        should_pass_validation = FALSE,
        expected_duplicate_ids = 4,  # IDs 1, 3, 5 appear multiple times
        expected_duplicate_records = 2,  # Complete duplicates
        expected_issues_min = 4
      )
    },
    
    "invalid_dates" = {
      list(
        should_pass_validation = FALSE,
        expected_invalid_dates = 4,  # 4 records with end < start
        expected_issues_min = 4,
        should_trigger_warnings = TRUE
      )
    },
    
    stop("Unknown test scenario: ", test_scenario)
  )
}

#' Create Test Data for Date Logic Functions
#'
#' Generates specific datasets for testing date calculation functions
#' 
#' @param date_scenario Type of date scenario to test
#' @return data.table with employment records
generate_date_test_data <- function(date_scenario) {
  
  switch(date_scenario,
    
    # Basic contract duration calculation
    "basic_duration" = {
      data.table(
        id = 1:4,
        cf = "PERSON001",
        inizio = as.Date(c("2023-01-01", "2023-01-01", "2023-02-01", "2023-01-15")),
        fine = as.Date(c("2023-01-01", "2023-01-31", "2023-02-28", "2023-01-15")),  # 1, 31, 28, 1 days
        prior = c(1, 1, 0, 0)
      )
    },
    
    # Unemployment gaps between contracts
    "unemployment_gaps" = {
      data.table(
        id = 1:4,
        cf = rep("PERSON001", 4),
        inizio = as.Date(c("2023-01-01", "2023-02-05", "2023-04-01", "2023-05-15")),
        fine = as.Date(c("2023-01-31", "2023-03-31", "2023-04-30", "2023-06-30")),
        prior = c(1, 0, 1, 0)
      )
    },
    
    # Consecutive contracts (no gap)
    "consecutive_contracts" = {
      data.table(
        id = 1:3,
        cf = rep("PERSON001", 3),
        inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01")),
        fine = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30")),  # End+1 = next start
        prior = c(1, 0, 1)
      )
    },
    
    # Date format standardization
    "mixed_date_formats" = {
      data.table(
        id = 1:4,
        cf = rep("PERSON001", 4),
        inizio = c(as.Date("2023-01-01"), 19358, "2023-03-01", as.Date("2023-04-01")),  # Mixed Date, numeric, character
        fine = c("2023-01-31", as.Date("2023-02-28"), 19417, as.Date("2023-04-30")),    # Mixed formats
        prior = c(1, 0, 1, 0)
      )
    },
    
    # Event generation testing
    "event_generation" = {
      data.table(
        id = 1:3,
        cf = rep("PERSON001", 3),
        inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")),
        fine = as.Date(c("2023-02-28", "2023-05-31", "2023-08-31")),
        prior = c(1, 0, 1)
      )
    },
    
    stop("Unknown date scenario: ", date_scenario)
  )
}

#' Expected Results for Date Logic Tests
#'
#' @param date_scenario Test scenario name
#' @return Expected calculation results
generate_expected_date_results <- function(date_scenario) {
  
  switch(date_scenario,
    
    "basic_duration" = {
      list(
        expected_durations = c(1, 31, 28, 1),  # Days inclusive of start and end
        expected_total = 61
      )
    },
    
    "unemployment_gaps" = {
      list(
        expected_gaps = c(4, 0, 14),  # Days between contracts (exclusive)
        # Gap 1: Jan 31 -> Feb 5 = Feb 1,2,3,4 = 4 days
        # Gap 2: Mar 31 -> Apr 1 = 0 days (consecutive)
        # Gap 3: Apr 30 -> May 15 = May 1-14 = 14 days
        expected_unemployment_durations = c(4, 0, 14)
      )
    },
    
    "event_generation" = {
      list(
        expected_event_count = 6,  # 3 start + 3 end events
        expected_start_dates = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01")),
        expected_end_dates = as.Date(c("2023-03-01", "2023-06-01", "2023-09-01")),  # FINE + 1
        expected_values = c(1, 1, 1, -1, -1, -1)
      )
    },
    
    stop("Unknown date scenario: ", date_scenario)
  )
}