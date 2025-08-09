# Helper functions for generating synthetic datasets for vecshift testing

#' Generate Synthetic Employment Data for Testing
#' 
#' Creates various synthetic employment datasets to test different scenarios
#' that the vecshift function should handle correctly.
#' 
#' @param scenario Character string specifying the type of test data to generate
#' @return A data.table with employment records
#' 
generate_test_data <- function(scenario) {
  require(data.table)
  
  switch(scenario,
    
    # Basic single employment record
    "single_employment" = {
      data.table(
        id = 1L,
        cf = "PERSON001",
        INIZIO = as.Date("2023-01-01"),
        FINE = as.Date("2023-12-31"),
        prior = 1L  # full-time
      )
    },
    
    # Single part-time employment
    "single_parttime" = {
      data.table(
        id = 1L,
        cf = "PERSON001", 
        INIZIO = as.Date("2023-01-01"),
        FINE = as.Date("2023-12-31"),
        prior = 0L  # part-time
      )
    },
    
    # Employment with gap (unemployment period)
    "employment_with_gap" = {
      data.table(
        id = c(1L, 2L),
        cf = c("PERSON001", "PERSON001"),
        INIZIO = as.Date(c("2023-01-01", "2023-07-01")),
        FINE = as.Date(c("2023-03-31", "2023-12-31")),
        prior = c(1L, 1L)  # both full-time
      )
    },
    
    # Consecutive employment (no gap)
    "consecutive_employment" = {
      data.table(
        id = c(1L, 2L),
        cf = c("PERSON001", "PERSON001"),
        INIZIO = as.Date(c("2023-01-01", "2023-04-01")),
        FINE = as.Date(c("2023-03-31", "2023-12-31")),
        prior = c(1L, 0L)  # full-time then part-time
      )
    },
    
    # Overlapping employment (multiple concurrent jobs)
    "overlapping_employment" = {
      data.table(
        id = c(1L, 2L),
        cf = c("PERSON001", "PERSON001"),
        INIZIO = as.Date(c("2023-01-01", "2023-06-01")),
        FINE = as.Date(c("2023-12-31", "2023-09-30")),
        prior = c(1L, 0L)  # full-time overlapping with part-time
      )
    },
    
    # Complex overlapping scenario (3 jobs with various overlaps)
    "complex_overlapping" = {
      data.table(
        id = c(1L, 2L, 3L),
        cf = c("PERSON001", "PERSON001", "PERSON001"),
        INIZIO = as.Date(c("2023-01-01", "2023-03-15", "2023-06-01")),
        FINE = as.Date(c("2023-08-31", "2023-07-15", "2023-12-31")),
        prior = c(1L, 0L, 1L)  # ft, pt, ft with various overlaps
      )
    },
    
    # Single day employment
    "single_day" = {
      data.table(
        id = 1L,
        cf = "PERSON001",
        INIZIO = as.Date("2023-06-15"),
        FINE = as.Date("2023-06-15"),
        prior = 1L
      )
    },
    
    # Multiple people
    "multiple_people" = {
      data.table(
        id = c(1L, 2L, 3L, 4L),
        cf = c("PERSON001", "PERSON001", "PERSON002", "PERSON002"),
        INIZIO = as.Date(c("2023-01-01", "2023-07-01", "2023-02-01", "2023-08-01")),
        FINE = as.Date(c("2023-06-30", "2023-12-31", "2023-05-31", "2023-11-30")),
        prior = c(1L, 0L, 0L, 1L)
      )
    },
    
    # Edge case: same start and end after adding 1 day
    "edge_dates" = {
      data.table(
        id = c(1L, 2L),
        cf = c("PERSON001", "PERSON001"),
        INIZIO = as.Date(c("2023-01-01", "2023-01-02")),
        FINE = as.Date(c("2023-01-01", "2023-01-03")),
        prior = c(1L, 0L)
      )
    },
    
    # All part-time overlapping
    "all_parttime_overlap" = {
      data.table(
        id = c(1L, 2L, 3L),
        cf = c("PERSON001", "PERSON001", "PERSON001"),
        INIZIO = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
        FINE = as.Date(c("2023-06-30", "2023-05-31", "2023-08-31")),
        prior = c(0L, 0L, 0L)  # all part-time
      )
    },
    
    # All full-time overlapping
    "all_fulltime_overlap" = {
      data.table(
        id = c(1L, 2L),
        cf = c("PERSON001", "PERSON001"),
        INIZIO = as.Date(c("2023-01-01", "2023-06-01")),
        FINE = as.Date(c("2023-12-31", "2023-09-30")),
        prior = c(1L, 2L)  # both full-time (prior > 0)
      )
    },
    
    # Negative prior values (should be treated as part-time)
    "negative_prior" = {
      data.table(
        id = c(1L, 2L),
        cf = c("PERSON001", "PERSON001"),
        INIZIO = as.Date(c("2023-01-01", "2023-07-01")),
        FINE = as.Date(c("2023-06-30", "2023-12-31")),
        prior = c(-1L, 1L)  # negative should be part-time
      )
    },
    
    # Empty dataset
    "empty" = {
      data.table(
        id = integer(0),
        cf = character(0),
        INIZIO = as.Date(character(0)),
        FINE = as.Date(character(0)),
        prior = integer(0)
      )
    },
    
    stop("Unknown scenario: ", scenario)
  )
}

#' Generate Invalid Test Data for Error Testing
#' 
#' Creates invalid datasets to test error handling
#' 
#' @param error_type Type of error to simulate
#' @return Invalid data structure for testing
#' 
generate_invalid_data <- function(error_type) {
  require(data.table)
  
  switch(error_type,
    
    # Missing required columns
    "missing_columns" = {
      data.table(
        id = 1L,
        cf = "PERSON001",
        INIZIO = as.Date("2023-01-01")
        # Missing FINE and prior
      )
    },
    
    # Wrong column types
    "wrong_types" = {
      data.table(
        id = 1L,
        cf = "PERSON001",
        INIZIO = "not-a-date",  # Should be Date or numeric
        FINE = as.Date("2023-12-31"),
        prior = "not-numeric"   # Should be numeric
      )
    },
    
    # End before start
    "invalid_dates" = {
      data.table(
        id = 1L,
        cf = "PERSON001",
        INIZIO = as.Date("2023-12-31"),
        FINE = as.Date("2023-01-01"),  # End before start
        prior = 1L
      )
    },
    
    # Not a data.table
    "not_datatable" = {
      data.frame(  # Regular data.frame, not data.table
        id = 1L,
        cf = "PERSON001",
        INIZIO = as.Date("2023-01-01"),
        FINE = as.Date("2023-12-31"),
        prior = 1L
      )
    },
    
    # NA values in required fields
    "na_values" = {
      data.table(
        id = c(1L, 2L),
        cf = c("PERSON001", NA_character_),
        INIZIO = as.Date(c("2023-01-01", "2023-06-01")),
        FINE = as.Date(c("2023-12-31", NA)),
        prior = c(1L, NA_integer_)
      )
    },
    
    stop("Unknown error type: ", error_type)
  )
}

#' Create Expected Results for Test Data
#' 
#' Manually creates the expected output for specific test scenarios
#' to validate the vecshift function results
#' 
#' @param scenario Test scenario name
#' @return Expected data.table result
#' 
create_expected_result <- function(scenario) {
  require(data.table)
  
  switch(scenario,
    
    "single_employment" = {
      data.table(
        cf = "PERSON001",
        inizio = as.Date("2023-01-01"),
        fine = as.Date("2024-01-01"),  # FINE + 1
        arco = 1L,
        prior = 1L,
        id = 1L,
        durata = 365L,
        stato = "occ_ft"
      )
    },
    
    "single_parttime" = {
      data.table(
        cf = "PERSON001", 
        inizio = as.Date("2023-01-01"),
        fine = as.Date("2024-01-01"),
        arco = 1L,
        prior = 0L,
        id = 1L,
        durata = 365L,
        stato = "occ_pt"
      )
    },
    
    "employment_with_gap" = {
      data.table(
        cf = rep("PERSON001", 3),
        inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01")),
        fine = as.Date(c("2023-04-01", "2023-07-01", "2024-01-01")),
        arco = c(1L, 0L, 1L),
        prior = c(1L, 0L, 1L),
        id = c(1L, 0L, 2L),
        durata = c(90L, 91L, 184L),  # Note: unemployment durata = fine-inizio-1
        stato = c("occ_ft", "disoccupato", "occ_ft")
      )
    },
    
    stop("Expected result not defined for scenario: ", scenario)
  )
}