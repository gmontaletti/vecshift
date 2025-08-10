#' Date Logic Module for Employment Period Calculations
#'
#' @description
#' This module handles proper date calculations for employment periods,
#' including the critical FINE+1 logic and temporal segment boundaries.
#' 
#' @details
#' ## Employment Date Logic
#' 
#' The core principle is that employment contracts define inclusive date ranges:
#' - **Contract Period**: INIZIO to FINE (both days inclusive)
#' - **Unemployment Period**: (FINE + 1) to (next INIZIO - 1)
#' 
#' ### Why FINE + 1?
#' 
#' When a contract ends on date FINE:
#' - The person works during FINE (last day of contract)
#' - Unemployment starts the next day: FINE + 1
#' - Next contract might start immediately on FINE + 1 (no gap) or later
#' 
#' ### Event-Based Transformation
#' 
#' Each contract generates two events:
#' 1. **Start Event**: date = INIZIO, value = +1 (employment begins)
#' 2. **End Event**: date = FINE + 1, value = -1 (employment ends, unemployment begins)
#' 
#' The cumulative sum of event values gives the overlap count (arco):
#' - arco = 0: Unemployment (no active contracts)
#' - arco = 1: Single employment (one active contract)  
#' - arco > 1: Multiple employment (overlapping contracts)
#' 
#' ### Duration Calculation
#' 
#' Duration depends on employment status:
#' - **Employment segments** (arco >= 1): durata = fine - inizio
#' - **Unemployment segments** (arco = 0): durata = fine - inizio - 1
#' 
#' The -1 for unemployment accounts for the fact that unemployment
#' periods use exclusive end dates in the event structure.
#'
#' @name date_logic
NULL

#' Calculate Contract Duration
#'
#' @description
#' Calculates the duration of an employment contract in days.
#' Contract dates are inclusive (person works both start and end dates).
#'
#' @param start_date Contract start date (Date or numeric)
#' @param end_date Contract end date (Date or numeric)
#' @return Integer duration in days (inclusive of both dates)
#'
#' @export
#' @examples
#' start_date <- as.Date("2023-01-01")
#' end_date <- as.Date("2023-01-31")
#' contract_duration(start_date, end_date)  # Returns 31 days
contract_duration <- function(start_date, end_date) {
  if (any(end_date < start_date, na.rm = TRUE)) {
    warning("Some end dates are before start dates")
  }
  return(as.integer(end_date - start_date + 1))
}

#' Calculate Unemployment Duration
#'
#' @description  
#' Calculates unemployment duration between two contracts.
#' Unemployment starts the day after the first contract ends
#' and continues until the day before the second contract starts.
#'
#' @param first_end End date of first contract
#' @param second_start Start date of second contract
#' @return Integer duration in days, or 0 if no gap
#'
#' @export
#' @examples
#' first_end <- as.Date("2023-01-31")
#' second_start <- as.Date("2023-02-05")
#' unemployment_duration(first_end, second_start)  # Returns 3 days (Feb 1-3)
unemployment_duration <- function(first_end, second_start) {
  gap_start <- first_end + 1
  gap_end <- second_start - 1
  duration <- as.integer(gap_end - gap_start + 1)
  return(pmax(0, duration))  # Return 0 if no gap or overlap
}

#' Create Employment Events with Proper Date Logic
#'
#' @description
#' Transforms employment contracts into start/end events with proper date handling.
#' Each contract becomes two events: start (value=1) and end (value=-1, date=FINE+1).
#'
#' @param dt Data.table with employment contracts
#' @param start_col Name of start date column (default: "inizio")
#' @param end_col Name of end date column (default: "fine")  
#' @param id_col Name of ID column (default: "id")
#' @param person_col Name of person identifier column (default: "cf")
#' @param type_col Name of employment type column (default: "prior")
#'
#' @return Data.table with employment events, sorted by person and date
#'
#' @export
#' @importFrom data.table rbindlist setorder
create_employment_events_with_dates <- function(dt, 
                                               start_col = "inizio",
                                               end_col = "fine",
                                               id_col = "id", 
                                               person_col = "cf",
                                               type_col = "prior") {
  
  # Create start events (employment begins)
  start_events <- dt[, .(
    id = get(id_col),
    cf = get(person_col), 
    cdata = get(start_col),
    value = 1,
    prior = get(type_col)
  )]
  
  # Create end events (unemployment begins day after contract ends)
  end_events <- dt[, .(
    id = get(id_col),
    cf = get(person_col),
    cdata = get(end_col) + 1,  # Critical: unemployment starts day after
    value = -1,
    prior = 0
  )]
  
  # Combine and sort by person and date
  events <- rbindlist(list(start_events, end_events))
  setorder(events, cf, cdata)
  
  return(events)
}

#' Validate Date Consistency
#'
#' @description
#' Performs comprehensive validation of date logic in employment records.
#' Checks for logical inconsistencies, overlaps, and potential data quality issues.
#'
#' @param dt Data.table with employment records
#' @param start_col Name of start date column
#' @param end_col Name of end date column
#' @param person_col Name of person identifier column
#' @param detailed Logical. If TRUE, returns detailed diagnostic information
#'
#' @return List with validation results
#'
#' @export
#' @importFrom data.table copy setnames
validate_date_consistency <- function(dt, 
                                     start_col = "inizio", 
                                     end_col = "fine",
                                     person_col = "cf",
                                     detailed = FALSE) {
  
  results <- list()
  
  # Check for invalid date ranges (end before start)
  invalid_ranges <- dt[, get(end_col) < get(start_col)]
  results$invalid_ranges <- sum(invalid_ranges, na.rm = TRUE)
  
  # Check for zero-duration contracts
  zero_duration <- dt[, get(end_col) == get(start_col)]
  results$zero_duration_contracts <- sum(zero_duration, na.rm = TRUE)
  
  # Check for overlapping contracts within same person
  if (detailed) {
    # Create temporary data.table with renamed columns for easier access
    temp_dt <- copy(dt)
    setnames(temp_dt, c(start_col, end_col, person_col), c("start_temp", "end_temp", "person_temp"))
    
    overlaps_detail <- temp_dt[, {
      if (.N > 1) {
        setorder(.SD, start_temp)
        overlaps <- 0
        gaps <- numeric()
        for (i in 2:.N) {
          prev_end <- end_temp[i-1]
          curr_start <- start_temp[i]
          if (curr_start <= prev_end) {
            overlaps <- overlaps + 1
          } else {
            gap_days <- as.integer(curr_start - prev_end - 1)
            gaps <- c(gaps, gap_days)
          }
        }
        list(
          n_contracts = .N,
          n_overlaps = overlaps,
          mean_gap_days = if (length(gaps) > 0) mean(gaps) else NA_real_,
          total_employment_days = sum(contract_duration(start_temp, end_temp))
        )
      } else {
        list(
          n_contracts = .N, 
          n_overlaps = 0,
          mean_gap_days = NA_real_,
          total_employment_days = contract_duration(start_temp, end_temp)
        )
      }
    }, by = person_temp]
    
    # Restore original column name
    setnames(overlaps_detail, "person_temp", person_col)
    
    results$person_details <- overlaps_detail
    results$total_persons_with_overlaps <- sum(overlaps_detail$n_overlaps > 0)
  } else {
    # Simple overlap check
    temp_dt <- copy(dt)
    setnames(temp_dt, c(start_col, end_col, person_col), c("start_temp", "end_temp", "person_temp"))
    
    overlaps <- temp_dt[, {
      if (.N > 1) {
        setorder(.SD, start_temp)
        as.integer(sum(start_temp[2:.N] <= end_temp[1:(.N-1)]))
      } else {
        0L
      }
    }, by = person_temp]
    
    results$total_overlapping_pairs <- sum(overlaps$V1)
  }
  
  # Summary flags
  results$has_date_issues <- results$invalid_ranges > 0
  results$has_overlaps <- if (detailed) {
    results$total_persons_with_overlaps > 0
  } else {
    results$total_overlapping_pairs > 0
  }
  
  return(results)
}

#' Convert Date Formats
#'
#' @description
#' Utility functions to handle different date formats commonly found
#' in employment data (Date objects, numeric dates, character strings).
#'
#' @param date_vector Vector of dates in various formats
#' @param origin Origin date for numeric dates (default: "1970-01-01")
#'
#' @return Date vector
#'
#' @export
standardize_dates <- function(date_vector, origin = "1970-01-01") {
  if (inherits(date_vector, "Date")) {
    return(date_vector)
  } else if (is.numeric(date_vector)) {
    return(as.Date(date_vector, origin = origin))
  } else if (is.character(date_vector)) {
    return(as.Date(date_vector))
  } else {
    stop("Unsupported date format")
  }
}

#' Calculate Temporal Coverage Statistics
#'
#' @description
#' Analyzes temporal coverage patterns in employment data,
#' including employment rates, gap distributions, and seasonal patterns.
#'
#' @param dt Data.table with employment records
#' @param start_col Name of start date column
#' @param end_col Name of end date column  
#' @param person_col Name of person identifier column
#'
#' @return List with coverage statistics
#'
#' @export
analyze_temporal_coverage <- function(dt,
                                    start_col = "inizio",
                                    end_col = "fine", 
                                    person_col = "cf") {
  
  coverage <- dt[, {
    # Calculate person's observation period
    obs_start <- min(get(start_col))
    obs_end <- max(get(end_col))
    obs_days <- as.integer(obs_end - obs_start + 1)
    
    # Calculate employment days
    emp_days <- sum(contract_duration(get(start_col), get(end_col)))
    
    # Calculate employment rate
    emp_rate <- emp_days / obs_days
    
    list(
      observation_start = obs_start,
      observation_end = obs_end,
      observation_days = obs_days,
      employment_days = emp_days,
      employment_rate = emp_rate,
      n_contracts = .N
    )
  }, by = c(person_col)]
  
  # Summary statistics
  summary_stats <- list(
    mean_employment_rate = mean(coverage$employment_rate, na.rm = TRUE),
    median_employment_rate = median(coverage$employment_rate, na.rm = TRUE),
    mean_contracts_per_person = mean(coverage$n_contracts),
    total_person_years = sum(coverage$observation_days / 365.25, na.rm = TRUE),
    overall_data_span = c(
      min(coverage$observation_start, na.rm = TRUE),
      max(coverage$observation_end, na.rm = TRUE)
    )
  )
  
  return(list(
    person_coverage = coverage,
    summary = summary_stats
  ))
}