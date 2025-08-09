#' Transform Employment Records into Temporal Segments (Modular Version)
#'
#' @description 
#' Processes employment contract records with temporal boundaries to create 
#' continuous temporal segments that track employment status over time. The function
#' identifies unemployment periods (when no contracts are active), single employment
#' periods, and overlapping employment situations.
#'
#' This is a modular refactoring of the original vecshift function, breaking down
#' the complex processing into smaller, maintainable components.
#'
#' @param dt A data.table containing employment contract records with the following required columns:
#'   \itemize{
#'     \item{\code{id}}: Contract identifier (unique key for each employment contract)
#'     \item{\code{cf}}: Person identifier (e.g., fiscal code)
#'     \item{\code{INIZIO}}: Contract start date (Date or numeric)
#'     \item{\code{FINE}}: Contract end date (Date or numeric)
#'     \item{\code{prior}}: Employment type indicator (0 or negative for part-time, positive for full-time)
#'   }
#' @param validate Logical. If TRUE (default), performs input validation. Set to FALSE for performance with validated data.
#' @param verbose Logical. If TRUE, displays progress messages for large datasets.
#'
#' @return A data.table with temporal segments containing employment status classifications
#'
#' @export
#' @importFrom data.table data.table setorder rbindlist fcase shift
vecshift <- function(dt, validate = TRUE, verbose = FALSE) {
  
  require("data.table")
  
  if (validate) {
    validate_input(dt)
  }
  
  if (verbose && nrow(dt) > 10000) {
    message("Processing ", nrow(dt), " employment records...")
  }
  
  # Step 1: Create employment events
  events <- create_employment_events(dt)
  
  if (verbose) message("Created employment events")
  
  # Step 2: Process temporal segments
  segments <- process_temporal_segments(events)
  
  if (verbose) message("Processed temporal segments")
  
  # Step 3: Classify employment states
  result <- classify_employment_states(segments)
  
  if (verbose) message("Classification complete")
  
  return(result)
}

#' Transform Employment Records into Temporal Segments (Modular Version)
#'
#' @description 
#' This is an alias for the main vecshift function, maintained for backward compatibility.
#' The function now uses a modular approach with separate functions for validation,
#' event creation, segment processing, and state classification.
#'
#' @param dt A data.table containing employment contract records
#' @param validate Logical. If TRUE (default), performs input validation
#' @param verbose Logical. If TRUE, displays progress messages for large datasets
#'
#' @return A data.table with temporal segments containing employment status classifications
#' @export
vecshift_modular <- function(dt, validate = TRUE, verbose = FALSE) {
  
  require("data.table")
  
  if (validate) {
    validate_input(dt)
  }
  
  if (verbose && nrow(dt) > 10000) {
    message("Processing ", nrow(dt), " employment records...")
  }
  
  # Step 1: Create employment events
  events <- create_employment_events(dt)
  
  if (verbose) message("Created employment events")
  
  # Step 2: Process temporal segments
  segments <- process_temporal_segments(events)
  
  if (verbose) message("Processed temporal segments")
  
  # Step 3: Classify employment states
  result <- classify_employment_states(segments)
  
  if (verbose) message("Classification complete")
  
  return(result)
}

#' Validate Input Data
#'
#' @description 
#' Performs comprehensive validation of input data.table to ensure it contains
#' all required columns with appropriate data types and logical consistency.
#'
#' @param dt A data.table to validate
#' @return NULL (throws errors if validation fails)
#' @keywords internal
validate_input <- function(dt) {
  
  # Check if input is a data.table
  if (!inherits(dt, "data.table")) {
    stop("Input 'dt' must be a data.table object. Use as.data.table() to convert.")
  }
  
  # Check for required columns
  required_cols <- c("id", "cf", "INIZIO", "FINE", "prior")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Validate column types
  if (!is.numeric(dt$INIZIO) && !inherits(dt$INIZIO, "Date")) {
    stop("Column 'INIZIO' must be numeric or Date type")
  }
  if (!is.numeric(dt$FINE) && !inherits(dt$FINE, "Date")) {
    stop("Column 'FINE' must be numeric or Date type")
  }
  if (!is.numeric(dt$prior)) {
    stop("Column 'prior' must be numeric")
  }
  
  # Check for logical consistency
  invalid_dates <- dt$FINE < dt$INIZIO
  if (any(invalid_dates, na.rm = TRUE)) {
    n_invalid <- sum(invalid_dates, na.rm = TRUE)
    warning(sprintf("Found %d records where FINE < INIZIO. These may produce unexpected results.", n_invalid))
  }
  
  # Check for NA values
  if (any(is.na(dt$INIZIO)) || any(is.na(dt$FINE))) {
    warning("NA values found in date columns. These records will be excluded from processing.")
  }
  
  invisible(NULL)
}

#' Create Employment Events
#'
#' @description 
#' Transforms employment contract records into start and end events.
#' Each contract is split into two events: a start event (value = 1) and 
#' an end event (value = -1) occurring the day after the contract ends.
#'
#' @param dt A validated data.table with employment records
#' @return A data.table of employment events sorted by person and date
#' @keywords internal
create_employment_events <- function(dt) {
  
  # Create start events
  start_events <- dt[, .(
    id = id,
    cf = cf,
    cdata = INIZIO,
    value = 1,
    prior = prior
  )]
  
  # Create end events (day after contract ends)
  end_events <- dt[, .(
    id = id,
    cf = cf,
    cdata = FINE + 1,
    value = -1,
    prior = 0
  )]
  
  # Combine and sort events
  events <- rbindlist(list(start_events, end_events))
  setorder(events, cf, cdata)
  
  return(events)
}

#' Process Temporal Segments
#'
#' @description 
#' Processes sorted employment events to create temporal segments.
#' Uses cumulative sum to track the number of overlapping contracts (arco)
#' and creates segments between consecutive events for each person.
#'
#' @param events A data.table of sorted employment events
#' @return A data.table with temporal segments
#' @keywords internal
process_temporal_segments <- function(events) {
  
  # Calculate cumulative overlapping contracts
  events[, arco := cumsum(value)]
  
  # Normalize prior values (0 for part-time, 1 for full-time)
  events[, prior := fcase(
    prior <= 0, 0,
    default = 1
  )]
  
  # Create segments between consecutive events - matching original logic exactly
  segments <- events[, .(
    cf = cf[1:(length(cf)-1)],
    acf = cf[2:(length(cf))],
    inizio = cdata[1:(length(cf)-1)],
    fine = cdata[2:(length(cf))],
    arco = arco[1:(length(cf)-1)],
    prior = prior[1:(length(cf)-1)],
    id = id[1:(length(cf)-1)]
  )]
  
  # Keep only segments within the same person
  segments <- segments[cf == acf]
  segments[, acf := NULL]
  
  # Mark unemployment periods (id = 0 when arco = 0)
  segments[arco == 0, id := 0]
  
  return(segments)
}

#' Classify Employment States
#'
#' @description 
#' Classifies each temporal segment into specific employment states based on
#' the number of overlapping contracts and employment types. Also calculates
#' segment duration.
#'
#' @param segments A data.table with temporal segments
#' @return A data.table with classified employment states and durations
#' @keywords internal
classify_employment_states <- function(segments) {
  
  # Calculate duration based on employment status
  segments[, durata := fcase(
    arco >= 1, fine - inizio,
    default = fine - inizio - 1
  )]
  
  # Sort to ensure proper shift operations
  setorder(segments, cf, inizio)
  
  # Classify employment states
  segments[, stato := fcase(
    # Unemployment
    arco == 0 & durata <= 8, "disoccupato",
    arco == 0 & durata > 8, "disoccupato",
    
    # Single employment
    arco == 1 & prior == 1, "occ_ft",
    arco == 1 & prior == 0, "occ_pt",
    
    # Overlapping employment
    arco > 1 & (prior > shift(prior, type = "lag")), "over_pt_ft",
    arco > 1 & (prior < shift(prior, type = "lag")), "over_ft_pt",
    arco > 1 & (prior == shift(prior, type = "lag")) & prior == 0, "over_pt_pt",
    
    # Default: overlapping full-time
    default = "over_ft_ft"
  ), by = cf]
  
  # Filter out zero-duration segments
  result <- segments[durata > 0]
  
  return(result)
}

#' Validate Employment Data Quality
#'
#' @description 
#' Performs data quality checks on employment records and provides
#' detailed diagnostic information about potential issues.
#'
#' @param dt A data.table with employment records
#' @return A list with validation results and diagnostics
#' @export
validate_employment_data <- function(dt) {
  
  diagnostics <- list()
  
  # Check for required columns
  required_cols <- c("id", "cf", "INIZIO", "FINE", "prior")
  diagnostics$missing_columns <- setdiff(required_cols, names(dt))
  
  if (length(diagnostics$missing_columns) == 0) {
    # Check for invalid date ranges
    diagnostics$invalid_dates <- sum(dt$FINE < dt$INIZIO, na.rm = TRUE)
    
    # Check for NA values
    diagnostics$na_counts <- sapply(dt[, ..required_cols], function(x) sum(is.na(x)))
    
    # Check for duplicate IDs
    diagnostics$duplicate_ids <- sum(duplicated(dt$id))
    
    # Check date ranges
    if (is.numeric(dt$INIZIO)) {
      diagnostics$date_range <- range(dt$INIZIO, na.rm = TRUE)
    } else {
      diagnostics$date_range <- range(as.Date(dt$INIZIO), na.rm = TRUE)
    }
    
    # Check prior value distribution
    diagnostics$prior_distribution <- table(dt$prior > 0)
    names(diagnostics$prior_distribution) <- c("Part-time", "Full-time")
    
    # Check for overlapping contracts per person
    overlaps <- dt[, {
      if (.N > 1) {
        setorder(.SD, INIZIO)
        overlap_count <- 0
        for (i in 2:.N) {
          if (INIZIO[i] <= FINE[i-1]) {
            overlap_count <- overlap_count + 1
          }
        }
        list(n_contracts = .N, n_overlaps = overlap_count)
      } else {
        list(n_contracts = .N, n_overlaps = 0)
      }
    }, by = cf]
    
    diagnostics$persons_with_overlaps <- sum(overlaps$n_overlaps > 0)
    diagnostics$total_overlaps <- sum(overlaps$n_overlaps)
  }
  
  # Generate summary
  diagnostics$is_valid <- length(diagnostics$missing_columns) == 0 && 
                          diagnostics$invalid_dates == 0 &&
                          all(diagnostics$na_counts == 0)
  
  class(diagnostics) <- c("employment_validation", "list")
  return(diagnostics)
}

#' Print Employment Data Validation Results
#'
#' @param x An employment_validation object
#' @param ... Additional arguments (ignored)
#' @export
print.employment_validation <- function(x, ...) {
  cat("Employment Data Validation Report\n")
  cat("==================================\n\n")
  
  if (length(x$missing_columns) > 0) {
    cat("Missing Required Columns:", paste(x$missing_columns, collapse = ", "), "\n\n")
  } else {
    cat("✓ All required columns present\n\n")
    
    cat("Data Quality Issues:\n")
    cat("-------------------\n")
    cat("Invalid date ranges (FINE < INIZIO):", x$invalid_dates, "\n")
    cat("Duplicate IDs:", x$duplicate_ids, "\n")
    cat("Persons with overlapping contracts:", x$persons_with_overlaps, "\n")
    cat("Total overlapping periods:", x$total_overlaps, "\n\n")
    
    cat("NA Values:\n")
    cat("----------\n")
    for (col in names(x$na_counts)) {
      if (x$na_counts[col] > 0) {
        cat(sprintf("%s: %d\n", col, x$na_counts[col]))
      }
    }
    if (all(x$na_counts == 0)) {
      cat("✓ No NA values found\n")
    }
    cat("\n")
    
    cat("Date Range:", format(x$date_range[1]), "to", format(x$date_range[2]), "\n\n")
    
    cat("Employment Type Distribution:\n")
    print(x$prior_distribution)
    cat("\n")
    
    if (x$is_valid) {
      cat("✓ Data is valid and ready for processing\n")
    } else {
      cat("⚠ Data has quality issues that should be addressed\n")
    }
  }
  
  invisible(x)
}

#' Summarize Employment Periods
#'
#' @description 
#' Creates a summary of employment periods from vecshift output,
#' including duration statistics and state transitions.
#'
#' @param result Output from vecshift or vecshift_modular function
#' @return A data.table with employment period summaries by person
#' @export
summarize_employment_periods <- function(result) {
  
  summary <- result[, {
    list(
      n_periods = .N,
      total_duration = sum(durata),
      unemployment_duration = sum(durata[stato == "disoccupato"]),
      employment_duration = sum(durata[stato != "disoccupato"]),
      n_unemployment_periods = sum(stato == "disoccupato"),
      n_employment_periods = sum(stato != "disoccupato"),
      n_overlapping_periods = sum(grepl("^over_", stato)),
      avg_period_duration = mean(durata),
      employment_rate = sum(durata[stato != "disoccupato"]) / sum(durata)
    )
  }, by = cf]
  
  return(summary)
}