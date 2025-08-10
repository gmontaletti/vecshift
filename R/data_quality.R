#' Data Quality and Validation Module
#'
#' @description
#' Comprehensive data quality assessment and validation for employment records.
#' Provides flexible column mapping, data type validation, logical consistency checks,
#' and detailed quality reporting for employment temporal analysis.
#'
#' @name data_quality
NULL

#' Standardize Column Names for Employment Data
#'
#' @description
#' Maps custom column names to standardized internal names used by vecshift functions.
#' Allows flexibility in input data structure while maintaining consistent processing.
#'
#' @param dt Input data.table with employment records
#' @param column_map Named list mapping standard names to actual column names:
#'   \itemize{
#'     \item{\code{id}}: Contract identifier column name (required)
#'     \item{\code{cf}}: Person identifier column name (required) 
#'     \item{\code{inizio}}: Start date column name (required)
#'     \item{\code{fine}}: End date column name (required)
#'     \item{\code{prior}}: Employment type column name (required)
#'   }
#' @param validate Logical. If TRUE, validates the mapped columns
#'
#' @return Data.table with standardized column names
#'
#' @export
#' @importFrom data.table setnames copy
#' 
#' @examples
#' \dontrun{
#' library(data.table)
#' # Data with custom column names
#' dt <- data.table(
#'   contract_id = 1:3,
#'   person_code = c("A001", "A001", "B002"),
#'   start_date = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
#'   end_date = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
#'   employment_type = c(1, 0, 1)
#' )
#' 
#' # Define column mapping
#' col_map <- list(
#'   id = "contract_id",
#'   cf = "person_code", 
#'   inizio = "start_date",
#'   fine = "end_date",
#'   prior = "employment_type"
#' )
#' 
#' # Standardize column names
#' standardized_dt <- standardize_columns(dt, col_map)
#' }
standardize_columns <- function(dt, column_map, validate = TRUE) {
  
  # Required standard column names
  required_standard <- c("id", "cf", "inizio", "fine", "prior")
  
  # Check that all required mappings are provided
  missing_mappings <- setdiff(required_standard, names(column_map))
  if (length(missing_mappings) > 0) {
    stop("Missing column mappings for: ", paste(missing_mappings, collapse = ", "))
  }
  
  # Check that mapped columns exist in the data
  mapped_columns <- unlist(column_map)
  missing_columns <- setdiff(mapped_columns, names(dt))
  if (length(missing_columns) > 0) {
    stop("Mapped columns not found in data: ", paste(missing_columns, collapse = ", "))
  }
  
  # Create a copy and rename columns
  result_dt <- copy(dt)
  
  # Rename columns according to mapping
  for (standard_name in names(column_map)) {
    original_name <- column_map[[standard_name]]
    if (original_name != standard_name) {
      setnames(result_dt, original_name, standard_name)
    }
  }
  
  # Keep only the standardized columns (remove extra columns)
  result_dt <- result_dt[, ..required_standard]
  
  if (validate) {
    validate_employment_data_types(result_dt)
  }
  
  return(result_dt)
}

#' Validate Employment Data Types and Formats
#'
#' @description
#' Performs comprehensive validation of employment data types, formats,
#' and basic logical consistency. Throws errors for critical issues
#' and warnings for potential problems.
#'
#' @param dt Data.table with standardized employment columns
#' @param strict Logical. If TRUE, throws errors for warnings
#'
#' @return Invisible validation results list
#'
#' @export
validate_employment_data_types <- function(dt, strict = FALSE) {
  
  validation_results <- list()
  
  # Check if input is a data.table
  if (!inherits(dt, "data.table")) {
    stop("Input must be a data.table object. Use as.data.table() to convert.")
  }
  
  # Check for required columns
  required_cols <- c("id", "cf", "inizio", "fine", "prior")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Validate ID column
  if (!is.numeric(dt$id) && !is.character(dt$id)) {
    stop("Column 'id' must be numeric or character type")
  }
  
  # Check for duplicate IDs
  if (any(duplicated(dt$id))) {
    n_dups <- sum(duplicated(dt$id))
    msg <- paste("Found", n_dups, "duplicate contract IDs")
    validation_results$duplicate_ids <- n_dups
    if (strict) {
      stop(msg)
    } else {
      warning(msg)
    }
  }
  
  # Validate person identifier
  if (!is.character(dt$cf) && !is.numeric(dt$cf)) {
    stop("Column 'cf' must be character or numeric type")
  }
  
  # Validate date columns
  date_cols <- c("inizio", "fine")
  for (col in date_cols) {
    col_data <- dt[[col]]
    if (!is.numeric(col_data) && !inherits(col_data, "Date")) {
      stop(paste("Column", col, "must be numeric or Date type"))
    }
    
    # Check for NA values in dates
    if (any(is.na(col_data))) {
      n_na <- sum(is.na(col_data))
      msg <- paste("Found", n_na, "NA values in column", col)
      validation_results[[paste0("na_", col)]] <- n_na
      if (strict) {
        stop(msg)
      } else {
        warning(msg)
      }
    }
  }
  
  # Validate prior column
  if (!is.numeric(dt$prior)) {
    stop("Column 'prior' must be numeric type")
  }
  
  # Check for logical date consistency
  invalid_dates <- dt$fine < dt$inizio
  if (any(invalid_dates, na.rm = TRUE)) {
    n_invalid <- sum(invalid_dates, na.rm = TRUE)
    msg <- paste("Found", n_invalid, "records where end date < start date")
    validation_results$invalid_date_ranges <- n_invalid
    if (strict) {
      stop(msg)
    } else {
      warning(msg)
    }
  }
  
  # Check for zero-duration contracts
  zero_duration <- dt$fine == dt$inizio
  if (any(zero_duration, na.rm = TRUE)) {
    n_zero <- sum(zero_duration, na.rm = TRUE)
    validation_results$zero_duration_contracts <- n_zero
    if (n_zero > nrow(dt) * 0.1) {  # More than 10% zero duration
      warning("High proportion (", round(n_zero/nrow(dt)*100, 1), 
              "%) of zero-duration contracts detected")
    }
  }
  
  validation_results$is_valid <- length(validation_results) == 0 ||
                                 all(sapply(validation_results, function(x) is.numeric(x) && x == 0))
  
  invisible(validation_results)
}

#' Comprehensive Data Quality Assessment
#'
#' @description
#' Performs exhaustive data quality analysis including missing values,
#' duplicates, outliers, temporal patterns, and logical inconsistencies.
#' Generates detailed quality report with recommendations.
#'
#' @param dt Data.table with employment records (standardized columns)
#' @param person_col Name of person identifier column
#' @param include_distributions Logical. Include distribution analysis
#' @param include_temporal Logical. Include temporal pattern analysis
#'
#' @return List with comprehensive quality assessment results
#'
#' @export
#' @importFrom data.table uniqueN
assess_data_quality <- function(dt, 
                               person_col = "cf",
                               include_distributions = TRUE,
                               include_temporal = TRUE) {
  
  quality_report <- list()
  
  # Basic data dimensions
  quality_report$dimensions <- list(
    n_records = nrow(dt),
    n_persons = uniqueN(dt[[person_col]]),
    n_unique_ids = uniqueN(dt$id),
    date_range = if (is.numeric(dt$inizio)) {
      range(dt$inizio, na.rm = TRUE)
    } else {
      range(as.Date(dt$inizio), na.rm = TRUE)  
    }
  )
  
  # Missing value analysis
  quality_report$missing_values <- sapply(dt, function(x) sum(is.na(x)))
  
  # Duplicate analysis
  quality_report$duplicates <- list(
    duplicate_ids = sum(duplicated(dt$id)),
    duplicate_records = sum(duplicated(dt))
  )
  
  # Logical consistency checks
  quality_report$logical_consistency <- list(
    invalid_date_ranges = sum(dt$fine < dt$inizio, na.rm = TRUE),
    zero_duration_contracts = sum(dt$fine == dt$inizio, na.rm = TRUE),
    negative_prior_values = sum(dt$prior < 0, na.rm = TRUE),
    missing_person_ids = sum(is.na(dt[[person_col]]) | dt[[person_col]] == "")
  )
  
  # Person-level analysis
  person_stats <- dt[, {
    list(
      n_contracts = .N,
      total_days = sum(contract_duration(inizio, fine)),
      date_span_days = as.integer(max(fine) - min(inizio) + 1),
      has_overlaps = if (.N > 1) {
        setorder(.SD, inizio)
        any(inizio[2:.N] <= fine[1:(.N-1)])
      } else FALSE
    )
  }, by = c(person_col)]
  
  quality_report$person_level <- list(
    contracts_per_person = summary(person_stats$n_contracts),
    persons_with_overlaps = sum(person_stats$has_overlaps),
    employment_days_distribution = summary(person_stats$total_days),
    coverage_rate_distribution = summary(person_stats$total_days / person_stats$date_span_days)
  )
  
  # Distribution analysis
  if (include_distributions) {
    quality_report$distributions <- list(
      prior_values = table(dt$prior),
      contract_durations = summary(contract_duration(dt$inizio, dt$fine)),
      contracts_by_person = table(person_stats$n_contracts),
      employment_type_by_person = dt[, list(
        ft_contracts = sum(prior > 0),
        pt_contracts = sum(prior <= 0)
      ), by = c(person_col)][, {
        list(
          only_ft = sum(ft_contracts > 0 & pt_contracts == 0),
          only_pt = sum(ft_contracts == 0 & pt_contracts > 0), 
          mixed = sum(ft_contracts > 0 & pt_contracts > 0),
          no_contracts = sum(ft_contracts == 0 & pt_contracts == 0)
        )
      }]
    )
  }
  
  # Temporal pattern analysis
  if (include_temporal) {
    if (inherits(dt$inizio, "Date") || is.numeric(dt$inizio)) {
      dates <- if (is.numeric(dt$inizio)) as.Date(dt$inizio, origin = "1970-01-01") else dt$inizio
      
      quality_report$temporal_patterns <- list(
        contracts_by_year = table(format(dates, "%Y")),
        contracts_by_month = table(format(dates, "%m")),
        seasonal_distribution = table(quarters(dates)),
        temporal_clustering = analyze_temporal_clustering(dt, person_col)
      )
    }
  }
  
  # Data quality score
  total_records <- nrow(dt)
  quality_issues <- sum(
    quality_report$missing_values,
    quality_report$duplicates$duplicate_ids,
    quality_report$logical_consistency$invalid_date_ranges,
    quality_report$logical_consistency$missing_person_ids
  )
  
  quality_report$quality_score <- list(
    overall_score = max(0, 1 - quality_issues / total_records),
    issues_per_1000_records = (quality_issues / total_records) * 1000,
    is_production_ready = quality_issues < (total_records * 0.01)  # Less than 1% issues
  )
  
  class(quality_report) <- c("employment_quality_report", "list")
  return(quality_report)
}

#' Analyze Temporal Clustering Patterns
#' 
#' @description
#' Detects patterns in contract timing such as seasonal employment,
#' batch processing indicators, or systematic data collection periods.
#'
#' @param dt Data.table with employment records
#' @param person_col Person identifier column name
#'
#' @return List with temporal clustering analysis
#' 
#' @keywords internal
analyze_temporal_clustering <- function(dt, person_col = "cf") {
  
  # Convert dates for analysis
  start_dates <- if (is.numeric(dt$inizio)) {
    as.Date(dt$inizio, origin = "1970-01-01")
  } else {
    as.Date(dt$inizio)
  }
  
  # Day of month analysis (detect batch processing)
  day_of_month <- table(format(start_dates, "%d"))
  first_days_pct <- (day_of_month["01"] / sum(day_of_month)) * 100
  
  # Weekend start analysis (detect data quality issues)
  weekdays <- weekdays(start_dates)
  weekend_starts <- sum(weekdays %in% c("Saturday", "Sunday"), na.rm = TRUE)
  
  # Month-end patterns
  is_month_end <- format(start_dates, "%d") %in% c("28", "29", "30", "31")
  month_end_pct <- (sum(is_month_end, na.rm = TRUE) / length(start_dates)) * 100
  
  return(list(
    first_day_percentage = as.numeric(first_days_pct),
    weekend_starts = weekend_starts,
    weekend_start_percentage = (weekend_starts / length(start_dates)) * 100,
    month_end_percentage = month_end_pct,
    suggests_batch_processing = first_days_pct > 20,  # More than 20% start on 1st
    suggests_data_issues = (weekend_starts / length(start_dates)) > 0.15  # >15% weekend starts
  ))
}

#' Clean Employment Data
#'
#' @description
#' Applies automatic data cleaning procedures to address common data quality issues.
#' Includes options for handling missing values, duplicates, and logical inconsistencies.
#'
#' @param dt Data.table with employment records
#' @param remove_duplicates Logical. Remove duplicate records
#' @param remove_invalid_dates Logical. Remove records with invalid date ranges  
#' @param remove_zero_duration Logical. Remove zero-duration contracts
#' @param fill_missing_prior Logical. Fill missing prior values with mode
#' @param verbose Logical. Print cleaning summary
#'
#' @return Cleaned data.table with cleaning summary as attribute
#'
#' @export
clean_employment_data <- function(dt,
                                 remove_duplicates = TRUE,
                                 remove_invalid_dates = TRUE, 
                                 remove_zero_duration = FALSE,
                                 fill_missing_prior = TRUE,
                                 verbose = TRUE) {
  
  original_rows <- nrow(dt)
  cleaning_log <- list()
  
  # Work on a copy
  cleaned_dt <- copy(dt)
  
  # Remove duplicates
  if (remove_duplicates) {
    dups_before <- sum(duplicated(cleaned_dt))
    cleaned_dt <- unique(cleaned_dt)
    dups_removed <- dups_before - sum(duplicated(cleaned_dt))
    cleaning_log$duplicates_removed <- dups_removed
    if (verbose && dups_removed > 0) {
      message("Removed ", dups_removed, " duplicate records")
    }
  }
  
  # Remove invalid date ranges
  if (remove_invalid_dates) {
    invalid_before <- sum(cleaned_dt$fine < cleaned_dt$inizio, na.rm = TRUE)
    cleaned_dt <- cleaned_dt[!(fine < inizio)]
    cleaning_log$invalid_dates_removed <- invalid_before
    if (verbose && invalid_before > 0) {
      message("Removed ", invalid_before, " records with invalid date ranges")
    }
  }
  
  # Remove zero-duration contracts
  if (remove_zero_duration) {
    zero_before <- sum(cleaned_dt$fine == cleaned_dt$inizio, na.rm = TRUE)
    cleaned_dt <- cleaned_dt[!(fine == inizio)]
    cleaning_log$zero_duration_removed <- zero_before
    if (verbose && zero_before > 0) {
      message("Removed ", zero_before, " zero-duration contracts")
    }
  }
  
  # Fill missing prior values
  if (fill_missing_prior && any(is.na(cleaned_dt$prior))) {
    na_count <- sum(is.na(cleaned_dt$prior))
    prior_mode <- as.numeric(names(sort(table(cleaned_dt$prior), decreasing = TRUE)[1]))
    cleaned_dt[is.na(prior), prior := prior_mode]
    cleaning_log$prior_values_filled <- na_count
    if (verbose && na_count > 0) {
      message("Filled ", na_count, " missing prior values with mode: ", prior_mode)
    }
  }
  
  final_rows <- nrow(cleaned_dt)
  cleaning_log$rows_before <- original_rows
  cleaning_log$rows_after <- final_rows
  cleaning_log$rows_removed <- original_rows - final_rows
  cleaning_log$cleaning_rate <- (original_rows - final_rows) / original_rows
  
  if (verbose) {
    message("Data cleaning complete: ", original_rows, " -> ", final_rows, 
            " records (", round(cleaning_log$cleaning_rate * 100, 2), "% removed)")
  }
  
  # Add cleaning log as attribute
  attr(cleaned_dt, "cleaning_log") <- cleaning_log
  
  return(cleaned_dt)
}

#' Print Employment Quality Report
#'
#' @description
#' Formats and prints a comprehensive data quality report with
#' recommendations for data improvement.
#'
#' @param x An employment_quality_report object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.employment_quality_report <- function(x, ...) {
  cat("Employment Data Quality Report\n")
  cat("===============================\n\n")
  
  # Dimensions
  cat("Data Dimensions:\n")
  cat("---------------\n")
  cat(sprintf("Records: %s\n", format(x$dimensions$n_records, big.mark = ",")))
  cat(sprintf("Persons: %s\n", format(x$dimensions$n_persons, big.mark = ",")))
  cat(sprintf("Unique IDs: %s\n", format(x$dimensions$n_unique_ids, big.mark = ",")))
  cat(sprintf("Date Range: %s to %s\n", 
              format(x$dimensions$date_range[1]), 
              format(x$dimensions$date_range[2])))
  cat("\n")
  
  # Quality score
  score_pct <- round(x$quality_score$overall_score * 100, 1)
  cat("Overall Quality Score: ", score_pct, "%\n")
  if (x$quality_score$is_production_ready) {
    cat("✓ Data is production ready\n")
  } else {
    cat("⚠ Data quality issues detected - review recommended\n")
  }
  cat("\n")
  
  # Issues summary
  cat("Data Quality Issues:\n")
  cat("-------------------\n")
  cat("Missing Values:\n")
  for (col in names(x$missing_values)) {
    if (x$missing_values[col] > 0) {
      cat(sprintf("  %s: %d\n", col, x$missing_values[col]))
    }
  }
  if (all(x$missing_values == 0)) cat("  None detected ✓\n")
  
  cat("\nDuplicates:\n")
  cat(sprintf("  Duplicate IDs: %d\n", x$duplicates$duplicate_ids))
  cat(sprintf("  Duplicate Records: %d\n", x$duplicates$duplicate_records))
  
  cat("\nLogical Consistency:\n")
  cat(sprintf("  Invalid Date Ranges: %d\n", x$logical_consistency$invalid_date_ranges))
  cat(sprintf("  Zero Duration Contracts: %d\n", x$logical_consistency$zero_duration_contracts))
  cat(sprintf("  Missing Person IDs: %d\n", x$logical_consistency$missing_person_ids))
  
  # Person-level patterns
  cat("\nPerson-Level Patterns:\n")
  cat("---------------------\n")
  cat(sprintf("Persons with Overlapping Contracts: %d (%.1f%%)\n",
              x$person_level$persons_with_overlaps,
              x$person_level$persons_with_overlaps / x$dimensions$n_persons * 100))
  cat("Contracts per Person: ")
  cat(sprintf("Min=%d, Med=%.1f, Max=%d\n", 
              x$person_level$contracts_per_person["Min."],
              x$person_level$contracts_per_person["Median"],
              x$person_level$contracts_per_person["Max."]))
  
  # Recommendations
  cat("\nRecommendations:\n")
  cat("---------------\n")
  if (x$duplicates$duplicate_ids > 0) {
    cat("• Remove or investigate duplicate contract IDs\n")
  }
  if (x$logical_consistency$invalid_date_ranges > 0) {
    cat("• Fix records where end date is before start date\n")
  }
  if (sum(x$missing_values) > 0) {
    cat("• Address missing values in key columns\n")
  }
  if (x$person_level$persons_with_overlaps > x$dimensions$n_persons * 0.5) {
    cat("• High overlap rate detected - verify this matches expected employment patterns\n")
  }
  
  invisible(x)
}