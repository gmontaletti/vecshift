#' Integrated Employment Transformation System
#'
#' @description
#' Advanced employment transformation system that combines the high-performance
#' core engine with modular components for data quality, date handling, and
#' status classification. Provides flexible activation of modules while
#' preserving fast path performance when modules are not needed.
#'
#' @name vecshift_integrated
NULL

#' Transform Employment Records with Integrated Modular System
#'
#' @description
#' Main entry point for the integrated vecshift system. Automatically detects
#' data quality issues, applies appropriate transformations, and provides
#' comprehensive employment status classification with full customization support.
#'
#' @param dt Input data.table with employment records
#' @param column_map Named list mapping standard names to actual column names.
#'   If NULL, assumes standard names: id, cf, inizio, fine, prior
#' @param use_fast_core Logical. Use high-performance core (default: TRUE)
#' @param enable_validation Logical. Enable data quality validation (default: TRUE)  
#' @param enable_cleaning Logical. Enable automatic data cleaning (default: FALSE)
#' @param status_rules Custom status classification rules (default: NULL for standard rules)
#' @param date_standardization Logical. Standardize date formats (default: TRUE)
#' @param quality_report Logical. Generate data quality report (default: FALSE)
#' @param verbose Logical. Print processing messages (default: FALSE)
#'
#' @return List containing:
#'   \itemize{
#'     \item{\code{result}}: Data.table with temporal segments and employment classifications
#'     \item{\code{quality_report}}: Data quality assessment (if requested)
#'     \item{\code{processing_info}}: Processing metadata and performance stats
#'   }
#'
#' @export
#' @importFrom data.table copy
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Standard usage with default column names
#' dt <- data.table(
#'   id = 1:3,
#'   cf = c("A001", "A001", "B002"), 
#'   inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
#'   fine = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
#'   prior = c(1, 0, 1)
#' )
#' 
#' result <- vecshift_integrated(dt)
#' print(result$result)
#' 
#' # Custom column mapping with quality assessment
#' dt_custom <- data.table(
#'   contract_id = 1:3,
#'   person_code = c("A001", "A001", "B002"),
#'   start_date = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
#'   end_date = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
#'   employment_type = c(1, 0, 1)
#' )
#' 
#' col_map <- list(
#'   id = "contract_id",
#'   cf = "person_code",
#'   inizio = "start_date", 
#'   fine = "end_date",
#'   prior = "employment_type"
#' )
#' 
#' result <- vecshift_integrated(
#'   dt_custom, 
#'   column_map = col_map,
#'   quality_report = TRUE,
#'   verbose = TRUE
#' )
#' 
#' print(result$quality_report)
#' }
vecshift_integrated <- function(dt,
                               column_map = NULL,
                               use_fast_core = TRUE,
                               enable_validation = TRUE,
                               enable_cleaning = FALSE,
                               status_rules = NULL, 
                               date_standardization = TRUE,
                               quality_report = FALSE,
                               verbose = FALSE) {
  
  # Initialize processing metadata
  processing_start <- Sys.time()
  processing_info <- list(
    input_rows = nrow(dt),
    modules_used = character(),
    performance_mode = ifelse(use_fast_core, "fast_core", "modular"),
    processing_time = NA
  )
  
  if (verbose) {
    message("Starting vecshift integrated processing...")
    message("Input: ", nrow(dt), " records")
  }
  
  # Step 1: Column standardization
  if (!is.null(column_map)) {
    processing_info$modules_used <- c(processing_info$modules_used, "column_mapping")
    if (verbose) message("Applying column mapping...")
    
    dt_processed <- standardize_columns(dt, column_map, validate = enable_validation)
  } else {
    # Check if data already has standard column names
    required_cols <- c("id", "cf", "INIZIO", "FINE", "prior")
    if (!all(required_cols %in% names(dt))) {
      stop("Data does not have standard column names and no column_map provided. ",
           "Required columns: ", paste(required_cols, collapse = ", "))
    }
    dt_processed <- copy(dt)
  }
  
  # Step 2: Date standardization
  if (date_standardization) {
    processing_info$modules_used <- c(processing_info$modules_used, "date_standardization")
    if (verbose) message("Standardizing date formats...")
    
    # Ensure dates are in proper format
    if (!inherits(dt_processed$INIZIO, "Date")) {
      dt_processed$INIZIO <- as.Date(dt_processed$INIZIO)
    }
    if (!inherits(dt_processed$FINE, "Date")) {
      dt_processed$FINE <- as.Date(dt_processed$FINE)
    }
  }
  
  # Step 3: Data quality assessment and validation
  quality_assessment <- NULL
  if (enable_validation || quality_report) {
    processing_info$modules_used <- c(processing_info$modules_used, "data_quality")
    if (verbose) message("Assessing data quality...")
    
    if (quality_report) {
      quality_assessment <- assess_data_quality(
        dt_processed, 
        include_distributions = TRUE,
        include_temporal = TRUE
      )
      if (verbose) {
        message("Quality score: ", round(quality_assessment$quality_score$overall_score * 100, 1), "%")
      }
    } else {
      # Just validate without full assessment
      validate_employment_data_types(dt_processed, strict = FALSE)
    }
  }
  
  # Step 4: Data cleaning (if requested)
  if (enable_cleaning) {
    processing_info$modules_used <- c(processing_info$modules_used, "data_cleaning")
    if (verbose) message("Cleaning data...")
    
    dt_processed <- clean_employment_data(
      dt_processed,
      remove_duplicates = TRUE,
      remove_invalid_dates = TRUE,
      remove_zero_duration = FALSE,  # Keep zero-duration by default
      verbose = verbose
    )
    
    processing_info$cleaned_rows <- nrow(dt_processed)
    processing_info$rows_removed <- processing_info$input_rows - processing_info$cleaned_rows
  }
  
  # Step 5: Core transformation
  if (verbose) message("Applying core transformation...")
  
  if (use_fast_core) {
    # Use high-performance core without status classification
    processing_info$modules_used <- c(processing_info$modules_used, "fast_core")
    result <- vecshift(dt_processed, classify_status = FALSE)
  } else {
    # Use main vecshift function with appropriate parameters
    processing_info$modules_used <- c(processing_info$modules_used, "vecshift") 
    
    # Apply transformation with or without status classification
    if (!is.null(status_rules)) {
      processing_info$modules_used <- c(processing_info$modules_used, "custom_status_rules")
      result <- vecshift(dt_processed, classify_status = TRUE, status_rules = status_rules)
    } else {
      processing_info$modules_used <- c(processing_info$modules_used, "default_status_rules")
      result <- vecshift(dt_processed, classify_status = TRUE)
    }
  }
  
  # Step 6: Post-processing status classification (if using fast core with custom rules)
  if (use_fast_core && !is.null(status_rules)) {
    processing_info$modules_used <- c(processing_info$modules_used, "custom_status_rules")
    if (verbose) message("Applying custom status rules...")
    
    result <- classify_employment_status(result, rules = status_rules)
  }
  
  # Step 7: Validation of results (if requested)
  if (enable_validation && "default_status_rules" %in% processing_info$modules_used) {
    validation_results <- validate_status_classifications(result, status_rules)
    processing_info$status_validation <- validation_results
    
    if (!validation_results$is_valid && verbose) {
      warning("Status classification validation detected ", 
              validation_results$total_impossible, " issues")
    }
  }
  
  # Finalize processing info
  processing_info$output_rows <- nrow(result)
  processing_info$processing_time <- as.numeric(Sys.time() - processing_start, units = "secs")
  processing_info$records_per_second <- processing_info$input_rows / processing_info$processing_time
  
  if (verbose) {
    message("Processing complete: ", processing_info$output_rows, " segments generated")
    message("Performance: ", round(processing_info$records_per_second, 0), " records/second")
    message("Modules used: ", paste(processing_info$modules_used, collapse = ", "))
  }
  
  # Prepare return object
  return_object <- list(
    result = result,
    processing_info = processing_info
  )
  
  if (quality_report && !is.null(quality_assessment)) {
    return_object$quality_report <- quality_assessment
  }
  
  class(return_object) <- c("vecshift_integrated_result", "list")
  return(return_object)
}

#' High-Performance Employment Transformation (Fast Path)
#'
#' @description
#' Direct access to the high-performance core transformation with minimal overhead.
#' Use this when you have pre-validated data and need maximum speed.
#' Equivalent to vecshift_integrated with all modules disabled.
#'
#' @param dt Data.table with standardized employment records (id, cf, inizio, fine, prior)
#' @param validate_input Logical. Minimal input validation (default: TRUE)
#'
#' @return Data.table with temporal segments and employment status classifications
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(
#'   id = 1:3,
#'   cf = c("A001", "A001", "B002"),
#'   inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
#'   fine = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
#'   prior = c(1, 0, 1)
#' )
#' 
#' result <- vecshift_fast_path(dt)
#' }
vecshift_fast_path <- function(dt, validate_input = TRUE) {
  
  if (validate_input) {
    # Minimal validation only
    required_cols <- c("id", "cf", "INIZIO", "FINE", "prior") 
    missing_cols <- setdiff(required_cols, names(dt))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
  }
  
  return(vecshift(dt, classify_status = FALSE))
}

#' Development Mode Employment Transformation  
#'
#' @description
#' Employment transformation optimized for development, debugging, and testing.
#' Enables all modules with detailed logging and validation for comprehensive
#' analysis and troubleshooting.
#'
#' @param dt Input data.table with employment records
#' @param column_map Column mapping (if needed)
#' @param custom_status_rules Custom classification rules (optional)
#'
#' @return Comprehensive results with full quality assessment and validation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(
#'   id = 1:3,
#'   cf = c("A001", "A001", "B002"),
#'   inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
#'   fine = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
#'   prior = c(1, 0, 1)
#' )
#' 
#' result <- vecshift_dev_mode(dt)
#' print(result$quality_report)
#' print(result$processing_info)
#' }
vecshift_dev_mode <- function(dt, column_map = NULL, custom_status_rules = NULL) {
  
  return(vecshift_integrated(
    dt = dt,
    column_map = column_map,
    use_fast_core = FALSE,  # Use modular for debugging
    enable_validation = TRUE,
    enable_cleaning = TRUE,
    status_rules = custom_status_rules,
    date_standardization = TRUE,
    quality_report = TRUE,
    verbose = TRUE
  ))
}

#' Production Mode Employment Transformation
#'
#' @description  
#' Employment transformation optimized for production workloads with maximum
#' performance. Minimal overhead with essential validation only.
#'
#' @param dt Input data.table with employment records  
#' @param column_map Column mapping (if needed)
#' @param enable_cleaning Enable data cleaning (default: FALSE)
#'
#' @return Results optimized for production use
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Production usage with pre-validated data
#' result <- vecshift_production_mode(clean_data)
#' }
vecshift_production_mode <- function(dt, column_map = NULL, enable_cleaning = FALSE) {
  
  return(vecshift_integrated(
    dt = dt,
    column_map = column_map,
    use_fast_core = TRUE,  # Maximum performance
    enable_validation = FALSE,  # Skip validation for speed
    enable_cleaning = enable_cleaning,
    status_rules = NULL,  # Use default rules
    date_standardization = TRUE,
    quality_report = FALSE,
    verbose = FALSE
  ))
}

#' Benchmark Performance Across Different Modes
#'
#' @description
#' Compares performance across different processing modes to help choose
#' the optimal configuration for your use case.
#'
#' @param dt Sample data.table for benchmarking
#' @param n_iterations Number of benchmark iterations (default: 5)
#'
#' @return Data.table with performance comparison results
#'
# @export
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' sample_data <- data.table(
#'   id = 1:1000,
#'   cf = sample(paste0("CF", 1:100), 1000, replace = TRUE),
#'   inizio = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), 1000),
#'   fine = inizio + sample(30:365, 1000, replace = TRUE),
#'   prior = sample(0:1, 1000, replace = TRUE)
#' )
#' 
#' benchmark_results <- benchmark_vecshift_modes(sample_data)
#' print(benchmark_results)
#' }
benchmark_vecshift_modes <- function(dt, n_iterations = 5) {
  
  modes <- list(
    fast_path = function(x) vecshift_fast_path(x),
    production = function(x) vecshift_production_mode(x)$result,
    integrated_default = function(x) vecshift_integrated(x)$result,
    dev_mode = function(x) vecshift_dev_mode(x)$result
  )
  
  results <- list()
  
  for (mode_name in names(modes)) {
    mode_func <- modes[[mode_name]]
    
    # Warm-up run
    tryCatch(mode_func(dt), error = function(e) NULL)
    
    # Benchmark runs
    times <- numeric(n_iterations)
    for (i in 1:n_iterations) {
      start_time <- Sys.time()
      tryCatch({
        result <- mode_func(dt)
        times[i] <- as.numeric(Sys.time() - start_time, units = "secs")
      }, error = function(e) {
        times[i] <- NA
      })
    }
    
    valid_times <- times[!is.na(times)]
    if (length(valid_times) > 0) {
      results[[mode_name]] <- list(
        mean_time = mean(valid_times),
        median_time = median(valid_times),
        min_time = min(valid_times),
        max_time = max(valid_times),
        records_per_sec = nrow(dt) / mean(valid_times),
        success_rate = length(valid_times) / n_iterations
      )
    } else {
      results[[mode_name]] <- list(
        mean_time = NA, median_time = NA, min_time = NA, max_time = NA,
        records_per_sec = NA, success_rate = 0
      )
    }
  }
  
  # Convert to data.table for easy viewing
  benchmark_dt <- data.table(
    mode = names(results),
    mean_time_sec = sapply(results, function(x) x$mean_time),
    records_per_sec = sapply(results, function(x) x$records_per_sec),
    success_rate = sapply(results, function(x) x$success_rate)
  )
  
  # Calculate relative performance
  if (!all(is.na(benchmark_dt$mean_time_sec))) {
    fastest_time <- min(benchmark_dt$mean_time_sec, na.rm = TRUE)
    benchmark_dt[, relative_speed := fastest_time / mean_time_sec]
  }
  
  setorder(benchmark_dt, mean_time_sec)
  
  return(benchmark_dt)
}

#' Print Vecshift Integrated Results
#'
#' @param x A vecshift_integrated_result object
#' @param ... Additional arguments (ignored)
#' @export  
print.vecshift_integrated_result <- function(x, ...) {
  cat("Vecshift Integrated Results\n")
  cat("==========================\n\n")
  
  cat("Processing Summary:\n")
  cat("------------------\n")
  cat(sprintf("Input Records: %s\n", format(x$processing_info$input_rows, big.mark = ",")))
  cat(sprintf("Output Segments: %s\n", format(x$processing_info$output_rows, big.mark = ",")))
  
  if (!is.null(x$processing_info$cleaned_rows)) {
    cat(sprintf("Records Cleaned: %s (%.1f%% removed)\n", 
                format(x$processing_info$rows_removed, big.mark = ","),
                x$processing_info$rows_removed / x$processing_info$input_rows * 100))
  }
  
  cat(sprintf("Processing Time: %.3f seconds\n", x$processing_info$processing_time))
  cat(sprintf("Performance: %s records/second\n", 
              format(round(x$processing_info$records_per_second), big.mark = ",")))
  cat(sprintf("Mode: %s\n", x$processing_info$performance_mode))
  cat("\n")
  
  cat("Modules Used:\n")
  cat("-------------\n")
  for (module in x$processing_info$modules_used) {
    cat(sprintf("• %s\n", gsub("_", " ", module)))
  }
  cat("\n")
  
  # Status distribution from results
  if ("stato" %in% names(x$result)) {
    cat("Employment Status Distribution:\n")
    cat("------------------------------\n")
    status_counts <- table(x$result$stato)
    for (status in names(status_counts)) {
      count <- status_counts[status]
      pct <- round(count / sum(status_counts) * 100, 1)
      cat(sprintf("%-15s: %6d (%4.1f%%)\n", status, count, pct))
    }
    cat("\n")
  }
  
  # Quality report summary (if available)
  if (!is.null(x$quality_report)) {
    cat("Data Quality Summary:\n")
    cat("--------------------\n")
    cat(sprintf("Quality Score: %.1f%%\n", x$quality_report$quality_score$overall_score * 100))
    if (x$quality_report$quality_score$is_production_ready) {
      cat("✓ Production Ready\n")
    } else {
      cat("⚠ Quality Issues Detected\n")
    }
  }
  
  cat("\nResult Preview:\n")
  cat("---------------\n")
  print(head(x$result, 5))
  
  if (nrow(x$result) > 5) {
    cat(sprintf("... and %d more rows\n", nrow(x$result) - 5))
  }
  
  invisible(x)
}