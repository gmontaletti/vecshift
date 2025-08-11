#' Process Employment Data Through Complete Pipeline
#'
#' @description
#' An optimized helper function that chains together vecshift(), merge_original_columns(),
#' and merge_consecutive_employment_fast() operations for efficient memory usage and
#' performance. This function provides a complete pipeline from raw employment contracts
#' to processed temporal segments with merged original data and collapsed consecutive periods.
#'
#' @details
#' The function performs the following operations in sequence:
#' \enumerate{
#'   \item \strong{vecshift()}: Transforms employment records into temporal segments
#'   \item \strong{merge_original_columns()}: Merges additional columns from original data (optional)
#'   \item \strong{merge_overlapping_values()}: Handles overlapping employment values (optional)
#'   \item \strong{merge_consecutive_employment_fast()}: Collapses consecutive employment periods (optional)
#' }
#'
#' The function is optimized for memory efficiency by:
#' \itemize{
#'   \item Using data.table's reference semantics to minimize copying
#'   \item Avoiding intermediate object creation where possible
#'   \item Allowing selective execution of pipeline steps
#'   \item Gracefully handling missing functions with fallback behavior
#' }
#'
#' @param original_data A data.table containing employment contract records with required columns:
#'   \itemize{
#'     \item{\code{id}}: Contract identifier
#'     \item{\code{cf}}: Person identifier  
#'     \item{\code{INIZIO}}: Contract start date
#'     \item{\code{FINE}}: Contract end date
#'     \item{\code{prior}}: Employment type indicator
#'   }
#' @param apply_vecshift Logical. If TRUE (default), applies vecshift transformation.
#'   Set to FALSE if input data is already processed vecshift output.
#' @param merge_columns Character vector of column names to merge from original_data.
#'   Set to NULL (default) to skip column merging step.
#' @param handle_overlaps Logical. If TRUE (default when merge_columns is specified),
#'   processes overlapping employment values. Only applies when merge_columns is not NULL.
#' @param collapse_consecutive Logical. If TRUE (default), collapses consecutive
#'   employment periods using the fast implementation.
#' @param classify_status Logical. If TRUE (default), applies employment status
#'   classification during vecshift step.
#' @param status_rules Optional custom status rules for vecshift. If NULL, uses defaults.
#' @param validate_functions Logical. If TRUE (default), checks for function availability
#'   before execution and provides informative error messages.
#' @param show_progress Logical. If TRUE (default), displays a progress bar showing
#'   the current processing step, percentage completion, and estimated time remaining.
#'   Uses the 'progress' package if available, falls back to utils::txtProgressBar or
#'   simple messages if not available.
#'
#' @return A data.table containing the processed employment segments. The exact structure
#'   depends on which pipeline steps were applied:
#'   \itemize{
#'     \item \strong{Base vecshift output}: cf, inizio, fine, arco, prior, id, durata, stato
#'     \item \strong{With merged columns}: Additional columns from original_data
#'     \item \strong{With collapsed consecutive}: collapsed, n_periods, and aggregated values
#'   }
#'
#' @note
#' This function automatically detects which processing functions are available and
#' gracefully handles missing dependencies. If merge_original_columns() or
#' merge_consecutive_employment_fast() are not available, the corresponding steps
#' are skipped with a warning message.
#'
#' @export
#' @importFrom data.table data.table copy setDT
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample employment data with metadata
#' employment_data <- data.table(
#'   id = 1:4,
#'   cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002"),
#'   INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01", "2023-02-01")),
#'   FINE = as.Date(c("2023-03-31", "2023-06-30", "2023-12-31", "2023-11-30")),
#'   prior = c(1, 1, 0, 1),
#'   company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD"),
#'   salary = c(50000, 55000, 25000, 60000),
#'   department = c("IT", "IT", "HR", "Finance")
#' )
#' 
#' # Complete pipeline with all steps and progress bar
#' result_full <- process_employment_pipeline(
#'   original_data = employment_data,
#'   merge_columns = c("company", "salary", "department"),
#'   handle_overlaps = TRUE,
#'   collapse_consecutive = TRUE,
#'   show_progress = TRUE
#' )
#' print(result_full)
#' 
#' # Only vecshift transformation without progress bar
#' result_basic <- process_employment_pipeline(
#'   original_data = employment_data,
#'   merge_columns = NULL,
#'   collapse_consecutive = FALSE,
#'   show_progress = FALSE
#' )
#' 
#' # Pipeline with column merging but no consecutive collapsing
#' result_merged <- process_employment_pipeline(
#'   original_data = employment_data,
#'   merge_columns = c("company", "salary"),
#'   collapse_consecutive = FALSE
#' )
#' 
#' # Process already-transformed vecshift output
#' segments <- vecshift(employment_data)
#' result_post <- process_employment_pipeline(
#'   original_data = employment_data,
#'   apply_vecshift = FALSE,
#'   merge_columns = c("company"),
#'   collapse_consecutive = TRUE
#' )
#' }
process_employment_pipeline <- function(original_data,
                                       apply_vecshift = TRUE,
                                       merge_columns = NULL,
                                       handle_overlaps = !is.null(merge_columns),
                                       collapse_consecutive = TRUE,
                                       classify_status = TRUE,
                                       status_rules = NULL,
                                       validate_functions = TRUE,
                                       show_progress = TRUE) {
  
  # Load required package
  require("data.table")
  
  # Initialize progress tracking
  progress_steps <- list()
  progress_names <- character()
  pb <- NULL
  start_time <- Sys.time()
  
  # Determine which steps will be executed
  if (show_progress) {
    progress_steps[["validation"]] <- TRUE
    progress_names <- c(progress_names, "Input validation")
    
    if (apply_vecshift) {
      progress_steps[["vecshift"]] <- TRUE
      progress_names <- c(progress_names, "Applying vecshift transformation")
    }
    
    if (!is.null(merge_columns)) {
      progress_steps[["merge_columns"]] <- TRUE
      progress_names <- c(progress_names, "Merging original columns")
      
      if (handle_overlaps) {
        progress_steps[["handle_overlaps"]] <- TRUE  
        progress_names <- c(progress_names, "Handling overlapping values")
      }
    }
    
    if (collapse_consecutive) {
      progress_steps[["collapse"]] <- TRUE
      progress_names <- c(progress_names, "Collapsing consecutive periods")
    }
    
    progress_steps[["cleanup"]] <- TRUE
    progress_names <- c(progress_names, "Final cleanup")
    
    total_steps <- length(progress_steps)
    current_step <- 0
    
    # Try to initialize progress bar (prefer progress package, fallback to utils)
    progress_available <- FALSE
    if (requireNamespace("progress", quietly = TRUE)) {
      pb <- progress::progress_bar$new(
        format = "[:bar] :percent | Step :current/:total | :what | ETA: :eta",
        total = total_steps,
        clear = FALSE,
        width = 80
      )
      progress_available <- TRUE
    } else if (show_progress) {
      # Fallback to utils::txtProgressBar 
      pb <- utils::txtProgressBar(min = 0, max = total_steps, style = 3, width = 50)
      cat("Processing employment pipeline with", total_steps, "steps...\n")
    }
  }
  
  # Helper function to update progress
  update_progress <- function(step_name) {
    if (show_progress) {
      current_step <<- current_step + 1
      
      if (progress_available && !is.null(pb)) {
        pb$tick(tokens = list(what = step_name, current = current_step, total = total_steps))
      } else if (!is.null(pb)) {
        utils::setTxtProgressBar(pb, current_step)
        cat("\n", sprintf("[%d/%d] %s", current_step, total_steps, step_name), "\n")
      } else {
        cat(sprintf("[%d/%d] %s\n", current_step, total_steps, step_name))
      }
    }
  }
  
  # Start progress tracking
  if (show_progress) {
    update_progress("Input validation")
  }
  
  # Input validation
  if (!inherits(original_data, "data.table")) {
    stop("Parameter 'original_data' must be a data.table object. Use as.data.table() to convert.")
  }
  
  # Validate function availability if requested
  if (validate_functions) {
    if (!is.null(merge_columns) && !exists("merge_original_columns", mode = "function")) {
      stop("Function 'merge_original_columns' not found. Please ensure the merge_columns.R file is loaded.")
    }
    
    if (!is.null(merge_columns) && handle_overlaps && !exists("merge_overlapping_values", mode = "function")) {
      stop("Function 'merge_overlapping_values' not found. Please ensure the merge_columns.R file is loaded.")
    }
    
    if (collapse_consecutive && !exists("merge_consecutive_employment_fast", mode = "function")) {
      stop("Function 'merge_consecutive_employment_fast' not found. Please ensure the merge_consecutive_employment.R file is loaded.")
    }
  }
  
  # Check required columns for vecshift if applying it
  if (apply_vecshift) {
    required_cols <- c("id", "cf", "INIZIO", "FINE", "prior")
    missing_cols <- setdiff(required_cols, names(original_data))
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns for vecshift:", paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Validate merge_columns if specified
  if (!is.null(merge_columns)) {
    if (!is.character(merge_columns) || length(merge_columns) == 0) {
      stop("Parameter 'merge_columns' must be a non-empty character vector or NULL.")
    }
    
    missing_merge_cols <- setdiff(merge_columns, names(original_data))
    if (length(missing_merge_cols) > 0) {
      stop(paste("Columns specified in 'merge_columns' not found in original_data:", 
                 paste(missing_merge_cols, collapse = ", ")))
    }
  }
  
  # Step 1: Apply vecshift transformation if requested
  if (apply_vecshift) {
    if (show_progress) {
      update_progress("Applying vecshift transformation")
    }
    
    if (!exists("vecshift", mode = "function")) {
      stop("Function 'vecshift' not found. Please ensure the vecshift.R file is loaded.")
    }
    
    result <- vecshift(original_data, classify_status = classify_status, status_rules = status_rules)
    
    # Provide memory usage information if in verbose mode
    if (getOption("vecshift.verbose", FALSE)) {
      cat("Step 1 (vecshift) completed. Rows:", nrow(result), "\n")
    }
  } else {
    # Use original_data as starting point (assume it's already processed)
    result <- copy(original_data)
    setDT(result)
  }
  
  # Step 2: Merge original columns if requested
  if (!is.null(merge_columns)) {
    if (show_progress) {
      update_progress("Merging original columns")
    }
    
    if (exists("merge_original_columns", mode = "function")) {
      # Apply merge_original_columns
      result <- merge_original_columns(
        original_data = original_data,
        segments = result,
        columns = merge_columns
      )
      
      if (getOption("vecshift.verbose", FALSE)) {
        cat("Step 2 (merge_original_columns) completed. Columns added:", length(merge_columns), "\n")
      }
      
      # Step 2b: Handle overlapping values if requested
      if (handle_overlaps && exists("merge_overlapping_values", mode = "function")) {
        if (show_progress) {
          update_progress("Handling overlapping values")
        }
        
        result <- merge_overlapping_values(
          segments_with_columns = result,
          columns = merge_columns
        )
        
        if (getOption("vecshift.verbose", FALSE)) {
          cat("Step 2b (merge_overlapping_values) completed.\n")
        }
      } else if (handle_overlaps) {
        warning("merge_overlapping_values function not available. Skipping overlap handling.")
      }
      
    } else {
      warning("merge_original_columns function not available. Skipping column merging step.")
    }
  }
  
  # Step 3: Collapse consecutive employment periods if requested
  if (collapse_consecutive) {
    if (show_progress) {
      update_progress("Collapsing consecutive periods")
    }
    
    if (exists("merge_consecutive_employment_fast", mode = "function")) {
      result <- merge_consecutive_employment_fast(result)
      
      if (getOption("vecshift.verbose", FALSE)) {
        cat("Step 3 (merge_consecutive_employment_fast) completed. Final rows:", nrow(result), "\n")
      }
    } else {
      warning("merge_consecutive_employment_fast function not available. Skipping consecutive period collapsing.")
    }
  }
  
  # Final validation and cleanup
  if (show_progress) {
    update_progress("Final cleanup")
  }
  
  if (nrow(result) == 0) {
    warning("Pipeline resulted in empty data.table. Please check input data and parameters.")
  }
  
  # Ensure proper ordering by person and time
  if ("cf" %in% names(result) && "inizio" %in% names(result)) {
    setorder(result, cf, inizio)
  }
  
  # Add pipeline metadata as attributes for debugging/tracking
  setattr(result, "pipeline_steps", list(
    vecshift_applied = apply_vecshift,
    columns_merged = !is.null(merge_columns),
    overlaps_handled = handle_overlaps && !is.null(merge_columns),
    consecutive_collapsed = collapse_consecutive,
    status_classified = classify_status
  ))
  
  if (!is.null(merge_columns)) {
    setattr(result, "merged_columns", merge_columns)
  }
  
  # Clean up progress bar and show completion message
  if (show_progress) {
    if (progress_available && !is.null(pb)) {
      # Progress bar automatically completes when all steps are ticked
      cat("\n")
    } else if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, total_steps)
      close(pb)
      cat("\n")
    }
    
    # Show completion summary
    end_time <- Sys.time()
    elapsed_time <- difftime(end_time, start_time, units = "secs")
    
    cat(sprintf("Pipeline completed in %.2f seconds. Processed %d rows -> %d rows.\n", 
                as.numeric(elapsed_time), nrow(original_data), nrow(result)))
  }
  
  return(result)
}


#' Check Pipeline Function Availability
#'
#' @description
#' Utility function to check which pipeline functions are available in the current
#' environment. This helps users understand what pipeline steps can be executed.
#'
#' @return A named logical vector indicating which functions are available:
#'   \itemize{
#'     \item{\code{vecshift}}: Core transformation function
#'     \item{\code{merge_original_columns}}: Column merging function
#'     \item{\code{merge_overlapping_values}}: Overlap handling function
#'     \item{\code{merge_consecutive_employment_fast}}: Consecutive period collapsing
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Check which functions are available
#' availability <- check_pipeline_functions()
#' print(availability)
#' 
#' # Use availability to conditionally run pipeline steps
#' if (availability["merge_original_columns"]) {
#'   # Can safely merge columns
#' }
#' }
check_pipeline_functions <- function() {
  functions_to_check <- c(
    "vecshift",
    "merge_original_columns", 
    "merge_overlapping_values",
    "merge_consecutive_employment_fast"
  )
  
  availability <- sapply(functions_to_check, function(fn) {
    exists(fn, mode = "function")
  })
  
  return(availability)
}


#' Get Pipeline Recommendations
#'
#' @description
#' Analyzes input data and provides recommendations for optimal pipeline
#' configuration based on data characteristics and available functions.
#'
#' @param data A data.table containing employment data to analyze
#' @param target_operation Character string indicating the intended use:
#'   \itemize{
#'     \item{\code{"analysis"}}: For statistical analysis (default)
#'     \item{\code{"reporting"}}: For business reporting
#'     \item{\code{"visualization"}}: For data visualization
#'     \item{\code{"export"}}: For data export/sharing
#'   }
#'
#' @return A list containing:
#'   \itemize{
#'     \item{\code{recommendations}}: Named logical vector of recommended settings
#'     \item{\code{merge_columns}}: Suggested columns to merge (if any)
#'     \item{\code{reasoning}}: Character vector explaining the recommendations
#'     \item{\code{warnings}}: Any potential issues detected
#'   }
#'
#' @export
#' @importFrom data.table data.table
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample data
#' sample_data <- data.table(
#'   id = 1:100,
#'   cf = sample(paste0("PERSON", 1:20), 100, replace = TRUE),
#'   INIZIO = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), 100),
#'   FINE = INIZIO + sample(30:365, 100, replace = TRUE),
#'   prior = sample(c(0, 1), 100, replace = TRUE),
#'   company = sample(c("CompanyA", "CompanyB", "CompanyC"), 100, replace = TRUE),
#'   salary = sample(25000:80000, 100, replace = TRUE)
#' )
#' 
#' # Get recommendations for analysis
#' recs <- get_pipeline_recommendations(sample_data, "analysis")
#' print(recs)
#' 
#' # Apply recommendations
#' result <- process_employment_pipeline(
#'   original_data = sample_data,
#'   merge_columns = recs$merge_columns,
#'   apply_vecshift = recs$recommendations["apply_vecshift"],
#'   handle_overlaps = recs$recommendations["handle_overlaps"],
#'   collapse_consecutive = recs$recommendations["collapse_consecutive"]
#' )
#' }
get_pipeline_recommendations <- function(data, target_operation = "analysis") {
  
  # Input validation
  if (!inherits(data, "data.table")) {
    stop("Parameter 'data' must be a data.table object.")
  }
  
  valid_operations <- c("analysis", "reporting", "visualization", "export")
  if (!target_operation %in% valid_operations) {
    stop(paste("Parameter 'target_operation' must be one of:", paste(valid_operations, collapse = ", ")))
  }
  
  # Initialize recommendations
  recommendations <- list(
    apply_vecshift = TRUE,
    handle_overlaps = TRUE,
    collapse_consecutive = TRUE,
    classify_status = TRUE
  )
  
  reasoning <- character()
  warnings <- character()
  merge_columns <- NULL
  
  # Analyze data characteristics
  n_rows <- nrow(data)
  n_persons <- if ("cf" %in% names(data)) length(unique(data$cf)) else NA
  
  # Check for additional columns beyond required vecshift columns
  required_cols <- c("id", "cf", "INIZIO", "FINE", "prior")
  extra_cols <- setdiff(names(data), required_cols)
  
  if (length(extra_cols) > 0) {
    # Filter out likely internal/system columns
    system_cols <- c("durata", "stato", "arco", "inizio", "fine")
    candidate_cols <- setdiff(extra_cols, system_cols)
    
    if (length(candidate_cols) > 0) {
      merge_columns <- candidate_cols
      reasoning <- c(reasoning, paste("Additional columns detected:", paste(candidate_cols, collapse = ", ")))
    }
  }
  
  # Adjust recommendations based on target operation
  if (target_operation == "analysis") {
    # For analysis, typically want full processing
    recommendations$collapse_consecutive <- TRUE
    reasoning <- c(reasoning, "Analysis target: enabling consecutive period collapsing for cleaner temporal segments")
    
  } else if (target_operation == "reporting") {
    # For reporting, might want to preserve individual periods
    recommendations$collapse_consecutive <- FALSE
    reasoning <- c(reasoning, "Reporting target: preserving individual periods for detailed reports")
    
  } else if (target_operation == "visualization") {
    # For visualization, collapsed periods are usually better
    recommendations$collapse_consecutive <- TRUE
    reasoning <- c(reasoning, "Visualization target: collapsing consecutive periods for cleaner plots")
    
  } else if (target_operation == "export") {
    # For export, provide comprehensive data
    recommendations$handle_overlaps <- !is.null(merge_columns)
    recommendations$collapse_consecutive <- TRUE
    reasoning <- c(reasoning, "Export target: comprehensive processing for external use")
  }
  
  # Check data size and performance implications
  if (n_rows > 100000) {
    warnings <- c(warnings, paste("Large dataset detected (", n_rows, " rows). Consider processing in chunks for memory efficiency."))
  }
  
  # Check for potential data quality issues
  if ("INIZIO" %in% names(data) && "FINE" %in% names(data)) {
    invalid_ranges <- sum(data$FINE < data$INIZIO, na.rm = TRUE)
    if (invalid_ranges > 0) {
      warnings <- c(warnings, paste(invalid_ranges, "records have FINE < INIZIO. Consider data cleaning."))
    }
  }
  
  # Check function availability and adjust recommendations
  availability <- check_pipeline_functions()
  
  if (!availability["merge_original_columns"] && !is.null(merge_columns)) {
    warnings <- c(warnings, "merge_original_columns not available. Column merging will be skipped.")
    recommendations$handle_overlaps <- FALSE
  }
  
  if (!availability["merge_consecutive_employment_fast"]) {
    warnings <- c(warnings, "merge_consecutive_employment_fast not available. Consecutive collapsing will be skipped.")
    recommendations$collapse_consecutive <- FALSE
  }
  
  return(list(
    recommendations = recommendations,
    merge_columns = merge_columns,
    reasoning = reasoning,
    warnings = warnings,
    data_summary = list(
      n_rows = n_rows,
      n_persons = n_persons,
      extra_columns = length(extra_cols),
      target_operation = target_operation
    )
  ))
}