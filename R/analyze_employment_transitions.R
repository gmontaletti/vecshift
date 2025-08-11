#' Analyze Employment Transitions from Pipeline Output
#'
#' @description
#' Analyzes employment transitions from the output of process_employment_pipeline().
#' Identifies transitions between employment periods that are separated by unemployment
#' periods and provides transition pattern analysis for specified variables.
#'
#' @details
#' A transition occurs when there are consecutive employment periods (arco >= 1) 
#' separated by an unemployment period (arco = 0) of at least the minimum duration.
#' The function analyzes patterns in the "from" → "to" transitions for specified columns.
#' 
#' For each column, the function provides:
#' \itemize{
#'   \item{\strong{from}}: Value in the employment period before unemployment
#'   \item{\strong{to}}: Value in the employment period after unemployment
#'   \item{\strong{weight}}: Number of transitions (.N)
#'   \item{\strong{transition_duration}}: Mean duration of intermediate unemployment periods
#'   \item{\strong{For numeric columns}}: from_mean, to_mean (duration-weighted mean values for that transition)
#'   \item{\strong{For character columns}}: from_mode, to_mode (mode values for that transition)
#' }
#'
#' @param pipeline_result Output from process_employment_pipeline(). Must be a data.table
#'   with columns: cf (person identifier), arco (employment overlap count), 
#'   inizio/fine (period dates), and durata (period duration).
#' @param transition_columns Character vector of column names to analyze. If NULL (default),
#'   uses the merged_columns attribute from pipeline_result.
#' @param min_unemployment_duration Minimum duration (in days) of unemployment period 
#'   to consider a transition (default: 1).
#' @param return_list Logical. If TRUE, returns a list of data.tables (one per variable).
#'   If FALSE (default), returns a single combined data.table with a 'variable' column.
#' @param show_progress Logical. If TRUE (default), displays a progress bar showing
#'   the current processing step, percentage completion, and estimated time remaining.
#'   Uses the 'progress' package if available, falls back to utils::txtProgressBar or
#'   simple messages if not available.
#'
#' @return If return_list = FALSE, returns a data.table with columns:
#'   \itemize{
#'     \item{\code{variable}}: Name of the analyzed variable
#'     \item{\code{from}}: Value before transition
#'     \item{\code{to}}: Value after transition
#'     \item{\code{weight}}: Number of transitions
#'     \item{\code{transition_duration}}: Mean unemployment duration
#'     \item{\code{from_mean/from_mode}}: Duration-weighted aggregated "from" values (numeric/character)
#'     \item{\code{to_mean/to_mode}}: Duration-weighted aggregated "to" values (numeric/character)
#'     \item{\code{[column]_from_mean/[column]_from_mode}}: For each "other" column, duration-weighted aggregated values from the "from" period
#'     \item{\code{[column]_to_mean/[column]_to_mode}}: For each "other" column, duration-weighted aggregated values from the "to" period
#'   }
#'   If return_list = TRUE, returns a named list of data.tables, one for each variable.
#'   
#'   "Other columns" are all columns in the dataset that are not in transition_columns and are not
#'   standard vecshift output columns (cf, inizio, fine, arco, prior, durata, id, stato, collapsed, n_periods).
#'
#' @export
#' @importFrom data.table data.table setorder copy rbindlist setcolorder setnames shift melt :=
#' @importFrom collapse fmean fmode
#' @importFrom stats median weighted.mean
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample employment data with transitions
#' employment_data <- data.table(
#'   id = 1:6,
#'   cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON002"),
#'   INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-08-01", 
#'                      "2023-02-01", "2023-06-01", "2023-10-01")),
#'   FINE = as.Date(c("2023-02-28", "2023-05-31", "2023-12-31", 
#'                    "2023-04-30", "2023-08-31", "2023-12-31")),
#'   prior = c(1, 0, 1, 1, 1, 0),
#'   company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD", "CompanyE", "CompanyF"),
#'   salary = c(50000, 25000, 60000, 55000, 65000, 30000)
#' )
#' 
#' # Process through pipeline
#' result <- process_employment_pipeline(
#'   original_data = employment_data,
#'   merge_columns = c("company", "salary")
#' )
#' 
#' # Analyze transitions with progress bar
#' transitions <- analyze_employment_transitions(result, show_progress = TRUE)
#' print(transitions)
#' 
#' # Analyze specific columns with minimum unemployment duration (no progress bar)
#' transitions_company <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_columns = "company",
#'   min_unemployment_duration = 7,
#'   return_list = TRUE,
#'   show_progress = FALSE
#' )
#' 
#' # Note: If dataset has additional columns like 'salary', 'hours', 'region',
#' # the output will include additional columns such as:
#' # salary_from_mean, salary_to_mean (for numeric columns)
#' # region_from_mode, region_to_mode (for character columns)
#' }
analyze_employment_transitions <- function(pipeline_result,
                                         transition_columns = NULL,
                                         min_unemployment_duration = 1,
                                         return_list = FALSE,
                                         show_progress = TRUE) {
  
  # Load required packages
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  if (!requireNamespace("collapse", quietly = TRUE)) {
    stop("Package 'collapse' is required but not installed.")
  }
  
  # Input validation
  if (!inherits(pipeline_result, "data.table")) {
    stop("Parameter 'pipeline_result' must be a data.table object from process_employment_pipeline().")
  }
  
  # Check for required columns
  required_cols <- c("cf", "arco", "inizio", "fine", "durata")
  missing_cols <- setdiff(required_cols, names(pipeline_result))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in pipeline_result:", paste(missing_cols, collapse = ", ")))
  }
  
  # Validate parameters first
  if (!is.numeric(min_unemployment_duration) || min_unemployment_duration < 0) {
    stop("Parameter 'min_unemployment_duration' must be a non-negative numeric value.")
  }
  
  if (!is.logical(return_list)) {
    stop("Parameter 'return_list' must be a logical value (TRUE or FALSE).")
  }
  
  if (!is.logical(show_progress)) {
    stop("Parameter 'show_progress' must be a logical value (TRUE or FALSE).")
  }
  
  # Determine columns to analyze
  if (is.null(transition_columns)) {
    # Try to extract from merged_columns attribute
    merged_cols <- attr(pipeline_result, "merged_columns")
    if (!is.null(merged_cols)) {
      transition_columns <- merged_cols
    } else {
      # Suggest available columns (exclude standard vecshift columns)
      standard_cols <- c("cf", "inizio", "fine", "arco", "prior", "id", "durata", "stato", 
                        "collapsed", "n_periods")
      available_cols <- setdiff(names(pipeline_result), standard_cols)
      
      if (length(available_cols) > 0) {
        warning(paste("No transition_columns specified and no merged_columns attribute found.",
                     "Available columns for analysis:", paste(available_cols, collapse = ", "),
                     ". Specify transition_columns parameter."))
      } else {
        warning("No transition_columns specified and no additional columns found for analysis.")
      }
      return(data.table())
    }
  }
  
  # Validate transition_columns
  if (!is.character(transition_columns) || length(transition_columns) == 0) {
    stop("Parameter 'transition_columns' must be a non-empty character vector.")
  }
  
  missing_transition_cols <- setdiff(transition_columns, names(pipeline_result))
  if (length(missing_transition_cols) > 0) {
    stop(paste("Columns specified in 'transition_columns' not found in pipeline_result:",
               paste(missing_transition_cols, collapse = ", ")))
  }
  
  # Identify "other columns" - all columns not in transition_columns and not standard vecshift columns
  standard_cols <- c("cf", "inizio", "fine", "arco", "prior", "durata", "id", "stato", 
                     "collapsed", "n_periods")
  other_columns <- setdiff(names(pipeline_result), c(transition_columns, standard_cols))
  
  # Validate other_columns exist in the dataset
  missing_other_cols <- setdiff(other_columns, names(pipeline_result))
  if (length(missing_other_cols) > 0) {
    other_columns <- setdiff(other_columns, missing_other_cols)
  }
  
  # Initialize progress tracking
  progress_steps <- list()
  progress_names <- character()
  pb <- NULL
  start_time <- Sys.time()
  
  # Determine which steps will be executed (optimized steps)
  if (show_progress) {
    progress_steps[["validation"]] <- TRUE
    progress_names <- c(progress_names, "Input validation and setup")
    
    progress_steps[["filtering"]] <- TRUE
    progress_names <- c(progress_names, "Filtering single-period persons")
    
    progress_steps[["transitions_identify"]] <- TRUE
    progress_names <- c(progress_names, "Identifying transitions using shift operations")
    
    progress_steps[["processing"]] <- TRUE
    progress_names <- c(progress_names, "Processing all columns simultaneously")
    
    progress_steps[["aggregations"]] <- TRUE
    progress_names <- c(progress_names, "Final aggregation and formatting")
    
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
      cat("Analyzing employment transitions with", total_steps, "steps...\n")
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
    update_progress("Input validation and setup")
  }
  
  # Work with a copy to avoid modifying original data
  dt <- copy(pipeline_result)
  
  # Ensure proper ordering by person and time
  setorder(dt, cf, inizio)
  
  # Filter out single-period persons early for significant performance gain
  if (show_progress) {
    update_progress("Filtering single-period persons")
  }
  
  # Count periods per person and filter out those with ≤ 2 periods (can't have transitions)
  person_counts <- dt[, .N, by = cf]
  multi_period_persons <- person_counts[N > 2, cf]
  
  if (length(multi_period_persons) == 0) {
    # No persons with enough periods for transitions
    if (show_progress && !is.null(pb)) {
      if (progress_available) {
        # Skip remaining steps
        for (i in (current_step + 1):total_steps) {
          pb$tick()
        }
        cat("\n")
      } else {
        utils::setTxtProgressBar(pb, total_steps)
        close(pb)
        cat("\n")
      }
      cat("No persons found with sufficient periods for transition analysis.\n")
    }
    
    # Return empty result structure
    empty_results <- list()
    for (col in transition_columns) {
      if (is.numeric(pipeline_result[[col]])) {
        empty_result <- data.table(
          from = character(0), to = character(0), weight = integer(0),
          transition_duration = numeric(0), from_mean = numeric(0), to_mean = numeric(0)
        )
      } else {
        empty_result <- data.table(
          from = character(0), to = character(0), weight = integer(0),
          transition_duration = numeric(0), from_mode = character(0), to_mode = character(0)
        )
      }
      if (!return_list) empty_result[, variable := col]
      empty_results[[col]] <- empty_result
    }
    
    if (return_list) {
      return(empty_results)
    } else {
      combined_result <- rbindlist(empty_results, fill = TRUE, use.names = TRUE)
      if ("variable" %in% names(combined_result)) {
        setcolorder(combined_result, c("variable", setdiff(names(combined_result), "variable")))
      }
      return(combined_result)
    }
  }
  
  # Filter dataset to only multi-period persons
  dt <- dt[cf %in% multi_period_persons]
  
  # Identify transitions using vectorized shift operations
  if (show_progress) {
    update_progress("Identifying transitions using shift operations")
  }
  
  # Use shift operations to get previous, current, and next period information
  dt[, `:=`(
    prev_arco = shift(arco, n = 1, type = "lag"),
    next_arco = shift(arco, n = 1, type = "lead"),
    next_next_arco = shift(arco, n = 2, type = "lead")
  ), by = cf]
  
  # Add from/to values for all transition columns on the full dataset
  for (col in transition_columns) {
    dt[, paste0("from_", col) := shift(get(col), n = 1, type = "lag"), by = cf]
    dt[, paste0("to_", col) := shift(get(col), n = 1, type = "lead"), by = cf]
  }
  
  # Add from/to values for all other columns as well
  for (col in other_columns) {
    dt[, paste0("from_", col) := shift(get(col), n = 1, type = "lag"), by = cf]
    dt[, paste0("to_", col) := shift(get(col), n = 1, type = "lead"), by = cf]
  }
  
  # Also capture the durations of the from and to employment periods for weighting
  dt[, from_durata := shift(durata, n = 1, type = "lag"), by = cf]
  dt[, to_durata := shift(durata, n = 1, type = "lead"), by = cf]
  
  # Identify unemployment periods that are part of transitions
  # Current period: unemployment (arco == 0), previous: employment (prev_arco >= 1), next: employment (next_arco >= 1)
  dt[, is_transition_unemployment := (
    !is.na(prev_arco) & prev_arco >= 1 &          # Previous: employment
    !is.na(arco) & arco == 0 &                    # Current: unemployment
    !is.na(next_arco) & next_arco >= 1 &          # Next: employment  
    !is.na(durata) & durata >= min_unemployment_duration  # Min duration
  )]
  
  # Filter to only transition unemployment periods
  transition_periods <- dt[is_transition_unemployment == TRUE]
  
  if (nrow(transition_periods) == 0) {
    # No transitions found
    if (show_progress && !is.null(pb)) {
      if (progress_available) {
        # Skip remaining steps
        for (i in (current_step + 1):total_steps) {
          pb$tick()
        }
        cat("\n")
      } else {
        utils::setTxtProgressBar(pb, total_steps)
        close(pb)
        cat("\n")
      }
      cat("No employment transitions found matching criteria.\n")
    }
    
    # Return empty result structure
    empty_results <- list()
    for (col in transition_columns) {
      if (is.numeric(pipeline_result[[col]])) {
        empty_result <- data.table(
          from = character(0), to = character(0), weight = integer(0),
          transition_duration = numeric(0), from_mean = numeric(0), to_mean = numeric(0)
        )
      } else {
        empty_result <- data.table(
          from = character(0), to = character(0), weight = integer(0),
          transition_duration = numeric(0), from_mode = character(0), to_mode = character(0)
        )
      }
      if (!return_list) empty_result[, variable := col]
      empty_results[[col]] <- empty_result
    }
    
    if (return_list) {
      return(empty_results)
    } else {
      combined_result <- rbindlist(empty_results, fill = TRUE, use.names = TRUE)
      if ("variable" %in% names(combined_result)) {
        setcolorder(combined_result, c("variable", setdiff(names(combined_result), "variable")))
      }
      return(combined_result)
    }
  }
  
  # Process all columns simultaneously
  if (show_progress) {
    update_progress("Processing all columns simultaneously")
  }
  
  # Process columns separately to avoid mixed type issues in melt
  transitions_long_list <- list()
  
  for (i in seq_along(transition_columns)) {
    col <- transition_columns[i]
    from_col <- paste0("from_", col)
    to_col <- paste0("to_", col)
    
    # Create base columns for subset
    base_cols <- c("cf", "durata", from_col, to_col, "from_durata", "to_durata")
    
    # Add other columns' from/to values
    other_from_cols <- paste0("from_", other_columns)
    other_to_cols <- paste0("to_", other_columns)
    all_other_cols <- c(other_from_cols, other_to_cols)
    
    # Filter to only include columns that exist in the dataset
    existing_other_cols <- intersect(all_other_cols, names(transition_periods))
    
    # Create subset with transition column data and all other columns
    col_subset <- transition_periods[, c(base_cols, existing_other_cols), with = FALSE]
    
    # Rename main transition columns for consistent processing
    setnames(col_subset, c(from_col, to_col), c("from", "to"))
    
    # Add variable identifier
    col_subset[, variable := col]
    
    # Remove rows where either from or to is NA
    col_subset <- col_subset[!is.na(from) & !is.na(to)]
    
    transitions_long_list[[col]] <- col_subset
  }
  
  # Combine all columns
  transitions_long <- rbindlist(transitions_long_list, use.names = TRUE, fill = TRUE)
  
  # Final aggregation and formatting
  if (show_progress) {
    update_progress("Final aggregation and formatting")
  }
  
  # Initialize results list
  results_list <- list()
  
  # Process each column's aggregation
  for (col in transition_columns) {
    col_data <- transitions_long[variable == col]
    
    if (nrow(col_data) > 0) {
      # Determine if column is numeric or character
      if (is.numeric(pipeline_result[[col]])) {
        # For numeric columns: calculate duration-weighted means and aggregate
        base_agg <- col_data[, .(
          weight = .N,
          transition_duration = fmean(durata),
          from_mean = {
            w <- as.numeric(from_durata[!is.na(from) & !is.na(from_durata)])
            v <- as.numeric(from)[!is.na(from) & !is.na(from_durata)]
            if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) weighted.mean(v, w = w) else fmean(as.numeric(from))
          },
          to_mean = {
            w <- as.numeric(to_durata[!is.na(to) & !is.na(to_durata)])
            v <- as.numeric(to)[!is.na(to) & !is.na(to_durata)]
            if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) weighted.mean(v, w = w) else fmean(as.numeric(to))
          }
        ), by = .(from, to)]
      } else {
        # For character/factor columns: calculate modes
        base_agg <- col_data[, .(
          weight = .N,
          transition_duration = fmean(durata),
          from_mode = fmode(from),
          to_mode = fmode(to)
        ), by = .(from, to)]
      }
      
      # Add aggregations for other columns
      if (length(other_columns) > 0) {
        for (other_col in other_columns) {
          other_from_col <- paste0("from_", other_col)
          other_to_col <- paste0("to_", other_col)
          
          # Check if both from and to columns exist in the data
          if (other_from_col %in% names(col_data) && other_to_col %in% names(col_data)) {
            
            # Determine if other column is numeric or character
            if (is.numeric(pipeline_result[[other_col]])) {
              # For numeric other columns: calculate duration-weighted means
              # Use list() to create dynamic column names properly in data.table
              from_col_name <- paste0(other_col, "_from_mean")
              to_col_name <- paste0(other_col, "_to_mean")
              
              other_agg <- col_data[, {
                from_value <- {
                  w <- as.numeric(from_durata[!is.na(get(other_from_col)) & !is.na(from_durata)])
                  v <- as.numeric(get(other_from_col))[!is.na(get(other_from_col)) & !is.na(from_durata)]
                  if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) weighted.mean(v, w = w) else fmean(as.numeric(get(other_from_col)))
                }
                to_value <- {
                  w <- as.numeric(to_durata[!is.na(get(other_to_col)) & !is.na(to_durata)])
                  v <- as.numeric(get(other_to_col))[!is.na(get(other_to_col)) & !is.na(to_durata)]
                  if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) weighted.mean(v, w = w) else fmean(as.numeric(get(other_to_col)))
                }
                # Return as a named list
                result <- list(from_value, to_value)
                names(result) <- c(from_col_name, to_col_name)
                result
              }, by = .(from, to)]
            } else {
              # For character other columns: calculate modes
              from_col_name <- paste0(other_col, "_from_mode")
              to_col_name <- paste0(other_col, "_to_mode")
              
              other_agg <- col_data[, {
                from_value <- fmode(get(other_from_col))
                to_value <- fmode(get(other_to_col))
                # Return as a named list
                result <- list(from_value, to_value)
                names(result) <- c(from_col_name, to_col_name)
                result
              }, by = .(from, to)]
            }
            
            # Merge with base aggregation
            base_agg <- merge(base_agg, other_agg, by = c("from", "to"), all.x = TRUE)
          }
        }
      }
      
      result_col <- base_agg
      
      # Order by weight (most common transitions first)
      setorder(result_col, -weight)
      
      # Add variable name if not returning list
      if (!return_list) {
        result_col[, variable := col]
      }
      
      results_list[[col]] <- result_col
      
    } else {
      # No transitions found for this column - create empty result with proper structure
      if (is.numeric(pipeline_result[[col]])) {
        empty_result <- data.table(
          from = character(0), to = character(0), weight = integer(0),
          transition_duration = numeric(0), from_mean = numeric(0), to_mean = numeric(0)
        )
      } else {
        empty_result <- data.table(
          from = character(0), to = character(0), weight = integer(0),
          transition_duration = numeric(0), from_mode = character(0), to_mode = character(0)
        )
      }
      
      # Add empty columns for other columns
      if (length(other_columns) > 0) {
        for (other_col in other_columns) {
          if (is.numeric(pipeline_result[[other_col]])) {
            empty_result[, paste0(other_col, "_from_mean") := numeric(0)]
            empty_result[, paste0(other_col, "_to_mean") := numeric(0)]
          } else {
            empty_result[, paste0(other_col, "_from_mode") := character(0)]
            empty_result[, paste0(other_col, "_to_mode") := character(0)]
          }
        }
      }
      
      if (!return_list) {
        empty_result[, variable := col]
      }
      
      results_list[[col]] <- empty_result
    }
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
    
    total_transitions <- sum(sapply(results_list, function(x) {
      if (nrow(x) > 0 && "weight" %in% names(x)) {
        sum(x$weight, na.rm = TRUE)
      } else {
        0
      }
    }))
    
    cat(sprintf("Transition analysis completed in %.2f seconds. Analyzed %d columns and found %d total transitions.\n", 
                as.numeric(elapsed_time), length(transition_columns), total_transitions))
  }
  
  # Return results
  if (return_list) {
    return(results_list)
  } else {
    # Combine all results into single data.table
    if (length(results_list) > 0) {
      # Reorder columns to put variable first
      combined_result <- rbindlist(results_list, fill = TRUE, use.names = TRUE)
      if ("variable" %in% names(combined_result)) {
        setcolorder(combined_result, c("variable", setdiff(names(combined_result), "variable")))
      }
      return(combined_result)
    } else {
      # Return empty data.table with expected structure
      return(data.table(
        variable = character(0),
        from = character(0),
        to = character(0),
        weight = integer(0),
        transition_duration = numeric(0)
      ))
    }
  }
}