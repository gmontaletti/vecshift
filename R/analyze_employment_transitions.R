#' Analyze Employment Transitions from Pipeline Output
#'
#' @description
#' Analyzes employment transitions from the output of process_employment_pipeline().
#' Identifies transitions between employment periods that are separated by unemployment
#' periods and provides transition pattern analysis. The function allows specification
#' of which variable to use for transition analysis and which variables to compute
#' statistics for.
#'
#' @details
#' A transition occurs when there are consecutive employment periods (arco >= 1) 
#' separated by an unemployment period (arco = 0) of at least the minimum duration.
#' The function analyzes patterns in the "from" → "to" transitions for one specified
#' transition variable, while computing summary statistics for additional variables.
#' 
#' For each transition, the function provides:
#' \itemize{
#'   \item{\strong{from}}: Value in the employment period before unemployment (transition variable)
#'   \item{\strong{to}}: Value in the employment period after unemployment (transition variable)
#'   \item{\strong{weight}}: Number of transitions (.N)
#'   \item{\strong{transition_duration}}: Mean duration of intermediate unemployment periods
#'   \item{\strong{For numeric statistics variables}}: [variable]_from_median, [variable]_to_median
#'   \item{\strong{For character statistics variables}}: [variable]_from_mode, [variable]_to_mode
#' }
#'
#' @param pipeline_result Output from process_employment_pipeline(). Must be a data.table
#'   with columns: cf (person identifier), arco (employment overlap count), 
#'   inizio/fine (period dates), and durata (period duration).
#' @param transition_variable Character string specifying the variable to use for 
#'   transition analysis (from/to values). If NULL (default), uses the first 
#'   non-standard attribute in the data.table.
#' @param statistics_variables Character vector specifying variables to compute 
#'   summary statistics for. If NULL (default), uses all non-standard attributes 
#'   except the transition variable.
#' @param min_unemployment_duration Minimum duration (in days) of unemployment period 
#'   to consider a transition (default: 1).
#' @param max_unemployment_duration Maximum duration (in days) of unemployment period 
#'   to consider a transition. If NULL (default), no upper limit is applied. When not 
#'   NULL, only transitions with unemployment duration between min_unemployment_duration 
#'   and max_unemployment_duration (inclusive) are included.
#' @param output_transition_matrix Logical. If TRUE, returns a square transition matrix 
#'   instead of the normal aggregated data.table. Rows represent "from" states, columns 
#'   represent "to" states, and values are transition weights (counts). Non-populated 
#'   cells contain zeros. Matrix uses unique values from the transition_variable as 
#'   row/column names (default: FALSE).
#' @param show_progress Logical. If TRUE (default), displays a progress bar showing
#'   the current processing step, percentage completion, and estimated time remaining.
#'   Uses the 'progress' package if available, falls back to utils::txtProgressBar or
#'   simple messages if not available.
#'
#' @return When \code{output_transition_matrix} is FALSE (default), returns a data.table with columns:
#'   \itemize{
#'     \item{\code{from}}: Value before transition (from transition_variable)
#'     \item{\code{to}}: Value after transition (from transition_variable)
#'     \item{\code{weight}}: Number of transitions
#'     \item{\code{transition_duration}}: Mean unemployment duration
#'     \item{\code{[variable]_from_median/[variable]_from_mode}}: For each statistics variable, 
#'       duration-weighted aggregated values from the "from" period (median for numeric, mode for character)
#'     \item{\code{[variable]_to_median/[variable]_to_mode}}: For each statistics variable, 
#'       duration-weighted aggregated values from the "to" period (median for numeric, mode for character)
#'   }
#'   
#'   When \code{output_transition_matrix} is TRUE, returns a square matrix where rows represent 
#'   "from" states, columns represent "to" states, and values are transition weights (counts). 
#'   Non-populated cells contain zeros.
#'   
#'   Note: If the transition variable is also in statistics_variables, it will have
#'   corresponding [transition_variable]_from_median and [transition_variable]_to_median columns.
#'
#' @export
#' @importFrom data.table data.table setorder copy setnames shift :=
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
#' # Analyze company transitions with salary statistics
#' transitions <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   statistics_variables = c("salary")
#' )
#' print(transitions)
#' 
#' # Analyze salary transitions with company statistics and minimum unemployment duration
#' transitions_salary <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "salary",
#'   statistics_variables = c("company"),
#'   min_unemployment_duration = 7,
#'   show_progress = FALSE
#' )
#' 
#' # Analyze transitions with duration constraints (unemployment between 7-30 days)
#' transitions_constrained <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   min_unemployment_duration = 7,
#'   max_unemployment_duration = 30
#' )
#' 
#' # Get transition matrix instead of data.table
#' transition_matrix <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   output_transition_matrix = TRUE
#' )
#' print(transition_matrix)
#' 
#' # Output will include columns like:
#' # from, to (salary values for transitions)
#' # company_from_mode, company_to_mode (company statistics)
#' # salary_from_median, salary_to_median (if salary is in statistics_variables)
#' }
analyze_employment_transitions <- function(pipeline_result,
                                         transition_variable = NULL,
                                         statistics_variables = NULL,
                                         min_unemployment_duration = 1,
                                         max_unemployment_duration = NULL,
                                         output_transition_matrix = FALSE,
                                         show_progress = TRUE) {
  
  # Load required packages
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  
  # Check for collapse package (use base R alternatives if not available)
  use_collapse <- requireNamespace("collapse", quietly = TRUE)
  
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
  
  # Validate parameters
  if (!is.numeric(min_unemployment_duration) || min_unemployment_duration < 0) {
    stop("Parameter 'min_unemployment_duration' must be a non-negative numeric value.")
  }
  
  if (!is.null(max_unemployment_duration)) {
    if (!is.numeric(max_unemployment_duration) || max_unemployment_duration < 0) {
      stop("Parameter 'max_unemployment_duration' must be NULL or a non-negative numeric value.")
    }
    if (max_unemployment_duration < min_unemployment_duration) {
      stop("Parameter 'max_unemployment_duration' must be greater than or equal to 'min_unemployment_duration'.")
    }
  }
  
  if (!is.logical(output_transition_matrix) || length(output_transition_matrix) != 1) {
    stop("Parameter 'output_transition_matrix' must be a single logical value (TRUE or FALSE).")
  }
  
  if (!is.logical(show_progress)) {
    stop("Parameter 'show_progress' must be a logical value (TRUE or FALSE).")
  }
  
  # Identify available columns (exclude standard vecshift columns)
  standard_cols <- c("cf", "inizio", "fine", "arco", "prior", "durata", "id", "stato", 
                     "collapsed", "n_periods")
  available_cols <- setdiff(names(pipeline_result), standard_cols)
  
  if (length(available_cols) == 0) {
    stop("No additional columns found for analysis. Pipeline result should contain variables beyond standard vecshift output columns.")
  }
  
  # Determine transition variable
  if (is.null(transition_variable)) {
    # Use first available column as default
    transition_variable <- available_cols[1]
    if (show_progress) {
      message(sprintf("Using '%s' as transition variable (first available attribute)", transition_variable))
    }
  }
  
  # Validate transition variable
  if (!is.character(transition_variable) || length(transition_variable) != 1) {
    stop("Parameter 'transition_variable' must be a single character string.")
  }
  
  if (!transition_variable %in% names(pipeline_result)) {
    stop(sprintf("Transition variable '%s' not found in pipeline_result.", transition_variable))
  }
  
  # Determine statistics variables
  if (is.null(statistics_variables)) {
    # Use all available columns except the transition variable
    statistics_variables <- setdiff(available_cols, transition_variable)
    if (show_progress && length(statistics_variables) > 0) {
      message(sprintf("Using %d variables for statistics: %s", 
                     length(statistics_variables), 
                     paste(statistics_variables, collapse = ", ")))
    }
  }
  
  # Validate statistics variables
  if (!is.character(statistics_variables)) {
    stop("Parameter 'statistics_variables' must be a character vector.")
  }
  
  missing_stats_cols <- setdiff(statistics_variables, names(pipeline_result))
  if (length(missing_stats_cols) > 0) {
    stop(paste("Statistics variables not found in pipeline_result:",
               paste(missing_stats_cols, collapse = ", ")))
  }
  
  # Initialize progress tracking
  start_time <- Sys.time()
  
  if (show_progress) {
    message("Starting employment transition analysis...")
  }
  
  # Work with a copy to avoid modifying original data
  dt <- copy(pipeline_result)
  
  # Ensure proper ordering by person and time
  setorder(dt, cf, inizio)
  
  if (show_progress) {
    message("Filtering persons with sufficient periods for transitions...")
  }
  
  # Count periods per person and filter out those with ≤ 2 periods (can't have transitions)
  person_counts <- dt[, .N, by = cf]
  multi_period_persons <- person_counts[N > 2, cf]
  
  if (length(multi_period_persons) == 0) {
    if (show_progress) {
      message("No persons found with sufficient periods for transition analysis.")
    }
    
    # Return appropriate empty result structure
    if (output_transition_matrix) {
      # Return empty 0x0 matrix
      return(matrix(numeric(0), nrow = 0, ncol = 0, dimnames = list(character(0), character(0))))
    } else {
      empty_result <- data.table(
        from = character(0),
        to = character(0),
        weight = integer(0),
        transition_duration = numeric(0)
      )
      
      # Add statistics columns for each statistics variable
      for (stat_var in statistics_variables) {
        if (is.numeric(pipeline_result[[stat_var]])) {
          empty_result[, paste0(stat_var, "_from_median") := numeric(0)]
          empty_result[, paste0(stat_var, "_to_median") := numeric(0)]
        } else {
          empty_result[, paste0(stat_var, "_from_mode") := character(0)]
          empty_result[, paste0(stat_var, "_to_mode") := character(0)]
        }
      }
      
      return(empty_result)
    }
  }
  
  # Filter dataset to only multi-period persons
  dt <- dt[cf %in% multi_period_persons]
  
  if (show_progress) {
    message("Identifying employment transitions...")
  }
  
  # Use shift operations to get previous, current, and next period information
  dt[, `:=`(
    prev_arco = shift(arco, n = 1, type = "lag"),
    next_arco = shift(arco, n = 1, type = "lead")
  ), by = cf]
  
  # Add from/to values for transition variable
  from_transition_col <- paste0("from_", transition_variable)
  to_transition_col <- paste0("to_", transition_variable)
  dt[, (from_transition_col) := shift(get(transition_variable), n = 1, type = "lag"), by = cf]
  dt[, (to_transition_col) := shift(get(transition_variable), n = 1, type = "lead"), by = cf]
  
  # Add from/to values for statistics variables
  for (stat_var in statistics_variables) {
    from_stat_col <- paste0("from_", stat_var)
    to_stat_col <- paste0("to_", stat_var)
    dt[, (from_stat_col) := shift(get(stat_var), n = 1, type = "lag"), by = cf]
    dt[, (to_stat_col) := shift(get(stat_var), n = 1, type = "lead"), by = cf]
  }
  
  # Also capture the durations of the from and to employment periods for weighting
  dt[, from_durata := shift(durata, n = 1, type = "lag"), by = cf]
  dt[, to_durata := shift(durata, n = 1, type = "lead"), by = cf]
  
  # Identify unemployment periods that are part of transitions
  # Current period: unemployment (arco == 0), previous: employment (prev_arco >= 1), next: employment (next_arco >= 1)
  if (is.null(max_unemployment_duration)) {
    dt[, is_transition_unemployment := (
      !is.na(prev_arco) & prev_arco >= 1 &          # Previous: employment
      !is.na(arco) & arco == 0 &                    # Current: unemployment
      !is.na(next_arco) & next_arco >= 1 &          # Next: employment  
      !is.na(durata) & durata >= min_unemployment_duration  # Min duration
    )]
  } else {
    dt[, is_transition_unemployment := (
      !is.na(prev_arco) & prev_arco >= 1 &          # Previous: employment
      !is.na(arco) & arco == 0 &                    # Current: unemployment
      !is.na(next_arco) & next_arco >= 1 &          # Next: employment  
      !is.na(durata) & durata >= min_unemployment_duration &  # Min duration
      !is.na(durata) & durata <= max_unemployment_duration    # Max duration
    )]
  }
  
  # Filter to only transition unemployment periods
  transition_periods <- dt[is_transition_unemployment == TRUE]
  
  if (nrow(transition_periods) == 0) {
    if (show_progress) {
      message("No employment transitions found matching criteria.")
    }
    
    # Return appropriate empty result structure
    if (output_transition_matrix) {
      # Return empty 0x0 matrix
      return(matrix(numeric(0), nrow = 0, ncol = 0, dimnames = list(character(0), character(0))))
    } else {
      empty_result <- data.table(
        from = character(0),
        to = character(0),
        weight = integer(0),
        transition_duration = numeric(0)
      )
      
      # Add statistics columns for each statistics variable
      for (stat_var in statistics_variables) {
        if (is.numeric(pipeline_result[[stat_var]])) {
          empty_result[, paste0(stat_var, "_from_median") := numeric(0)]
          empty_result[, paste0(stat_var, "_to_median") := numeric(0)]
        } else {
          empty_result[, paste0(stat_var, "_from_mode") := character(0)]
          empty_result[, paste0(stat_var, "_to_mode") := character(0)]
        }
      }
      
      return(empty_result)
    }
  }
  
  if (show_progress) {
    message("Computing transition statistics...")
  }
  
  # Create subset with required columns for aggregation
  required_cols <- c("cf", "durata", from_transition_col, to_transition_col, "from_durata", "to_durata")
  
  # Add statistics variable columns
  stat_from_cols <- paste0("from_", statistics_variables)
  stat_to_cols <- paste0("to_", statistics_variables)
  all_stat_cols <- c(stat_from_cols, stat_to_cols)
  
  # Filter to only include columns that exist in the dataset
  existing_stat_cols <- intersect(all_stat_cols, names(transition_periods))
  
  # Create subset
  col_subset <- transition_periods[, c(required_cols, existing_stat_cols), with = FALSE]
  
  # Rename transition columns for consistent processing
  setnames(col_subset, c(from_transition_col, to_transition_col), c("from", "to"))
  
  # Remove rows where either from or to is NA for the transition variable
  transitions_data <- col_subset[!is.na(from) & !is.na(to)]
  
  if (nrow(transitions_data) == 0) {
    if (show_progress) {
      message("No valid transitions found after filtering missing values.")
    }
    
    # Return appropriate empty result structure
    if (output_transition_matrix) {
      # Return empty 0x0 matrix
      return(matrix(numeric(0), nrow = 0, ncol = 0, dimnames = list(character(0), character(0))))
    } else {
      empty_result <- data.table(
        from = character(0),
        to = character(0),
        weight = integer(0),
        transition_duration = numeric(0)
      )
      
      # Add statistics columns
      for (stat_var in statistics_variables) {
        if (is.numeric(pipeline_result[[stat_var]])) {
          empty_result[, paste0(stat_var, "_from_median") := numeric(0)]
          empty_result[, paste0(stat_var, "_to_median") := numeric(0)]
        } else {
          empty_result[, paste0(stat_var, "_from_mode") := character(0)]
          empty_result[, paste0(stat_var, "_to_mode") := character(0)]
        }
      }
      
      return(empty_result)
    }
  }
  
  # Create base aggregation with transition counts and duration
  if (use_collapse) {
    base_agg <- transitions_data[, .(
      weight = .N,
      transition_duration = collapse::fmean(durata)
    ), by = .(from, to)]
  } else {
    base_agg <- transitions_data[, .(
      weight = .N,
      transition_duration = mean(durata, na.rm = TRUE)
    ), by = .(from, to)]
  }
  
  # Add statistics for each statistics variable
  for (stat_var in statistics_variables) {
    from_stat_col <- paste0("from_", stat_var)
    to_stat_col <- paste0("to_", stat_var)
    
    # Check if both from and to columns exist in the data
    if (from_stat_col %in% names(transitions_data) && to_stat_col %in% names(transitions_data)) {
      
      if (is.numeric(pipeline_result[[stat_var]])) {
        # For numeric variables: compute duration-weighted median
        from_col_name <- paste0(stat_var, "_from_median")
        to_col_name <- paste0(stat_var, "_to_median")
        
        stat_agg <- transitions_data[, {
          from_value <- {
            w <- as.numeric(from_durata[!is.na(get(from_stat_col)) & !is.na(from_durata)])
            v <- as.numeric(get(from_stat_col))[!is.na(get(from_stat_col)) & !is.na(from_durata)]
            if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
              # For weighted median, use repeated values approach
              rep_v <- rep(v, times = pmax(1, round(w)))  # Ensure minimum weight of 1
              median(rep_v, na.rm = TRUE)
            } else {
              median(as.numeric(get(from_stat_col)), na.rm = TRUE)
            }
          }
          to_value <- {
            w <- as.numeric(to_durata[!is.na(get(to_stat_col)) & !is.na(to_durata)])
            v <- as.numeric(get(to_stat_col))[!is.na(get(to_stat_col)) & !is.na(to_durata)]
            if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
              # For weighted median, use repeated values approach
              rep_v <- rep(v, times = pmax(1, round(w)))  # Ensure minimum weight of 1
              median(rep_v, na.rm = TRUE)
            } else {
              median(as.numeric(get(to_stat_col)), na.rm = TRUE)
            }
          }
          
          result <- list(from_value, to_value)
          names(result) <- c(from_col_name, to_col_name)
          result
        }, by = .(from, to)]
        
      } else {
        # For character/factor variables: compute mode
        from_col_name <- paste0(stat_var, "_from_mode")
        to_col_name <- paste0(stat_var, "_to_mode")
        
        stat_agg <- transitions_data[, {
          if (use_collapse) {
            from_value <- collapse::fmode(get(from_stat_col))
            to_value <- collapse::fmode(get(to_stat_col))
          } else {
            # Base R mode calculation (most frequent value)
            from_value <- {
              tbl <- table(get(from_stat_col), useNA = "no")
              if (length(tbl) > 0) names(tbl)[which.max(tbl)] else NA_character_
            }
            to_value <- {
              tbl <- table(get(to_stat_col), useNA = "no")
              if (length(tbl) > 0) names(tbl)[which.max(tbl)] else NA_character_
            }
          }
          
          result <- list(from_value, to_value)
          names(result) <- c(from_col_name, to_col_name)
          result
        }, by = .(from, to)]
      }
      
      # Merge with base aggregation
      base_agg <- merge(base_agg, stat_agg, by = c("from", "to"), all.x = TRUE)
    }
  }
  
  # Order by weight (most common transitions first)
  setorder(base_agg, -weight)
  
  # Show completion message
  if (show_progress) {
    end_time <- Sys.time()
    elapsed_time <- difftime(end_time, start_time, units = "secs")
    
    total_transitions <- sum(base_agg$weight, na.rm = TRUE)
    
    message(sprintf("Transition analysis completed in %.2f seconds. Found %d unique transition patterns with %d total transitions.", 
                   as.numeric(elapsed_time), nrow(base_agg), total_transitions))
  }
  
  # Return transition matrix if requested
  if (output_transition_matrix) {
    # Get unique from/to values to create matrix dimensions
    unique_states <- sort(unique(c(base_agg$from, base_agg$to)))
    
    # Create empty matrix with zeros
    transition_matrix <- matrix(0, 
                               nrow = length(unique_states), 
                               ncol = length(unique_states),
                               dimnames = list(unique_states, unique_states))
    
    # Fill matrix with transition weights
    for (i in 1:nrow(base_agg)) {
      from_state <- as.character(base_agg$from[i])
      to_state <- as.character(base_agg$to[i])
      weight <- base_agg$weight[i]
      
      transition_matrix[from_state, to_state] <- weight
    }
    
    return(transition_matrix)
  }
  
  return(base_agg)
}