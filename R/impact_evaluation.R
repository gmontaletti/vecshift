#' Impact Evaluation: Event Identification and Treatment Group Definition
#'
#' This module provides comprehensive event identification functionality for impact evaluation
#' studies using employment data. It supports flexible treatment conditions, multiple events
#' per person, and robust event timing identification.
#'
#' @name impact_evaluation
#' @author vecshift package
NULL

#' Identify Treatment Events in Employment Data
#'
#' Identifies treatment events in employment data based on flexible conditions.
#' Supports single or multiple conditions, temporal constraints, and handles
#' multiple events per person.
#'
#' @param data A data.table containing employment records (output from vecshift)
#' @param treatment_conditions List of conditions defining treatment events.
#'   Each condition can be:
#'   - A string expression (e.g., "COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'")
#'   - A named list with 'column', 'operator', and 'value'
#'   - A function that takes the data and returns a logical vector
#' @param event_window Numeric vector of length 2 defining the event window
#'   relative to the treatment date. Default: c(-365, 365) (1 year before/after)
#' @param min_pre_period Minimum number of days required before the event.
#'   Default: 90
#' @param min_post_period Minimum number of days required after the event.
#'   Default: 90
#' @param multiple_events How to handle multiple events per person:
#'   - "first": Use only the first event
#'   - "last": Use only the last event
#'   - "all": Include all events (creates multiple observations per person)
#'   Default: "first"
#' @param require_employment_before Logical. Require employment before the event?
#'   Default: TRUE
#' @param id_column Character. Name of the person identifier column. Default: "cf"
#' @param date_column Character. Name of the date column to use for event timing.
#'   Default: "inizio"
#'
#' @return A data.table with treatment event identification including:
#'   \item{cf}{Person identifier}
#'   \item{event_date}{Date of the treatment event}
#'   \item{event_id}{Unique event identifier}
#'   \item{is_treated}{Logical indicator for treatment status}
#'   \item{days_to_event}{Days from observation date to event (negative = before)}
#'   \item{in_event_window}{Logical indicator for observations within event window}
#'   \item{pre_event_period}{Logical indicator for pre-event observations}
#'   \item{post_event_period}{Logical indicator for post-event observations}
#'   \item{event_sequence}{Sequence number if multiple events per person}
#'   \item{treatment_condition_met}{Description of which condition triggered treatment}
#'
#' @examples
#' \dontrun{
#' # Identify permanent contract events
#' events <- identify_treatment_events(
#'   data = employment_data,
#'   treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
#'   event_window = c(-180, 365),
#'   multiple_events = "first"
#' )
#'
#' # Multiple conditions example
#' events <- identify_treatment_events(
#'   data = employment_data,
#'   treatment_conditions = list(
#'     list(column = "COD_TIPOLOGIA_CONTRATTUALE", operator = "==", value = "C.01.00"),
#'     list(column = "durata", operator = ">", value = 365)
#'   ),
#'   event_window = c(-365, 730)
#' )
#' }
#'
#' @export
identify_treatment_events <- function(data,
                                    treatment_conditions,
                                    event_window = c(-365, 365),
                                    min_pre_period = 90,
                                    min_post_period = 90,
                                    multiple_events = "first",
                                    require_employment_before = TRUE,
                                    id_column = "cf",
                                    date_column = "inizio") {
  
  # Input validation
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  if (!id_column %in% names(data)) {
    stop(paste("ID column", id_column, "not found in data"))
  }
  
  if (!date_column %in% names(data)) {
    stop(paste("Date column", date_column, "not found in data"))
  }
  
  if (length(event_window) != 2 || event_window[2] <= event_window[1]) {
    stop("event_window must be a numeric vector of length 2 with window[2] > window[1]")
  }
  
  if (!multiple_events %in% c("first", "last", "all")) {
    stop("multiple_events must be one of: 'first', 'last', 'all'")
  }
  
  # Ensure date column is properly formatted
  if (!inherits(data[[date_column]], "Date")) {
    data[[date_column]] <- as.Date(data[[date_column]])
  }
  
  # Create working copy
  dt <- copy(data)
  setnames(dt, c(id_column, date_column), c("cf", "event_date"))
  
  # Initialize treatment identification columns
  dt[, `:=`(
    is_treated = FALSE,
    event_id = NA_integer_,
    treatment_condition_met = NA_character_,
    event_sequence = 0L
  )]
  
  # Process each treatment condition
  for (i in seq_along(treatment_conditions)) {
    condition <- treatment_conditions[[i]]
    
    # Apply condition based on type
    if (is.character(condition)) {
      # String expression
      condition_met <- dt[, eval(parse(text = condition))]
      condition_desc <- condition
    } else if (is.list(condition) && all(c("column", "operator", "value") %in% names(condition))) {
      # Structured condition
      col_name <- condition$column
      operator <- condition$operator
      value <- condition$value
      
      if (!col_name %in% names(dt)) {
        warning(paste("Column", col_name, "not found in data, skipping condition"))
        next
      }
      
      condition_expr <- paste0(col_name, " ", operator, " ", 
                              if (is.character(value)) paste0("'", value, "'") else value)
      condition_met <- dt[, eval(parse(text = condition_expr))]
      condition_desc <- condition_expr
    } else if (is.function(condition)) {
      # Function condition
      condition_met <- condition(dt)
      condition_desc <- paste("Custom function", i)
    } else {
      stop(paste("Invalid treatment condition format at position", i))
    }
    
    # Mark treated observations
    dt[condition_met == TRUE, `:=`(
      is_treated = TRUE,
      treatment_condition_met = fifelse(
        is.na(treatment_condition_met),
        condition_desc,
        paste(treatment_condition_met, "OR", condition_desc)
      )
    )]
  }
  
  if (sum(dt$is_treated) == 0) {
    warning("No treatment events identified with the given conditions")
    return(dt[, .(cf = character(0), event_date = as.Date(character(0)))])
  }
  
  # Identify unique treatment events per person
  treatment_events <- dt[is_treated == TRUE, .(
    cf,
    event_date,
    treatment_condition_met
  )]
  
  # Handle multiple events per person
  if (multiple_events == "first") {
    treatment_events <- treatment_events[, .SD[which.min(event_date)], by = cf]
  } else if (multiple_events == "last") {
    treatment_events <- treatment_events[, .SD[which.max(event_date)], by = cf]
  }
  
  # Add event sequence numbers
  treatment_events[, event_sequence := seq_len(.N), by = cf]
  treatment_events[, event_id := .I]
  
  # Check minimum observation periods
  person_data_ranges <- dt[, .(
    first_obs = min(event_date, na.rm = TRUE),
    last_obs = max(event_date, na.rm = TRUE)
  ), by = cf]
  
  treatment_events <- merge(treatment_events, person_data_ranges, by = "cf")
  
  # Filter events with sufficient pre/post periods
  treatment_events <- treatment_events[
    event_date - first_obs >= min_pre_period &
    last_obs - event_date >= min_post_period
  ]
  
  if (nrow(treatment_events) == 0) {
    warning("No treatment events meet the minimum pre/post period requirements")
    return(dt[, .(cf = character(0), event_date = as.Date(character(0)))])
  }
  
  # Optional: require employment before event
  if (require_employment_before) {
    employed_before <- dt[, .(
      employed_before_event = any(over_id > 0, na.rm = TRUE)
    ), by = cf]
    
    treatment_events <- merge(treatment_events, employed_before, by = "cf")
    treatment_events <- treatment_events[employed_before_event == TRUE]
    treatment_events[, employed_before_event := NULL]
  }
  
  if (nrow(treatment_events) == 0) {
    warning("No treatment events meet the employment history requirements")
    return(dt[, .(cf = character(0), event_date = as.Date(character(0)))])
  }
  
  # Create final event identification data
  event_data <- merge(dt, treatment_events[, .(cf, event_date, event_id, treatment_condition_met, event_sequence)], 
                     by = "cf", all = TRUE)
  
  # Calculate event timing variables
  event_data[!is.na(event_id), `:=`(
    days_to_event = as.numeric(get(date_column) - event_date),
    in_event_window = (as.numeric(get(date_column) - event_date) >= event_window[1] & 
                      as.numeric(get(date_column) - event_date) <= event_window[2]),
    pre_event_period = (as.numeric(get(date_column) - event_date) < 0 & 
                       as.numeric(get(date_column) - event_date) >= event_window[1]),
    post_event_period = (as.numeric(get(date_column) - event_date) > 0 & 
                        as.numeric(get(date_column) - event_date) <= event_window[2]),
    is_treated = TRUE
  )]
  
  # Fill missing values for non-treated
  event_data[is.na(is_treated), is_treated := FALSE]
  event_data[is.na(in_event_window), in_event_window := FALSE]
  event_data[is.na(pre_event_period), pre_event_period := FALSE]
  event_data[is.na(post_event_period), post_event_period := FALSE]
  event_data[is.na(event_sequence), event_sequence := 0L]
  
  # Restore original column names if needed
  setnames(event_data, c("cf", "event_date"), c(id_column, date_column))
  
  return(event_data[])
}

#' Create Treatment and Control Groups
#'
#' Creates matched treatment and control groups for impact evaluation based on
#' event identification results.
#'
#' @param event_data Data.table from identify_treatment_events()
#' @param control_conditions Optional conditions for control group eligibility.
#'   Similar format to treatment_conditions in identify_treatment_events()
#' @param matching_variables Character vector of variables to use for matching.
#'   Default: NULL (no matching, uses all eligible controls)
#' @param exact_match_variables Character vector of variables requiring exact matches.
#'   Default: NULL
#' @param control_ratio Numeric. Ratio of control to treatment units. Default: 1
#' @param caliper Numeric. Maximum distance for propensity score matching. Default: 0.1
#' @param exclude_future_treated Logical. Exclude future-treated units from control group?
#'   Default: TRUE
#' @param replacement Logical. Sample control units with replacement? Default: FALSE
#'
#' @return A data.table with treatment and control group assignments including:
#'   \item{group_assignment}{Factor: "treatment" or "control"}
#'   \item{match_id}{Matching pair identifier}
#'   \item{propensity_score}{Estimated propensity score (if matching used)}
#'   \item{match_distance}{Distance to matched unit}
#'   \item{control_weight}{Weight for control observations}
#'
#' @examples
#' \dontrun{
#' # Create treatment and control groups
#' groups <- create_treatment_control_groups(
#'   event_data = identified_events,
#'   matching_variables = c("age", "sector", "region"),
#'   exact_match_variables = c("gender"),
#'   control_ratio = 2
#' )
#' }
#'
#' @export
create_treatment_control_groups <- function(event_data,
                                          control_conditions = NULL,
                                          matching_variables = NULL,
                                          exact_match_variables = NULL,
                                          control_ratio = 1,
                                          caliper = 0.1,
                                          exclude_future_treated = TRUE,
                                          replacement = FALSE) {
  
  if (!inherits(event_data, "data.table")) {
    stop("event_data must be a data.table")
  }
  
  if (!"is_treated" %in% names(event_data)) {
    stop("event_data must contain 'is_treated' column from identify_treatment_events()")
  }
  
  # Create working copy
  dt <- copy(event_data)
  
  # Initialize group assignment
  dt[, `:=`(
    group_assignment = factor(NA, levels = c("treatment", "control")),
    match_id = NA_integer_,
    propensity_score = NA_real_,
    match_distance = NA_real_,
    control_weight = 1.0
  )]
  
  # Assign treatment group
  dt[is_treated == TRUE & !is.na(event_id), group_assignment := "treatment"]
  
  # Identify potential control units
  potential_controls <- dt[is_treated == FALSE]
  
  # Apply control conditions if specified
  if (!is.null(control_conditions)) {
    for (condition in control_conditions) {
      if (is.character(condition)) {
        eligible <- potential_controls[, eval(parse(text = condition))]
      } else if (is.list(condition)) {
        col_name <- condition$column
        operator <- condition$operator
        value <- condition$value
        condition_expr <- paste0(col_name, " ", operator, " ", 
                                if (is.character(value)) paste0("'", value, "'") else value)
        eligible <- potential_controls[, eval(parse(text = condition_expr))]
      } else if (is.function(condition)) {
        eligible <- condition(potential_controls)
      }
      potential_controls <- potential_controls[eligible]
    }
  }
  
  # Exclude future treated if requested
  if (exclude_future_treated) {
    # Find units that become treated later
    future_treated_ids <- dt[is_treated == TRUE, unique(get(names(dt)[1]))]  # assuming first column is ID
    potential_controls <- potential_controls[!get(names(potential_controls)[1]) %in% future_treated_ids]
  }
  
  # Simple assignment if no matching
  if (is.null(matching_variables)) {
    n_treatment <- dt[group_assignment == "treatment", .N]
    n_controls_needed <- min(nrow(potential_controls), n_treatment * control_ratio)
    
    if (n_controls_needed > 0) {
      control_sample <- potential_controls[sample(.N, n_controls_needed)]
      
      # Update group assignments
      merge_cols <- intersect(names(dt), names(control_sample))
      dt[control_sample, on = merge_cols, group_assignment := "control"]
    }
  } else {
    # Implement basic matching logic here
    # This is simplified - full matching would use MatchIt in impact_matching.R
    warning("Matching with matching_variables requires the impact_matching.R module")
  }
  
  # Add match IDs for simple pairing
  treatment_units <- dt[group_assignment == "treatment"]
  control_units <- dt[group_assignment == "control"]
  
  if (nrow(treatment_units) > 0 && nrow(control_units) > 0) {
    n_matches <- min(nrow(treatment_units), floor(nrow(control_units) / control_ratio))
    
    if (n_matches > 0) {
      match_ids <- rep(1:n_matches, each = (1 + control_ratio))
      
      # Assign match IDs
      treatment_units[1:n_matches, match_id := 1:n_matches]
      control_units[1:(n_matches * control_ratio), 
                    match_id := rep(1:n_matches, each = control_ratio)]
      
      # Update main data
      dt[treatment_units, on = intersect(names(dt), names(treatment_units)), 
         match_id := i.match_id]
      dt[control_units, on = intersect(names(dt), names(control_units)), 
         match_id := i.match_id]
    }
  }
  
  return(dt[!is.na(group_assignment)])
}

#' Assess Treatment Event Quality
#'
#' Provides diagnostic information about treatment event identification including
#' timing distributions, balance assessment, and data quality metrics.
#'
#' @param event_data Data.table from identify_treatment_events()
#' @param assessment_variables Character vector of variables to include in balance assessment
#' @param output_format Character. Output format: "summary", "detailed", or "both"
#'
#' @return A list containing:
#'   \item{event_summary}{Summary statistics of identified events}
#'   \item{timing_distribution}{Distribution of event timing}
#'   \item{balance_table}{Balance assessment between treatment and control}
#'   \item{data_quality}{Data quality metrics}
#'   \item{recommendations}{Recommendations for improving identification}
#'
#' @examples
#' \dontrun{
#' assessment <- assess_treatment_event_quality(
#'   event_data = identified_events,
#'   assessment_variables = c("age", "sector", "prior_employment")
#' )
#' print(assessment$event_summary)
#' }
#'
#' @export
assess_treatment_event_quality <- function(event_data,
                                         assessment_variables = NULL,
                                         output_format = "summary") {
  
  if (!inherits(event_data, "data.table")) {
    stop("event_data must be a data.table")
  }
  
  # Event summary statistics
  event_summary <- list(
    total_observations = nrow(event_data),
    total_treated = sum(event_data$is_treated, na.rm = TRUE),
    total_controls = sum(!event_data$is_treated, na.rm = TRUE),
    unique_treated_persons = event_data[is_treated == TRUE, uniqueN(cf)],
    treatment_rate = mean(event_data$is_treated, na.rm = TRUE),
    events_per_person = event_data[is_treated == TRUE, .N, by = cf][, mean(N)],
    date_range = range(event_data$event_date, na.rm = TRUE)
  )
  
  # Timing distribution
  timing_dist <- NULL
  if ("days_to_event" %in% names(event_data)) {
    timing_dist <- event_data[!is.na(days_to_event), .(
      min_days = min(days_to_event),
      q25_days = quantile(days_to_event, 0.25),
      median_days = median(days_to_event),
      q75_days = quantile(days_to_event, 0.75),
      max_days = max(days_to_event),
      mean_days = mean(days_to_event),
      sd_days = sd(days_to_event)
    )]
  }
  
  # Balance assessment
  balance_table <- NULL
  if (!is.null(assessment_variables)) {
    available_vars <- intersect(assessment_variables, names(event_data))
    if (length(available_vars) > 0) {
      balance_list <- list()
      for (var in available_vars) {
        if (is.numeric(event_data[[var]])) {
          balance_list[[var]] <- event_data[, .(
            treated_mean = mean(get(var)[is_treated == TRUE], na.rm = TRUE),
            control_mean = mean(get(var)[is_treated == FALSE], na.rm = TRUE),
            treated_sd = sd(get(var)[is_treated == TRUE], na.rm = TRUE),
            control_sd = sd(get(var)[is_treated == FALSE], na.rm = TRUE)
          )]
          balance_list[[var]][, `:=`(
            variable = var,
            standardized_diff = (treated_mean - control_mean) / 
              sqrt((treated_sd^2 + control_sd^2) / 2)
          )]
        }
      }
      if (length(balance_list) > 0) {
        balance_table <- rbindlist(balance_list)
      }
    }
  }
  
  # Data quality metrics
  data_quality <- list(
    missing_event_dates = sum(is.na(event_data$event_date)),
    missing_treatment_indicators = sum(is.na(event_data$is_treated)),
    invalid_event_windows = sum(event_data$in_event_window & is.na(event_data$days_to_event), na.rm = TRUE),
    duplicate_events = event_data[is_treated == TRUE, .N - uniqueN(cf)],
    data_completeness = 1 - sum(is.na(event_data)) / (nrow(event_data) * ncol(event_data))
  )
  
  # Recommendations
  recommendations <- character()
  
  if (event_summary$treatment_rate < 0.01) {
    recommendations <- c(recommendations, 
                        "Very low treatment rate - consider relaxing treatment conditions")
  }
  
  if (event_summary$treatment_rate > 0.8) {
    recommendations <- c(recommendations, 
                        "Very high treatment rate - consider more specific treatment conditions")
  }
  
  if (!is.null(balance_table) && any(abs(balance_table$standardized_diff) > 0.25)) {
    recommendations <- c(recommendations, 
                        "Large standardized differences detected - consider matching or stratification")
  }
  
  if (data_quality$data_completeness < 0.9) {
    recommendations <- c(recommendations, 
                        "High missing data rate - consider data cleaning or imputation")
  }
  
  # Compile results
  result <- list(
    event_summary = event_summary,
    timing_distribution = timing_dist,
    balance_table = balance_table,
    data_quality = data_quality,
    recommendations = recommendations
  )
  
  # Set class for printing
  class(result) <- c("treatment_event_assessment", "list")
  
  return(result)
}

#' Print method for treatment event assessment
#' @param x A treatment_event_assessment object
#' @param ... Additional arguments passed to print
#' @method print treatment_event_assessment
#' @export
print.treatment_event_assessment <- function(x, ...) {
  cat("Treatment Event Assessment\n")
  cat("==========================\n\n")
  
  cat("Event Summary:\n")
  cat("  Total observations:", x$event_summary$total_observations, "\n")
  cat("  Treated units:", x$event_summary$total_treated, "\n")
  cat("  Control units:", x$event_summary$total_controls, "\n")
  cat("  Treatment rate:", sprintf("%.2f%%", x$event_summary$treatment_rate * 100), "\n")
  cat("  Unique treated persons:", x$event_summary$unique_treated_persons, "\n")
  
  if (!is.null(x$timing_distribution)) {
    cat("\nTiming Distribution (days to event):\n")
    cat("  Min:", x$timing_distribution$min_days, "\n")
    cat("  Median:", x$timing_distribution$median_days, "\n")
    cat("  Max:", x$timing_distribution$max_days, "\n")
  }
  
  if (!is.null(x$balance_table)) {
    cat("\nBalance Assessment:\n")
    print(x$balance_table[, .(variable, treated_mean, control_mean, standardized_diff)])
  }
  
  if (length(x$recommendations) > 0) {
    cat("\nRecommendations:\n")
    for (i in seq_along(x$recommendations)) {
      cat("  ", i, ". ", x$recommendations[i], "\n", sep = "")
    }
  }
  
  invisible(x)
}