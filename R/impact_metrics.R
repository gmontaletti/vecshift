#' Impact Evaluation: Pre/Post Event Metrics
#'
#' This module provides comprehensive metric calculation functionality for impact evaluation
#' studies. It calculates employment stability, contract quality, career complexity, and
#' transition pattern metrics for both pre- and post-event periods.
#'
#' @name impact_metrics
#' @author vecshift package
NULL

#' Calculate Employment Stability Metrics
#'
#' Calculates employment stability metrics for pre- and post-event periods including
#' employment rates, spell durations, and job turnover measures.
#'
#' @param data A data.table containing employment records with event identification
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param period_column Character. Column indicating pre/post event period. Default: "event_period"
#' @param date_column Character. Name of date column. Default: "inizio"
#' @param employment_indicator Character. Column indicating employment status. Default: "over_id"
#' @param min_spell_duration Numeric. Minimum duration (days) to count as employment spell. Default: 7
#'
#' @return A data.table with employment stability metrics:
#'   \item{cf}{Person identifier}
#'   \item{period}{Pre or post event period}
#'   \item{days_employed}{Total days employed in period}
#'   \item{days_unemployed}{Total days unemployed in period}
#'   \item{employment_rate}{Proportion of period employed}
#'   \item{employment_spells}{Number of distinct employment periods}
#'   \item{unemployment_spells}{Number of distinct unemployment periods}
#'   \item{avg_employment_spell}{Average duration of employment spells}
#'   \item{avg_unemployment_spell}{Average duration of unemployment spells}
#'   \item{max_employment_spell}{Longest employment spell in period}
#'   \item{job_turnover_rate}{Employment spells per year in period}
#'   \item{employment_stability_index}{Composite stability measure (0-1)}
#'
#' @examples
#' \dontrun{
#' stability_metrics <- calculate_employment_stability_metrics(
#'   data = event_data,
#'   min_spell_duration = 14
#' )
#' }
#'
#' @export
calculate_employment_stability_metrics <- function(data,
                                                 id_column = "cf",
                                                 period_column = "event_period",
                                                 date_column = "inizio",
                                                 employment_indicator = "over_id",
                                                 min_spell_duration = 7) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, period_column, date_column, employment_indicator, "durata")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy with standardized column names
  dt <- copy(data)
  setnames(dt, c(id_column, period_column, date_column), c("cf", "period", "date"))
  
  # Create employment indicator
  dt[, employed := fifelse(get(employment_indicator) > 0, TRUE, FALSE)]
  
  # Filter for valid periods and minimum durations
  dt <- dt[!is.na(period) & durata >= min_spell_duration]
  
  if (nrow(dt) == 0) {
    warning("No valid observations after filtering")
    return(data.table())
  }
  
  # Calculate basic employment metrics by person and period
  basic_metrics <- dt[, .(
    days_employed = sum(durata[employed == TRUE], na.rm = TRUE),
    days_unemployed = sum(durata[employed == FALSE], na.rm = TRUE),
    total_days = sum(durata, na.rm = TRUE),
    total_observations = .N
  ), by = .(cf, period)]
  
  # Calculate employment rate
  basic_metrics[, employment_rate := days_employed / (days_employed + days_unemployed)]
  basic_metrics[is.nan(employment_rate), employment_rate := 0]
  
  # Calculate spell-based metrics
  spell_metrics <- dt[order(cf, period, date), {
    # Identify spell changes
    spell_change <- c(TRUE, diff(employed) != 0)
    spell_id <- cumsum(spell_change)
    
    # Calculate spell durations
    spells <- .SD[, .(
      employed = first(employed),
      spell_duration = sum(durata),
      spell_start = min(date),
      spell_end = max(date)
    ), by = spell_id]
    
    # Employment spells
    emp_spells <- spells[employed == TRUE]
    unemp_spells <- spells[employed == FALSE]
    
    list(
      employment_spells = nrow(emp_spells),
      unemployment_spells = nrow(unemp_spells),
      avg_employment_spell = ifelse(nrow(emp_spells) > 0, mean(emp_spells$spell_duration), 0),
      avg_unemployment_spell = ifelse(nrow(unemp_spells) > 0, mean(unemp_spells$spell_duration), 0),
      max_employment_spell = ifelse(nrow(emp_spells) > 0, max(emp_spells$spell_duration), 0),
      max_unemployment_spell = ifelse(nrow(unemp_spells) > 0, max(unemp_spells$spell_duration), 0)
    )
  }, by = .(cf, period)]
  
  # Merge basic and spell metrics
  stability_metrics <- merge(basic_metrics, spell_metrics, by = c("cf", "period"))
  
  # Calculate derived metrics
  stability_metrics[, `:=`(
    job_turnover_rate = employment_spells / pmax(total_days / 365.25, 1/365.25), # Annualized rate
    employment_stability_index = pmin(1, (
      0.4 * employment_rate +
      0.3 * pmin(1, max_employment_spell / 365) +
      0.2 * pmax(0, 1 - pmin(1, employment_spells / 4)) +
      0.1 * pmin(1, avg_employment_spell / 90)
    ))
  )]
  
  return(stability_metrics[])
}

#' Calculate Contract Quality Metrics
#'
#' Calculates contract quality metrics including temporary to permanent transitions,
#' contract type distributions, and quality improvements over time.
#'
#' @param data A data.table containing employment records with event identification
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param period_column Character. Column indicating pre/post event period. Default: "event_period"
#' @param contract_type_column Character. Column indicating contract type. Default: "prior"
#' @param permanent_values Numeric vector. Values indicating permanent contracts. Default: c(1, 2, 3)
#' @param temporary_values Numeric vector. Values indicating temporary contracts. Default: c(0, -1)
#'
#' @return A data.table with contract quality metrics:
#'   \item{cf}{Person identifier}
#'   \item{period}{Pre or post event period}
#'   \item{permanent_contract_days}{Days in permanent contracts}
#'   \item{temporary_contract_days}{Days in temporary contracts}
#'   \item{permanent_contract_rate}{Proportion of employment in permanent contracts}
#'   \item{temp_to_perm_transitions}{Number of temporary to permanent transitions}
#'   \item{perm_to_temp_transitions}{Number of permanent to temporary transitions}
#'   \item{contract_stability_trend}{Trend in contract stability over time}
#'   \item{average_contract_quality}{Average contract quality score}
#'   \item{contract_improvement_rate}{Rate of contract quality improvement}
#'
#' @examples
#' \dontrun{
#' quality_metrics <- calculate_contract_quality_metrics(
#'   data = event_data,
#'   permanent_values = c(1, 2, 3),
#'   temporary_values = c(0, -1)
#' )
#' }
#'
#' @export
calculate_contract_quality_metrics <- function(data,
                                             id_column = "cf",
                                             period_column = "event_period",
                                             contract_type_column = "prior",
                                             permanent_values = c(1, 2, 3),
                                             temporary_values = c(0, -1)) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, period_column, contract_type_column, "durata", "over_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy
  dt <- copy(data)
  setnames(dt, c(id_column, period_column, contract_type_column), 
           c("cf", "period", "contract_type"))
  
  # Filter for employment periods only
  dt <- dt[over_id > 0 & !is.na(period)]
  
  if (nrow(dt) == 0) {
    warning("No employment observations found")
    return(data.table())
  }
  
  # Classify contract types
  dt[, `:=`(
    is_permanent = contract_type %in% permanent_values,
    is_temporary = contract_type %in% temporary_values,
    contract_quality_score = fcase(
      contract_type %in% permanent_values, 1.0,
      contract_type %in% temporary_values, 0.0,
      default = 0.5
    )
  )]
  
  # Calculate basic quality metrics
  quality_metrics <- dt[, .(
    permanent_contract_days = sum(durata[is_permanent == TRUE], na.rm = TRUE),
    temporary_contract_days = sum(durata[is_temporary == TRUE], na.rm = TRUE),
    total_employment_days = sum(durata, na.rm = TRUE),
    average_contract_quality = mean(contract_quality_score, na.rm = TRUE),
    contract_observations = .N
  ), by = .(cf, period)]
  
  # Calculate rates
  quality_metrics[, permanent_contract_rate := permanent_contract_days / 
                   (permanent_contract_days + temporary_contract_days)]
  quality_metrics[is.nan(permanent_contract_rate), permanent_contract_rate := 0]
  
  # Calculate transition metrics
  transition_metrics <- dt[order(cf, period, inizio), {
    if (.N <= 1) {
      list(
        temp_to_perm_transitions = 0,
        perm_to_temp_transitions = 0,
        contract_stability_trend = 0
      )
    } else {
      # Calculate transitions
      transitions <- diff(contract_quality_score)
      temp_to_perm <- sum(transitions > 0.3, na.rm = TRUE)  # Significant improvement
      perm_to_temp <- sum(transitions < -0.3, na.rm = TRUE) # Significant deterioration
      
      # Calculate trend using linear regression
      if (.N >= 3) {
        time_seq <- seq_len(.N)
        trend_coef <- coef(lm(contract_quality_score ~ time_seq))[2]
        trend <- ifelse(is.na(trend_coef), 0, trend_coef)
      } else {
        trend <- 0
      }
      
      list(
        temp_to_perm_transitions = temp_to_perm,
        perm_to_temp_transitions = perm_to_temp,
        contract_stability_trend = trend
      )
    }
  }, by = .(cf, period)]
  
  # Merge metrics
  contract_metrics <- merge(quality_metrics, transition_metrics, by = c("cf", "period"))
  
  # Calculate improvement rate
  contract_metrics[, contract_improvement_rate := 
    pmax(0, temp_to_perm_transitions) / pmax(1, contract_observations / 4)]
  
  return(contract_metrics[])
}

#' Calculate Career Complexity Metrics
#'
#' Calculates career complexity metrics including concurrent employment patterns,
#' employment diversity measures, and complexity indices.
#'
#' @param data A data.table containing employment records with event identification
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param period_column Character. Column indicating pre/post event period. Default: "event_period"
#' @param complexity_variables Character vector. Variables to use for complexity calculation.
#'   Default: c("over_id", "arco", "prior")
#'
#' @return A data.table with career complexity metrics:
#'   \item{cf}{Person identifier}
#'   \item{period}{Pre or post event period}
#'   \item{max_concurrent_jobs}{Maximum number of concurrent jobs}
#'   \item{avg_concurrent_jobs}{Average number of concurrent jobs}
#'   \item{concurrent_employment_days}{Days with multiple concurrent jobs}
#'   \item{concurrent_employment_rate}{Proportion of employment with multiple jobs}
#'   \item{employment_diversity_index}{Shannon diversity index of employment types}
#'   \item{job_complexity_score}{Overall job complexity score}
#'   \item{career_fragmentation_index}{Measure of career fragmentation}
#'
#' @examples
#' \dontrun{
#' complexity_metrics <- calculate_career_complexity_metrics(
#'   data = event_data,
#'   complexity_variables = c("over_id", "arco", "sector", "contract_type")
#' )
#' }
#'
#' @export
calculate_career_complexity_metrics <- function(data,
                                              id_column = "cf",
                                              period_column = "event_period",
                                              complexity_variables = c("over_id", "arco", "prior")) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, period_column, "durata")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for complexity variables
  available_vars <- intersect(complexity_variables, names(data))
  if (length(available_vars) == 0) {
    stop("None of the specified complexity variables found in data")
  }
  
  # Create working copy
  dt <- copy(data)
  setnames(dt, c(id_column, period_column), c("cf", "period"))
  
  # Filter for valid periods
  dt <- dt[!is.na(period)]
  
  if (nrow(dt) == 0) {
    warning("No valid observations after filtering")
    return(data.table())
  }
  
  # Calculate concurrent job metrics (if arco available)
  concurrent_metrics <- if ("arco" %in% available_vars) {
    dt[over_id > 0, .(
      max_concurrent_jobs = max(arco, na.rm = TRUE),
      avg_concurrent_jobs = mean(arco, na.rm = TRUE),
      concurrent_employment_days = sum(durata[arco > 1], na.rm = TRUE),
      total_employment_days = sum(durata, na.rm = TRUE)
    ), by = .(cf, period)]
  } else {
    # Default values if arco not available
    dt[, .(
      max_concurrent_jobs = 1,
      avg_concurrent_jobs = 1,
      concurrent_employment_days = 0,
      total_employment_days = sum(durata[over_id > 0], na.rm = TRUE)
    ), by = .(cf, period)]
  }
  
  concurrent_metrics[, concurrent_employment_rate := 
    concurrent_employment_days / pmax(1, total_employment_days)]
  
  # Calculate diversity metrics
  diversity_metrics <- dt[, {
    diversity_components <- list()
    
    # Employment type diversity (if prior available)
    if ("prior" %in% available_vars && over_id > 0) {
      emp_types <- get("prior")[over_id > 0]
      if (length(emp_types) > 0) {
        type_props <- table(emp_types) / length(emp_types)
        employment_diversity <- -sum(type_props * log(type_props + 1e-10))
        diversity_components$employment_diversity_index <- employment_diversity
      }
    }
    
    if (length(diversity_components) == 0) {
      diversity_components$employment_diversity_index <- 0
    }
    
    diversity_components
  }, by = .(cf, period)]
  
  # Calculate fragmentation index
  fragmentation_metrics <- dt[order(cf, period, inizio), {
    if (.N <= 1) {
      list(career_fragmentation_index = 0)
    } else {
      # Count employment/unemployment transitions
      employment_status <- over_id > 0
      transitions <- sum(diff(as.integer(employment_status)) != 0, na.rm = TRUE)
      
      # Normalize by period length (in years)
      period_length_years <- sum(durata, na.rm = TRUE) / 365.25
      fragmentation_rate <- transitions / pmax(1, period_length_years)
      
      list(career_fragmentation_index = pmin(1, fragmentation_rate / 4)) # Scale to 0-1
    }
  }, by = .(cf, period)]
  
  # Merge all metrics
  complexity_metrics <- Reduce(function(x, y) merge(x, y, by = c("cf", "period"), all = TRUE),
                               list(concurrent_metrics, diversity_metrics, fragmentation_metrics))
  
  # Calculate overall complexity score
  complexity_metrics[, job_complexity_score := pmin(1, (
    0.3 * pmin(1, max_concurrent_jobs / 3) +
    0.3 * pmin(1, concurrent_employment_rate) +
    0.2 * pmin(1, employment_diversity_index / 2) +
    0.2 * career_fragmentation_index
  ))]
  
  return(complexity_metrics[])
}

#' Calculate Transition Pattern Metrics
#'
#' Calculates transition pattern metrics including time to next job, contract type changes,
#' and transition frequency measures.
#'
#' @param data A data.table containing employment records with event identification
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param period_column Character. Column indicating pre/post event period. Default: "event_period"
#' @param date_column Character. Name of date column. Default: "inizio"
#'
#' @return A data.table with transition pattern metrics:
#'   \item{cf}{Person identifier}
#'   \item{period}{Pre or post event period}
#'   \item{avg_time_to_next_job}{Average days from job end to next job start}
#'   \item{median_time_to_next_job}{Median days from job end to next job start}
#'   \item{job_search_success_rate}{Proportion of unemployment spells ending in employment}
#'   \item{contract_type_changes}{Number of contract type changes}
#'   \item{upward_transitions}{Number of transitions to better contract types}
#'   \item{downward_transitions}{Number of transitions to worse contract types}
#'   \item{transition_frequency}{Number of job transitions per year}
#'   \item{employment_continuity_index}{Measure of employment continuity}
#'
#' @examples
#' \dontrun{
#' transition_metrics <- calculate_transition_pattern_metrics(
#'   data = event_data
#' )
#' }
#'
#' @export
calculate_transition_pattern_metrics <- function(data,
                                               id_column = "cf",
                                               period_column = "event_period",
                                               date_column = "inizio") {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(id_column, period_column, date_column, "durata", "over_id", "fine")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy
  dt <- copy(data)
  setnames(dt, c(id_column, period_column, date_column), c("cf", "period", "start_date"))
  
  # Ensure dates are proper format
  dt[, `:=`(
    start_date = as.Date(start_date),
    end_date = as.Date(fine)
  )]
  
  # Filter for valid periods
  dt <- dt[!is.na(period)]
  
  if (nrow(dt) == 0) {
    warning("No valid observations after filtering")
    return(data.table())
  }
  
  # Calculate transition metrics by person and period
  transition_metrics <- dt[order(cf, period, start_date), {
    if (.N <= 1) {
      list(
        avg_time_to_next_job = NA_real_,
        median_time_to_next_job = NA_real_,
        job_search_success_rate = NA_real_,
        contract_type_changes = 0,
        upward_transitions = 0,
        downward_transitions = 0,
        transition_frequency = 0,
        employment_continuity_index = 1
      )
    } else {
      # Employment status
      employed <- over_id > 0
      
      # Calculate gaps between employment periods
      emp_periods <- .SD[employed == TRUE]
      if (nrow(emp_periods) >= 2) {
        # Time between job endings and next job starts
        job_gaps <- emp_periods[2:.N, start_date] - emp_periods[1:(.N-1), end_date]
        job_gaps <- as.numeric(job_gaps[job_gaps > 0]) # Only positive gaps
        
        avg_gap <- if (length(job_gaps) > 0) mean(job_gaps, na.rm = TRUE) else NA_real_
        median_gap <- if (length(job_gaps) > 0) median(job_gaps, na.rm = TRUE) else NA_real_
      } else {
        avg_gap <- NA_real_
        median_gap <- NA_real_
      }
      
      # Job search success rate
      unemp_spells <- sum(!employed)
      successful_searches <- if (unemp_spells > 0) {
        # Count unemployment periods followed by employment
        unemp_to_emp <- sum(diff(as.integer(employed)) > 0, na.rm = TRUE)
        unemp_to_emp / unemp_spells
      } else {
        NA_real_
      }
      
      # Contract type changes (if prior column exists)
      contract_changes <- if ("prior" %in% names(.SD)) {
        sum(diff(prior) != 0, na.rm = TRUE)
      } else {
        0
      }
      
      # Transition quality (simplified - could be enhanced with actual contract rankings)
      upward <- downward <- 0
      if ("prior" %in% names(.SD) && length(unique(prior)) > 1) {
        quality_changes <- diff(prior)
        upward <- sum(quality_changes > 0, na.rm = TRUE)
        downward <- sum(quality_changes < 0, na.rm = TRUE)
      }
      
      # Overall transition frequency
      employment_transitions <- sum(diff(as.integer(employed)) != 0, na.rm = TRUE)
      period_years <- sum(durata, na.rm = TRUE) / 365.25
      transition_freq <- employment_transitions / pmax(1, period_years)
      
      # Employment continuity index
      total_employed_days <- sum(durata[employed], na.rm = TRUE)
      total_days <- sum(durata, na.rm = TRUE)
      continuity <- total_employed_days / pmax(1, total_days)
      
      list(
        avg_time_to_next_job = avg_gap,
        median_time_to_next_job = median_gap,
        job_search_success_rate = successful_searches,
        contract_type_changes = contract_changes,
        upward_transitions = upward,
        downward_transitions = downward,
        transition_frequency = transition_freq,
        employment_continuity_index = continuity
      )
    }
  }, by = .(cf, period)]
  
  return(transition_metrics[])
}

#' Comprehensive Impact Metrics Calculation
#'
#' Calculates all impact evaluation metrics (stability, quality, complexity, transitions)
#' in a single function call with consistent formatting and validation.
#'
#' @param data A data.table containing employment records with event identification
#' @param metrics Character vector. Metrics to calculate. Options: 
#'   c("stability", "quality", "complexity", "transitions", "all"). Default: "all"
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param period_column Character. Column indicating pre/post event period. Default: "event_period"
#' @param output_format Character. Output format: "wide", "long", or "list". Default: "wide"
#'
#' @return Based on output_format:
#'   \item{wide}{Single data.table with all metrics as columns}
#'   \item{long}{Long-format data.table with metric_name and metric_value columns}
#'   \item{list}{Named list with separate data.tables for each metric type}
#'
#' @examples
#' \dontrun{
#' # Calculate all metrics
#' all_metrics <- calculate_comprehensive_impact_metrics(
#'   data = event_data,
#'   metrics = "all",
#'   output_format = "wide"
#' )
#' 
#' # Calculate specific metrics
#' stability_quality <- calculate_comprehensive_impact_metrics(
#'   data = event_data,
#'   metrics = c("stability", "quality"),
#'   output_format = "list"
#' )
#' }
#'
#' @export
calculate_comprehensive_impact_metrics <- function(data,
                                                 metrics = "all",
                                                 id_column = "cf",
                                                 period_column = "event_period",
                                                 output_format = "wide") {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  if (!"all" %in% metrics) {
    valid_metrics <- c("stability", "quality", "complexity", "transitions")
    invalid_metrics <- setdiff(metrics, valid_metrics)
    if (length(invalid_metrics) > 0) {
      stop(paste("Invalid metrics specified:", paste(invalid_metrics, collapse = ", ")))
    }
  } else {
    metrics <- c("stability", "quality", "complexity", "transitions")
  }
  
  if (!output_format %in% c("wide", "long", "list")) {
    stop("output_format must be one of: 'wide', 'long', 'list'")
  }
  
  # Calculate requested metrics
  metric_results <- list()
  
  if ("stability" %in% metrics) {
    metric_results$stability <- calculate_employment_stability_metrics(
      data, id_column, period_column
    )
  }
  
  if ("quality" %in% metrics) {
    metric_results$quality <- calculate_contract_quality_metrics(
      data, id_column, period_column
    )
  }
  
  if ("complexity" %in% metrics) {
    metric_results$complexity <- calculate_career_complexity_metrics(
      data, id_column, period_column
    )
  }
  
  if ("transitions" %in% metrics) {
    metric_results$transitions <- calculate_transition_pattern_metrics(
      data, id_column, period_column
    )
  }
  
  # Return based on output format
  if (output_format == "list") {
    return(metric_results)
  }
  
  # Merge all metrics for wide or long format
  if (length(metric_results) == 0) {
    warning("No metrics calculated")
    return(data.table())
  }
  
  # Merge all metric tables
  merged_metrics <- Reduce(function(x, y) {
    merge(x, y, by = c(id_column, period_column), all = TRUE)
  }, metric_results)
  
  if (output_format == "wide") {
    return(merged_metrics)
  }
  
  # Convert to long format
  id_vars <- c(id_column, period_column)
  measure_vars <- setdiff(names(merged_metrics), id_vars)
  
  long_metrics <- melt(merged_metrics, id.vars = id_vars, measure.vars = measure_vars,
                       variable.name = "metric_name", value.name = "metric_value")
  
  # Add metric category
  long_metrics[, metric_category := fcase(
    grepl("employment|spell|turnover|stability", metric_name), "stability",
    grepl("contract|permanent|temporary|quality", metric_name), "quality", 
    grepl("concurrent|diversity|complexity|fragmentation", metric_name), "complexity",
    grepl("transition|continuity|search|time", metric_name), "transitions",
    default = "other"
  )]
  
  return(long_metrics[])
}