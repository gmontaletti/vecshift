#' Employment Status Classification Module
#'
#' @description
#' Flexible employment status classification system for temporal employment segments.
#' Provides customizable rules for labeling unemployment, single employment,
#' and overlapping employment scenarios with support for business-specific logic.
#'
#' @name status_labeling
NULL

#' Default Employment Status Classification Rules
#'
#' @description
#' Returns the default classification rules used by vecshift for employment status.
#' These can be modified to implement custom business logic for different contexts.
#'
#' @return List with default classification rules
#' @export
#'
#' @examples
#' # View default rules
#' default_rules <- get_default_status_rules()
#' print(default_rules)
get_default_status_rules <- function() {
  list(
    unemployment = list(
      condition = "arco == 0",
      duration_threshold = 8,  # days
      short_label = "disoccupato",
      long_label = "disoccupato"  # Could differentiate short/long unemployment
    ),
    single_employment = list(
      full_time = list(
        condition = "arco == 1 & prior == 1",
        label = "occ_ft"
      ),
      part_time = list(
        condition = "arco == 1 & prior == 0", 
        label = "occ_pt"
      )
    ),
    overlapping_employment = list(
      pt_to_ft = list(
        condition = "arco > 1 & (prior > shift(prior, type = 'lag'))",
        label = "over_pt_ft"
      ),
      ft_to_pt = list(
        condition = "arco > 1 & (prior < shift(prior, type = 'lag'))",
        label = "over_ft_pt"
      ),
      pt_to_pt = list(
        condition = "arco > 1 & (prior == shift(prior, type = 'lag')) & prior == 0",
        label = "over_pt_pt"
      ),
      ft_to_ft = list(
        condition = "default",  # Catch-all for remaining overlaps
        label = "over_ft_ft"
      )
    )
  )
}

#' Apply Employment Status Classification
#'
#' @description
#' Classifies employment segments using customizable rules. Supports both
#' default vecshift classification and custom business logic implementations.
#' Optimized for high-performance processing with minimal memory allocations.
#'
#' @param segments Data.table with temporal segments containing arco, prior, durata columns
#' @param rules List of classification rules (default: get_default_status_rules())
#' @param group_by Character vector of columns to group by for shift operations (default: "cf")
#'
#' @return Data.table with stato column containing employment status labels
#'
#' @export
#' @importFrom data.table fcase shift setorder setorderv
#' 
#' @examples
#' \dontrun{
#' library(data.table)
#' # Sample segments data
#' segments <- data.table(
#'   cf = rep("A001", 4),
#'   inizio = 1:4,
#'   fine = 2:5,
#'   arco = c(0, 1, 2, 1),
#'   prior = c(0, 1, 1, 0),
#'   durata = rep(1, 4),
#'   id = c(0, 1, 2, 3)
#' )
#' 
#' # Apply default classification
#' classified <- classify_employment_status(segments)
#' print(classified$stato)
#' }
classify_employment_status <- function(segments, 
                                     rules = get_default_status_rules(),
                                     group_by = "cf") {
  
  # Handle NULL rules by using defaults
  if (is.null(rules)) {
    rules <- get_default_status_rules()
  }
  
  # Pre-extract rule labels for faster access (avoid repeated $ operations)
  unemp_thresh <- rules$unemployment$duration_threshold
  unemp_short <- rules$unemployment$short_label
  unemp_long <- rules$unemployment$long_label
  ft_label <- rules$single_employment$full_time$label
  pt_label <- rules$single_employment$part_time$label
  pt_to_ft_label <- rules$overlapping_employment$pt_to_ft$label
  ft_to_pt_label <- rules$overlapping_employment$ft_to_pt$label
  pt_to_pt_label <- rules$overlapping_employment$pt_to_pt$label
  ft_to_ft_label <- rules$overlapping_employment$ft_to_ft$label
  
  # Ensure proper ordering for shift operations
  if (length(group_by) > 0) {
    setorderv(segments, c(group_by, "inizio"))
  } else {
    setorder(segments, inizio)
  }
  
  # Pre-compute shift operations to avoid repeated calculations
  if (length(group_by) > 0) {
    segments[, prior_lag := shift(prior, type = "lag"), by = group_by]
  } else {
    segments[, prior_lag := shift(prior, type = "lag")]
  }
  
  # Apply optimized classification rules using fcase with pre-computed values
  segments[, stato := fcase(
    # Unemployment (fastest conditions first - most common case)
    arco == 0L & durata <= unemp_thresh, unemp_short,
    arco == 0L & durata > unemp_thresh, unemp_long,
    
    # Single employment (second most common)
    arco == 1L & prior == 1L, ft_label,
    arco == 1L & prior == 0L, pt_label,
    
    # Overlapping employment (least common, but most complex conditions)
    arco > 1L & prior > prior_lag, pt_to_ft_label,
    arco > 1L & prior < prior_lag, ft_to_pt_label,
    arco > 1L & prior == prior_lag & prior == 0L, pt_to_pt_label,
    
    # Default for remaining overlapping scenarios
    default = ft_to_ft_label
  )]
  
  # Clean up temporary column
  segments[, prior_lag := NULL]
  
  return(segments)
}

#' Create Custom Status Classification Rules
#'
#' @description
#' Helper function to create custom employment status classification rules
#' for specific business contexts or analytical requirements.
#'
#' @param unemployment_threshold Days threshold for short vs long unemployment
#' @param custom_labels Named list of custom status labels
#' @param include_intensity Logical. Include employment intensity classifications
#' @param include_transitions Logical. Include transition-based classifications
#'
#' @return List of custom classification rules
#'
#' @export
#'
#' @examples
#' # Create rules with custom unemployment threshold and labels
#' custom_rules <- create_custom_status_rules(
#'   unemployment_threshold = 30,  # 30 days instead of 8
#'   custom_labels = list(
#'     unemployed_short = "job_search",
#'     unemployed_long = "long_term_unemployed",
#'     full_time = "employed_ft",
#'     part_time = "employed_pt"
#'   )
#' )
create_custom_status_rules <- function(unemployment_threshold = 8,
                                     custom_labels = NULL,
                                     include_intensity = FALSE,
                                     include_transitions = FALSE) {
  
  # Base labels
  base_labels <- list(
    unemployed_short = "disoccupato",
    unemployed_long = "disoccupato",
    full_time = "occ_ft",
    part_time = "occ_pt",
    overlap_pt_ft = "over_pt_ft",
    overlap_ft_pt = "over_ft_pt", 
    overlap_pt_pt = "over_pt_pt",
    overlap_ft_ft = "over_ft_ft"
  )
  
  # Apply custom labels if provided
  if (!is.null(custom_labels)) {
    for (name in names(custom_labels)) {
      if (name %in% names(base_labels)) {
        base_labels[[name]] <- custom_labels[[name]]
      }
    }
  }
  
  rules <- list(
    unemployment = list(
      condition = "arco == 0",
      duration_threshold = unemployment_threshold,
      short_label = base_labels$unemployed_short,
      long_label = base_labels$unemployed_long
    ),
    single_employment = list(
      full_time = list(
        condition = "arco == 1 & prior == 1",
        label = base_labels$full_time
      ),
      part_time = list(
        condition = "arco == 1 & prior == 0",
        label = base_labels$part_time
      )
    ),
    overlapping_employment = list(
      pt_to_ft = list(
        condition = "arco > 1 & (prior > shift(prior, type = 'lag'))",
        label = base_labels$overlap_pt_ft
      ),
      ft_to_pt = list(
        condition = "arco > 1 & (prior < shift(prior, type = 'lag'))",
        label = base_labels$overlap_ft_pt
      ),
      pt_to_pt = list(
        condition = "arco > 1 & (prior == shift(prior, type = 'lag')) & prior == 0",
        label = base_labels$overlap_pt_pt
      ),
      ft_to_ft = list(
        condition = "default",
        label = base_labels$overlap_ft_ft
      )
    )
  )
  
  # Add intensity-based classifications
  if (include_intensity) {
    rules$intensity_thresholds <- list(
      high_intensity = list(condition = "arco >= 3", label_suffix = "_high"),
      medium_intensity = list(condition = "arco == 2", label_suffix = "_med"),
      low_intensity = list(condition = "arco == 1", label_suffix = "_low")
    )
  }
  
  # Add transition-based classifications
  if (include_transitions) {
    rules$transitions <- list(
      entry_employment = list(condition = "arco == 1 & shift(arco, type = 'lag') == 0"),
      exit_employment = list(condition = "arco == 0 & shift(arco, type = 'lag') >= 1"),
      increase_overlap = list(condition = "arco > shift(arco, type = 'lag') & shift(arco, type = 'lag') > 0"),
      decrease_overlap = list(condition = "arco < shift(arco, type = 'lag') & arco > 0")
    )
  }
  
  return(rules)
}

#' Analyze Employment Status Patterns
#'
#' @description
#' Analyzes patterns in employment status classifications to provide insights
#' into employment dynamics, stability, and transition frequencies.
#'
#' @param classified_data Data.table with employment segments and stato column
#' @param person_col Name of person identifier column (default: "cf")
#' @param include_transitions Logical. Include transition analysis
#'
#' @return List with pattern analysis results
#'
#' @export
analyze_status_patterns <- function(classified_data,
                                  person_col = "cf", 
                                  include_transitions = TRUE) {
  
  patterns <- list()
  
  # Overall status distribution
  patterns$status_distribution <- table(classified_data$stato)
  patterns$status_proportions <- prop.table(patterns$status_distribution)
  
  # Duration analysis by status
  patterns$duration_by_status <- classified_data[, {
    list(
      mean_duration = mean(durata, na.rm = TRUE),
      median_duration = median(durata, na.rm = TRUE),
      total_duration = sum(durata, na.rm = TRUE),
      n_segments = .N
    )
  }, by = stato]
  
  # Person-level patterns (optimized aggregation)
  person_patterns <- classified_data[, {
    statuses <- unique(stato)
    n_statuses <- length(statuses)
    n_segments <- .N
    total_duration <- sum(durata, na.rm = TRUE)
    has_unemployment <- "disoccupato" %in% statuses
    has_overlap <- any(grepl("^over_", statuses, perl = TRUE))
    # Optimized employment rate calculation
    employment_duration <- sum(durata[stato != "disoccupato"], na.rm = TRUE)
    employment_rate <- if (total_duration > 0) employment_duration / total_duration else 0
    
    list(
      n_statuses = n_statuses,
      n_segments = n_segments,
      total_duration = total_duration,
      has_unemployment = has_unemployment,
      has_overlap = has_overlap,
      employment_rate = employment_rate
    )
  }, by = c(person_col)]
  
  patterns$person_level <- list(
    employment_stability = person_patterns[, {
      list(
        stable_workers = sum(n_statuses <= 2),  # Only 1-2 different statuses
        unstable_workers = sum(n_statuses > 4),  # More than 4 different statuses
        mean_employment_rate = mean(employment_rate, na.rm = TRUE),
        persons_with_overlaps = sum(has_overlap)
      )
    }],
    status_diversity = table(person_patterns$n_statuses)
  )
  
  # Transition analysis (optimized)
  if (include_transitions) {
    setorderv(classified_data, c(person_col, "inizio"))
    
    # More efficient transition calculation using shift
    transitions_dt <- classified_data[, {
      if (.N > 1L) {
        from_status <- stato[1L:(.N-1L)]
        to_status <- stato[2L:.N]
        # Pre-allocate and use paste0 for better performance
        transition_pairs <- paste0(from_status, "->", to_status)
        list(transition = transition_pairs)
      } else {
        list(transition = character(0))
      }
    }, by = c(person_col)]
    
    if (nrow(transitions_dt) > 0L) {
      transition_vec <- transitions_dt$transition
      
      # Pre-compile patterns for faster matching
      patterns_trans <- list(
        unemployment_entry = "-> disoccupato",
        unemployment_exit = "disoccupato ->",
        overlap_formation = "-> over_",
        overlap_dissolution = "over_.*-> (?!over_)"
      )
      
      # Vectorized pattern matching
      patterns$transitions <- list(
        most_common = head(sort(table(transition_vec), decreasing = TRUE), 10L),
        unemployment_entries = sum(grepl(patterns_trans$unemployment_entry, transition_vec, fixed = TRUE)),
        unemployment_exits = sum(grepl(patterns_trans$unemployment_exit, transition_vec, fixed = TRUE)),
        overlap_formations = sum(grepl(patterns_trans$overlap_formation, transition_vec, fixed = TRUE)),
        overlap_dissolutions = sum(grepl(patterns_trans$overlap_dissolution, transition_vec, perl = TRUE))
      )
    }
  }
  
  # Employment quality indicators
  patterns$quality_indicators <- list(
    continuous_employment_rate = person_patterns[, mean(1 - has_unemployment)],
    average_employment_segments = person_patterns[, mean(n_segments)],
    overlap_prevalence = person_patterns[, mean(has_overlap)],
    employment_concentration = classified_data[, {
      emp_durations <- as.numeric(sum(durata[!grepl("disoccupato", stato)]))
      total_duration <- as.numeric(sum(durata))
      emp_durations / total_duration
    }]
  )
  
  class(patterns) <- c("employment_status_patterns", "list")
  return(patterns)
}

#' Validate Employment Status Classifications
#'
#' @description  
#' Validates the consistency and logical correctness of employment status
#' classifications, checking for impossible combinations and missing labels.
#' Optimized for large datasets with vectorized operations.
#'
#' @param classified_data Data.table with employment segments and status labels
#' @param rules Classification rules used (for validation reference)
#'
#' @return List with validation results and any detected issues
#'
#' @export
validate_status_classifications <- function(classified_data, rules = get_default_status_rules()) {
  
  validation <- list()
  
  # Pre-extract stato and other columns to avoid repeated column access
  stato_col <- classified_data$stato
  arco_col <- classified_data$arco
  prior_col <- classified_data$prior
  n_rows <- nrow(classified_data)
  
  # Check for missing status labels (vectorized)
  missing_status <- is.na(stato_col) | stato_col == ""
  validation$missing_labels <- sum(missing_status)
  
  # Pre-compile regex patterns for better performance
  patterns <- list(
    unemployment = "disoccupato",
    single_employment = "^occ_",
    overlap = "^over_",
    fulltime = "_ft",
    parttime = "_pt"
  )
  
  # Pre-compute logical vectors for each pattern (vectorized operations)
  is_unemployment <- grepl(patterns$unemployment, stato_col, fixed = TRUE)
  is_single_emp <- grepl(patterns$single_employment, stato_col)
  is_overlap <- grepl(patterns$overlap, stato_col)
  is_fulltime <- grepl(patterns$fulltime, stato_col, fixed = TRUE)
  is_parttime <- grepl(patterns$parttime, stato_col, fixed = TRUE)
  
  # Check for impossible combinations using pre-computed logical vectors
  impossible <- list(
    unemployment_with_employment = sum(is_unemployment & arco_col > 0L, na.rm = TRUE),
    single_employment_wrong_arco = sum(is_single_emp & arco_col != 1L, na.rm = TRUE),
    overlap_without_overlap = sum(is_overlap & arco_col <= 1L, na.rm = TRUE),
    fulltime_with_parttime_prior = sum(is_fulltime & prior_col == 0L, na.rm = TRUE),
    parttime_with_fulltime_prior = sum(is_parttime & prior_col == 1L, na.rm = TRUE)
  )
  
  validation$impossible_combinations <- impossible
  validation$total_impossible <- sum(unlist(impossible, use.names = FALSE))
  
  # Check status coverage (use unique with na.rm for efficiency)
  expected_statuses <- c(
    rules$unemployment$short_label,
    rules$single_employment$full_time$label,
    rules$single_employment$part_time$label,
    rules$overlapping_employment$pt_to_ft$label,
    rules$overlapping_employment$ft_to_pt$label,
    rules$overlapping_employment$pt_to_pt$label,
    rules$overlapping_employment$ft_to_ft$label
  )
  
  # Optimized unique extraction
  observed_statuses <- unique(stato_col[!missing_status])
  validation$unexpected_statuses <- setdiff(observed_statuses, expected_statuses)
  validation$missing_expected_statuses <- setdiff(expected_statuses, observed_statuses)
  
  # Overall validation status
  validation$is_valid <- validation$missing_labels == 0L && 
                        validation$total_impossible == 0L &&
                        length(validation$unexpected_statuses) == 0L
  
  # Avoid division by zero
  validation$validation_rate <- if (n_rows > 0L) {
    1 - (validation$missing_labels + validation$total_impossible) / n_rows
  } else {
    1.0
  }
  
  class(validation) <- c("employment_status_validation", "list")
  return(validation)
}

#' Print Employment Status Patterns Analysis
#'
#' @param x An employment_status_patterns object
#' @param ... Additional arguments (ignored)
#' @export
print.employment_status_patterns <- function(x, ...) {
  cat("Employment Status Pattern Analysis\n")
  cat("=================================\n\n")
  
  # Status distribution
  cat("Status Distribution:\n")
  cat("-------------------\n")
  for (status in names(x$status_distribution)) {
    count <- x$status_distribution[status]
    pct <- round(x$status_proportions[status] * 100, 1)
    cat(sprintf("%-15s: %6d (%4.1f%%)\n", status, count, pct))
  }
  cat("\n")
  
  # Duration patterns
  cat("Average Duration by Status:\n")
  cat("---------------------------\n")
  for (i in 1:nrow(x$duration_by_status)) {
    row <- x$duration_by_status[i]
    cat(sprintf("%-15s: %6.1f days (%d segments)\n", 
                row$stato, row$mean_duration, row$n_segments))
  }
  cat("\n")
  
  # Employment quality
  cat("Employment Quality Indicators:\n")
  cat("-----------------------------\n")
  cat(sprintf("Continuous Employment Rate: %.1f%%\n", 
              x$quality_indicators$continuous_employment_rate * 100))
  cat(sprintf("Average Segments per Person: %.1f\n", 
              x$quality_indicators$average_employment_segments))
  cat(sprintf("Overlap Prevalence: %.1f%%\n", 
              x$quality_indicators$overlap_prevalence * 100))
  cat(sprintf("Overall Employment Concentration: %.1f%%\n", 
              x$quality_indicators$employment_concentration * 100))
  
  # Transitions if available
  if (!is.null(x$transitions)) {
    cat("\nMost Common Transitions:\n")
    cat("-----------------------\n")
    for (i in 1:min(5, length(x$transitions$most_common))) {
      trans_name <- names(x$transitions$most_common)[i]
      trans_count <- x$transitions$most_common[i]
      cat(sprintf("%-25s: %d\n", trans_name, trans_count))
    }
  }
  
  invisible(x)
}

#' Print Employment Status Validation Results
#'
#' @param x An employment_status_validation object  
#' @param ... Additional arguments (ignored)
#' @export
print.employment_status_validation <- function(x, ...) {
  cat("Employment Status Validation Results\n")
  cat("===================================\n\n")
  
  if (x$is_valid) {
    cat("✓ All status classifications are valid\n")
  } else {
    cat("⚠ Status classification issues detected\n")
  }
  
  cat(sprintf("Validation Rate: %.1f%%\n\n", x$validation_rate * 100))
  
  cat("Issue Summary:\n")
  cat("-------------\n")
  cat(sprintf("Missing Labels: %d\n", x$missing_labels))
  cat(sprintf("Impossible Combinations: %d\n", x$total_impossible))
  
  if (x$total_impossible > 0) {
    cat("\nImpossible Combination Details:\n")
    for (issue in names(x$impossible_combinations)) {
      count <- x$impossible_combinations[[issue]]
      if (count > 0) {
        cat(sprintf("  %s: %d\n", gsub("_", " ", issue), count))
      }
    }
  }
  
  if (length(x$unexpected_statuses) > 0) {
    cat("\nUnexpected Status Labels:\n")
    cat(paste(x$unexpected_statuses, collapse = ", "), "\n")
  }
  
  if (length(x$missing_expected_statuses) > 0) {
    cat("\nMissing Expected Status Labels:\n") 
    cat(paste(x$missing_expected_statuses, collapse = ", "), "\n")
  }
  
  invisible(x)
}

#' Classify Employment Status with Consolidated Overlaps
#'
#' @description
#' Internal function that provides consolidated classification of overlapping 
#' employment periods using over_id grouping. This creates single status
#' labels for entire overlapping employment periods rather than 
#' segment-by-segment classification.
#'
#' @param segments Data.table with employment segments including over_id column
#' @param rules Classification rules 
#' @param group_by Character vector of grouping columns
#'
#' @return Data.table with consolidated employment status classifications
#' @keywords internal
.classify_with_consolidated_overlaps <- function(segments, rules, group_by) {
  
  # First apply standard classification
  segments <- classify_employment_status(segments, rules, group_by)
  
  # Create consolidated classifications for over_id > 0
  if (length(group_by) > 0) {
    # Group by person and over_id to create consolidated periods
    consolidated <- segments[over_id > 0, {
      list(
        inizio = min(inizio),
        fine = max(fine), 
        durata = as.numeric(max(fine) - min(inizio) + 1),
        arco = max(arco),
        prior_min = min(prior, na.rm = TRUE),
        prior_max = max(prior, na.rm = TRUE),
        n_segments = .N,
        n_contracts = length(unique(prior[prior > 0])),  # Count distinct contract types
        original_states = list(unique(stato))
      )
    }, by = c(group_by, "over_id")]
    
    # Apply consolidated classification rules
    consolidated[, stato := fcase(
      # Consolidated full-time employment
      prior_max == 1 & prior_min == 1, 
        ifelse(n_segments > 1, "consolidated_ft", "occ_ft"),
      
      # Consolidated part-time employment  
      prior_max == 0 & prior_min == 0,
        ifelse(n_segments > 1, "consolidated_pt", "occ_pt"),
      
      # Mixed employment types
      prior_min != prior_max,
        ifelse(n_segments > 1, "consolidated_mixed", "over_mixed"),
      
      # Default
      default = "consolidated_employment"
    )]
    
    # Combine with unemployment periods (over_id = 0)
    unemployment <- segments[over_id == 0]
    
    # Select relevant columns for combination
    consolidated_cols <- intersect(names(unemployment), names(consolidated))
    result <- rbind(
      unemployment[, ..consolidated_cols],
      consolidated[, ..consolidated_cols],
      fill = TRUE
    )
    
  } else {
    # Simplified version without person grouping
    result <- segments
    result[over_id > 0, stato := paste0("consolidated_", gsub("^(occ_|over_)", "", stato))]
  }
  
  # Ensure proper ordering
  if (length(group_by) > 0) {
    setorderv(result, c(group_by, "inizio"))
  } else {
    setorder(result, inizio)
  }
  
  return(result)
}