#' Impact Evaluation: Control Group Identification and Matching
#'
#' This module provides comprehensive matching functionality for impact evaluation
#' studies including propensity score matching, coarsened exact matching, and
#' balance assessment with diagnostic tools.
#'
#' @name impact_matching
#' @author vecshift package
NULL

#' Propensity Score Matching for Impact Evaluation
#'
#' Performs propensity score matching to identify comparable control units for
#' treatment effect estimation. Supports various matching algorithms and provides
#' comprehensive diagnostics.
#'
#' @param data A data.table containing treatment and control observations
#' @param treatment_var Character. Name of treatment indicator variable. Default: "is_treated"
#' @param matching_variables Character vector. Variables to include in propensity score model
#' @param exact_match_vars Character vector. Variables requiring exact matches. Default: NULL
#' @param method Character. Matching method: "nearest", "optimal", "full", "genetic". Default: "nearest"
#' @param ratio Numeric. Ratio of control to treatment units. Default: 1
#' @param caliper Numeric. Maximum allowable distance for matches. Default: NULL (no caliper)
#' @param replace Logical. Allow replacement in matching? Default: FALSE
#' @param estimand Character. Target estimand: "ATT", "ATE", "ATC". Default: "ATT"
#' @param link Character. Link function for propensity model: "logit", "probit". Default: "logit"
#' @param distance_metric Character. Distance metric: "glm", "gam", "gbm", "randomforest". Default: "glm"
#'
#' @return A list containing:
#'   \item{matched_data}{Data.table with matched observations}
#'   \item{match_matrix}{Matrix showing which units were matched}
#'   \item{propensity_scores}{Propensity scores for all observations}
#'   \item{balance_before}{Balance statistics before matching}
#'   \item{balance_after}{Balance statistics after matching}
#'   \item{match_summary}{Summary of matching procedure}
#'   \item{common_support}{Information about common support region}
#'
#' @examples
#' \dontrun{
#' # Basic propensity score matching
#' ps_match <- propensity_score_matching(
#'   data = analysis_data,
#'   matching_variables = c("age", "education", "sector", "region"),
#'   exact_match_vars = c("gender"),
#'   method = "nearest",
#'   ratio = 2
#' )
#' 
#' # Advanced matching with caliper
#' ps_match_advanced <- propensity_score_matching(
#'   data = analysis_data,
#'   matching_variables = c("age", "education", "prior_employment", "wage"),
#'   method = "optimal",
#'   caliper = 0.1,
#'   distance_metric = "gbm"
#' )
#' }
#'
#' @export
propensity_score_matching <- function(data,
                                    treatment_var = "is_treated",
                                    matching_variables,
                                    exact_match_vars = NULL,
                                    method = "nearest",
                                    ratio = 1,
                                    caliper = NULL,
                                    replace = FALSE,
                                    estimand = "ATT",
                                    link = "logit",
                                    distance_metric = "glm") {
  
  # Check for required packages
  required_packages <- c("MatchIt", "cobalt")
  missing_packages <- setdiff(required_packages, rownames(installed.packages()))
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not installed:", paste(missing_packages, collapse = ", "),
               "\nPlease install with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))"))
  }
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  if (!treatment_var %in% names(data)) {
    stop(paste("Treatment variable", treatment_var, "not found in data"))
  }
  
  missing_vars <- setdiff(matching_variables, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("Matching variables not found:", paste(missing_vars, collapse = ", ")))
  }
  
  if (!is.null(exact_match_vars)) {
    missing_exact <- setdiff(exact_match_vars, names(data))
    if (length(missing_exact) > 0) {
      stop(paste("Exact match variables not found:", paste(missing_exact, collapse = ", ")))
    }
  }
  
  # Convert to data.frame for MatchIt
  match_data <- as.data.frame(data)
  
  # Create propensity score formula
  if (!is.null(exact_match_vars)) {
    formula_str <- paste(treatment_var, "~", 
                        paste(c(matching_variables, exact_match_vars), collapse = " + "))
    exact_str <- paste(exact_match_vars, collapse = " + ")
  } else {
    formula_str <- paste(treatment_var, "~", 
                        paste(matching_variables, collapse = " + "))
    exact_str <- NULL
  }
  
  match_formula <- as.formula(formula_str)
  
  # Perform matching using MatchIt
  tryCatch({
    match_result <- MatchIt::matchit(
      formula = match_formula,
      data = match_data,
      method = method,
      ratio = ratio,
      caliper = caliper,
      replace = replace,
      estimand = estimand,
      link = link,
      distance = distance_metric,
      exact = if (!is.null(exact_str)) as.formula(paste("~", exact_str)) else NULL
    )
  }, error = function(e) {
    stop(paste("Matching failed:", e$message))
  })
  
  # Extract matched data
  matched_data <- MatchIt::match.data(match_result)
  matched_dt <- as.data.table(matched_data)
  
  # Calculate balance before and after matching
  balance_before <- tryCatch({
    cobalt::bal.tab(match_result, un = TRUE, thresholds = c(m = 0.1))
  }, error = function(e) {
    warning(paste("Balance calculation failed:", e$message))
    NULL
  })
  
  balance_after <- tryCatch({
    cobalt::bal.tab(match_result, thresholds = c(m = 0.1))
  }, error = function(e) {
    warning(paste("Balance calculation failed:", e$message))
    NULL
  })
  
  # Extract propensity scores
  propensity_scores <- data.table(
    row_id = 1:nrow(match_data),
    propensity_score = match_result$distance %||% NA_real_,
    is_treated = match_data[[treatment_var]]
  )
  
  # Common support analysis
  if (!is.null(match_result$distance)) {
    ps_treated <- match_result$distance[match_data[[treatment_var]] == 1]
    ps_control <- match_result$distance[match_data[[treatment_var]] == 0]
    
    common_support <- list(
      treated_range = range(ps_treated, na.rm = TRUE),
      control_range = range(ps_control, na.rm = TRUE),
      overlap_range = c(max(min(ps_treated, na.rm = TRUE), min(ps_control, na.rm = TRUE)),
                       min(max(ps_treated, na.rm = TRUE), max(ps_control, na.rm = TRUE))),
      treated_in_support = sum(ps_treated >= max(min(ps_treated, na.rm = TRUE), min(ps_control, na.rm = TRUE)) &
                              ps_treated <= min(max(ps_treated, na.rm = TRUE), max(ps_control, na.rm = TRUE))),
      control_in_support = sum(ps_control >= max(min(ps_treated, na.rm = TRUE), min(ps_control, na.rm = TRUE)) &
                              ps_control <= min(max(ps_treated, na.rm = TRUE), max(ps_control, na.rm = TRUE)))
    )
  } else {
    common_support <- NULL
  }
  
  # Match summary
  match_summary <- list(
    method = method,
    total_treated = sum(match_data[[treatment_var]]),
    total_control = sum(!match_data[[treatment_var]]),
    matched_treated = nrow(matched_dt[get(treatment_var) == 1]),
    matched_control = nrow(matched_dt[get(treatment_var) == 0]),
    matching_ratio = ratio,
    caliper_used = !is.null(caliper),
    caliper_value = caliper,
    replacement_used = replace,
    exact_variables = exact_match_vars
  )
  
  return(list(
    matched_data = matched_dt,
    match_matrix = match_result$match.matrix,
    propensity_scores = propensity_scores,
    balance_before = balance_before,
    balance_after = balance_after,
    match_summary = match_summary,
    common_support = common_support,
    matchit_object = match_result
  ))
}

#' Coarsened Exact Matching (CEM)
#'
#' Performs coarsened exact matching to create balanced treatment and control groups
#' by automatically binning continuous variables and exactly matching on specified factors.
#'
#' @param data A data.table containing treatment and control observations
#' @param treatment_var Character. Name of treatment indicator variable. Default: "is_treated"
#' @param matching_variables Character vector. Variables to include in matching
#' @param automatic_binning Logical. Automatically bin continuous variables? Default: TRUE
#' @param cutpoints Named list. Custom cutpoints for continuous variables. Default: NULL
#' @param k2k Logical. Perform k-to-k matching (1:1 ratio)? Default: FALSE
#' @param keep_all Logical. Keep all matched observations? Default: TRUE
#'
#' @return A list containing:
#'   \item{matched_data}{Data.table with matched observations and weights}
#'   \item{match_summary}{Summary of CEM procedure}
#'   \item{imbalance_measures}{L1 and other imbalance statistics}
#'   \item{strata_info}{Information about matching strata}
#'
#' @examples
#' \dontrun{
#' # Basic CEM
#' cem_match <- coarsened_exact_matching(
#'   data = analysis_data,
#'   matching_variables = c("age", "education", "sector"),
#'   automatic_binning = TRUE
#' )
#' 
#' # CEM with custom cutpoints
#' cem_match_custom <- coarsened_exact_matching(
#'   data = analysis_data,
#'   matching_variables = c("age", "wage", "experience"),
#'   cutpoints = list(
#'     age = c(25, 35, 45, 55),
#'     wage = c(1000, 2000, 3000, 4000)
#'   )
#' )
#' }
#'
#' @export
coarsened_exact_matching <- function(data,
                                   treatment_var = "is_treated", 
                                   matching_variables,
                                   automatic_binning = TRUE,
                                   cutpoints = NULL,
                                   k2k = FALSE,
                                   keep_all = TRUE) {
  
  # Check for required package
  if (!"cem" %in% rownames(installed.packages())) {
    stop("Package 'cem' not installed. Please install with: install.packages('cem')")
  }
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  if (!treatment_var %in% names(data)) {
    stop(paste("Treatment variable", treatment_var, "not found in data"))
  }
  
  missing_vars <- setdiff(matching_variables, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("Matching variables not found:", paste(missing_vars, collapse = ", ")))
  }
  
  # Convert to data.frame for CEM
  match_data <- as.data.frame(data)
  
  # Prepare matching variables
  X <- match_data[, matching_variables, drop = FALSE]
  treatment <- match_data[[treatment_var]]
  
  # Apply custom cutpoints if provided
  if (!is.null(cutpoints)) {
    for (var in names(cutpoints)) {
      if (var %in% matching_variables && is.numeric(match_data[[var]])) {
        X[[var]] <- cut(X[[var]], breaks = cutpoints[[var]], include.lowest = TRUE)
      }
    }
  }
  
  # Perform CEM
  tryCatch({
    cem_result <- cem::cem(
      treatment = treatment,
      data = X,
      k2k = k2k,
      keep.all = keep_all
    )
  }, error = function(e) {
    stop(paste("CEM matching failed:", e$message))
  })
  
  # Create matched dataset with weights
  matched_indices <- which(cem_result$w > 0)
  matched_data <- as.data.table(match_data[matched_indices, ])
  matched_data[, cem_weight := cem_result$w[matched_indices]]
  matched_data[, cem_strata := cem_result$strata[matched_indices]]
  
  # Calculate imbalance measures
  imbalance_measures <- list(
    l1_before = cem_result$L1$L1,
    l1_after = cem_result$L1$L1.after,
    l1_reduction = (cem_result$L1$L1 - cem_result$L1$L1.after) / cem_result$L1$L1,
    breaks_used = cem_result$breaks
  )
  
  # Strata information
  strata_info <- data.table(
    strata = names(table(cem_result$strata[cem_result$strata > 0])),
    n_treated = as.vector(tapply(treatment[cem_result$w > 0], 
                                cem_result$strata[cem_result$w > 0], 
                                function(x) sum(x == 1))),
    n_control = as.vector(tapply(treatment[cem_result$w > 0], 
                                cem_result$strata[cem_result$w > 0], 
                                function(x) sum(x == 0)))
  )
  strata_info[, total_n := n_treated + n_control]
  
  # Match summary
  match_summary <- list(
    method = "CEM",
    total_treated = sum(treatment),
    total_control = sum(!treatment),
    matched_treated = sum(treatment[matched_indices]),
    matched_control = sum(!treatment[matched_indices]),
    n_strata = length(unique(cem_result$strata[cem_result$strata > 0])),
    automatic_binning = automatic_binning,
    k2k_matching = k2k,
    cutpoints_used = cutpoints
  )
  
  return(list(
    matched_data = matched_data,
    match_summary = match_summary,
    imbalance_measures = imbalance_measures,
    strata_info = strata_info,
    cem_object = cem_result
  ))
}

#' Balance Assessment for Matched Data
#'
#' Comprehensive balance assessment for matched treatment and control groups including
#' standardized mean differences, variance ratios, and distributional balance tests.
#'
#' @param matched_data Data.table with matched observations
#' @param treatment_var Character. Name of treatment indicator variable. Default: "is_treated"
#' @param balance_variables Character vector. Variables to assess balance for
#' @param weight_var Character. Name of weight variable (for weighted balance). Default: NULL
#' @param thresholds Named list. Balance thresholds: list(mean_diff = 0.1, variance_ratio = 2)
#'
#' @return A list containing:
#'   \item{balance_table}{Data.table with balance statistics for each variable}
#'   \item{overall_balance}{Overall balance summary}
#'   \item{distributional_tests}{Results of distributional balance tests}
#'   \item{balance_plots}{Data for creating balance plots}
#'
#' @examples
#' \dontrun{
#' balance_results <- assess_balance(
#'   matched_data = ps_match$matched_data,
#'   balance_variables = c("age", "education", "wage", "sector"),
#'   thresholds = list(mean_diff = 0.1, variance_ratio = 2)
#' )
#' }
#'
#' @export
assess_balance <- function(matched_data,
                          treatment_var = "is_treated",
                          balance_variables,
                          weight_var = NULL,
                          thresholds = list(mean_diff = 0.1, variance_ratio = 2)) {
  
  if (!inherits(matched_data, "data.table")) {
    stop("matched_data must be a data.table")
  }
  
  if (!treatment_var %in% names(matched_data)) {
    stop(paste("Treatment variable", treatment_var, "not found in data"))
  }
  
  missing_vars <- setdiff(balance_variables, names(matched_data))
  if (length(missing_vars) > 0) {
    stop(paste("Balance variables not found:", paste(missing_vars, collapse = ", ")))
  }
  
  # Check if weights should be used
  use_weights <- !is.null(weight_var) && weight_var %in% names(matched_data)
  
  # Calculate balance statistics
  balance_results <- list()
  
  for (var in balance_variables) {
    if (is.numeric(matched_data[[var]])) {
      # Numeric variable balance
      if (use_weights) {
        treated_stats <- matched_data[get(treatment_var) == 1, .(
          mean = weighted.mean(get(var), get(weight_var), na.rm = TRUE),
          var = sum(get(weight_var) * (get(var) - weighted.mean(get(var), get(weight_var), na.rm = TRUE))^2, na.rm = TRUE) / sum(get(weight_var), na.rm = TRUE),
          n = sum(get(weight_var), na.rm = TRUE)
        )]
        
        control_stats <- matched_data[get(treatment_var) == 0, .(
          mean = weighted.mean(get(var), get(weight_var), na.rm = TRUE),
          var = sum(get(weight_var) * (get(var) - weighted.mean(get(var), get(weight_var), na.rm = TRUE))^2, na.rm = TRUE) / sum(get(weight_var), na.rm = TRUE),
          n = sum(get(weight_var), na.rm = TRUE)
        )]
      } else {
        treated_stats <- matched_data[get(treatment_var) == 1, .(
          mean = mean(get(var), na.rm = TRUE),
          var = var(get(var), na.rm = TRUE),
          n = .N
        )]
        
        control_stats <- matched_data[get(treatment_var) == 0, .(
          mean = mean(get(var), na.rm = TRUE),
          var = var(get(var), na.rm = TRUE),
          n = .N
        )]
      }
      
      # Standardized mean difference
      pooled_sd <- sqrt((treated_stats$var + control_stats$var) / 2)
      std_mean_diff <- (treated_stats$mean - control_stats$mean) / pooled_sd
      
      # Variance ratio
      var_ratio <- treated_stats$var / control_stats$var
      
      balance_results[[var]] <- data.table(
        variable = var,
        variable_type = "numeric",
        treated_mean = treated_stats$mean,
        control_mean = control_stats$mean,
        mean_diff = treated_stats$mean - control_stats$mean,
        std_mean_diff = std_mean_diff,
        treated_var = treated_stats$var,
        control_var = control_stats$var,
        var_ratio = var_ratio,
        treated_n = treated_stats$n,
        control_n = control_stats$n,
        balance_concern = abs(std_mean_diff) > thresholds$mean_diff || 
                         var_ratio > thresholds$variance_ratio || 
                         var_ratio < (1/thresholds$variance_ratio)
      )
    } else {
      # Categorical variable balance
      if (use_weights) {
        treated_props <- matched_data[get(treatment_var) == 1, .(
          prop = sum(get(weight_var), na.rm = TRUE) / sum(matched_data[get(treatment_var) == 1, get(weight_var)], na.rm = TRUE)
        ), by = c(var)]
        
        control_props <- matched_data[get(treatment_var) == 0, .(
          prop = sum(get(weight_var), na.rm = TRUE) / sum(matched_data[get(treatment_var) == 0, get(weight_var)], na.rm = TRUE)
        ), by = c(var)]
      } else {
        treated_props <- matched_data[get(treatment_var) == 1, .(
          prop = .N / matched_data[get(treatment_var) == 1, .N]
        ), by = c(var)]
        
        control_props <- matched_data[get(treatment_var) == 0, .(
          prop = .N / matched_data[get(treatment_var) == 0, .N]
        ), by = c(var)]
      }
      
      # Merge proportions
      cat_balance <- merge(treated_props, control_props, by = var, suffixes = c("_treated", "_control"), all = TRUE)
      cat_balance[is.na(prop_treated), prop_treated := 0]
      cat_balance[is.na(prop_control), prop_control := 0]
      
      # Calculate maximum difference in proportions
      max_prop_diff <- max(abs(cat_balance$prop_treated - cat_balance$prop_control), na.rm = TRUE)
      
      balance_results[[var]] <- data.table(
        variable = var,
        variable_type = "categorical",
        treated_mean = NA_real_,
        control_mean = NA_real_,
        mean_diff = NA_real_,
        std_mean_diff = max_prop_diff, # Use this field for max prop diff
        treated_var = NA_real_,
        control_var = NA_real_,
        var_ratio = NA_real_,
        treated_n = matched_data[get(treatment_var) == 1, if(use_weights) sum(get(weight_var)) else .N],
        control_n = matched_data[get(treatment_var) == 0, if(use_weights) sum(get(weight_var)) else .N],
        balance_concern = max_prop_diff > thresholds$mean_diff
      )
    }
  }
  
  # Combine balance results
  balance_table <- rbindlist(balance_results)
  
  # Overall balance summary
  overall_balance <- list(
    n_variables = nrow(balance_table),
    n_balanced = sum(!balance_table$balance_concern),
    n_imbalanced = sum(balance_table$balance_concern),
    prop_balanced = mean(!balance_table$balance_concern),
    max_std_diff = max(abs(balance_table$std_mean_diff), na.rm = TRUE),
    mean_abs_std_diff = mean(abs(balance_table$std_mean_diff), na.rm = TRUE)
  )
  
  # Distributional tests (if applicable)
  distributional_tests <- list()
  for (var in balance_variables[balance_variables %in% names(matched_data)]) {
    if (is.numeric(matched_data[[var]])) {
      tryCatch({
        ks_test <- ks.test(matched_data[get(treatment_var) == 1, get(var)],
                          matched_data[get(treatment_var) == 0, get(var)])
        distributional_tests[[var]] <- list(
          test = "Kolmogorov-Smirnov",
          statistic = ks_test$statistic,
          p_value = ks_test$p.value
        )
      }, error = function(e) {
        distributional_tests[[var]] <- list(
          test = "Kolmogorov-Smirnov",
          statistic = NA,
          p_value = NA,
          error = e$message
        )
      })
    }
  }
  
  return(list(
    balance_table = balance_table,
    overall_balance = overall_balance,
    distributional_tests = distributional_tests,
    thresholds_used = thresholds
  ))
}

#' Match Quality Diagnostics
#'
#' Provides comprehensive diagnostics for matching quality including common support,
#' match distances, and recommendations for improving matches.
#'
#' @param matching_result List object from propensity_score_matching() or coarsened_exact_matching()
#' @param diagnostic_plots Logical. Generate data for diagnostic plots? Default: TRUE
#'
#' @return A list containing:
#'   \item{quality_summary}{Overall quality assessment}
#'   \item{match_distances}{Distribution of matching distances}
#'   \item{common_support_analysis}{Common support region analysis}
#'   \item{recommendations}{Specific recommendations for improvement}
#'   \item{diagnostic_data}{Data for creating diagnostic plots}
#'
#' @examples
#' \dontrun{
#' diagnostics <- assess_match_quality(
#'   matching_result = ps_match,
#'   diagnostic_plots = TRUE
#' )
#' print(diagnostics$quality_summary)
#' }
#'
#' @export
assess_match_quality <- function(matching_result,
                                diagnostic_plots = TRUE) {
  
  if (!is.list(matching_result)) {
    stop("matching_result must be a list object from matching functions")
  }
  
  # Extract components
  matched_data <- matching_result$matched_data
  match_summary <- matching_result$match_summary
  
  # Quality metrics
  quality_metrics <- list(
    matching_method = match_summary$method,
    treatment_retention_rate = match_summary$matched_treated / match_summary$total_treated,
    control_utilization_rate = match_summary$matched_control / match_summary$total_control,
    effective_sample_size = match_summary$matched_treated + match_summary$matched_control,
    balance_achieved = if ("balance_after" %in% names(matching_result)) {
      !is.null(matching_result$balance_after)
    } else {
      NA
    }
  )
  
  # Match distance analysis (for PSM)
  match_distances <- NULL
  if ("propensity_scores" %in% names(matching_result)) {
    ps_data <- matching_result$propensity_scores
    if (!is.null(ps_data) && "propensity_score" %in% names(ps_data)) {
      match_distances <- list(
        min_distance = min(ps_data$propensity_score, na.rm = TRUE),
        max_distance = max(ps_data$propensity_score, na.rm = TRUE),
        mean_distance = mean(ps_data$propensity_score, na.rm = TRUE),
        median_distance = median(ps_data$propensity_score, na.rm = TRUE),
        distance_range = range(ps_data$propensity_score, na.rm = TRUE)
      )
    }
  }
  
  # Common support analysis
  common_support_analysis <- NULL
  if ("common_support" %in% names(matching_result)) {
    cs_info <- matching_result$common_support
    if (!is.null(cs_info)) {
      common_support_analysis <- list(
        overlap_exists = cs_info$overlap_range[2] > cs_info$overlap_range[1],
        treated_in_support_pct = cs_info$treated_in_support / match_summary$total_treated * 100,
        control_in_support_pct = cs_info$control_in_support / match_summary$total_control * 100,
        support_quality = "good" # Could be enhanced with more sophisticated assessment
      )
    }
  }
  
  # Generate recommendations
  recommendations <- character()
  
  if (quality_metrics$treatment_retention_rate < 0.8) {
    recommendations <- c(recommendations, 
                        "Low treatment retention rate - consider relaxing matching criteria")
  }
  
  if (quality_metrics$control_utilization_rate > 0.9) {
    recommendations <- c(recommendations, 
                        "Very high control utilization - may indicate limited control pool")
  }
  
  if (!is.null(common_support_analysis)) {
    if (common_support_analysis$treated_in_support_pct < 80) {
      recommendations <- c(recommendations, 
                          "Limited common support for treated units - consider trimming or different matching approach")
    }
    
    if (common_support_analysis$control_in_support_pct < 50) {
      recommendations <- c(recommendations, 
                          "Limited common support for control units - consider expanding control pool")
    }
  }
  
  if (match_summary$method == "nearest" && is.null(match_summary$caliper_value)) {
    recommendations <- c(recommendations, 
                        "Consider using caliper matching to improve match quality")
  }
  
  # Diagnostic plot data
  diagnostic_data <- list()
  if (diagnostic_plots) {
    if ("propensity_scores" %in% names(matching_result)) {
      diagnostic_data$propensity_histogram <- matching_result$propensity_scores
    }
    
    if ("matched_data" %in% names(matching_result)) {
      diagnostic_data$balance_data <- matched_data
    }
  }
  
  # Overall quality assessment
  quality_score <- (
    0.3 * pmin(1, quality_metrics$treatment_retention_rate) +
    0.2 * pmin(1, quality_metrics$control_utilization_rate) +
    0.3 * ifelse(!is.null(common_support_analysis), 
                 (common_support_analysis$treated_in_support_pct + common_support_analysis$control_in_support_pct) / 200, 
                 0.5) +
    0.2 * ifelse(length(recommendations) == 0, 1, pmax(0, 1 - length(recommendations) * 0.2))
  )
  
  quality_summary <- list(
    overall_quality_score = quality_score,
    quality_rating = cut(quality_score, 
                        breaks = c(0, 0.5, 0.7, 0.85, 1), 
                        labels = c("Poor", "Fair", "Good", "Excellent"), 
                        include.lowest = TRUE),
    key_metrics = quality_metrics,
    major_concerns = length(recommendations)
  )
  
  return(list(
    quality_summary = quality_summary,
    match_distances = match_distances,
    common_support_analysis = common_support_analysis,
    recommendations = recommendations,
    diagnostic_data = diagnostic_data
  ))
}

#' Print method for balance assessment
#' @param x A balance assessment object
#' @param ... Additional arguments passed to print
#' @method print balance_assessment
#' @export
print.balance_assessment <- function(x, ...) {
  cat("Balance Assessment Results\n")
  cat("==========================\n\n")
  
  cat("Overall Balance:\n")
  cat("  Variables assessed:", x$overall_balance$n_variables, "\n")
  cat("  Balanced variables:", x$overall_balance$n_balanced, "\n")
  cat("  Imbalanced variables:", x$overall_balance$n_imbalanced, "\n")
  cat("  Balance rate:", sprintf("%.1f%%", x$overall_balance$prop_balanced * 100), "\n")
  cat("  Max standardized difference:", sprintf("%.3f", x$overall_balance$max_std_diff), "\n")
  
  if (x$overall_balance$n_imbalanced > 0) {
    cat("\nImbalanced Variables:\n")
    imbalanced <- x$balance_table[balance_concern == TRUE]
    print(imbalanced[, .(variable, std_mean_diff, var_ratio)])
  }
  
  invisible(x)
}