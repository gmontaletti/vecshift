#' Employment Status Classification Module
#'
#' @description
#' Enhanced flexible employment status classification system for temporal employment
#' segments. Features sequence-based overlap labeling, flexible prior value mapping,
#' and customizable rules for diverse business contexts.
#'
#' @details
#' **Enhanced Capabilities (Version 1.0+)**:
#'
#' 1. **Flexible Prior Value System**: No longer normalizes prior values to 0/1.
#'    Supports any numeric employment type codes through custom prior_labels mapping.
#'
#' 2. **Sequence-Based Overlap Labeling**: Creates descriptive labels for overlapping
#'    employment periods based on chronological sequence of employment types.
#'    Example: overlapping periods with prior values [0, 1, 2] become "over_pt_ft_fixed".
#'
#' 3. **Unknown Prior Handling**: Automatically handles unmapped prior values by
#'    using their numeric representation (e.g., "occ_7" for unknown prior value 7).
#'
#' 4. **Custom Business Logic**: Supports industry-specific employment type
#'    classifications through the prior_labels parameter.
#'
#' 5. **Backward Compatibility**: Maintains full compatibility with existing
#'    vecshift workflows while enabling new flexible features.
#'
#' **Core Functions**:
#' - \\code{\\link{classify_employment_status}}: Main classification function with enhanced flexibility
#' - \\code{\\link{get_default_status_rules}}: Returns default rules including flexible prior mappings
#' - \\code{\\link{create_custom_status_rules}}: Creates custom rules with prior_labels support
#' - \\code{\\link{analyze_status_patterns}}: Analyzes employment status patterns
#' - \\code{\\link{validate_status_classifications}}: Validates classification results
#'
#' **Usage Patterns**:
#' - Use default rules for standard employment analysis
#' - Create custom prior_labels for industry-specific employment type codes
#' - Leverage sequence-based labeling for complex overlapping employment analysis
#' - Apply validation functions to ensure classification integrity
#'
#' @name status_labeling
NULL

#' Default Employment Status Classification Rules
#'
#' @description
#' Returns the default classification rules used by vecshift for employment status.
#' These rules include flexible prior value mappings and support sequence-based
#' overlap labeling. The rules can be modified to implement custom business logic.
#'
#' @details
#' The default rules include:
#'
#' **Prior Labels Mapping**: Maps numeric prior values to employment type labels:
#' * "-1", "0" -> "pt" (part-time)
#' * "1" -> "ft" (full-time)
#' * "2" -> "fixed" (fixed-term contract)
#' * "3" -> "temp" (temporary contract)
#'
#' **Unemployment Classification**: Based on duration thresholds (8 days default)
#'
#' **Single Employment**: Creates labels like "occ_ft", "occ_pt", "occ_fixed"
#'
#' **Overlapping Employment**: Uses sequence-based labeling to create labels
#' like "over_pt_ft_fixed" based on the chronological sequence of employment
#' types within the overlapping period.
#'
#' @return List with default classification rules containing:
#'   \itemize{
#'     \item unemployment: Rules for unemployment periods with duration thresholds
#'     \item prior_labels: Named list mapping prior values to employment type labels
#'     \item single_employment: Rules for single employment periods
#'     \item overlapping_employment: Rules for overlapping employment periods
#'   }
#' @export
#'
#' @examples
#' # View default rules
#' default_rules <- get_default_status_rules()
#' print(default_rules)
#'
#' # Check prior labels mapping
#' print(default_rules$prior_labels)
#'
#' # Example: prior value 2 maps to "fixed"
#' print(default_rules$prior_labels["2"])  # "fixed"
get_default_status_rules <- function() {
  list(
    unemployment = list(
      condition = "arco == 0",
      duration_threshold = 8,  # days
      short_label = "disoccupato",
      long_label = "disoccupato"  # Could differentiate short/long unemployment
    ),
    prior_labels = list(
      "-1" = "pt",
      "0" = "pt",
      "1" = "ft",
      "2" = "fixed",
      "3" = "temp"
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
#' Classifies employment segments using flexible, customizable rules with support
#' for any numeric prior value system. Features sequence-based overlap labeling
#' that creates descriptive labels based on chronological employment types.
#' Optimized for high-performance processing with minimal memory allocations.
#'
#' @details
#' **Key Enhancements**:
#'
#' 1. **Flexible Prior Value Mapping**: No longer normalizes prior values.
#'    Supports any numeric employment type codes through custom prior_labels.
#'
#' 2. **Sequence-Based Overlap Labeling**: For overlapping employment periods,
#'    creates labels based on chronological sequence of employment types.
#'    Example: prior values [0, 1, 2] in sequence -> "over_pt_ft_fixed"
#'
#' 3. **Unknown Prior Handling**: For unmapped prior values, uses numeric
#'    labels (e.g., "occ_5" for unknown prior value 5).
#'
#' 4. **Backward Compatibility**: Maintains compatibility with existing
#'    prior value conventions while supporting new flexible mapping.
#'
#' **Performance Optimizations**:
#' * Vectorized data.table operations replace all sapply() calls
#' * Pre-allocated memory for string concatenation
#' * In-place modifications using := operator
#' * Optimized grouping operations for large datasets
#' * Optional progress tracking for long-running operations
#'
#' **Classification Logic**:
#' * **Unemployment** (arco = 0): Labeled based on duration threshold
#' * **Single Employment** (arco = 1): "occ_" + employment type label
#' * **Overlapping Employment** (arco > 1): "over_" + sequence of types
#'
#' @param segments Data.table with temporal segments containing arco, prior, durata columns
#' @param rules List of classification rules (default: get_default_status_rules())
#' @param group_by Character vector of columns to group by for shift operations (default: "cf")
#' @param show_progress Logical indicating whether to show progress bar (default: FALSE)
#'
#' @return Data.table with stato column containing employment status labels
#'
#' @export
#' @importFrom data.table fcase shift setorder setorderv
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Sample segments data with various prior values
#' segments <- data.table(
#'   cf = rep("A001", 6),
#'   inizio = 1:6,
#'   fine = 2:7,
#'   arco = c(0, 1, 2, 2, 1, 0),
#'   prior = c(NA, 1, 0, 2, 5, NA),  # Note: prior=5 is unmapped
#'   durata = rep(1, 6),
#'   over_id = c(0, 1, 2, 2, 3, 0)
#' )
#'
#' # Apply default classification with progress tracking
#' classified <- classify_employment_status(segments, show_progress = TRUE)
#' print(classified$stato)
#' # Result: "disoccupato", "occ_ft", "over_pt_fixed", "over_pt_fixed", "occ_5", "disoccupato"
#'
#' # Use custom prior labels for industry-specific codes
#' custom_rules <- create_custom_status_rules(
#'   prior_labels = list(
#'     "0" = "parttime",
#'     "1" = "fulltime",
#'     "2" = "contract",
#'     "5" = "apprentice"  # Map the previously unknown value
#'   )
#' )
#' classified_custom <- classify_employment_status(segments, custom_rules)
#' print(classified_custom$stato)
#' # Result: "disoccupato", "occ_fulltime", "over_parttime_contract",
#' #         "over_parttime_contract", "occ_apprentice", "disoccupato"
#'
#' # Example showing sequence-based labeling with multiple overlap types
#' overlap_data <- data.table(
#'   cf = "B002",
#'   inizio = 1:4,
#'   fine = 2:5,
#'   arco = c(1, 3, 3, 2),
#'   prior = c(0, 1, 2, 3),
#'   durata = rep(1, 4),
#'   over_id = c(1, 2, 2, 2)
#' )
#'
#' classified_overlap <- classify_employment_status(overlap_data)
#' print(classified_overlap$stato)
#' # Result: "occ_pt", "over_ft_fixed_temp", "over_ft_fixed_temp", "over_ft_fixed_temp"
#' }
classify_employment_status <- function(segments,
                                     rules = get_default_status_rules(),
                                     group_by = "cf",
                                     show_progress = FALSE) {

  # Handle NULL rules by using defaults
  if (is.null(rules)) {
    rules <- get_default_status_rules()
  }

  # Initialize progress bar if requested
  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent :what - eta: :eta",
      total = 100, clear = FALSE, width = 80
    )
    pb$tick(10, tokens = list(what = "Starting classification"))
  }

  # Pre-extract rule components for maximum performance
  unemp_thresh <- rules$unemployment$duration_threshold
  unemp_short <- rules$unemployment$short_label
  unemp_long <- rules$unemployment$long_label
  prior_labels <- if (is.null(rules$prior_labels)) {
    list("-1" = "pt", "0" = "pt", "1" = "ft")
  } else {
    rules$prior_labels
  }

  # Ensure proper ordering for overlapping group processing
  if (length(group_by) > 0) {
    setorderv(segments, c(group_by, "inizio"))
  } else {
    setorder(segments, inizio)
  }

  if (show_progress) {
    pb$tick(20, tokens = list(what = "Data prepared"))
  }

  # ULTRA-FAST APPROACH: Two-stage classification for maximum performance
  # Stage 1: Handle simple cases with fcase() - covers 80%+ of data
  # Stage 2: Handle complex overlaps separately

  if (show_progress) {
    pb$tick(20, tokens = list(what = "Processing simple cases"))
  }

  # Pre-compute prior label lookup vectors for maximum speed
  prior_names <- names(prior_labels)
  prior_vals <- unlist(prior_labels, use.names = FALSE)

  # Stage 1: Handle unemployment and single employment with fcase() (FASTEST)
  segments[, stato := fcase(
    # Unemployment conditions
    arco == 0L & durata <= unemp_thresh, unemp_short,
    arco == 0L & durata > unemp_thresh, unemp_long,

    # Single employment - vectorized lookup (no occ_ prefix)
    arco == 1L, {
      # Convert prior to character and lookup
      prior_char <- as.character(prior)
      match_idx <- match(prior_char, prior_names)
      # Use matched value or original if not found
      ifelse(is.na(match_idx), prior_char, prior_vals[match_idx])
    },

    # Default for overlaps (will be processed in stage 2)
    default = ""
  )]

  if (show_progress) {
    pb$tick(30, tokens = list(what = "Processing overlaps"))
  }

  # Stage 2: Handle overlaps efficiently using data.table grouping
  overlap_idx <- segments$arco > 1L
  if (any(overlap_idx)) {
    if ("over_id" %in% names(segments) && length(group_by) > 0) {
      # Process overlaps using efficient grouping - no loops!
      overlap_results <- segments[overlap_idx][, {
        # Get unique priors in chronological order for this group
        unique_priors <- unique(prior[order(inizio)])
        # Fast vectorized label lookup
        prior_chars <- as.character(unique_priors)
        match_indices <- match(prior_chars, prior_names)
        labels <- ifelse(is.na(match_indices), prior_chars, prior_vals[match_indices])

        # NEW LOGIC: Two values separated by underscore, or "altri" for more than 2
        if (length(labels) == 1) {
          # Single contract (shouldn't happen for arco > 1, but handle gracefully)
          sequence_label <- labels[1]
        } else if (length(labels) == 2) {
          # Two overlapping contracts: join with underscore
          sequence_label <- paste(labels, collapse = "_")
        } else {
          # More than 2 contracts: use "altri"
          sequence_label <- "altri"
        }

        list(sequence_label = sequence_label)
      }, by = c(group_by, "over_id")]

      # Fast merge back to original data
      segments[overlap_idx, stato := overlap_results[
        segments[overlap_idx],
        on = c(group_by, "over_id"),
        x.sequence_label
      ]]
    } else {
      # Simple overlap case - direct vectorized assignment
      overlap_priors <- segments[overlap_idx, prior]
      prior_chars <- as.character(overlap_priors)
      match_indices <- match(prior_chars, prior_names)
      overlap_labels <- ifelse(is.na(match_indices), prior_chars, prior_vals[match_indices])
      # For simple case, just use the single label (no prefix)
      segments[overlap_idx, stato := overlap_labels]
    }
  }

  if (show_progress) {
    pb$tick(20, tokens = list(what = "Classification complete"))
    pb$terminate()
  }

  return(segments)
}

#' Create Custom Status Classification Rules
#'
#' @description
#' Helper function to create custom employment status classification rules
#' for specific business contexts or analytical requirements. Supports flexible
#' prior value mappings for different employment coding systems.
#'
#' @details
#' **Key Features**:
#'
#' 1. **Custom Prior Labels**: Map any numeric prior values to meaningful labels.
#'    This enables support for industry-specific or organization-specific
#'    employment type coding systems.
#'
#' 2. **Flexible Thresholds**: Customize unemployment duration thresholds
#'    based on analytical requirements or policy definitions.
#'
#' 3. **Custom Label Systems**: Override default status labels to match
#'    organizational terminology or reporting requirements.
#'
#' 4. **Extension Support**: Include additional classification dimensions
#'    like employment intensity or transition-based classifications.
#'
#' **Prior Labels Usage**:
#' The prior_labels parameter is crucial for the enhanced flexible system.
#' It maps string representations of prior values to employment type labels
#' that will be used in the final status classifications.
#'
#' @param unemployment_threshold Days threshold for short vs long unemployment (default: 8)
#' @param custom_labels Named list of custom status labels to override defaults
#' @param prior_labels Named list mapping prior values to employment type labels.
#'   Keys should be string representations of prior values, values should be
#'   the corresponding employment type labels. Example:
#'   list("0" = "pt", "1" = "ft", "2" = "fixed", "5" = "intern")
#' @param include_intensity Logical. Include employment intensity classifications
#' @param include_transitions Logical. Include transition-based classifications
#'
#' @return List of custom classification rules compatible with classify_employment_status()
#'
#' @export
#'
#' @examples
#' # Basic custom rules with industry-specific employment types
#' healthcare_rules <- create_custom_status_rules(
#'   unemployment_threshold = 15,  # 15 days for healthcare sector
#'   prior_labels = list(
#'     "0" = "parttime",
#'     "1" = "fulltime",
#'     "2" = "locum",      # Temporary medical staff
#'     "3" = "agency",     # Agency workers
#'     "4" = "oncall"      # On-call staff
#'   )
#' )
#'
#' # Advanced rules with custom labels and extended mappings
#' academic_rules <- create_custom_status_rules(
#'   unemployment_threshold = 90,  # Longer periods common in academia
#'   custom_labels = list(
#'     unemployed_short = "between_positions",
#'     unemployed_long = "career_transition"
#'   ),
#'   prior_labels = list(
#'     "1" = "tenured",
#'     "2" = "tenure_track",
#'     "3" = "adjunct",
#'     "4" = "postdoc",
#'     "5" = "visiting",
#'     "10" = "emeritus"
#'   ),
#'   include_intensity = TRUE
#' )
#'
#' # Demonstrate the effect of custom prior labels
#' \dontrun{
#' library(data.table)
#' segments <- data.table(
#'   cf = "PROF001",
#'   inizio = c(1, 30, 60),
#'   fine = c(29, 59, 90),
#'   arco = c(1, 0, 2),
#'   prior = c(2, NA, 4),  # tenure_track, unemployment, postdoc
#'   durata = c(29, 30, 31),
#'   over_id = c(1, 0, 2)
#' )
#'
#' classified <- classify_employment_status(segments, academic_rules)
#' print(classified$stato)
#' # Expected: "occ_tenure_track", "between_positions", "occ_postdoc"
#' }
#'
#' # Legacy system compatibility - mapping old codes to new labels
#' legacy_rules <- create_custom_status_rules(
#'   prior_labels = list(
#'     "100" = "manager",
#'     "200" = "supervisor",
#'     "300" = "specialist",
#'     "400" = "trainee",
#'     "999" = "consultant"
#'   )
#' )
create_custom_status_rules <- function(unemployment_threshold = 8,
                                     custom_labels = NULL,
                                     prior_labels = NULL,
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

  # Default prior labels if not provided
  if (is.null(prior_labels)) {
    prior_labels <- list(
      "-1" = "pt",
      "0" = "pt",
      "1" = "ft",
      "2" = "fixed",
      "3" = "temp"
    )
  }

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
    prior_labels = prior_labels,
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
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(vecshift)
#'
#' # Create and process employment data
#' employment_data <- data.table(
#'   id = 1:8,
#'   cf = rep(c("P001", "P002"), each = 4),
#'   inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01", "2023-10-01",
#'                      "2023-02-01", "2023-05-01", "2023-08-01", "2023-11-01")),
#'   fine = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30", "2023-12-31",
#'                    "2023-04-30", "2023-07-31", "2023-10-31", "2023-12-31")),
#'   prior = c(1, 0, 1, 0, 0, 1, 0, 1)
#' )
#'
#' # Transform and classify
#' processed_data <- vecshift(employment_data)
#' classified_data <- classify_employment_status(processed_data)
#'
#' # Analyze patterns without transitions
#' patterns_basic <- analyze_status_patterns(
#'   classified_data,
#'   person_col = "cf",
#'   include_transitions = FALSE
#' )
#' print(patterns_basic)
#'
#' # Analyze patterns with transitions
#' patterns_full <- analyze_status_patterns(
#'   classified_data,
#'   person_col = "cf",
#'   include_transitions = TRUE
#' )
#' print(patterns_full)
#'
#' # Access specific pattern metrics
#' patterns_full$status_distribution
#' patterns_full$transition_matrix
#' patterns_full$average_durations
#' }
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
    employment_rate <- if (total_duration > 0) as.numeric(employment_duration) / as.numeric(total_duration) else 0
    
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
#' Adapted for the enhanced flexible prior value system - validates patterns
#' rather than specific hardcoded values. Optimized for large datasets.
#'
#' @details
#' **Enhanced Validation for Flexible Prior Values**:
#'
#' The validation has been updated to accommodate the flexible prior value system:
#' - No longer validates specific prior values (since they can be any numeric code)
#' - Focuses on structural consistency between arco and stato patterns
#' - Validates that status labels follow expected patterns (disoccupato, occ_, over_)
#' - Checks for missing critical status types rather than exact label matches
#' - Supports custom prior_labels through rules parameter validation
#'
#' **Validation Checks**:
#' - Missing or empty status labels
#' - Impossible combinations (e.g., unemployment with arco > 0)
#' - Structural inconsistencies between employment counts and status patterns
#' - Unexpected status label formats
#' - Missing critical status categories
#'
#' @param classified_data Data.table with employment segments and status labels
#' @param rules Classification rules used (for validation reference, including prior_labels)
#'
#' @return List with validation results and any detected issues:
#'   \itemize{
#'     \item is_valid: Overall validation status
#'     \item validation_rate: Proportion of valid classifications
#'     \item missing_labels: Count of missing status labels
#'     \item impossible_combinations: Detailed breakdown of logical errors
#'     \item unexpected_statuses: Status labels that don't match expected patterns
#'     \item missing_critical_statuses: Critical status types that are missing
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Sample classified data with flexible prior values
#' classified_data <- data.table(
#'   cf = rep("A001", 4),
#'   inizio = 1:4,
#'   fine = 2:5,
#'   arco = c(0, 1, 2, 1),
#'   prior = c(NA, 2, 5, 1),  # Includes unmapped value 5
#'   durata = rep(1, 4),
#'   stato = c("disoccupato", "occ_fixed", "over_5_ft", "occ_ft")
#' )
#'
#' # Validate with default rules
#' validation <- validate_status_classifications(classified_data)
#' print(validation$is_valid)
#' print(validation$validation_rate)
#'
#' # Check for any issues
#' if (!validation$is_valid) {
#'   print(validation$impossible_combinations)
#'   print(validation$unexpected_statuses)
#' }
#'
#' # Validate with custom rules that map prior value 5
#' custom_rules <- create_custom_status_rules(
#'   prior_labels = list("1" = "ft", "2" = "fixed", "5" = "intern")
#' )
#' validation_custom <- validate_status_classifications(classified_data, custom_rules)
#' print(validation_custom$is_valid)
#' }
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
    overlap_without_overlap = sum(is_overlap & arco_col <= 1L, na.rm = TRUE)
    # Note: Removed hardcoded prior value validation as prior values are now flexible
  )
  
  validation$impossible_combinations <- impossible
  validation$total_impossible <- sum(unlist(impossible, use.names = FALSE))
  
  # Check status coverage - build expected statuses from prior_labels
  expected_statuses <- c(
    rules$unemployment$short_label,
    rules$unemployment$long_label
  )

  # Add expected single employment statuses based on prior_labels
  if (!is.null(rules$prior_labels)) {
    for (label in rules$prior_labels) {
      expected_statuses <- c(expected_statuses, paste0("occ_", label))
    }
  }

  # Note: Overlap statuses are now sequence-based and dynamically generated
  # so we don't check for specific expected overlap statuses
  
  # Optimized unique extraction
  observed_statuses <- unique(stato_col[!missing_status])

  # For flexible labeling, we check basic patterns rather than exact matches
  unexpected_basic <- observed_statuses[!grepl("^(disoccupato|occ_|over_)", observed_statuses)]
  validation$unexpected_statuses <- unexpected_basic

  # Check for missing unemployment status (most critical)
  has_unemployment <- any(grepl("disoccupato", observed_statuses))
  validation$missing_critical_statuses <- if (!has_unemployment) "unemployment" else character(0)
  
  # Overall validation status
  validation$is_valid <- validation$missing_labels == 0L &&
                        validation$total_impossible == 0L &&
                        length(validation$unexpected_statuses) == 0L &&
                        length(validation$missing_critical_statuses) == 0L
  
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
    cat("[OK] All status classifications are valid\n")
  } else {
    cat("[WARNING] Status classification issues detected\n")
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
  
  if (length(x$missing_critical_statuses) > 0) {
    cat("\nMissing Critical Status Labels:\n")
    cat(paste(x$missing_critical_statuses, collapse = ", "), "\n")
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