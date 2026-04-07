#' Classify extra columns into numeric and character groups
#'
#' @keywords internal
#' @noRd
.classify_extra_columns <- function(dt, extra_cols) {
  if (length(extra_cols) == 0L) {
    return(list(numeric = character(0), character = character(0)))
  }
  is_numeric <- vapply(
    extra_cols,
    function(col) {
      cls <- class(dt[[col]])
      any(c("numeric", "integer") %in% cls)
    },
    logical(1)
  )
  is_character <- vapply(
    extra_cols,
    function(col) {
      cls <- class(dt[[col]])
      any(c("character", "factor") %in% cls)
    },
    logical(1)
  )
  list(
    numeric = extra_cols[is_numeric],
    character = extra_cols[is_character]
  )
}

#' Merge Employment Periods with Multiple Consolidation Modes
#'
#' @description
#' Merges employment periods using different consolidation strategies based on the over_id column
#' from vecshift() output. The over_id column identifies continuous overlapping employment periods,
#' enabling sophisticated consolidation approaches for employment history analysis.
#'
#' @param dt A data.table output from vecshift() containing temporal segments with columns:
#'   \itemize{
#'     \item{\code{cf}}: Person identifier
#'     \item{\code{inizio}}: Segment start date
#'     \item{\code{fine}}: Segment end date
#'     \item{\code{arco}}: Number of overlapping contracts (0 = unemployment)
#'     \item{\code{prior}}: Employment type (numeric)
#'     \item{\code{over_id}}: Overlap identifier (0 = unemployment, >0 = overlapping periods)
#'     \item{Additional columns}: Any other contract parameters (numeric or character)
#'   }
#' @param consolidation_type Character string specifying consolidation approach:
#'   \itemize{
#'     \item{\code{"both"} (DEFAULT)}: First consolidate overlapping periods (same over_id > 0),
#'           then merge consecutive periods. Provides complete employment history consolidation.
#'     \item{\code{"overlapping"}}: Only consolidate segments with same over_id > 0.
#'           Merges simultaneous/overlapping contracts into single periods.
#'     \item{\code{"consecutive"}}: Only merge periods that are contiguous in time,
#'           regardless of over_id. Traditional consecutive period merging.
#'     \item{\code{"none"}}: No consolidation. Returns original segments unchanged.
#'   }
#'
#' @return A data.table with consolidated employment periods containing:
#'   \itemize{
#'     \item{\code{cf}}: Person identifier
#'     \item{\code{inizio}}: Period start date
#'     \item{\code{fine}}: Period end date
#'     \item{\code{arco}}: Number of overlapping contracts (preserved for unemployment)
#'     \item{\code{prior}}: Duration-weighted mean priority for collapsed periods
#'     \item{Character columns}: Concatenated as "first->last" for collapsed periods
#'     \item{Numeric columns}: Duration-weighted mean value for collapsed periods
#'     \item{Numeric columns with "_direction" suffix}: Direction of change (last - first)
#'     \item{\code{durata}}: Duration of the period in days
#'     \item{\code{collapsed}}: Logical indicating if period was collapsed (TRUE) or not (FALSE)
#'   }
#'
#' @export
#' @importFrom data.table data.table setDT copy rleid shift fifelse
#' @importFrom stats weighted.mean
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Create sample output from vecshift with over_id
#' dt <- data.table(
#'   cf = c("A", "A", "A", "A", "B", "B"),
#'   inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-02-15", "2023-04-01",
#'                      "2023-01-01", "2023-03-01")),
#'   fine = as.Date(c("2023-01-31", "2023-02-14", "2023-03-31", "2023-04-30",
#'                    "2023-02-28", "2023-03-31")),
#'   arco = c(1, 0, 1, 1, 1, 0),
#'   prior = c(1, 0, 0.5, 1, 1, 0),
#'   over_id = c(1, 0, 2, 3, 1, 0),
#'   contract_type = c("FT", NA, "PT", "FT", "FT", NA),
#'   salary = c(3000, NA, 1500, 3200, 2800, NA)
#' )
#'
#' # Complete consolidation (overlapping then consecutive)
#' result_both <- merge_consecutive_employment(dt, consolidation_type = "both")
#'
#' # Only merge overlapping contracts (same over_id > 0)
#' result_overlapping <- merge_consecutive_employment(dt, consolidation_type = "overlapping")
#'
#' # Only merge consecutive periods (traditional approach)
#' result_consecutive <- merge_consecutive_employment(dt, consolidation_type = "consecutive")
#'
#' # No consolidation (return original)
#' result_none <- merge_consecutive_employment(dt, consolidation_type = "none")
#' }
merge_consecutive_employment <- function(dt, consolidation_type = "both") {
  # Input validation
  if (!inherits(dt, "data.table")) {
    stop("Input 'dt' must be a data.table object")
  }

  # Validate consolidation_type
  valid_types <- c("both", "overlapping", "consecutive", "none")
  if (!consolidation_type %in% valid_types) {
    stop(paste(
      "consolidation_type must be one of:",
      paste(valid_types, collapse = ", ")
    ))
  }

  # Check for required columns
  required_cols <- c("cf", "inizio", "fine", "arco", "prior")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Early return for empty input
  if (nrow(dt) == 0L) {
    return(data.table::copy(dt))
  }

  # Early return if no consolidation requested
  if (consolidation_type == "none") {
    dt_result <- copy(dt)
    dt_result[, collapsed := FALSE] # Add collapsed column for consistency
    return(dt_result)
  }

  # Check if over_id column exists
  has_over_id <- "over_id" %in% names(dt)
  if (consolidation_type %in% c("both", "overlapping") && !has_over_id) {
    stop(
      "consolidation_type '",
      consolidation_type,
      "' requires over_id column from vecshift() output"
    )
  }

  # Make a copy to avoid modifying the original
  dt_work <- copy(dt)
  setDT(dt_work)

  # Sort by cf and inizio for consistent processing
  setorder(dt_work, cf, inizio)

  # Identify additional columns (beyond the required ones)
  base_cols <- c(
    "cf",
    "inizio",
    "fine",
    "arco",
    "prior",
    "durata",
    "id",
    "stato",
    "over_id"
  )
  extra_cols <- setdiff(names(dt_work), base_cols)

  # Classify extra columns by type
  .col_class <- .classify_extra_columns(dt_work, extra_cols)
  numeric_extra <- .col_class$numeric
  character_extra <- .col_class$character

  # Apply consolidation strategy
  if (consolidation_type == "both") {
    # Step 1: Consolidate overlapping periods (same over_id > 0)
    dt_step1 <- .consolidate_overlapping(
      dt_work,
      numeric_extra,
      character_extra
    )

    # Step 2: Consolidate consecutive periods from step 1 result
    dt_result <- .consolidate_consecutive(
      dt_step1,
      numeric_extra,
      character_extra
    )
  } else if (consolidation_type == "overlapping") {
    dt_result <- .consolidate_overlapping(
      dt_work,
      numeric_extra,
      character_extra
    )
  } else if (consolidation_type == "consecutive") {
    dt_result <- .consolidate_consecutive(
      dt_work,
      numeric_extra,
      character_extra
    )
  }

  return(dt_result)
}

# Helper function to consolidate overlapping periods (same over_id > 0)
.consolidate_overlapping <- function(dt_work, numeric_extra, character_extra) {
  # Mark employment status
  dt_work[, is_employment := arco > 0]

  # Create grouping: numeric IDs (negative .I for unemployment uniqueness, over_id for employment)
  dt_work[, final_group := fifelse(over_id == 0L, -.I, as.integer(over_id))]

  # Perform aggregation
  result <- .perform_aggregation(dt_work, numeric_extra, character_extra)

  return(result)
}

# Helper function to consolidate consecutive periods
.consolidate_consecutive <- function(dt_work, numeric_extra, character_extra) {
  # Mark employment status
  dt_work[, is_employment := arco > 0]

  # Create grouping variable for consecutive employment periods
  dt_work[, employment_group := rleid(cf, arco > 0)]

  # For employment periods, create sub-groups of truly consecutive periods
  dt_work[, prev_fine := shift(fine, 1L, type = "lag"), by = cf]
  dt_work[,
    is_consecutive := is_employment &
      !is.na(prev_fine) &
      (inizio == prev_fine | inizio == prev_fine + 1)
  ]

  # Create final grouping that respects both employment status and consecutiveness
  dt_work[,
    final_group := rleid(
      cf,
      is_employment,
      cumsum(!is_consecutive | is.na(is_consecutive))
    )
  ]

  # Perform aggregation
  result <- .perform_aggregation(dt_work, numeric_extra, character_extra)

  return(result)
}

# Helper function to perform aggregation
#
# Optimized strategy (v1.2.0):
# 1. Pre-compute group sizes via .N once
# 2. Single-row groups: no aggregation needed, just project columns
# 3. Multi-row groups: split into employment (true aggregation) and
#    non-employment (first-row pickup) and process vectorized over .SDcols
# 4. rbindlist the parts at the end
#
# Output column ordering and semantics MUST match the legacy implementation
# (preserved as .perform_aggregation_legacy below for reference/testing).
.perform_aggregation <- function(dt_work, numeric_extra, character_extra) {
  # Add duration calculation for each record for weighting
  dt_work[, record_durata := 1 + as.numeric(fine - inizio)]

  has_over_id <- "over_id" %in% names(dt_work)
  has_stato <- "stato" %in% names(dt_work)

  # Compute group sizes once
  dt_work[, .group_n := .N, by = .(cf, final_group)]

  # Determine output column ordering exactly as the legacy builder produced it.
  # Legacy order:
  #   inizio, fine, arco, prior, [over_id], [stato], durata, collapsed,
  #   then for each numeric_extra: <col>, <col>_direction,
  #   then each character_extra
  out_cols <- c("inizio", "fine", "arco", "prior")
  if (has_over_id) {
    out_cols <- c(out_cols, "over_id")
  }
  if (has_stato) {
    out_cols <- c(out_cols, "stato")
  }
  out_cols <- c(out_cols, "durata", "collapsed")
  if (length(numeric_extra) > 0) {
    for (col in numeric_extra) {
      out_cols <- c(out_cols, col, paste0(col, "_direction"))
    }
  }
  if (length(character_extra) > 0) {
    out_cols <- c(out_cols, character_extra)
  }
  # by-cols come first in data.table results
  final_cols <- c("cf", "final_group", out_cols)

  # ---- Part A: single-row groups (no aggregation) ----
  single_idx <- which(dt_work$.group_n == 1L)
  if (length(single_idx) > 0L) {
    dt_single <- dt_work[single_idx]
    # Build the result columns directly from the source row
    single_res <- data.table::data.table(
      cf = dt_single$cf,
      final_group = dt_single$final_group
    )
    single_res[, inizio := dt_single$inizio]
    single_res[, fine := dt_single$fine]
    single_res[, arco := dt_single$arco]
    single_res[, prior := dt_single$prior]
    if (has_over_id) {
      single_res[, over_id := dt_single$over_id]
    }
    if (has_stato) {
      single_res[, stato := dt_single$stato]
    }
    single_res[, durata := 1 + as.numeric(dt_single$fine - dt_single$inizio)]
    single_res[, collapsed := FALSE]
    if (length(numeric_extra) > 0) {
      for (col in numeric_extra) {
        single_res[, (col) := dt_single[[col]]]
        single_res[, (paste0(col, "_direction")) := NA_real_]
      }
    }
    if (length(character_extra) > 0) {
      for (col in character_extra) {
        single_res[, (col) := dt_single[[col]]]
      }
    }
  } else {
    single_res <- NULL
  }

  # ---- Part B: multi-row groups ----
  multi_dt <- dt_work[.group_n > 1L]
  if (nrow(multi_dt) > 0L) {
    # Sub-split: non-employment multi-row groups behave like "first row"
    # extraction for most columns (legacy: !is_employment[1] -> col[1]).
    # Employment multi-row groups use the weighted aggregation.

    # Helper: weighted mean preserving legacy NA semantics
    .wmean <- function(v, w) {
      keep <- !is.na(v) & !is.na(w)
      vk <- v[keep]
      wk <- w[keep]
      if (length(wk) > 0L && sum(wk, na.rm = TRUE) > 0) {
        round(stats::weighted.mean(vk, w = wk), 2)
      } else {
        round(mean(v, na.rm = TRUE), 2)
      }
    }

    # Direction for numeric: tail(vals, 1) - vals[1] over !is.na
    .direction <- function(v) {
      vals <- v[!is.na(v)]
      if (length(vals) > 0L) {
        round(vals[length(vals)] - vals[1L], 2)
      } else {
        NA_real_
      }
    }

    # Character merge: legacy semantics
    .char_merge <- function(v) {
      vals <- v[!is.na(v) & v != ""]
      if (length(vals) == 0L) {
        NA_character_
      } else if (length(unique(vals)) == 1L) {
        vals[1L]
      } else {
        paste0(vals[1L], "->", vals[length(vals)])
      }
    }

    # stato merge: legacy semantics (most frequent, or first if tie)
    .stato_merge <- function(v) {
      sv <- v[!is.na(v) & v != ""]
      if (length(sv) == 0L) {
        NA_character_
      } else if (length(unique(sv)) == 1L) {
        sv[1L]
      } else {
        st <- table(sv)
        names(st)[which.max(st)]
      }
    }

    # Build the aggregation expression list. Because the conditional
    # branches depend on is_employment[1] (constant within a group given
    # the calling code), we evaluate them per-group but using vectorised
    # primitives instead of substitute()/eval().

    multi_agg_call <- quote(list(
      inizio = inizio[1L],
      fine = fine[.N],
      arco = if (!is_employment[1L]) arco[1L] else .wmean(arco, record_durata),
      prior = if (!is_employment[1L]) {
        prior[1L]
      } else {
        .wmean(prior, record_durata)
      },
      durata = 1 + as.numeric(fine[.N] - inizio[1L]),
      collapsed = is_employment[1L] # .N > 1 always here
    ))

    multi_res <- multi_dt[, eval(multi_agg_call), by = .(cf, final_group)]

    if (has_over_id) {
      ov <- multi_dt[, .(over_id = over_id[1L]), by = .(cf, final_group)]
      multi_res[, over_id := ov$over_id]
    }
    if (has_stato) {
      st <- multi_dt[,
        .(stato = if (!is_employment[1L]) stato[1L] else .stato_merge(stato)),
        by = .(cf, final_group)
      ]
      multi_res[, stato := st$stato]
    }

    # Numeric extras: weighted mean + direction (two separate aggregations
    # so column names are unambiguous)
    if (length(numeric_extra) > 0L) {
      num_mean <- multi_dt[,
        lapply(.SD, function(v) {
          if (!is_employment[1L]) v[1L] else .wmean(v, record_durata)
        }),
        by = .(cf, final_group),
        .SDcols = numeric_extra
      ]
      num_dir <- multi_dt[,
        lapply(.SD, function(v) {
          if (!is_employment[1L]) NA_real_ else .direction(v)
        }),
        by = .(cf, final_group),
        .SDcols = numeric_extra
      ]
      for (col in numeric_extra) {
        multi_res[, (col) := num_mean[[col]]]
        multi_res[, (paste0(col, "_direction")) := num_dir[[col]]]
      }
    }

    # Character extras
    if (length(character_extra) > 0L) {
      chr_agg <- multi_dt[,
        lapply(.SD, function(v) {
          if (!is_employment[1L]) v[1L] else .char_merge(v)
        }),
        by = .(cf, final_group),
        .SDcols = character_extra
      ]
      for (col in character_extra) {
        multi_res[, (col) := chr_agg[[col]]]
      }
    }
  } else {
    multi_res <- NULL
  }

  # ---- Combine ----
  if (!is.null(single_res) && !is.null(multi_res)) {
    # Align column order before rbindlist
    data.table::setcolorder(single_res, final_cols)
    data.table::setcolorder(multi_res, final_cols)
    result <- data.table::rbindlist(
      list(single_res, multi_res),
      use.names = TRUE
    )
  } else if (!is.null(single_res)) {
    data.table::setcolorder(single_res, final_cols)
    result <- single_res
  } else if (!is.null(multi_res)) {
    data.table::setcolorder(multi_res, final_cols)
    result <- multi_res
  } else {
    # Empty input edge case
    result <- dt_work[0L, .SD, .SDcols = intersect(final_cols, names(dt_work))]
    result[, collapsed := logical(0)]
  }

  # Drop helper grouping column
  if ("final_group" %in% names(result)) {
    result[, final_group := NULL]
  }

  # Sort by cf and inizio
  setorder(result, cf, inizio)

  return(result)
}

#' Alternative Fast Implementation Using Event-Based Approach
#'
#' @description
#' A faster implementation inspired by vecshift's event-based mechanism for
#' collapsing consecutive employment periods.
#'
#' @param dt A data.table output from vecshift()
#' @return A data.table with collapsed consecutive employment periods
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(vecshift)
#'
#' # Create processed data with consecutive employment periods
#' employment_data <- data.table(
#'   id = 1:6,
#'   cf = rep("P001", 6),
#'   inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01",
#'                      "2023-05-01", "2023-08-01", "2023-10-01")),
#'   fine = as.Date(c("2023-01-31", "2023-03-31", "2023-04-30",
#'                    "2023-07-31", "2023-09-30", "2023-12-31")),
#'   prior = c(1, 1, 0, 0, 1, 1)
#' )
#'
#' # Process with vecshift
#' processed_data <- vecshift(employment_data)
#'
#' cat("Original segments:", nrow(processed_data), "\n")
#'
#' # Fast merge - consolidates consecutive same-type employment
#' merged_fast <- merge_consecutive_employment_fast(processed_data)
#'
#' cat("After fast merge:", nrow(merged_fast), "\n")
#' print(merged_fast[, .(cf, inizio, fine, arco, durata)])
#'
#' # Compare with standard merge for validation
#' merged_standard <- merge_consecutive_employment(processed_data)
#'
#' # Both methods should produce same number of segments
#' cat("\nStandard merge:", nrow(merged_standard), "segments\n")
#' cat("Fast merge:", nrow(merged_fast), "segments\n")
#' cat("Results match:", nrow(merged_fast) == nrow(merged_standard), "\n")
#' }
merge_consecutive_employment_fast <- function(dt) {
  # Input validation
  if (!inherits(dt, "data.table")) {
    stop("Input 'dt' must be a data.table object")
  }

  dt_work <- copy(dt)
  setDT(dt_work)

  # Sort by cf and inizio
  setorder(dt_work, cf, inizio)

  # Identify transitions between employment and unemployment
  dt_work[, employment_status := ifelse(arco > 0, 1, 0)]
  dt_work[,
    status_change := employment_status !=
      shift(employment_status, 1L, fill = -1),
    by = cf
  ]
  dt_work[, group_id := cumsum(status_change), by = cf]

  # Check for gaps in employment periods
  dt_work[, prev_fine := shift(fine, 1L, type = "lag"), by = .(cf, group_id)]
  dt_work[,
    has_gap := employment_status == 1 &
      !is.na(prev_fine) &
      (as.numeric(inizio - prev_fine) > 1)
  ]
  dt_work[, subgroup_id := cumsum(has_gap | status_change), by = cf]

  # Identify extra columns
  base_cols <- c(
    "cf",
    "inizio",
    "fine",
    "arco",
    "prior",
    "durata",
    "id",
    "stato",
    "over_id"
  )
  extra_cols <- setdiff(
    names(dt_work),
    c(
      base_cols,
      "employment_status",
      "status_change",
      "group_id",
      "prev_fine",
      "has_gap",
      "subgroup_id"
    )
  )

  # Classify extra columns by type
  .col_class <- .classify_extra_columns(dt_work, extra_cols)
  numeric_extra <- .col_class$numeric
  character_extra <- .col_class$character

  # Add duration calculation for each record for weighting
  dt_work[, record_durata := 1 + as.numeric(fine - inizio)]

  # Aggregate
  agg_exprs <- list(
    inizio = quote(inizio[1]),
    fine = quote(fine[.N]),
    arco = quote(ifelse(employment_status[1] == 0, arco[1], {
      w <- record_durata[!is.na(arco) & !is.na(record_durata)]
      v <- arco[!is.na(arco) & !is.na(record_durata)]
      if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
        round(weighted.mean(v, w = w), 2)
      } else {
        round(mean(arco, na.rm = TRUE), 2)
      }
    })),
    prior = quote(ifelse(.N == 1, prior[1], {
      w <- record_durata[!is.na(prior) & !is.na(record_durata)]
      v <- prior[!is.na(prior) & !is.na(record_durata)]
      if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
        round(weighted.mean(v, w = w), 2)
      } else {
        round(mean(prior, na.rm = TRUE), 2)
      }
    })),
    durata = quote(1 + as.numeric(fine[.N] - inizio[1])),
    collapsed = quote(.N > 1 & employment_status[1] == 1),
    n_periods = quote(as.integer(.N))
  )

  # Include over_id in aggregation if it exists
  if ("over_id" %in% names(dt_work)) {
    agg_exprs[["over_id"]] <- quote(
      if (.N == 1 || employment_status[1] == 0) over_id[1] else over_id[1]
    )
  }

  # Include stato (employment status) in aggregation if it exists
  if ("stato" %in% names(dt_work)) {
    agg_exprs[["stato"]] <- quote(
      if (.N == 1 || employment_status[1] == 0) {
        stato[1]
      } else {
        # For collapsed employment periods, prioritize the most common status
        # or use the first status if all are different
        stato_vals <- stato[!is.na(stato) & stato != ""]
        if (length(stato_vals) == 0) {
          NA_character_
        } else if (length(unique(stato_vals)) == 1) {
          stato_vals[1]
        } else {
          # Use the most frequent status, or first if tie
          stato_table <- table(stato_vals)
          names(stato_table)[which.max(stato_table)]
        }
      }
    )
  }

  # Add aggregation for numeric extras
  for (col in numeric_extra) {
    agg_exprs[[col]] <- substitute(
      ifelse(.N == 1 | employment_status[1] == 0, COL[1], {
        w <- record_durata[!is.na(COL) & !is.na(record_durata)]
        v <- COL[!is.na(COL) & !is.na(record_durata)]
        if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
          round(weighted.mean(v, w = w), 2)
        } else {
          round(mean(COL, na.rm = TRUE), 2)
        }
      }),
      list(COL = as.name(col))
    )

    direction_col <- paste0(col, "_direction")
    agg_exprs[[direction_col]] <- substitute(
      ifelse(.N == 1 | employment_status[1] == 0, NA_real_, {
        vals <- COL[!is.na(COL)]
        if (length(vals) > 0) round(tail(vals, 1) - vals[1], 2) else NA_real_
      }),
      list(COL = as.name(col))
    )
  }

  # Add aggregation for character extras
  for (col in character_extra) {
    agg_exprs[[col]] <- substitute(
      if (.N == 1 | employment_status[1] == 0) {
        COL[1]
      } else {
        vals <- COL[!is.na(COL) & COL != ""]
        if (length(vals) == 0) {
          NA_character_
        } else if (length(unique(vals)) == 1) {
          vals[1]
        } else {
          paste0(vals[1], "->", tail(vals, 1))
        }
      },
      list(COL = as.name(col))
    )
  }

  # Perform aggregation
  result <- dt_work[,
    eval(as.call(c(list(as.name("list")), agg_exprs))),
    by = .(cf, subgroup_id)
  ]

  # Clean up
  result[, subgroup_id := NULL]
  setorder(result, cf, inizio)

  return(result)
}

#' Fast consolidation with consolidation_type support
#'
#' High-performance version of merge_consecutive_employment with consolidation_type support
#' @param dt data.table object from vecshift() output
#' @param consolidation_type Character string specifying consolidation approach:
#'   \itemize{
#'     \item{"both"}: Consolidate both overlapping (same over_id > 0) and consecutive periods
#'     \item{"overlapping"}: Only consolidate periods with same over_id > 0
#'     \item{"consecutive"}: Only consolidate consecutive periods (traditional approach)
#'     \item{"none"}: No consolidation, just add collapsed column and return
#'   }
#' @return data.table with consolidated periods
merge_consecutive_employment_fast_with_type <- function(
  dt,
  consolidation_type = "both"
) {
  # Input validation
  if (!inherits(dt, "data.table")) {
    stop("Input 'dt' must be a data.table object")
  }

  # Validate consolidation_type
  valid_types <- c("both", "overlapping", "consecutive", "none")
  if (!consolidation_type %in% valid_types) {
    stop(paste(
      "consolidation_type must be one of:",
      paste(valid_types, collapse = ", ")
    ))
  }

  # Early return if no consolidation requested
  if (consolidation_type == "none") {
    dt_result <- copy(dt)
    dt_result[, collapsed := FALSE] # Add collapsed column for consistency
    return(dt_result)
  }

  # Check for over_id column if needed
  has_over_id <- "over_id" %in% names(dt)
  if (consolidation_type %in% c("both", "overlapping") && !has_over_id) {
    stop(
      "consolidation_type '",
      consolidation_type,
      "' requires over_id column from vecshift() output"
    )
  }

  dt_work <- copy(dt)
  setDT(dt_work)

  # Sort by cf and inizio
  setorder(dt_work, cf, inizio)

  # Create consolidation groups based on type
  if (consolidation_type == "overlapping") {
    # Only group by over_id for employment periods
    dt_work[, employment_status := ifelse(arco > 0, 1, 0)]
    dt_work[,
      consolidation_group := ifelse(
        employment_status == 1 & over_id > 0,
        paste0(cf, "_", over_id),
        paste0(cf, "_", .I)
      )
    ] # Each unemployment period gets unique group
  } else {
    # For "consecutive" or "both", identify transitions and gaps
    dt_work[, employment_status := ifelse(arco > 0, 1, 0)]
    dt_work[,
      status_change := employment_status !=
        shift(employment_status, 1L, fill = -1),
      by = cf
    ]

    if (consolidation_type == "consecutive") {
      # Traditional consecutive approach - only based on employment/unemployment transitions
      dt_work[, group_id := cumsum(status_change), by = cf]
      dt_work[,
        prev_fine := shift(fine, 1L, type = "lag"),
        by = .(cf, group_id)
      ]
      dt_work[,
        has_gap := employment_status == 1 &
          !is.na(prev_fine) &
          (as.numeric(inizio - prev_fine) > 1)
      ]
      dt_work[,
        consolidation_group := paste0(cf, "_", cumsum(has_gap | status_change))
      ]
    } else {
      # "both" - use over_id for overlapping, then consecutive logic
      if (has_over_id) {
        # First consolidate by over_id for employment periods
        dt_work[,
          temp_group := ifelse(
            employment_status == 1 & over_id > 0,
            paste0("emp_", over_id),
            paste0(
              "unemp_",
              cumsum(
                employment_status == 0 &
                  (is.na(shift(employment_status)) |
                    shift(employment_status) == 1)
              )
            )
          )
        ]
        dt_work[, consolidation_group := paste0(cf, "_", temp_group)]
        dt_work[, temp_group := NULL]
      } else {
        # Fall back to consecutive approach if no over_id
        dt_work[, group_id := cumsum(status_change), by = cf]
        dt_work[,
          prev_fine := shift(fine, 1L, type = "lag"),
          by = .(cf, group_id)
        ]
        dt_work[,
          has_gap := employment_status == 1 &
            !is.na(prev_fine) &
            (as.numeric(inizio - prev_fine) > 1)
        ]
        dt_work[,
          consolidation_group := paste0(
            cf,
            "_",
            cumsum(has_gap | status_change)
          )
        ]
      }
    }
  }

  # Identify extra columns
  base_cols <- c(
    "cf",
    "inizio",
    "fine",
    "arco",
    "prior",
    "durata",
    "id",
    "stato",
    "over_id"
  )
  internal_cols <- c(
    "employment_status",
    "status_change",
    "group_id",
    "prev_fine",
    "has_gap",
    "consolidation_group"
  )
  extra_cols <- setdiff(names(dt_work), c(base_cols, internal_cols))

  # Classify extra columns by type
  .col_class <- .classify_extra_columns(dt_work, extra_cols)
  numeric_extra <- .col_class$numeric
  character_extra <- .col_class$character

  # Add duration calculation for each record for weighting
  dt_work[, record_durata := 1 + as.numeric(fine - inizio)]

  # Aggregate by consolidation groups
  agg_exprs <- list(
    cf = quote(cf[1]),
    inizio = quote(inizio[1]),
    fine = quote(fine[.N]),
    arco = quote(ifelse(employment_status[1] == 0, arco[1], {
      w <- record_durata[!is.na(arco) & !is.na(record_durata)]
      v <- arco[!is.na(arco) & !is.na(record_durata)]
      if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
        round(weighted.mean(v, w = w), 2)
      } else {
        round(mean(arco, na.rm = TRUE), 2)
      }
    })),
    prior = quote(ifelse(.N == 1, prior[1], {
      w <- record_durata[!is.na(prior) & !is.na(record_durata)]
      v <- prior[!is.na(prior) & !is.na(record_durata)]
      if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
        round(weighted.mean(v, w = w), 2)
      } else {
        round(mean(prior, na.rm = TRUE), 2)
      }
    })),
    durata = quote(1 + as.numeric(fine[.N] - inizio[1])),
    collapsed = quote(.N > 1 & employment_status[1] == 1),
    n_periods = quote(as.integer(.N))
  )

  # Include over_id in aggregation if it exists
  if ("over_id" %in% names(dt_work)) {
    agg_exprs[["over_id"]] <- quote(
      if (.N == 1 || employment_status[1] == 0) over_id[1] else over_id[1]
    )
  }

  # Include stato (employment status) in aggregation if it exists
  if ("stato" %in% names(dt_work)) {
    agg_exprs[["stato"]] <- quote(
      if (.N == 1 || employment_status[1] == 0) {
        stato[1]
      } else {
        # For collapsed employment periods, prioritize the most common status
        # or use the first status if all are different
        stato_vals <- stato[!is.na(stato) & stato != ""]
        if (length(stato_vals) == 0) {
          NA_character_
        } else if (length(unique(stato_vals)) == 1) {
          stato_vals[1]
        } else {
          # Use the most frequent status, or first if tie
          stato_table <- table(stato_vals)
          names(stato_table)[which.max(stato_table)]
        }
      }
    )
  }

  # Add aggregation for numeric extras
  for (col in numeric_extra) {
    agg_exprs[[col]] <- substitute(
      ifelse(.N == 1 | employment_status[1] == 0, COL[1], {
        w <- record_durata[!is.na(COL) & !is.na(record_durata)]
        v <- COL[!is.na(COL) & !is.na(record_durata)]
        if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
          round(weighted.mean(v, w = w), 2)
        } else {
          round(mean(COL, na.rm = TRUE), 2)
        }
      }),
      list(COL = as.name(col))
    )

    direction_col <- paste0(col, "_direction")
    agg_exprs[[direction_col]] <- substitute(
      ifelse(.N == 1 | employment_status[1] == 0, NA_real_, {
        vals <- COL[!is.na(COL)]
        if (length(vals) > 0) round(tail(vals, 1) - vals[1], 2) else NA_real_
      }),
      list(COL = as.name(col))
    )
  }

  # Add aggregation for character extras
  for (col in character_extra) {
    agg_exprs[[col]] <- substitute(
      if (.N == 1 | employment_status[1] == 0) {
        COL[1]
      } else {
        vals <- COL[!is.na(COL) & COL != ""]
        if (length(vals) == 0) {
          NA_character_
        } else if (length(unique(vals)) == 1) {
          vals[1]
        } else {
          paste0(vals[1], "->", tail(vals, 1))
        }
      },
      list(COL = as.name(col))
    )
  }

  # Perform aggregation by consolidation groups
  result <- dt_work[,
    eval(as.call(c(list(as.name("list")), agg_exprs))),
    by = .(consolidation_group)
  ]

  # Clean up
  result[, consolidation_group := NULL]
  setorder(result, cf, inizio)

  return(result)
}
