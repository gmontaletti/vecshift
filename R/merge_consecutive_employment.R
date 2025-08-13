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
#' @importFrom data.table data.table setDT copy rleid shift
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
  # Load required package
  require("data.table")
  
  # Input validation
  if (!inherits(dt, "data.table")) {
    stop("Input 'dt' must be a data.table object")
  }
  
  # Validate consolidation_type
  valid_types <- c("both", "overlapping", "consecutive", "none")
  if (!consolidation_type %in% valid_types) {
    stop(paste("consolidation_type must be one of:", paste(valid_types, collapse = ", ")))
  }
  
  # Check for required columns
  required_cols <- c("cf", "inizio", "fine", "arco", "prior")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Early return if no consolidation requested
  if (consolidation_type == "none") {
    dt_result <- copy(dt)
    dt_result[, collapsed := FALSE]  # Add collapsed column for consistency
    return(dt_result)
  }
  
  # Check if over_id column exists
  has_over_id <- "over_id" %in% names(dt)
  if (consolidation_type %in% c("both", "overlapping") && !has_over_id) {
    stop("consolidation_type '", consolidation_type, "' requires over_id column from vecshift() output")
  }
  
  # Make a copy to avoid modifying the original
  dt_work <- copy(dt)
  setDT(dt_work)
  
  # Sort by cf and inizio for consistent processing
  setorder(dt_work, cf, inizio)
  
  # Identify additional columns (beyond the required ones)
  base_cols <- c("cf", "inizio", "fine", "arco", "prior", "durata", "id", "stato", "over_id")
  extra_cols <- setdiff(names(dt_work), base_cols)
  
  # Handle case where there are no extra columns
  if (length(extra_cols) > 0) {
    numeric_extra <- extra_cols[sapply(dt_work[, ..extra_cols], is.numeric)]
    character_extra <- extra_cols[sapply(dt_work[, ..extra_cols], is.character)]
  } else {
    numeric_extra <- character()
    character_extra <- character()
  }
  
  # Apply consolidation strategy
  if (consolidation_type == "both") {
    # Step 1: Consolidate overlapping periods (same over_id > 0)
    dt_step1 <- .consolidate_overlapping(dt_work, numeric_extra, character_extra)
    
    # Step 2: Consolidate consecutive periods from step 1 result
    dt_result <- .consolidate_consecutive(dt_step1, numeric_extra, character_extra)
    
  } else if (consolidation_type == "overlapping") {
    dt_result <- .consolidate_overlapping(dt_work, numeric_extra, character_extra)
    
  } else if (consolidation_type == "consecutive") {
    dt_result <- .consolidate_consecutive(dt_work, numeric_extra, character_extra)
  }
  
  return(dt_result)
}

# Helper function to consolidate overlapping periods (same over_id > 0)
.consolidate_overlapping <- function(dt_work, numeric_extra, character_extra) {
  # Mark employment status
  dt_work[, is_employment := arco > 0]
  
  # Create grouping: same over_id for overlapping employment, unique groups for unemployment
  dt_work[, final_group := ifelse(over_id == 0, 
                                  paste(cf, "unemp", seq_len(.N), sep = "_"),
                                  paste(cf, "over", over_id, sep = "_"))]
  
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
  dt_work[, is_consecutive := is_employment & !is.na(prev_fine) & 
          (inizio == prev_fine | inizio == prev_fine + 1)]
  
  # Create final grouping that respects both employment status and consecutiveness
  dt_work[, final_group := rleid(cf, is_employment, 
                                  cumsum(!is_consecutive | is.na(is_consecutive)))]
  
  # Perform aggregation
  result <- .perform_aggregation(dt_work, numeric_extra, character_extra)
  
  return(result)
}

# Helper function to perform aggregation
.perform_aggregation <- function(dt_work, numeric_extra, character_extra) {
  
  # Add duration calculation for each record for weighting
  dt_work[, record_durata := 1 + as.numeric(fine - inizio)]
  
  # Build list of aggregation expressions
  agg_list <- list()
  agg_list[["inizio"]] <- quote(inizio[1])
  agg_list[["fine"]] <- quote(fine[.N])
  agg_list[["arco"]] <- quote(if(.N == 1 || !is_employment[1]) arco[1] else {
    w <- record_durata[!is.na(arco) & !is.na(record_durata)]
    v <- arco[!is.na(arco) & !is.na(record_durata)]
    if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) round(weighted.mean(v, w = w), 2) else round(mean(arco, na.rm = TRUE), 2)
  })
  agg_list[["prior"]] <- quote(if(.N == 1 || !is_employment[1]) prior[1] else {
    w <- record_durata[!is.na(prior) & !is.na(record_durata)]
    v <- prior[!is.na(prior) & !is.na(record_durata)]
    if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) round(weighted.mean(v, w = w), 2) else round(mean(prior, na.rm = TRUE), 2)
  })
  
  # Include over_id in aggregation if it exists
  if ("over_id" %in% names(dt_work)) {
    agg_list[["over_id"]] <- quote(if(.N == 1 || !is_employment[1]) over_id[1] else over_id[1])
  }
  
  agg_list[["durata"]] <- quote(1 + as.numeric(fine[.N] - inizio[1]))
  agg_list[["collapsed"]] <- quote(.N > 1 & is_employment[1])
  
  # Add aggregations for numeric extra columns
  for (col in numeric_extra) {
    agg_list[[col]] <- substitute(
      if(.N == 1 || !is_employment[1]) COL[1] else {
        w <- record_durata[!is.na(COL) & !is.na(record_durata)]
        v <- COL[!is.na(COL) & !is.na(record_durata)]
        if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) round(weighted.mean(v, w = w), 2) else round(mean(COL, na.rm = TRUE), 2)
      },
      list(COL = as.name(col))
    )
    
    direction_col <- paste0(col, "_direction")
    agg_list[[direction_col]] <- substitute(
      if(.N == 1 || !is_employment[1]) {
        NA_real_
      } else {
        vals <- COL[!is.na(COL)]
        if(length(vals) > 0) round(tail(vals, 1) - vals[1], 2) else NA_real_
      },
      list(COL = as.name(col))
    )
  }
  
  # Add aggregations for character extra columns
  for (col in character_extra) {
    agg_list[[col]] <- substitute(
      if(.N == 1 || !is_employment[1]) {
        COL[1]
      } else {
        vals <- COL[!is.na(COL) & COL != ""]
        if(length(vals) == 0) {
          NA_character_
        } else if(length(unique(vals)) == 1) {
          vals[1]
        } else {
          paste0(vals[1], "->", tail(vals, 1))
        }
      },
      list(COL = as.name(col))
    )
  }
  
  # Aggregate by final_group
  result <- dt_work[, eval(as.call(c(list(as.name("list")), agg_list))), by = .(cf, final_group)]
  
  # Clean up temporary columns
  result[, final_group := NULL]
  
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
merge_consecutive_employment_fast <- function(dt) {
  require("data.table")
  
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
  dt_work[, status_change := employment_status != shift(employment_status, 1L, fill = -1), by = cf]
  dt_work[, group_id := cumsum(status_change), by = cf]
  
  # Check for gaps in employment periods
  dt_work[, prev_fine := shift(fine, 1L, type = "lag"), by = .(cf, group_id)]
  dt_work[, has_gap := employment_status == 1 & !is.na(prev_fine) & 
          (as.numeric(inizio - prev_fine) > 1)]
  dt_work[, subgroup_id := cumsum(has_gap | status_change), by = cf]
  
  # Identify extra columns
  base_cols <- c("cf", "inizio", "fine", "arco", "prior", "durata", "id", "stato")
  extra_cols <- setdiff(names(dt_work), c(base_cols, "employment_status", 
                                            "status_change", "group_id", 
                                            "prev_fine", "has_gap", "subgroup_id"))
  numeric_extra <- extra_cols[sapply(dt_work[, ..extra_cols], is.numeric)]
  character_extra <- extra_cols[sapply(dt_work[, ..extra_cols], is.character)]
  
  # Add duration calculation for each record for weighting
  dt_work[, record_durata := 1 + as.numeric(fine - inizio)]
  
  # Aggregate
  agg_exprs <- list(
    inizio = quote(inizio[1]),
    fine = quote(fine[.N]),
    arco = quote(ifelse(employment_status[1] == 0, arco[1], {
      w <- record_durata[!is.na(arco) & !is.na(record_durata)]
      v <- arco[!is.na(arco) & !is.na(record_durata)]
      if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) round(weighted.mean(v, w = w), 2) else round(mean(arco, na.rm = TRUE), 2)
    })),
    prior = quote(ifelse(.N == 1, prior[1], {
      w <- record_durata[!is.na(prior) & !is.na(record_durata)]
      v <- prior[!is.na(prior) & !is.na(record_durata)]
      if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) round(weighted.mean(v, w = w), 2) else round(mean(prior, na.rm = TRUE), 2)
    })),
    durata = quote(1 + as.numeric(fine[.N] - inizio[1])),
    collapsed = quote(.N > 1 & employment_status[1] == 1),
    n_periods = quote(.N)
  )
  
  # Add aggregation for numeric extras
  for (col in numeric_extra) {
    agg_exprs[[col]] <- substitute(
      ifelse(.N == 1 | employment_status[1] == 0, COL[1], {
        w <- record_durata[!is.na(COL) & !is.na(record_durata)]
        v <- COL[!is.na(COL) & !is.na(record_durata)]
        if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) round(weighted.mean(v, w = w), 2) else round(mean(COL, na.rm = TRUE), 2)
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
  result <- dt_work[, eval(as.call(c(list(as.name("list")), agg_exprs))), 
                     by = .(cf, subgroup_id)]
  
  # Clean up
  result[, subgroup_id := NULL]
  setorder(result, cf, inizio)
  
  return(result)
}