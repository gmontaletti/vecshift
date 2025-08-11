#' Collapse Consecutive Employment Periods
#'
#' @description
#' Collapses consecutive working periods (where arco > 0) into single segments for each person.
#' Consecutive periods are merged into one with the first start date and last end date,
#' maintaining unemployment periods and single-line contracts unchanged.
#'
#' @param dt A data.table output from vecshift() containing temporal segments with columns:
#'   \itemize{
#'     \item{\code{cf}}: Person identifier
#'     \item{\code{inizio}}: Segment start date
#'     \item{\code{fine}}: Segment end date  
#'     \item{\code{arco}}: Number of overlapping contracts (0 = unemployment)
#'     \item{\code{prior}}: Employment type (numeric)
#'     \item{Additional columns}: Any other contract parameters (numeric or character)
#'   }
#'
#' @return A data.table with collapsed consecutive employment periods containing:
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
#' # Create sample output from vecshift
#' dt <- data.table(
#'   cf = c("A", "A", "A", "A", "B", "B"),
#'   inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-02-15", "2023-04-01", 
#'                      "2023-01-01", "2023-03-01")),
#'   fine = as.Date(c("2023-01-31", "2023-02-14", "2023-03-31", "2023-04-30",
#'                    "2023-02-28", "2023-03-31")),
#'   arco = c(1, 0, 1, 1, 1, 0),
#'   prior = c(1, 0, 0.5, 1, 1, 0),
#'   contract_type = c("FT", NA, "PT", "FT", "FT", NA),
#'   salary = c(3000, NA, 1500, 3200, 2800, NA)
#' )
#'
#' # Collapse consecutive employment periods
#' result <- merge_consecutive_employment(dt)
#' print(result)
#' }
merge_consecutive_employment <- function(dt) {
  # Load required package
  require("data.table")
  
  # Input validation
  if (!inherits(dt, "data.table")) {
    stop("Input 'dt' must be a data.table object")
  }
  
  # Check for required columns
  required_cols <- c("cf", "inizio", "fine", "arco", "prior")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Make a copy to avoid modifying the original
  dt_work <- copy(dt)
  setDT(dt_work)
  
  # Identify additional columns (beyond the required ones)
  base_cols <- c("cf", "inizio", "fine", "arco", "prior", "durata", "id", "stato")
  extra_cols <- setdiff(names(dt_work), base_cols)
  
  # Handle case where there are no extra columns
  if (length(extra_cols) > 0) {
    numeric_extra <- extra_cols[sapply(dt_work[, ..extra_cols], is.numeric)]
    character_extra <- extra_cols[sapply(dt_work[, ..extra_cols], is.character)]
  } else {
    numeric_extra <- character()
    character_extra <- character()
  }
  
  # Create grouping variable for consecutive employment periods
  # A new group starts when: arco becomes 0 or becomes > 0 after being 0
  dt_work[, employment_group := rleid(cf, arco > 0)]
  
  # Mark which segments are employment (arco > 0)
  dt_work[, is_employment := arco > 0]
  
  # For employment periods, create sub-groups of truly consecutive periods
  # Consecutive means the next period starts exactly when the previous ends
  dt_work[, prev_fine := shift(fine, 1L, type = "lag"), by = cf]
  dt_work[, is_consecutive := is_employment & !is.na(prev_fine) & 
          (inizio == prev_fine | inizio == prev_fine + 1)]
  
  # Create final grouping that respects both employment status and consecutiveness
  dt_work[, final_group := rleid(cf, is_employment, 
                                  cumsum(!is_consecutive | is.na(is_consecutive)))]
  
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
  
  # Aggregate by group
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