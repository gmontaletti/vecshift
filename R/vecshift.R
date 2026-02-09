#' Transform Employment Records into Temporal Segments (Core Function)
#'
#' @description
#' Processes employment contract records with temporal boundaries to create
#' continuous temporal segments that track employment status over time. This core
#' function handles the event-based transformation logic, identifying unemployment
#' periods (when no contracts are active), single employment periods, and overlapping
#' employment situations.
#'
#' @details
#' The function implements precise date logic for temporal processing:
#' \itemize{
#'   \item Employment contracts are inclusive of both start and end dates
#'   \item A person works ON both inizio and fine dates
#'   \item End events are created at fine date
#'   \item Unemployment periods are identified (arco=0) and adjusted: inizio+1 and fine-1
#'   \item This ensures correct temporal boundaries for all segments
#' }
#'
#' The algorithm creates events for each contract start (+1) and end (-1 at fine), then uses
#' cumulative sums to track overlapping contracts. Unemployment segments are identified
#' (arco=0) and their dates are adjusted to represent the actual unemployment period.
#'
#' @param dt A data.table containing employment contract records with the following required columns:
#'   \itemize{
#'     \item{\code{id}}: Contract identifier (unique key for each employment contract)
#'     \item{\code{cf}}: Person identifier (e.g., fiscal code)
#'     \item{\code{inizio}}: Contract start date (Date or numeric)
#'     \item{\code{fine}}: Contract end date (Date or numeric)
#'     \item{\code{prior}}: Employment type indicator (0 or negative for part-time, positive for full-time)
#'   }
#'
#' @return A data.table with temporal segments containing:
#'   \itemize{
#'     \item{\code{cf}}: Person identifier
#'     \item{\code{inizio}}: Segment start date
#'     \item{\code{fine}}: Segment end date
#'     \item{\code{arco}}: Number of overlapping contracts (0 = unemployment)
#'     \item{\code{prior}}: Employment type for the segment (0 = part-time, 1 = full-time)
#'     \item{\code{id}}: Contract ID (0 for unemployment periods)
#'     \item{\code{durata}}: Duration of the segment in days
#'   }
#'
#' @note
#' This function performs only the core temporal transformation. For employment
#' status classification, use \code{\link{classify_employment_status}} separately
#' or use \code{\link{process_employment_pipeline}} for a complete processing pipeline.
#'
#' @seealso
#' \code{\link{classify_employment_status}} for status classification details
#' \code{\link{process_employment_pipeline}} for a complete processing pipeline
#'
#' @export
#' @importFrom data.table data.table setorder rbindlist fcase shift chmatch
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Create sample employment data
#' dt <- data.table(
#'   id = 1:3,
#'   cf = c("ABC123", "ABC123", "DEF456"),
#'   inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
#'   fine = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
#'   prior = c(1, 0, 1)  # 1 = full-time, 0 = part-time
#' )
#'
#' # Transform to temporal segments (pure transformation)
#' result <- vecshift(dt)
#' print(result)
#'
#' # Apply status classification separately
#' result_with_status <- classify_employment_status(result)
#' print(result_with_status)
#'
#' # Use complete pipeline for integrated processing
#' result_pipeline <- process_employment_pipeline(dt)
#' print(result_pipeline)
#' }
vecshift <- function(dt) {
  # Input validation
  if (!inherits(dt, "data.table")) {
    stop(
      "Input 'dt' must be a data.table object. Use as.data.table() to convert."
    )
  }

  # Check for required columns
  required_cols <- c("id", "cf", "inizio", "fine", "prior")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Validate column types
  if (!is.numeric(dt$inizio) && !inherits(dt$inizio, "Date")) {
    stop("Column 'inizio' must be numeric or Date type")
  }
  if (!is.numeric(dt$fine) && !inherits(dt$fine, "Date")) {
    stop("Column 'fine' must be numeric or Date type")
  }
  if (!is.numeric(dt$prior)) {
    stop("Column 'prior' must be numeric")
  }

  # Check for logical consistency
  if (any(dt$fine < dt$inizio, na.rm = TRUE)) {
    warning(
      "Some records have fine < inizio. These may produce unexpected results."
    )
  }

  # 1. Pre-compute integer cf mapping for fast radix sort -----
  cf_levels <- unique(dt$cf)

  # 2. Create start/end events -----
  result <- rbindlist(list(
    dt[, .(
      id,
      cf,
      cf_int = chmatch(cf, cf_levels),
      cdata = inizio,
      value = 1,
      prior
    )],
    dt[, .(
      id,
      cf,
      cf_int = chmatch(cf, cf_levels),
      cdata = fine,
      value = -1,
      prior = 0
    )]
  ))

  # 3. Sort on integer key (radix O(n) instead of string O(n*k)) -----
  setorder(result, cf_int, cdata)
  result[, cf_int := NULL]

  # 4. Cumulative overlap count and prior normalization -----
  result[, arco := cumsum(value)]
  result[,
    prior := fcase(
      prior <= 0 , 0 ,
      default = 1
    )
  ]

  # 5. Create segments using shift (avoids 7-column copy) -----
  result[, `:=`(
    acf = shift(cf, 1L, type = "lead"),
    fine = shift(cdata, 1L, type = "lead")
  )]
  result <- result[!is.na(fine)]
  result[, inizio := cdata]
  result[, c("cdata", "value") := NULL]

  # 6. Over_id computation -----
  result[, over_id := (arco > 0)]
  result[shift(over_id, type = "lag") == TRUE, over_id := FALSE]
  result[,
    first_in_over := fcase(
      over_id == TRUE | arco == 0 , 0L ,
      default = -1L
    )
  ]
  result[, over_id := cumsum(over_id)]
  result[arco == 0, over_id := 0]

  # 7. Cross-person boundary filter -----
  result <- result[cf == acf]
  result[, acf := NULL]

  # 8. Unemployment adjustments (consolidated) -----
  result[arco == 0, `:=`(id = 0L, inizio = inizio + 1L, fine = fine - 1L)]

  # 9. Duration calculation and zero-duration filter -----
  result[, durata := 1 + fine - inizio + first_in_over]
  result <- result[durata > 0]
  result[, first_in_over := NULL]

  return(result)
}
