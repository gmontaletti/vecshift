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
#'   \item A person works ON both INIZIO and FINE dates
#'   \item End events are created at FINE date
#'   \item Unemployment periods are identified (arco=0) and adjusted: inizio+1 and fine-1
#'   \item This ensures correct temporal boundaries for all segments
#' }
#'
#' The algorithm creates events for each contract start (+1) and end (-1 at FINE), then uses
#' cumulative sums to track overlapping contracts. Unemployment segments are identified
#' (arco=0) and their dates are adjusted to represent the actual unemployment period.
#'
#' @param dt A data.table containing employment contract records with the following required columns:
#'   \itemize{
#'     \item{\code{id}}: Contract identifier (unique key for each employment contract)
#'     \item{\code{cf}}: Person identifier (e.g., fiscal code)
#'     \item{\code{INIZIO}}: Contract start date (Date or numeric)
#'     \item{\code{FINE}}: Contract end date (Date or numeric)
#'     \item{\code{prior}}: Employment type indicator (0 or negative for part-time, positive for full-time)
#'   }
#' @param classify_status Logical. If TRUE (default), applies employment status classification.
#'   Set to FALSE to return raw segments without status labels.
#' @param status_rules Optional custom status classification rules. If NULL (default),
#'   uses standard classification. See \code{\link{get_default_status_rules}} for structure.
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
#'     \item{\code{stato}}: Employment status classification (if classify_status = TRUE)
#'   }
#'
#' @note
#' Status classification is delegated to the \code{\link{classify_employment_status}}
#' function from the status_labeling module, ensuring consistent and customizable
#' employment status attribution across the package.
#'
#' @seealso
#' \code{\link{classify_employment_status}} for status classification details
#' \code{\link{get_default_status_rules}} for default classification rules
#' \code{\link{create_custom_status_rules}} for creating custom rules
#'
#' @export
#' @importFrom data.table data.table setorder rbindlist fcase
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Create sample employment data
#' dt <- data.table(
#'   id = 1:3,
#'   cf = c("ABC123", "ABC123", "DEF456"),
#'   INIZIO = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
#'   FINE = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
#'   prior = c(1, 0, 1)  # 1 = full-time, 0 = part-time
#' )
#'
#' # Transform to temporal segments with status classification
#' result <- vecshift(dt)
#' print(result)
#'
#' # Transform without status classification
#' result_raw <- vecshift(dt, classify_status = FALSE)
#' print(result_raw)
#'
#' # Use custom status rules
#' custom_rules <- create_custom_status_rules(
#'   unemployment_threshold = 30,
#'   custom_labels = list(unemployed_short = "seeking_job")
#' )
#' result_custom <- vecshift(dt, status_rules = custom_rules)
#' }
vecshift <- function(dt, classify_status = TRUE, status_rules = NULL) {
  # Load required package
  require("data.table")

  # Input validation
  if (!inherits(dt, "data.table")) {
    stop("Input 'dt' must be a data.table object. Use as.data.table() to convert.")
  }

  # Check for required columns
  required_cols <- c("id", "cf", "INIZIO", "FINE", "prior")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Validate column types
  if (!is.numeric(dt$INIZIO) && !inherits(dt$INIZIO, "Date")) {
    stop("Column 'INIZIO' must be numeric or Date type")
  }
  if (!is.numeric(dt$FINE) && !inherits(dt$FINE, "Date")) {
    stop("Column 'FINE' must be numeric or Date type")
  }
  if (!is.numeric(dt$prior)) {
    stop("Column 'prior' must be numeric")
  }

  # Check for logical consistency
  if (any(dt$FINE < dt$INIZIO, na.rm = TRUE)) {
    warning("Some records have FINE < INIZIO. These may produce unexpected results.")
  }

  # Main processing logic
  result <- setorder(
    rbindlist(
      list(
        dt[, .(id, cf, cdata = INIZIO, value = 1
               , prior
        )]
        , dt[, .(id, cf, cdata = FINE, value = -1
                 , prior = 0
        )]
      )
    )
    , cf, cdata)[
      , arco := cumsum(value)][
        , prior := fcase(prior <= 0, 0, default = 1)
      ][
        , .(
          cf = cf[1:(length(cf)-1)]
          , acf = cf[2:(length(cf))]
          , inizio = cdata[1:(length(cf)-1)]
          , fine = cdata[2:(length(cf))]
          , arco = arco[1:(length(cf)-1)]
          , prior = prior[1:(length(cf)-1)]
          , id = id[1:(length(cf)-1)]
        )][, over_id := (arco > 0)][
          shift(over_id, type = "lag") == TRUE, over_id := FALSE ][
            , first_in_over := fcase(over_id == TRUE | arco == 0, 0L, default = -1L)][
              , over_id := cumsum(over_id)][arco == 0, over_id := 0][
                cf == acf][
                  , acf := NULL][
                    arco == 0, id := 0][
                      arco == 0, inizio := inizio + 1][
                        arco == 0, fine := fine - 1][
                          , durata := 1 + fine-inizio + first_in_over][durata > 0][
                            , first_in_over := NULL]

  # Apply employment status classification if requested
  if (classify_status) {
    result <- classify_employment_status(result, rules = status_rules, group_by = "cf")
  }

  return(result)
}


