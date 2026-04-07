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
#' @param dt A data.table containing employment contract records. The required
#'   columns are identified through the `person_col`, `start_col`, `end_col`,
#'   `id_col`, and `type_col` parameters. By default the function expects the
#'   Italian-labor names (`cf`, `inizio`, `fine`, `id`, `prior`); supplying
#'   alternative names lets users adopt the package on non-Italian datasets
#'   without renaming columns externally.
#' @param person_col Character. Name of the column holding the person
#'   identifier (e.g. fiscal code). Default: `"cf"`.
#' @param start_col Character. Name of the column holding the contract start
#'   date (Date or numeric). Default: `"inizio"`.
#' @param end_col Character. Name of the column holding the contract end date
#'   (Date or numeric). Default: `"fine"`.
#' @param id_col Character. Name of the column holding the unique contract
#'   identifier. Default: `"id"`.
#' @param type_col Character. Name of the column holding the employment-type
#'   indicator. Values `<= 0` are treated as part-time and positive values as
#'   full-time. Default: `"prior"`.
#' @param granularity Character. Time granularity of the input dates. The only
#'   value currently implemented is `"day"`. The values `"month"` and `"hour"`
#'   are reserved for future releases and currently raise a not-implemented
#'   error. The parameter is included so that downstream code can adopt the
#'   API today without breaking when finer granularities are added.
#'
#' @return A `vecshift_result` object (an S3 wrapper around a data.table; it
#'   inherits from `data.table` and `data.frame`, so all data.table operations
#'   work unchanged) with temporal segments containing:
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
vecshift <- function(
  dt,
  person_col = "cf",
  start_col = "inizio",
  end_col = "fine",
  id_col = "id",
  type_col = "prior",
  granularity = "day"
) {
  # Input validation
  if (!inherits(dt, "data.table")) {
    stop(
      "Input 'dt' must be a data.table object. Use as.data.table() to convert."
    )
  }

  # Validate granularity (P3) -----
  if (!is.character(granularity) || length(granularity) != 1L) {
    stop("'granularity' must be a single character string.")
  }
  if (!granularity %in% c("day", "month", "hour")) {
    stop(
      "Invalid 'granularity': '",
      granularity,
      "'. Must be one of: 'day', 'month', 'hour'."
    )
  }
  if (granularity == "month") {
    stop(
      "granularity = 'month' is not yet implemented. ",
      "The parameter is reserved for a future release; use 'day' for now."
    )
  }
  if (granularity == "hour") {
    stop(
      "granularity = 'hour' is not yet implemented. ",
      "The parameter is reserved for a future release; use 'day' for now."
    )
  }

  # Resolve user-supplied column names against canonical Italian names -----
  # The rest of the function body uses the canonical names; if the user passed
  # different names we rename a copy of the input so we never mutate the
  # caller's data.table.
  canonical <- c(
    cf = person_col,
    inizio = start_col,
    fine = end_col,
    id = id_col,
    prior = type_col
  )
  needs_rename <- canonical != names(canonical)
  if (any(needs_rename)) {
    missing_user_cols <- setdiff(canonical, names(dt))
    if (length(missing_user_cols) > 0L) {
      stop(
        "Missing required columns: ",
        paste(missing_user_cols, collapse = ", "),
        ". vecshift() requires columns identified by ",
        "person_col, start_col, end_col, id_col, type_col."
      )
    }
    dt <- data.table::copy(dt)
    data.table::setnames(
      dt,
      old = unname(canonical[needs_rename]),
      new = names(canonical)[needs_rename]
    )
  }

  # Check for required columns (canonical names) -----
  required_cols <- c("id", "cf", "inizio", "fine", "prior")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      ". vecshift() requires: id, cf, inizio, fine, prior. ",
      "Use the person_col / start_col / end_col / id_col / type_col ",
      "parameters to map alternative column names."
    )
  }

  # Validate column types
  if (!is.numeric(dt$inizio) && !inherits(dt$inizio, "Date")) {
    stop(
      "Column 'inizio' must be Date or numeric, got: ",
      class(dt$inizio)[1],
      ". Use as.Date() to convert."
    )
  }
  if (!is.numeric(dt$fine) && !inherits(dt$fine, "Date")) {
    stop(
      "Column 'fine' must be Date or numeric, got: ",
      class(dt$fine)[1],
      ". Use as.Date() to convert."
    )
  }
  if (!is.numeric(dt$prior)) {
    stop(
      "Column 'prior' must be numeric, got: ",
      class(dt$prior)[1],
      ". Use as.numeric() to convert."
    )
  }

  # Check for logical consistency
  if (any(dt$fine < dt$inizio, na.rm = TRUE)) {
    warning(
      "Some records have fine < inizio. These may produce unexpected results."
    )
  }

  # Early return for empty input
  if (nrow(dt) == 0L) {
    empty <- data.table::data.table(
      cf = character(0),
      inizio = dt$inizio[0],
      fine = dt$fine[0],
      arco = integer(0),
      prior = numeric(0),
      id = integer(0),
      durata = numeric(0),
      over_id = integer(0)
    )
    return(new_vecshift_result(
      empty,
      metadata = list(
        granularity = granularity,
        vecshift_version = utils::packageVersion("vecshift"),
        generated_at = Sys.time()
      )
    ))
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
  # NOTE (v1.2.0): merging steps 7 and 9 into a single filter was considered
  # but deferred. The unemployment adjustment in step 8 mutates inizio/fine
  # for arco==0 rows BEFORE durata is computed, so the durata predicate is
  # not known at the time of the cf==acf filter. A single-pass version would
  # require duplicating step-8 logic into a predicate expression, hurting
  # readability for negligible gain.
  result[, durata := 1 + fine - inizio + first_in_over]
  result <- result[durata > 0]
  result[, first_in_over := NULL]

  return(new_vecshift_result(
    result,
    metadata = list(
      granularity = granularity,
      vecshift_version = utils::packageVersion("vecshift"),
      generated_at = Sys.time()
    )
  ))
}
