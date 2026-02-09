#' Add Unemployment Periods at the Beginning and End of Employment Histories
#'
#' @description
#' Extends vecshift output by adding unemployment periods at the beginning and/or
#' end of employment histories. This creates complete temporal coverage for analysis
#' requiring uniform observation periods by:
#' \itemize{
#'   \item{\strong{Head}: Adding unemployment from min_date to first employment start}
#'   \item{\strong{Tail}: Adding unemployment from last employment end to max_date}
#' }
#'
#' @details
#' The function can add unemployment periods at both ends of employment histories:
#'
#' \strong{Head Unemployment} (when \code{add_head = TRUE}):
#' \itemize{
#'   \item Identifies individuals whose first employment starts after min_date
#'   \item Creates unemployment from min_date to (first_start - 1)
#'   \item Only added if duration >= min_duration
#' }
#'
#' \strong{Tail Unemployment} (when \code{add_tail = TRUE}):
#' \itemize{
#'   \item Identifies individuals whose last employment ends before max_date
#'   \item Creates unemployment from (last_end + 1) to max_date
#'   \item Only added if duration >= min_duration
#' }
#'
#' All unemployment periods follow vecshift formatting:
#' \itemize{
#'   \item{\code{arco = 0}}: No active employment contracts
#'   \item{\code{over_id = 0}}: Unemployment period
#'   \item{\code{id = 0}}: No associated contract ID
#'   \item{\code{prior = 0}}: Standard for unemployment periods
#'   \item{\code{durata}}: Correctly calculated duration
#'   \item{\code{stato = "disoccupato"}}: Employment status (if present in input)
#' }
#'
#' The function preserves the temporal invariant that elapsed time equals sum
#' of durations by person, ensuring consistency with vecshift's core logic.
#'
#' @param vecshift_data A data.table output from vecshift() containing temporal
#'   employment segments with required columns: cf, inizio, fine, arco, prior,
#'   id, over_id, durata. Optional: stato column for employment status.
#' @param min_date Date or numeric. The start date for unemployment heads (optional).
#'   Only used when add_head = TRUE. Individuals with first employment starting
#'   after this date will get unemployment heads added.
#' @param max_date Date or numeric. The end date for unemployment tails (optional).
#'   Only used when add_tail = TRUE. Individuals with last employment ending
#'   before this date will get unemployment tails added.
#' @param min_duration Integer. Minimum duration in days for unemployment
#'   periods to be added. Shorter periods are excluded (default: 1).
#' @param add_head Logical. Whether to add unemployment periods at the beginning
#'   of employment histories (default: FALSE). Requires min_date to be specified.
#' @param add_tail Logical. Whether to add unemployment periods at the end
#'   of employment histories (default: TRUE). Requires max_date to be specified.
#'
#' @return A data.table with the same structure as input, extended with
#'   unemployment tail segments where applicable. Maintains original ordering
#'   by cf and temporal sequence.
#'
#' @note
#' This function is optimized for performance using data.table operations and
#' follows vecshift coding conventions. It handles edge cases such as:
#' \itemize{
#'   \item Individuals already starting at/before min_date (no head added)
#'   \item Individuals already ending at/after max_date (no tail added)
#'   \item Very short unemployment periods (filtered by min_duration)
#'   \item Proper integration with over_id consolidation logic
#'   \item Maintains temporal ordering by cf and inizio
#' }
#'
#' @seealso
#' \code{\link{vecshift}} for the main temporal transformation function
#' \code{\link{classify_employment_status}} for status classification
#'
#' @export
#' @importFrom data.table data.table setorder rbindlist copy
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Create sample employment data
#' dt <- data.table(
#'   id = 1:2,
#'   cf = c("ABC123", "DEF456"),
#'   INIZIO = as.Date(c("2023-03-01", "2023-06-01")),
#'   FINE = as.Date(c("2023-05-31", "2023-08-31")),
#'   prior = c(1, 0)
#' )
#'
#' # Transform to vecshift format
#' result <- vecshift(dt)
#'
#' # Add unemployment tails only (backward compatible)
#' extended_result <- add_unemployment_periods(
#'   result,
#'   max_date = as.Date("2024-12-31")
#' )
#'
#' # Add unemployment heads only
#' extended_result <- add_unemployment_periods(
#'   result,
#'   min_date = as.Date("2022-01-01"),
#'   add_head = TRUE,
#'   add_tail = FALSE
#' )
#'
#' # Add both head and tail unemployment
#' extended_result <- add_unemployment_periods(
#'   result,
#'   min_date = as.Date("2022-01-01"),
#'   max_date = as.Date("2024-12-31"),
#'   add_head = TRUE,
#'   add_tail = TRUE
#' )
#'
#' # Check the added unemployment periods
#' print(extended_result[arco == 0])
#' }
add_unemployment_periods <- function(
  vecshift_data,
  min_date = NULL,
  max_date = NULL,
  min_duration = 1L,
  add_head = FALSE,
  add_tail = TRUE
) {
  # Input validation
  if (!inherits(vecshift_data, "data.table")) {
    stop(
      "Input 'vecshift_data' must be a data.table object from vecshift() output."
    )
  }

  # Check for required columns from vecshift output
  required_cols <- c(
    "cf",
    "inizio",
    "fine",
    "arco",
    "prior",
    "id",
    "over_id",
    "durata"
  )
  missing_cols <- setdiff(required_cols, names(vecshift_data))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required vecshift columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Validate operation parameters
  if (!add_head && !add_tail) {
    stop("At least one of add_head or add_tail must be TRUE")
  }

  if (add_head && is.null(min_date)) {
    stop("min_date must be specified when add_head = TRUE")
  }

  if (add_tail && is.null(max_date)) {
    stop("max_date must be specified when add_tail = TRUE")
  }

  # Validate and convert date parameters
  if (add_head) {
    if (length(min_date) != 1) {
      stop("min_date must be a single Date or numeric value")
    }

    # Convert min_date to same type as inizio column for consistency
    if (inherits(vecshift_data$inizio, "Date")) {
      if (!inherits(min_date, "Date")) {
        min_date <- as.Date(min_date, origin = "1970-01-01")
      }
    } else if (is.numeric(vecshift_data$inizio)) {
      if (inherits(min_date, "Date")) {
        min_date <- as.numeric(min_date)
      }
    }
  }

  if (add_tail) {
    if (length(max_date) != 1) {
      stop("max_date must be a single Date or numeric value")
    }

    # Convert max_date to same type as fine column for consistency
    if (inherits(vecshift_data$fine, "Date")) {
      if (!inherits(max_date, "Date")) {
        max_date <- as.Date(max_date, origin = "1970-01-01")
      }
    } else if (is.numeric(vecshift_data$fine)) {
      if (inherits(max_date, "Date")) {
        max_date <- as.numeric(max_date)
      }
    }
  }

  # Validate min_duration
  if (!is.numeric(min_duration) || min_duration < 0) {
    stop("min_duration must be a non-negative integer")
  }
  min_duration <- as.integer(min_duration)

  # Early return if no data
  if (nrow(vecshift_data) == 0L) {
    return(vecshift_data)
  }

  # Check if stato column exists (indicates status classification was applied)
  has_stato <- "stato" %in% names(vecshift_data)

  # Find maximum over_id for proper numbering of new unemployment periods
  max_over_id <- max(vecshift_data$over_id, na.rm = TRUE)
  if (is.infinite(max_over_id) || is.na(max_over_id)) {
    max_over_id <- 0L
  }

  # Initialize result as copy of input data
  extended_result <- copy(vecshift_data)

  # Get the class of durata from original data to match it properly
  durata_class <- class(vecshift_data$durata)[1] # Get first class in case of multiple

  # Create list to collect new unemployment periods
  new_unemployment <- list()

  # Process head unemployment if requested
  if (add_head) {
    # Order by cf and inizio to get first record per person efficiently
    setorder(extended_result, cf, inizio)

    # Get first record per person
    first_employment <- extended_result[, .SD[1], by = cf]

    # Filter for individuals whose first employment starts after min_date
    # and would have unemployment head duration >= min_duration
    head_candidates <- first_employment[
      inizio > min_date & (inizio - min_date) >= min_duration
    ]

    # Create unemployment head segments if there are candidates
    if (nrow(head_candidates) > 0L) {
      unemployment_heads <- head_candidates[, .(
        cf = cf,
        inizio = min_date,
        fine = inizio - 1L, # Unemployment ends day before first employment starts
        arco = 0, # No active employment (numeric, not integer)
        prior = 0, # Standard for unemployment (numeric, not integer)
        id = 0L, # No contract ID for unemployment (integer)
        over_id = 0L, # Unemployment periods have over_id = 0 (integer)
        durata = if (durata_class == "difftime") {
          as.difftime(inizio - min_date, units = "days")
        } else {
          as.numeric(inizio - min_date) # For numeric dates
        }
      )]

      # Add stato column if it exists in original data
      if (has_stato) {
        unemployment_heads[, stato := "disoccupato"]
      }

      new_unemployment <- append(new_unemployment, list(unemployment_heads))
    }
  }

  # Process tail unemployment if requested
  if (add_tail) {
    # Order by cf and fine to get last record per person efficiently
    setorder(extended_result, cf, fine)

    # Get last record per person (most efficient approach)
    last_employment <- extended_result[, .SD[.N], by = cf]

    # Filter for individuals whose last employment ends before max_date
    # and would have unemployment tail duration >= min_duration
    tail_candidates <- last_employment[
      fine < max_date & (max_date - fine) >= min_duration
    ]

    # Create unemployment tail segments if there are candidates
    if (nrow(tail_candidates) > 0L) {
      unemployment_tails <- tail_candidates[, .(
        cf = cf,
        inizio = fine + 1L, # Unemployment starts day after last employment ends
        fine = max_date,
        arco = 0, # No active employment (numeric, not integer)
        prior = 0, # Standard for unemployment (numeric, not integer)
        id = 0L, # No contract ID for unemployment (integer)
        over_id = 0L, # Unemployment periods have over_id = 0 (integer)
        durata = if (durata_class == "difftime") {
          as.difftime(max_date - fine, units = "days")
        } else {
          as.numeric(max_date - fine) # For numeric dates
        }
      )]

      # Add stato column if it exists in original data
      if (has_stato) {
        unemployment_tails[, stato := "disoccupato"]
      }

      new_unemployment <- append(new_unemployment, list(unemployment_tails))
    }
  }

  # Early return if no unemployment periods to add
  if (length(new_unemployment) == 0L) {
    return(vecshift_data)
  }

  # Combine original data with new unemployment periods using efficient rbindlist
  all_parts <- c(list(vecshift_data), new_unemployment)
  extended_result <- rbindlist(
    all_parts,
    use.names = TRUE,
    fill = FALSE
  )

  # Restore proper ordering by cf and temporal sequence (inizio)
  setorder(extended_result, cf, inizio)

  return(extended_result)
}

#' Add Unemployment Periods at the End of Employment Histories (Legacy)
#'
#' @description
#' This function is maintained for backward compatibility. It calls the new
#' \\code{add_unemployment_periods()} function with \\code{add_tail = TRUE} and
#' \\code{add_head = FALSE}.
#'
#' @param vecshift_data A data.table output from vecshift()
#' @param max_date Date or numeric. The end date for unemployment tails
#' @param min_tail_duration Integer. Minimum duration for unemployment tails (default: 1)
#'
#' @return A data.table with unemployment tail periods added
#' @export
#' @seealso \\code{\\link{add_unemployment_periods}} for the enhanced version with head/tail options
#'
add_unemployment_tail <- function(
  vecshift_data,
  max_date,
  min_tail_duration = 1L
) {
  add_unemployment_periods(
    vecshift_data = vecshift_data,
    max_date = max_date,
    min_duration = min_tail_duration,
    add_head = FALSE,
    add_tail = TRUE
  )
}
