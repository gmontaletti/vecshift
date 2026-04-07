#' Validate Input Data for vecshift Processing
#'
#' @description
#' Diagnostic function that checks an input data.table (or data.frame) for
#' issues that would cause [vecshift()] to fail or produce unexpected
#' results. This function does not modify the data; it only reports issues.
#'
#' @details
#' The function performs the following checks:
#' \enumerate{
#'   \item Object is a data.frame / data.table.
#'   \item All required columns exist (using the user-supplied column names).
#'   \item No `NA` values in the required columns.
#'   \item Start and end columns have a date or numeric class.
#'   \item No record has `end < start`.
#'   \item Contract IDs are unique.
#' }
#'
#' @param dt A data.table (or data.frame) containing employment records.
#' @param person_col Character. Name of the person identifier column. Default: `"cf"`.
#' @param start_col Character. Name of the start-date column. Default: `"inizio"`.
#' @param end_col Character. Name of the end-date column. Default: `"fine"`.
#' @param id_col Character. Name of the contract identifier column. Default: `"id"`.
#' @param type_col Character. Name of the employment-type column. Default: `"prior"`.
#'
#' @return An object of class `vecshift_validation`. Use `print()` to display
#'   a human-readable report. The object is a list with elements:
#'   \itemize{
#'     \item `issues`: a named list of detected issues (empty if none).
#'     \item `n_rows`: number of input rows.
#'     \item `n_persons`: number of unique persons (`NA` if person column missing).
#'     \item `n_issues`: total number of issue categories detected.
#'   }
#'
#' @export
#' @examples
#' dt <- data.table::data.table(
#'   id = c(1L, 2L, 2L),
#'   cf = c("A", "A", "B"),
#'   inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-03-01")),
#'   fine = as.Date(c("2023-12-31", "2024-05-31", "2023-02-28")),
#'   prior = c(1, 1, 0)
#' )
#' validate_vecshift_input(dt)
validate_vecshift_input <- function(
  dt,
  person_col = "cf",
  start_col = "inizio",
  end_col = "fine",
  id_col = "id",
  type_col = "prior"
) {
  issues <- list()

  # 1. Check object type -----
  if (!is.data.frame(dt)) {
    issues$not_data_frame <- "Input is not a data.frame or data.table."
    return(structure(
      list(
        issues = issues,
        n_rows = NA_integer_,
        n_persons = NA_integer_,
        n_issues = length(issues)
      ),
      class = "vecshift_validation"
    ))
  }

  # 2. Check column existence -----
  required <- c(person_col, start_col, end_col, id_col, type_col)
  missing_cols <- setdiff(required, names(dt))
  if (length(missing_cols) > 0L) {
    issues$missing_columns <- missing_cols
  }

  n_rows <- nrow(dt)
  n_persons <- if (person_col %in% names(dt)) {
    length(unique(dt[[person_col]]))
  } else {
    NA_integer_
  }

  if (length(missing_cols) > 0L) {
    return(structure(
      list(
        issues = issues,
        n_rows = n_rows,
        n_persons = n_persons,
        n_issues = length(issues)
      ),
      class = "vecshift_validation"
    ))
  }

  # 3. NA counts -----
  na_counts <- vapply(
    required,
    function(col) sum(is.na(dt[[col]])),
    integer(1)
  )
  if (any(na_counts > 0L)) {
    issues$na_counts <- na_counts[na_counts > 0L]
  }

  # 4. Date / numeric types -----
  start_class <- class(dt[[start_col]])[1]
  end_class <- class(dt[[end_col]])[1]
  if (!start_class %in% c("Date", "IDate", "numeric", "integer")) {
    issues$start_col_type <- sprintf(
      "'%s' has class '%s'; expected Date or numeric.",
      start_col,
      start_class
    )
  }
  if (!end_class %in% c("Date", "IDate", "numeric", "integer")) {
    issues$end_col_type <- sprintf(
      "'%s' has class '%s'; expected Date or numeric.",
      end_col,
      end_class
    )
  }

  # 5. fine < inizio -----
  if (
    start_class %in%
      c("Date", "IDate", "numeric", "integer") &&
      end_class %in% c("Date", "IDate", "numeric", "integer")
  ) {
    bad_order <- which(dt[[end_col]] < dt[[start_col]])
    if (length(bad_order) > 0L) {
      issues$end_before_start <- list(
        n = length(bad_order),
        row_indices = utils::head(bad_order, 10)
      )
    }
  }

  # 6. Duplicate IDs -----
  if (id_col %in% names(dt)) {
    n_dup <- sum(duplicated(dt[[id_col]]))
    if (n_dup > 0L) {
      issues$duplicate_ids <- n_dup
    }
  }

  structure(
    list(
      issues = issues,
      n_rows = n_rows,
      n_persons = n_persons,
      n_issues = length(issues)
    ),
    class = "vecshift_validation"
  )
}

#' Print method for vecshift_validation
#'
#' @param x A `vecshift_validation` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.vecshift_validation <- function(x, ...) {
  cat("<vecshift_validation>\n")
  cat("  Rows:           ", x$n_rows, "\n", sep = "")
  cat(
    "  Persons:        ",
    if (is.na(x$n_persons)) "NA" else x$n_persons,
    "\n",
    sep = ""
  )
  cat("  Issues found:   ", x$n_issues, "\n", sep = "")
  if (x$n_issues == 0L) {
    cat("\n  Status: OK -- input data passes all checks.\n")
  } else {
    cat("\n  Issues:\n")
    for (nm in names(x$issues)) {
      cat("    - ", nm, ": ", sep = "")
      val <- x$issues[[nm]]
      if (is.character(val) && length(val) == 1L) {
        cat(val, "\n", sep = "")
      } else if (is.numeric(val) && length(val) == 1L) {
        cat(val, "\n", sep = "")
      } else if (is.character(val)) {
        cat(paste(val, collapse = ", "), "\n", sep = "")
      } else if (is.list(val) && !is.null(val$n)) {
        cat(
          val$n,
          " row(s) (first: ",
          paste(val$row_indices, collapse = ","),
          ")\n",
          sep = ""
        )
      } else {
        cat(
          paste(names(val), val, sep = "=", collapse = ", "),
          "\n",
          sep = ""
        )
      }
    }
  }
  invisible(x)
}
