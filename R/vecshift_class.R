#' vecshift_result S3 class
#'
#' @description
#' Lightweight S3 wrapper around the data.table returned by [vecshift()].
#' The result inherits from `data.table` and `data.frame`, so all data.table
#' operations (subsetting, `:=`, joins, etc.) continue to work unchanged.
#' The class only adds dedicated `print()` and `summary()` methods plus a
#' `vecshift_metadata` attribute.
#'
#' @name vecshift_result
NULL

#' Construct a vecshift_result object
#'
#' Internal constructor. Sets the class vector and attaches metadata in place
#' (no copy) on an existing data.table.
#'
#' @param dt A data.table produced by the core vecshift transformation.
#' @param metadata A named list of metadata to attach as the
#'   `vecshift_metadata` attribute.
#'
#' @return The same data.table, with updated class and attribute.
#' @keywords internal
#' @noRd
new_vecshift_result <- function(dt, metadata = list()) {
  data.table::setattr(
    dt,
    "class",
    c("vecshift_result", "data.table", "data.frame")
  )
  data.table::setattr(dt, "vecshift_metadata", metadata)
  dt
}

#' Test whether an object is a vecshift_result
#'
#' @param x Any R object.
#'
#' @return A logical scalar: `TRUE` if `x` inherits from `vecshift_result`,
#'   `FALSE` otherwise.
#'
#' @export
#' @examples
#' dt <- data.table::data.table(
#'   id = 1L, cf = "A",
#'   inizio = as.Date("2023-01-01"),
#'   fine = as.Date("2023-12-31"),
#'   prior = 1
#' )
#' res <- vecshift(dt)
#' is_vecshift_result(res)
is_vecshift_result <- function(x) {
  inherits(x, "vecshift_result")
}

#' Print method for vecshift_result
#'
#' @param x A `vecshift_result` object.
#' @param ... Further arguments passed to the underlying data.table print
#'   method.
#'
#' @return `x`, invisibly.
#' @export
print.vecshift_result <- function(x, ...) {
  n_segments <- nrow(x)
  if (n_segments == 0L) {
    cat("<vecshift_result> 0 segments\n")
    return(invisible(x))
  }
  n_persons <- if ("cf" %in% names(x)) length(unique(x$cf)) else NA_integer_
  date_min <- if ("inizio" %in% names(x)) min(x$inizio, na.rm = TRUE) else NA
  date_max <- if ("fine" %in% names(x)) max(x$fine, na.rm = TRUE) else NA
  n_overlap <- if ("over_id" %in% names(x)) {
    sum(x$over_id > 0, na.rm = TRUE)
  } else {
    NA_integer_
  }

  cat("<vecshift_result>\n")
  cat("  Persons:        ", n_persons, "\n", sep = "")
  cat("  Segments:       ", n_segments, "\n", sep = "")
  if (!is.na(date_min) && !is.na(date_max)) {
    cat(
      "  Date range:     ",
      format(date_min),
      " to ",
      format(date_max),
      "\n",
      sep = ""
    )
  }
  if (!is.na(n_overlap)) {
    cat(
      "  Overlap segs:   ",
      n_overlap,
      " (",
      round(100 * n_overlap / n_segments, 1),
      "%)\n",
      sep = ""
    )
  }
  if ("stato" %in% names(x)) {
    stato_tab <- sort(table(x$stato), decreasing = TRUE)
    cat(
      "  Status:         ",
      paste0(names(stato_tab), "=", as.integer(stato_tab), collapse = ", "),
      "\n",
      sep = ""
    )
  }
  cat("\n")
  NextMethod()
}

#' Summary method for vecshift_result
#'
#' Computes a compact summary including a verification of the duration
#' invariant: per person, the sum of `durata` should equal the elapsed
#' time between the earliest `inizio` and the latest `fine`.
#'
#' @param object A `vecshift_result` object.
#' @param ... Unused.
#'
#' @return An object of class `summary.vecshift_result`.
#' @export
summary.vecshift_result <- function(object, ...) {
  invariant_ok <- TRUE
  if (
    all(c("cf", "inizio", "fine", "durata") %in% names(object)) &&
      nrow(object) > 0L
  ) {
    check <- object[,
      list(
        sum_durata = sum(as.numeric(durata)),
        elapsed = as.numeric(max(fine) - min(inizio)) + 1
      ),
      by = cf
    ]
    invariant_ok <- all(
      abs(check$sum_durata - check$elapsed) < 1e-6,
      na.rm = TRUE
    )
  }
  out <- list(
    n_persons = if ("cf" %in% names(object)) {
      length(unique(object$cf))
    } else {
      0L
    },
    n_segments = nrow(object),
    duration_invariant = invariant_ok,
    metadata = attr(object, "vecshift_metadata")
  )
  if ("stato" %in% names(object)) {
    out$status_distribution <- as.list(table(object$stato))
  }
  structure(out, class = "summary.vecshift_result")
}

#' Print method for summary.vecshift_result
#'
#' @param x A `summary.vecshift_result` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.summary.vecshift_result <- function(x, ...) {
  cat("vecshift_result summary\n")
  cat("  Persons:            ", x$n_persons, "\n", sep = "")
  cat("  Segments:           ", x$n_segments, "\n", sep = "")
  cat(
    "  Duration invariant: ",
    if (x$duration_invariant) "OK" else "FAILED",
    "\n",
    sep = ""
  )
  if (!is.null(x$status_distribution)) {
    cat("  Status distribution:\n")
    for (nm in names(x$status_distribution)) {
      cat("    ", nm, ": ", x$status_distribution[[nm]], "\n", sep = "")
    }
  }
  if (!is.null(x$metadata) && length(x$metadata) > 0L) {
    cat("  Metadata:\n")
    for (nm in names(x$metadata)) {
      val <- x$metadata[[nm]]
      cat("    ", nm, ": ", format(val), "\n", sep = "")
    }
  }
  invisible(x)
}
