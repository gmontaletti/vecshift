#' Merge Original Data Columns with Temporal Segments
#'
#' @description
#' Enriches vecshift output by merging additional columns from the original
#' contract data. This function allows you to carry forward metadata and
#' additional attributes from the original employment contracts into the
#' temporal segments created by vecshift().
#'
#' @details
#' The function performs an inner join between the segments data and the
#' original contract data using the 'id' column as the key. Since vecshift()
#' creates multiple temporal segments from single contracts, the same original
#' contract data may appear in multiple segment rows.
#' 
#' Unemployment periods (where id = 0) are preserved but will not have
#' merged columns from the original data, as they don't correspond to
#' specific contracts.
#'
#' @param original_data A data.table containing the original contract records
#'   that were processed by vecshift(). Must include 'id' column and the
#'   columns specified in the 'columns' parameter.
#' @param segments A data.table containing the output from vecshift().
#'   Must include 'id' column for merging.
#' @param columns Character vector specifying which columns from original_data
#'   to merge into segments. Column names must exist in original_data.
#'
#' @return A data.table combining the segments data with the requested columns
#'   from original_data. The temporal ordering and structure of segments is
#'   preserved. Unemployment periods (id = 0) are included but without the
#'   merged columns.
#'
#' @export
#' @importFrom data.table data.table setorder merge.data.table
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample employment data with additional metadata
#' original_dt <- data.table(
#'   id = 1:3,
#'   cf = c("ABC123", "ABC123", "DEF456"),
#'   INIZIO = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
#'   FINE = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
#'   prior = c(1, 0, 1),
#'   company = c("CompanyA", "CompanyB", "CompanyC"),
#'   salary = c(50000, 25000, 60000),
#'   department = c("IT", "HR", "Finance")
#' )
#' 
#' # Transform to temporal segments
#' segments <- vecshift(original_dt)
#' 
#' # Merge additional columns
#' enriched <- merge_original_columns(
#'   original_data = original_dt,
#'   segments = segments,
#'   columns = c("company", "salary", "department")
#' )
#' 
#' print(enriched)
#' 
#' # Merge single column
#' with_company <- merge_original_columns(
#'   original_data = original_dt,
#'   segments = segments,
#'   columns = "company"
#' )
#' }
merge_original_columns <- function(original_data, segments, columns) {
  # Load required package
  require("data.table")
  
  # Input validation
  if (!inherits(original_data, "data.table")) {
    stop("Parameter 'original_data' must be a data.table object. Use as.data.table() to convert.")
  }
  
  if (!inherits(segments, "data.table")) {
    stop("Parameter 'segments' must be a data.table object. Use as.data.table() to convert.")
  }
  
  if (!is.character(columns) || length(columns) == 0) {
    stop("Parameter 'columns' must be a non-empty character vector specifying column names.")
  }
  
  # Check for required id columns
  if (!"id" %in% names(original_data)) {
    stop("Original data must contain an 'id' column for merging.")
  }
  
  if (!"id" %in% names(segments)) {
    stop("Segments data must contain an 'id' column for merging.")
  }
  
  # Check that requested columns exist in original_data
  missing_cols <- setdiff(columns, names(original_data))
  if (length(missing_cols) > 0) {
    stop(paste("Columns not found in original_data:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for column name conflicts (except 'id' which is the merge key)
  conflicting_cols <- intersect(columns, names(segments))
  conflicting_cols <- setdiff(conflicting_cols, "id")  # 'id' is expected
  if (length(conflicting_cols) > 0) {
    warning(paste("Column name conflicts detected. Columns from original_data will overwrite segments columns:", 
                  paste(conflicting_cols, collapse = ", ")))
  }
  
  # Create subset of original data with only id and requested columns
  merge_data <- original_data[, c("id", columns), with = FALSE]
  
  # Separate unemployment periods (id = 0) from employment segments
  unemployment_segments <- segments[id == 0]
  employment_segments <- segments[id > 0]
  
  # Perform the merge for employment segments
  if (nrow(employment_segments) > 0) {
    merged_employment <- merge(employment_segments, merge_data, by = "id", all.x = TRUE, all.y = FALSE)
    
    # Check if any employment segments failed to merge
    unmatched_ids <- unique(employment_segments$id[!employment_segments$id %in% merge_data$id])
    if (length(unmatched_ids) > 0) {
      warning(paste("Some employment segment IDs were not found in original data:", 
                    paste(unmatched_ids, collapse = ", ")))
    }
  } else {
    merged_employment <- employment_segments
  }
  
  # For unemployment segments, add the requested columns with NA values
  if (nrow(unemployment_segments) > 0) {
    for (col in columns) {
      if (is.character(original_data[[col]])) {
        unemployment_segments[, (col) := NA_character_]
      } else if (is.numeric(original_data[[col]])) {
        unemployment_segments[, (col) := NA_real_]
      } else if (inherits(original_data[[col]], "Date")) {
        unemployment_segments[, (col) := as.Date(NA)]
      } else if (is.logical(original_data[[col]])) {
        unemployment_segments[, (col) := NA]
      } else {
        # For other types, use generic NA
        unemployment_segments[, (col) := NA]
      }
    }
  }
  
  # Combine employment and unemployment segments
  if (nrow(merged_employment) > 0 && nrow(unemployment_segments) > 0) {
    result <- rbindlist(list(merged_employment, unemployment_segments), use.names = TRUE, fill = TRUE)
  } else if (nrow(merged_employment) > 0) {
    result <- merged_employment
  } else {
    result <- unemployment_segments
  }
  
  # Restore temporal ordering by cf and inizio
  if (nrow(result) > 0) {
    if ("cf" %in% names(result) && "inizio" %in% names(result)) {
      setorder(result, cf, inizio)
    } else {
      warning("Could not restore temporal ordering. Missing 'cf' or 'inizio' columns.")
    }
  }
  
  return(result)
}

#' Merge Overlapping Values for Employment Periods
#'
#' @description
#' Processes merged segment data to handle overlapping employment periods (arco > 1)
#' by combining values from the previous row according to data type-specific rules.
#' This function operates on the output from merge_original_columns() and handles
#' the merging logic for overlapping employment scenarios.
#'
#' @details
#' When employment periods overlap (arco > 1), this function combines values from
#' the previous row with the current row based on data type:
#' \itemize{
#'   \item{\strong{Character/Factor columns}}: Values are combined with "->" separator 
#'         showing transitions (e.g., "CompanyA->CompanyB")
#'   \item{\strong{Numeric columns}}: Values are summed (e.g., overlapping salaries)
#'   \item{\strong{Multiple overlaps (arco > 2)}}: Chain all values in sequence for 
#'         characters, continue summing for numerics
#'   \item{\strong{Factors}}: Converted to character before processing
#' }
#'
#' The function processes rows sequentially within each person (cf), looking at the 
#' previous row's value when arco > 1 and merging accordingly. For arco = 1 or 0,
#' values remain unchanged.
#'
#' @param segments_with_columns A data.table containing the output from 
#'   merge_original_columns(), which includes temporal segments with merged columns.
#'   Must contain 'cf', 'arco' columns and be ordered by person and time.
#' @param columns Character vector specifying which columns to process for 
#'   overlapping periods. These columns must exist in segments_with_columns.
#'
#' @return A data.table with the same structure as the input, but with overlapping
#'   period values merged according to data type rules. All other columns and 
#'   rows remain unchanged.
#'
#' @export
#' @importFrom data.table data.table setorder copy
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample employment data with overlapping periods
#' original_dt <- data.table(
#'   id = 1:3,
#'   cf = c("PERSON001", "PERSON001", "PERSON001"),
#'   INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-06-01")),
#'   FINE = as.Date(c("2023-06-30", "2023-08-31", "2023-12-31")),
#'   prior = c(1, 1, 0),
#'   company = c("CompanyA", "CompanyB", "CompanyC"),
#'   salary = c(50000, 30000, 25000)
#' )
#' 
#' # Transform to segments and merge columns
#' segments <- vecshift(original_dt)
#' with_columns <- merge_original_columns(original_dt, segments, c("company", "salary"))
#' 
#' # Handle overlapping values
#' result <- merge_overlapping_values(with_columns, c("company", "salary"))
#' print(result)
#' }
merge_overlapping_values <- function(segments_with_columns, columns) {
  # Load required package
  require("data.table")
  
  # Input validation
  if (!inherits(segments_with_columns, "data.table")) {
    stop("Parameter 'segments_with_columns' must be a data.table object. Use as.data.table() to convert.")
  }
  
  if (!is.character(columns) || length(columns) == 0) {
    stop("Parameter 'columns' must be a non-empty character vector specifying column names.")
  }
  
  # Check for required columns
  required_cols <- c("cf", "arco")
  missing_required <- setdiff(required_cols, names(segments_with_columns))
  if (length(missing_required) > 0) {
    stop(paste("Missing required columns:", paste(missing_required, collapse = ", "), 
               ". These are needed to identify overlapping periods."))
  }
  
  # Check that requested columns exist in segments_with_columns
  missing_cols <- setdiff(columns, names(segments_with_columns))
  if (length(missing_cols) > 0) {
    stop(paste("Columns not found in segments_with_columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # If no overlapping periods exist, return unchanged
  if (!any(segments_with_columns$arco > 1, na.rm = TRUE)) {
    return(segments_with_columns)
  }
  
  # Work with a copy to avoid modifying the original data
  result <- copy(segments_with_columns)
  
  # Ensure proper ordering by person and time (assuming inizio exists)
  if ("inizio" %in% names(result)) {
    setorder(result, cf, inizio)
  } else {
    warning("Column 'inizio' not found. Results may not be properly ordered by time.")
    setorder(result, cf)
  }
  
  # Use data.table's efficient operations to process overlapping periods
  # Add row numbers within each person group for efficient lag operations
  result[, row_num := seq_len(.N), by = cf]
  
  # Process each column separately for better performance
  for (col in columns) {
    # Create lag column for previous row values within each person
    result[, paste0(col, "_prev") := shift(.SD[[col]], 1, type = "lag"), by = cf, .SDcols = col]
    
    # Determine column type once
    col_class <- class(result[[col]])[1]
    
    # Only update rows where arco > 1 and both current and previous values are not NA
    overlap_condition <- result$arco > 1 & result$row_num > 1 & 
                        !is.na(result[[col]]) & !is.na(result[[paste0(col, "_prev")]])
    
    if (any(overlap_condition, na.rm = TRUE)) {
      if (col_class == "factor") {
        # Convert factors to character and combine with arrow notation
        result[overlap_condition, 
               (col) := paste(as.character(get(paste0(col, "_prev"))), 
                             as.character(get(col)), 
                             sep = "->")]
      } else if (col_class == "character") {
        # Combine character values with arrow notation
        result[overlap_condition, 
               (col) := paste(get(paste0(col, "_prev")), 
                             get(col), 
                             sep = "->")]
      } else if (col_class %in% c("numeric", "integer", "double")) {
        # Sum numeric values
        result[overlap_condition, 
               (col) := get(paste0(col, "_prev")) + get(col)]
      } else {
        # For other types, convert to character and combine
        result[overlap_condition, 
               (col) := paste(as.character(get(paste0(col, "_prev"))), 
                             as.character(get(col)), 
                             sep = "->")]
      }
    }
    
    # Clean up the temporary lag column
    result[, paste0(col, "_prev") := NULL]
  }
  
  # Clean up the row number column
  result[, row_num := NULL]
  
  return(result)
}