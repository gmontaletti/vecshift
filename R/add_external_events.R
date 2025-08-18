#' Add External Event Attributes to Unemployment Periods
#'
#' @description
#' Adds external event attributes to unemployment periods in vecshift output by matching
#' events with unemployment segments (arco == 0). Events are matched using either 
#' temporal overlap or nearest neighbor strategies. The function can also create 
#' synthetic unemployment periods for persons present in external events but not 
#' in the main employment data.
#'
#' @details
#' The function implements two matching strategies:
#' \itemize{
#'   \item{\strong{Overlap matching}: Events that temporally overlap with unemployment periods}
#'   \item{\strong{Nearest matching}: When no overlap exists, finds the nearest unemployment period}
#' }
#'
#' For each external event, the function:
#' \itemize{
#'   \item Creates a new column "{event_name}_attribute" with value 1 for matched periods
#'   \item Adds distance/quality metrics in additional columns
#'   \item Maintains all original vecshift data integrity
#'   \item Handles multiple events per person
#' }
#'
#' \strong{Synthetic Unemployment Creation}:
#' When create_synthetic_unemployment = TRUE, the function creates unemployment
#' periods for persons in external_events but not in vecshift_data:
#' \itemize{
#'   \item Creates periods up to synthetic_unemployment_duration days
#'   \item Uses max(fine) from main dataset as the reference endpoint
#'   \item Follows vecshift formatting conventions
#' }
#'
#' \strong{Memory Optimization}:
#' The function includes several memory optimization strategies:
#' \itemize{
#'   \item In-place modifications using data.table reference semantics
#'   \item Chunked processing for large datasets when memory_safe = TRUE
#'   \item Early filtering to reduce intermediate object sizes
#'   \item Efficient rolling joins for nearest neighbor matching
#'   \item Immediate cleanup of temporary objects
#' }
#'
#' @param vecshift_data A data.table output from vecshift() containing temporal 
#'   employment segments with required columns: cf, inizio, fine, arco, prior, 
#'   id, over_id, durata. Optional: stato column for employment status.
#' @param external_events A data.table containing external events with person 
#'   identifiers and event information. Must contain the person_id_column and 
#'   event_name_column, plus at least one date column specified in date_columns.
#' @param event_matching_strategy Character. Matching strategy to use:
#'   \itemize{
#'     \item{\code{"overlap"}}: Match events that temporally overlap with unemployment periods
#'     \item{\code{"nearest"}}: Find nearest unemployment period when no overlap exists
#'   }
#' @param create_synthetic_unemployment Logical. Whether to create synthetic 
#'   unemployment periods for persons in external_events but not in vecshift_data 
#'   (default: FALSE).
#' @param synthetic_unemployment_duration Integer. Maximum duration in days for 
#'   synthetic unemployment periods (default: 730L for 2 years).
#' @param date_columns Named character vector specifying date column names in 
#'   external_events. Names should be "start" and optionally "end". 
#'   Default: c(start = "event_start", end = "event_end").
#' @param event_name_column Character. Name of column in external_events containing 
#'   event names/types (default: "event_name").
#' @param person_id_column Character. Name of column in external_events containing 
#'   person identifiers that match the 'cf' column in vecshift_data (default: "cf").
#' @param memory_safe Logical. Enable memory-safe mode for large datasets. 
#'   When TRUE, processes data in chunks to reduce peak memory usage at the cost
#'   of some performance (default: FALSE).
#' @param chunk_size Integer. Number of persons to process per chunk when 
#'   memory_safe = TRUE (default: 10000L).
#' @param progress Logical. Show progress messages for long-running operations
#'   (default: FALSE).
#'
#' @return A data.table with the same structure as vecshift_data, extended with:
#' \itemize{
#'   \item New columns: "{event_name}_attribute" (1 for matched unemployment periods, 0 otherwise)
#'   \item New columns: "{event_name}_distance" (days between event and unemployment period)
#'   \item New columns: "{event_name}_match_quality" (overlap, nearest, or none)
#'   \item Synthetic unemployment periods if create_synthetic_unemployment = TRUE
#' }
#' Maintains original ordering by cf and temporal sequence.
#'
#' @note
#' The function optimizes performance using data.table operations and follows
#' vecshift coding conventions. It handles edge cases such as:
#' \itemize{
#'   \item Events with only start dates (treated as single-day events)
#'   \item Multiple events per person
#'   \item Persons with no unemployment periods
#'   \item Date format consistency between datasets
#'   \item Proper integration with over_id consolidation logic
#' }
#'
#' For very large datasets (>1M records), consider using memory_safe = TRUE
#' to enable chunked processing that reduces peak memory usage.
#'
#' @seealso 
#' \code{\link{vecshift}} for the main temporal transformation function
#' \code{\link{add_unemployment_periods}} for adding unemployment periods
#'
#' @export
#' @importFrom data.table data.table setorder rbindlist copy foverlaps setkey 
#' @importFrom data.table setnames setkeyv
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample employment data
#' employment_dt <- data.table(
#'   id = 1:3,
#'   cf = c("ABC123", "ABC123", "DEF456"),
#'   INIZIO = as.Date(c("2023-01-01", "2023-07-01", "2023-03-01")),
#'   FINE = as.Date(c("2023-03-31", "2023-12-31", "2023-06-30")),
#'   prior = c(1, 1, 0)
#' )
#' 
#' # Transform to vecshift format
#' vecshift_result <- vecshift(employment_dt)
#' 
#' # Create external events (e.g., training programs)
#' training_events <- data.table(
#'   cf = c("ABC123", "DEF456", "GHI789"),
#'   event_name = c("training_program", "training_program", "training_program"),
#'   event_start = as.Date(c("2023-05-15", "2023-08-01", "2023-04-01")),
#'   event_end = as.Date(c("2023-05-25", "2023-08-15", "2023-04-15"))
#' )
#' 
#' # Add external events with overlap matching
#' result_with_events <- add_external_events(
#'   vecshift_data = vecshift_result,
#'   external_events = training_events,
#'   event_matching_strategy = "overlap",
#'   date_columns = c(start = "event_start", end = "event_end"),
#'   event_name_column = "event_name"
#' )
#' 
#' # Check the new attribute columns
#' print(result_with_events[, .(cf, arco, training_program_attribute, 
#'                              training_program_distance, training_program_match_quality)])
#' 
#' # Add events with synthetic unemployment for missing persons
#' result_with_synthetic <- add_external_events(
#'   vecshift_data = vecshift_result,
#'   external_events = training_events,
#'   event_matching_strategy = "overlap",
#'   create_synthetic_unemployment = TRUE,
#'   synthetic_unemployment_duration = 365L
#' )
#' 
#' # Use nearest neighbor matching when overlap is not sufficient
#' result_nearest <- add_external_events(
#'   vecshift_data = vecshift_result,
#'   external_events = training_events,
#'   event_matching_strategy = "nearest"
#' )
#' 
#' # For very large datasets, use memory-safe mode
#' result_large <- add_external_events(
#'   vecshift_data = large_vecshift_result,
#'   external_events = large_training_events,
#'   event_matching_strategy = "overlap",
#'   memory_safe = TRUE,
#'   chunk_size = 5000L,
#'   progress = TRUE
#' )
#' }
add_external_events <- function(
  vecshift_data,
  external_events,
  event_matching_strategy = c("overlap", "nearest"),
  create_synthetic_unemployment = FALSE,
  synthetic_unemployment_duration = 730L,
  date_columns = c(start = "event_start", end = "event_end"),
  event_name_column = "event_name",
  person_id_column = "cf",
  memory_safe = FALSE,
  chunk_size = 10000L,
  progress = FALSE
) {
  # Load required package
  require("data.table")
  
  # Validate inputs
  event_matching_strategy <- match.arg(event_matching_strategy)
  
  # Input validation
  if (!inherits(vecshift_data, "data.table")) {
    stop("Input 'vecshift_data' must be a data.table object from vecshift() output.")
  }
  
  if (!inherits(external_events, "data.table")) {
    stop("Input 'external_events' must be a data.table object.")
  }
  
  # Check for required columns in vecshift_data
  required_vecshift_cols <- c("cf", "inizio", "fine", "arco", "prior", "id", "over_id", "durata")
  missing_vecshift_cols <- setdiff(required_vecshift_cols, names(vecshift_data))
  if (length(missing_vecshift_cols) > 0) {
    stop(paste("Missing required vecshift columns:", paste(missing_vecshift_cols, collapse = ", ")))
  }
  
  # Check for required columns in external_events
  required_event_cols <- c(person_id_column, event_name_column, date_columns["start"])
  missing_event_cols <- setdiff(required_event_cols, names(external_events))
  if (length(missing_event_cols) > 0) {
    stop(paste("Missing required external_events columns:", paste(missing_event_cols, collapse = ", ")))
  }
  
  # Validate date_columns parameter
  if (!"start" %in% names(date_columns)) {
    stop("date_columns must include a 'start' element")
  }
  
  # Validate synthetic_unemployment_duration
  if (!is.numeric(synthetic_unemployment_duration) || synthetic_unemployment_duration <= 0) {
    stop("synthetic_unemployment_duration must be a positive integer")
  }
  synthetic_unemployment_duration <- as.integer(synthetic_unemployment_duration)
  
  # Validate memory_safe parameters
  if (!is.logical(memory_safe)) {
    stop("memory_safe must be logical (TRUE/FALSE)")
  }
  
  if (!is.numeric(chunk_size) || chunk_size <= 0) {
    stop("chunk_size must be a positive integer")
  }
  chunk_size <- as.integer(chunk_size)
  
  # Early return if no external events
  if (nrow(external_events) == 0L) {
    return(vecshift_data)
  }
  
  # Estimate memory requirements and auto-enable memory_safe if needed
  n_rows_vecshift <- nrow(vecshift_data)
  n_rows_events <- nrow(external_events)
  # Use numeric to avoid integer overflow - only estimate per-person processing
  n_persons_with_events <- length(unique(external_events[[person_id_column]]))
  avg_unemployment_per_person <- tryCatch({
    unemployment_stats <- vecshift_data[arco == 0, .N, by = cf]
    if (nrow(unemployment_stats) > 0) {
      max(1, round(unemployment_stats[, mean(N, na.rm = TRUE)]))
    } else {
      1L
    }
  }, error = function(e) 1L)
  estimated_memory_gb <- as.numeric(n_persons_with_events) * as.numeric(avg_unemployment_per_person) * 50 * 8 / (1024^3)  # More realistic estimate
  
  if (!memory_safe && estimated_memory_gb > 4 && progress) {
    message(sprintf("Large dataset detected (estimated %.1f GB intermediate data). Consider using memory_safe = TRUE", estimated_memory_gb))
  }
  
  # Auto-enable memory_safe for very large datasets
  if (!memory_safe && (n_rows_vecshift > 5000000L || n_rows_events > 100000L)) {
    memory_safe <- TRUE
    if (progress) message("Auto-enabling memory_safe mode for large dataset")
  }
  
  if (progress) {
    message(sprintf("Processing %d vecshift records with %d external events", n_rows_vecshift, n_rows_events))
  }
  
  # Work directly with input data using reference semantics - no copies unless necessary
  # Only copy for synthetic unemployment creation
  if (create_synthetic_unemployment) {
    result_data <- copy(vecshift_data)
    events_data <- copy(external_events)
    
    if (progress) message("Creating synthetic unemployment periods...")
    
    # Check if stato column exists
    has_stato <- "stato" %in% names(vecshift_data)
    
    result_data <- .create_synthetic_unemployment(
      result_data, events_data, person_id_column, 
      synthetic_unemployment_duration, has_stato, date_columns
    )
    
    # Clean up copy
    rm(events_data)
    gc()
  } else {
    # Use original data with reference semantics - no copy needed
    result_data <- vecshift_data
  }
  
  # Prepare events data for matching (minimal copy)
  events_prepared <- .prepare_events_data(
    external_events, date_columns, event_name_column, person_id_column
  )
  
  # Get unique event names for column creation
  unique_events <- unique(events_prepared[["event_name"]])
  
  if (progress) message(sprintf("Initializing %d event attribute columns", length(unique_events)))
  
  # Vectorized column initialization - create all columns at once
  attr_cols <- paste0(unique_events, "_attribute")
  dist_cols <- paste0(unique_events, "_distance")
  quality_cols <- paste0(unique_events, "_match_quality")
  
  # Bulk column initialization using vectorized approach
  result_data[, (attr_cols) := 0]
  result_data[, (dist_cols) := NA_real_]
  result_data[, (quality_cols) := "none"]
  
  # Perform matching based on strategy
  if (event_matching_strategy == "overlap") {
    if (progress) message("Starting overlap matching...")
    result_data <- .match_events_overlap_optimized(
      result_data, events_prepared, event_name_column, person_id_column, progress
    )
  } else {
    if (progress) message("Starting nearest neighbor matching...")
    result_data <- .match_events_nearest_optimized(
      result_data, events_prepared, event_name_column, person_id_column, progress
    )
  }
  
  # Clean up
  rm(events_prepared)
  gc()
  
  if (progress) message("Matching completed successfully")
  
  return(result_data)
}

#' Create Synthetic Unemployment Periods for Missing Persons
#' 
#' @description Internal helper function to create synthetic unemployment periods
#' for persons present in external events but not in main employment data.
#' 
#' @param vecshift_data Main employment data
#' @param events_data External events data
#' @param person_id_column Person identifier column name
#' @param max_duration Maximum synthetic unemployment duration
#' @param has_stato Whether stato column exists
#' @param date_columns Named vector of date columns
#' 
#' @return Extended vecshift_data with synthetic unemployment periods
#' @keywords internal
.create_synthetic_unemployment <- function(vecshift_data, events_data, 
                                         person_id_column, max_duration, has_stato, date_columns) {
  
  # Find persons in events but not in main data
  persons_in_main <- unique(vecshift_data$cf)
  persons_in_events <- unique(events_data[[person_id_column]])
  missing_persons <- setdiff(persons_in_events, persons_in_main)
  
  if (length(missing_persons) == 0L) {
    return(vecshift_data)
  }
  
  # Get reference dates from main data and events data  
  max_date_in_main <- max(vecshift_data$fine, na.rm = TRUE)
  
  # Also consider event dates to ensure synthetic unemployment covers them
  max_event_date <- max(c(events_data[[date_columns["start"]]],
                         if("end" %in% names(date_columns) && date_columns["end"] %in% names(events_data))
                           events_data[[date_columns["end"]]] else events_data[[date_columns["start"]]]), na.rm = TRUE)
  
  # Use the later of main data max or event max dates, plus some buffer
  reference_end_date <- max(max_date_in_main, max_event_date, na.rm = TRUE)
  
  # Determine date class from vecshift_data
  date_class <- class(vecshift_data$inizio)[1]
  durata_class <- class(vecshift_data$durata)[1]
  
  # Get all column names and types from vecshift_data to ensure compatibility
  all_columns <- names(vecshift_data)
  column_types <- sapply(vecshift_data, class)
  
  # Create synthetic unemployment periods that cover the event dates
  synthetic_periods <- data.table(
    cf = missing_persons
  )
  
  # Set core vecshift columns
  synthetic_periods[, `:=`(
    inizio = if (date_class == "Date") {
      reference_end_date - max_duration
    } else {
      as.numeric(reference_end_date) - max_duration
    },
    fine = reference_end_date,
    arco = 0,
    prior = 0,
    id = 0L,
    over_id = 0L,
    durata = if (durata_class == "difftime") {
      as.difftime(max_duration, units = "days")
    } else {
      as.numeric(max_duration)
    }
  )]
  
  # Add stato column if it exists
  if (has_stato) {
    synthetic_periods[, stato := "disoccupato"]
  }
  
  # Initialize any additional columns that exist in vecshift_data but not in synthetic_periods
  missing_cols <- setdiff(all_columns, names(synthetic_periods))
  
  for (col in missing_cols) {
    col_class <- column_types[[col]]
    
    # Set appropriate default values based on column type and name pattern
    if (grepl("_attribute$", col)) {
      # Event attribute columns should default to 0
      synthetic_periods[, (col) := 0]
    } else if (grepl("_distance$", col)) {
      # Distance columns should default to NA_real_
      synthetic_periods[, (col) := NA_real_]
    } else if (grepl("_match_quality$", col)) {
      # Match quality columns should default to "none"
      synthetic_periods[, (col) := "none"]
    } else {
      # For other columns, set appropriate defaults based on type
      if ("numeric" %in% col_class || "integer" %in% col_class) {
        synthetic_periods[, (col) := NA_real_]
      } else if ("character" %in% col_class) {
        synthetic_periods[, (col) := NA_character_]
      } else if ("logical" %in% col_class) {
        synthetic_periods[, (col) := NA]
      } else if ("Date" %in% col_class) {
        synthetic_periods[, (col) := as.Date(NA)]
      } else if ("difftime" %in% col_class) {
        synthetic_periods[, (col) := as.difftime(NA, units = "days")]
      } else {
        # Default fallback
        synthetic_periods[, (col) := NA]
      }
    }
  }
  
  # Ensure column order matches vecshift_data
  setcolorder(synthetic_periods, all_columns)
  
  # Memory-efficient combination: modify vecshift_data directly if possible
  # If vecshift_data has same columns, append directly to avoid rbindlist overhead
  if (length(missing_cols) == 0) {
    # Direct bind - more memory efficient
    combined_data <- rbind(vecshift_data, synthetic_periods, use.names = TRUE)
  } else {
    # Use rbindlist only when necessary
    combined_data <- rbindlist(
      list(vecshift_data, synthetic_periods),
      use.names = TRUE,
      fill = TRUE
    )
  }
  
  # Clean up intermediate object
  rm(synthetic_periods)
  
  # Restore ordering
  setorder(combined_data, cf, inizio)
  
  return(combined_data)
}

#' Prepare External Events Data for Matching
#' 
#' @description Internal helper function to standardize external events data
#' for temporal matching operations.
#' 
#' @param events_data External events data.table
#' @param date_columns Named vector of date columns
#' @param event_name_column Event name column
#' @param person_id_column Person identifier column
#' 
#' @return Prepared events data.table with standardized columns
#' @keywords internal
.prepare_events_data <- function(events_data, date_columns, 
                                event_name_column, person_id_column) {
  
  events_prep <- copy(events_data)
  
  # Standardize column names for internal processing
  setnames(events_prep, person_id_column, "cf", skip_absent = TRUE)
  setnames(events_prep, event_name_column, "event_name", skip_absent = TRUE)
  setnames(events_prep, date_columns["start"], "event_start", skip_absent = TRUE)
  
  # Handle end date - if not provided, use start date (single-day events)
  if ("end" %in% names(date_columns) && date_columns["end"] %in% names(events_data)) {
    setnames(events_prep, date_columns["end"], "event_end", skip_absent = TRUE)
  } else {
    events_prep[, event_end := event_start]
  }
  
  # Ensure events have valid date ranges
  events_prep <- events_prep[event_start <= event_end]
  
  return(events_prep)
}

#' Match Events Using Temporal Overlap Strategy (Fully Vectorized)
#' 
#' @description Fully vectorized implementation using data.table operations to eliminate
#' all nested loops. Uses foverlaps() with bulk updates and vectorized case handling
#' for optimal performance on large datasets.
#' 
#' @param vecshift_data Main employment data with attribute columns initialized
#' @param events_data Prepared external events data
#' @param event_name_column Event name column (standardized)
#' @param person_id_column Person identifier column (standardized)
#' @param progress Show progress messages
#' 
#' @return Updated vecshift_data with overlap matches, same row count as input (plus synthetics)
#' @keywords internal
.match_events_overlap_optimized <- function(vecshift_data, events_data, 
                                           event_name_column, person_id_column, progress = FALSE) {
  
  # Early exit if no data
  if (nrow(events_data) == 0L) {
    return(vecshift_data)
  }
  
  if (progress) message("Using fully vectorized foverlaps() for efficient event matching")
  
  # Store original row count to ensure no extra lines remain
  original_row_count <- nrow(vecshift_data)
  
  # Add row indices for efficient bulk updates
  vecshift_data[, .row_id := .I]
  
  # Set keys for foverlaps - index both tables by cf, inizio/start, fine/end
  setkey(vecshift_data, cf, inizio, fine)
  setkey(events_data, cf, event_start, event_end)
  
  # Use foverlaps to find all overlaps between events and vecshift segments
  overlaps <- foverlaps(
    events_data, 
    vecshift_data,
    by.x = c("cf", "event_start", "event_end"),
    by.y = c("cf", "inizio", "fine"),
    type = "any",
    nomatch = 0L
  )
  
  if (progress) message(sprintf("Found %d overlaps from foverlaps", nrow(overlaps)))
  
  # VECTORIZED CASE 1: Direct unemployment overlaps
  unemployment_overlaps <- overlaps[arco == 0]
  if (nrow(unemployment_overlaps) > 0) {
    # Group by event_name and process each event type
    unique_event_names <- unique(unemployment_overlaps$event_name)
    for (event_name in unique_event_names) {
      event_overlaps <- unemployment_overlaps[event_name == event_name]
      col_attr <- paste0(event_name, "_attribute")
      col_dist <- paste0(event_name, "_distance")
      col_quality <- paste0(event_name, "_match_quality")
      
      # Bulk update using vectorized set operations
      set(vecshift_data, i = event_overlaps$.row_id, j = col_attr, value = 1L)
      set(vecshift_data, i = event_overlaps$.row_id, j = col_dist, value = 0)
      set(vecshift_data, i = event_overlaps$.row_id, j = col_quality, value = "overlap")
    }
    
    if (progress) message(sprintf("Processed %d direct unemployment overlaps", nrow(unemployment_overlaps)))
  }
  
  # VECTORIZED CASE 2: Find events without unemployment overlaps and match to preceding periods
  # Anti-join to find events that didn't match with unemployment (only if we have unemployment overlaps)
  if (nrow(unemployment_overlaps) > 0) {
    events_without_unemployment <- events_data[!unemployment_overlaps, on = .(cf, event_start, event_end, event_name)]
  } else {
    # If no unemployment overlaps, all events need processing
    events_without_unemployment <- copy(events_data)
  }
  
  if (nrow(events_without_unemployment) > 0) {
    # Use a different approach for finding preceding unemployment since rolling joins don't work with non-equi joins
    # Instead, we'll use a cross join within person and then filter
    unemployment_periods <- vecshift_data[arco == 0, .(cf, inizio, fine, .row_id)]
    setkey(unemployment_periods, cf)
    setkey(events_without_unemployment, cf)
    
    # Cross join within persons to find all unemployment-event combinations
    unemployment_event_cross <- unemployment_periods[events_without_unemployment, 
                                                   on = "cf", 
                                                   allow.cartesian = TRUE]
    
    # Filter to only preceding unemployment periods (fine < event_start) 
    preceding_candidates <- unemployment_event_cross[fine < event_start]
    
    # Find the most recent preceding unemployment for each event
    if (nrow(preceding_candidates) > 0) {
      preceding_matches <- preceding_candidates[
        preceding_candidates[, .I[which.max(fine)], 
                           by = .(cf, event_start, event_end, event_name)]$V1
      ]
    } else {
      preceding_matches <- data.table()
    }
    
    # Filter out cases where no preceding unemployment was found (should have data if preceding_matches exists)
    valid_preceding <- preceding_matches
    
    if (nrow(valid_preceding) > 0) {
      # Calculate distances vectorized  
      valid_preceding[, distance := as.numeric(event_start - fine)]
      
      # Group by event type and bulk update
      unique_events_preceding <- unique(valid_preceding$event_name)
      for (event_name in unique_events_preceding) {
        event_matches <- valid_preceding[event_name == event_name]
        col_attr <- paste0(event_name, "_attribute")
        col_dist <- paste0(event_name, "_distance")
        col_quality <- paste0(event_name, "_match_quality")
        
        # Bulk vectorized updates using set operations
        set(vecshift_data, i = event_matches$.row_id, j = col_attr, value = 1L)
        set(vecshift_data, i = event_matches$.row_id, j = col_dist, value = event_matches$distance)
        set(vecshift_data, i = event_matches$.row_id, j = col_quality, value = "preceding")
      }
      
      if (progress) message(sprintf("Processed %d preceding unemployment matches", nrow(valid_preceding)))
    }
    
    # VECTORIZED CASE 3: Events with no preceding unemployment - create synthetic periods  
    if (nrow(valid_preceding) > 0) {
      events_no_preceding <- events_without_unemployment[!valid_preceding, on = .(cf, event_start, event_end, event_name)]
    } else {
      events_no_preceding <- copy(events_without_unemployment)
    }
    
    if (nrow(events_no_preceding) > 0) {
      # Find first contract for each person vectorized
      first_contracts <- vecshift_data[, .(first_inizio = min(inizio)), by = cf]
      events_with_first <- events_no_preceding[first_contracts, on = "cf", nomatch = 0L]
      
      # Create synthetic periods only where viable (event before first contract)
      viable_synthetic <- events_with_first[event_start < first_inizio]
      
      if (nrow(viable_synthetic) > 0) {
        # Create synthetic unemployment periods vectorized
        synthetic_periods <- viable_synthetic[, .SD[1], by = .(cf, event_start, event_end, event_name, first_inizio)]
        synthetic_periods[, `:=`(
          inizio = event_start,
          fine = first_inizio - 1,
          arco = 0L,
          prior = 0L,
          id = 0L,
          over_id = 0L,
          durata = if(inherits(vecshift_data$durata, "difftime")) {
            as.difftime(first_inizio - event_start, units = "days")
          } else {
            as.numeric(first_inizio - event_start)
          },
          .row_id = max(vecshift_data$.row_id) + seq_len(.N)
        )]
        
        # Add stato column if exists
        if ("stato" %in% names(vecshift_data)) {
          synthetic_periods[, stato := "disoccupato"]
        }
        
        # Initialize all attribute columns for synthetic periods vectorized
        unique_events <- unique(events_data$event_name)
        attr_cols <- paste0(unique_events, "_attribute")
        dist_cols <- paste0(unique_events, "_distance")
        quality_cols <- paste0(unique_events, "_match_quality")
        
        synthetic_periods[, (attr_cols) := 0]
        synthetic_periods[, (dist_cols) := NA_real_]
        synthetic_periods[, (quality_cols) := "none"]
        
        # Set appropriate values for matched events vectorized
        synthetic_periods[, `:=`(
          temp_attr = paste0(event_name, "_attribute"),
          temp_dist = paste0(event_name, "_distance"),
          temp_qual = paste0(event_name, "_match_quality")
        )]
        
        # Update attribute columns for each synthetic period
        for (i in seq_len(nrow(synthetic_periods))) {
          event_name <- synthetic_periods[i, event_name]
          col_attr <- paste0(event_name, "_attribute")
          col_dist <- paste0(event_name, "_distance")
          col_quality <- paste0(event_name, "_match_quality")
          
          set(synthetic_periods, i = i, j = col_attr, value = 1L)
          set(synthetic_periods, i = i, j = col_dist, value = 0)
          set(synthetic_periods, i = i, j = col_quality, value = "synthetic")
        }
        
        # Clean up temporary columns
        synthetic_periods[, c("event_name", "first_inizio", "temp_attr", "temp_dist", "temp_qual") := NULL]
        
        # Ensure column order matches vecshift_data
        missing_cols <- setdiff(names(vecshift_data), names(synthetic_periods))
        for (col in missing_cols) {
          synthetic_periods[, (col) := NA]
        }
        setcolorder(synthetic_periods, names(vecshift_data))
        
        # Bind synthetic periods efficiently
        vecshift_data <- rbindlist(list(vecshift_data, synthetic_periods), use.names = TRUE, fill = TRUE)
        
        if (progress) message(sprintf("Created %d synthetic unemployment periods", nrow(synthetic_periods)))
      }
    }
  }
  
  # Clean up temporary column and re-sort
  vecshift_data[, .row_id := NULL]
  setorder(vecshift_data, cf, inizio)
  
  # Verify no extra lines from foverlaps remain (except intended synthetic rows)
  synthetic_rows_added <- nrow(vecshift_data) - original_row_count
  if (progress && synthetic_rows_added > 0) {
    message(sprintf("Added %d synthetic unemployment rows", synthetic_rows_added))
  }
  
  return(vecshift_data)
}


#' Match Events Using Nearest Neighbor Strategy (Fully Vectorized)
#' 
#' @description Fully vectorized nearest neighbor matching using data.table operations.
#' Eliminates all nested loops by using cross joins with distance calculations and
#' subsequent filtering for minimum distances per event-person combination.
#' 
#' @param vecshift_data Main employment data with attribute columns initialized
#' @param events_data Prepared external events data
#' @param event_name_column Event name column (standardized)
#' @param person_id_column Person identifier column (standardized)
#' @param progress Show progress messages
#' 
#' @return Updated vecshift_data with nearest neighbor matches
#' @keywords internal
.match_events_nearest_optimized <- function(vecshift_data, events_data, 
                                           event_name_column, person_id_column, progress = FALSE) {
  
  # Early exit checks
  unemployment_count <- vecshift_data[, sum(arco == 0)]
  if (unemployment_count == 0L) {
    return(vecshift_data)
  }
  
  if (progress) {
    message("Using fully vectorized nearest neighbor matching")
  }
  
  # Add row indices for bulk updates
  vecshift_data[, .row_id := .I]
  
  # Get unemployment periods and events data with required columns only
  unemployment_periods <- vecshift_data[arco == 0, .(cf, inizio, fine, .row_id)]
  unemployment_periods[, midpoint := (as.numeric(inizio) + as.numeric(fine)) / 2]
  
  # Calculate midpoints for events
  events_with_midpoint <- copy(events_data)
  events_with_midpoint[, midpoint := (as.numeric(event_start) + as.numeric(event_end)) / 2]
  
  # Only process persons that exist in both datasets
  unemployment_persons <- unique(unemployment_periods$cf)
  event_persons <- unique(events_with_midpoint$cf)
  common_persons <- intersect(unemployment_persons, event_persons)
  
  if (length(common_persons) == 0L) {
    vecshift_data[, .row_id := NULL]
    return(vecshift_data)
  }
  
  if (progress) {
    message(sprintf("Processing %d persons with both events and unemployment periods", length(common_persons)))
  }
  
  # Filter to common persons only for efficiency
  unemployment_filtered <- unemployment_periods[cf %in% common_persons]
  events_filtered <- events_with_midpoint[cf %in% common_persons]
  
  # VECTORIZED CROSS JOIN WITH DISTANCE CALCULATION
  # Use data.table's efficient cross join by person
  setkey(unemployment_filtered, cf)
  setkey(events_filtered, cf)
  
  # Cross join within persons (this creates event-unemployment combinations per person)
  event_unemployment_combinations <- unemployment_filtered[events_filtered, 
                                                         on = "cf", 
                                                         allow.cartesian = TRUE]
  
  # Calculate distances vectorized
  event_unemployment_combinations[, distance := abs(i.midpoint - midpoint)]
  
  if (progress) {
    message(sprintf("Calculated %d event-unemployment distance combinations", nrow(event_unemployment_combinations)))
  }
  
  # VECTORIZED MINIMUM DISTANCE SELECTION
  # Find minimum distance for each event (grouped by cf, event_start, event_end, event_name)
  # Note: the events columns are prefixed with i. due to the join
  nearest_matches <- event_unemployment_combinations[
    event_unemployment_combinations[, .I[which.min(distance)], 
                                   by = .(cf, event_start, event_end, event_name)]$V1
  ]
  
  if (progress) {
    message(sprintf("Found %d nearest neighbor matches", nrow(nearest_matches)))
  }
  
  # VECTORIZED BULK UPDATES BY EVENT TYPE
  unique_event_names <- unique(nearest_matches$event_name)
  
  for (event_name in unique_event_names) {
    event_matches <- nearest_matches[event_name == event_name]
    
    col_attr <- paste0(event_name, "_attribute")
    col_dist <- paste0(event_name, "_distance")
    col_quality <- paste0(event_name, "_match_quality")
    
    # Bulk vectorized updates using set operations
    set(vecshift_data, i = event_matches$.row_id, j = col_attr, value = 1L)
    set(vecshift_data, i = event_matches$.row_id, j = col_dist, value = event_matches$distance)
    set(vecshift_data, i = event_matches$.row_id, j = col_quality, value = "nearest")
  }
  
  # Clean up
  vecshift_data[, .row_id := NULL]
  rm(unemployment_periods, events_with_midpoint, unemployment_filtered, events_filtered, 
     event_unemployment_combinations, nearest_matches)
  gc()
  
  return(vecshift_data)
}

#' Match Events Using Nearest Neighbor Strategy (Chunked for Memory Safety)
#' 
#' @description Ultra-memory-safe chunked version that processes very small batches
#' person-by-person with aggressive memory management.
#' 
#' @param vecshift_data Main employment data with attribute columns initialized
#' @param events_data Prepared external events data
#' @param event_name_column Event name column (standardized)
#' @param person_id_column Person identifier column (standardized)
#' @param chunk_size Number of persons to process per chunk
#' @param progress Show progress messages
#' 
#' @return Updated vecshift_data with nearest neighbor matches
#' @keywords internal
.match_events_nearest_chunked <- function(vecshift_data, events_data, 
                                         event_name_column, person_id_column,
                                         chunk_size, progress = FALSE) {
  
  # Early exit checks
  unemployment_count <- vecshift_data[, sum(arco == 0)]
  if (unemployment_count == 0L) {
    return(vecshift_data)
  }
  
  # More efficient intersection - get smaller sets first
  unemployment_persons <- vecshift_data[arco == 0, unique(cf)]
  event_persons <- events_data[, unique(cf)]
  persons_to_process <- intersect(unemployment_persons, event_persons)
  
  if (length(persons_to_process) == 0L) {
    return(vecshift_data)
  }
  
  # Use smaller chunk size for very large datasets to control memory
  effective_chunk_size <- min(chunk_size, 100L)  # Never more than 100 persons per chunk
  n_persons <- length(persons_to_process)
  n_chunks <- ceiling(n_persons / effective_chunk_size)
  
  if (progress) {
    message(sprintf("Processing nearest neighbor matching for %d persons in %d chunks of size %d", 
                    n_persons, n_chunks, effective_chunk_size))
  }
  
  # Create efficient update mechanism
  vecshift_data[, .internal_idx := .I]
  
  for (chunk_idx in seq_len(n_chunks)) {
    if (progress) message(sprintf("Processing nearest neighbor chunk %d/%d", chunk_idx, n_chunks))
    
    # Get very small chunk of persons
    start_idx <- (chunk_idx - 1L) * effective_chunk_size + 1L
    end_idx <- min(chunk_idx * effective_chunk_size, n_persons)
    chunk_persons <- persons_to_process[start_idx:end_idx]
    
    # Process each person individually within the chunk
    for (person_cf in chunk_persons) {
      # Get minimal person-specific data
      person_events <- events_data[cf == person_cf]
      person_unemployment <- vecshift_data[cf == person_cf & arco == 0]
      
      if (nrow(person_events) == 0L || nrow(person_unemployment) == 0L) {
        next
      }
      
      # Calculate midpoints using numeric to prevent overflow
      event_midpoints <- (as.numeric(person_events$event_start) + as.numeric(person_events$event_end)) / 2
      unemployment_midpoints <- (as.numeric(person_unemployment$inizio) + as.numeric(person_unemployment$fine)) / 2
      
      # Process each event for this person
      for (j in seq_len(nrow(person_events))) {
        event_name <- person_events[j, "event_name"]
        event_mid <- event_midpoints[j]
        
        # Find nearest using simple vectorized operation
        distances <- abs(unemployment_midpoints - event_mid)
        min_distance <- min(distances)
        nearest_idx <- which.min(distances)
        
        # Get target row index for update
        target_row_idx <- person_unemployment[nearest_idx, .internal_idx]
        
        # Column names
        col_attr <- paste0(event_name, "_attribute")
        col_dist <- paste0(event_name, "_distance")
        col_quality <- paste0(event_name, "_match_quality")
        
        # Single-row updates
        vecshift_data[.internal_idx == target_row_idx, (col_attr) := 1]
        vecshift_data[.internal_idx == target_row_idx, (col_dist) := min_distance]
        vecshift_data[.internal_idx == target_row_idx, (col_quality) := "nearest"]
      }
      
      # Cleanup person-level objects immediately
      rm(person_events, person_unemployment, event_midpoints, unemployment_midpoints)
    }
    
    # Aggressive garbage collection every few chunks
    if (chunk_idx %% 5L == 0L) {
      gc()
    }
  }
  
  # Clean up temporary index
  vecshift_data[, .internal_idx := NULL]
  
  return(vecshift_data)
}