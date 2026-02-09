# Add External Event Attributes to Unemployment Periods

Adds external event attributes to unemployment periods in vecshift
output by matching events with unemployment segments (arco == 0). Events
are matched using either temporal overlap or nearest neighbor
strategies. The function can also create synthetic unemployment periods
for persons present in external events but not in the main employment
data.

## Usage

``` r
add_external_events(
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
)
```

## Arguments

- vecshift_data:

  A data.table output from vecshift() containing temporal employment
  segments with required columns: cf, inizio, fine, arco, prior, id,
  over_id, durata. Optional: stato column for employment status.

- external_events:

  A data.table containing external events with person identifiers and
  event information. Must contain the person_id_column and
  event_name_column, plus at least one date column specified in
  date_columns.

- event_matching_strategy:

  Character. Matching strategy to use:

  - `"overlap"`: Match events that temporally overlap with unemployment
    periods

  - `"nearest"`: Find nearest unemployment period when no overlap exists

- create_synthetic_unemployment:

  Logical. Whether to create synthetic unemployment periods for persons
  in external_events but not in vecshift_data (default: FALSE).

- synthetic_unemployment_duration:

  Integer. Maximum duration in days for synthetic unemployment periods
  (default: 730L for 2 years).

- date_columns:

  Named character vector specifying date column names in
  external_events. Names should be "start" and optionally "end".
  Default: c(start = "event_start", end = "event_end").

- event_name_column:

  Character. Name of column in external_events containing event
  names/types (default: "event_name").

- person_id_column:

  Character. Name of column in external_events containing person
  identifiers that match the 'cf' column in vecshift_data (default:
  "cf").

- memory_safe:

  Logical. Enable memory-safe mode for large datasets. When TRUE,
  processes data in chunks to reduce peak memory usage at the cost of
  some performance (default: FALSE).

- chunk_size:

  Integer. Number of persons to process per chunk when memory_safe =
  TRUE (default: 10000L).

- progress:

  Logical. Show progress messages for long-running operations (default:
  FALSE).

## Value

A data.table with the same structure as vecshift_data, extended with:

- New columns: "event_name_attribute" (1 for matched unemployment
  periods, 0 otherwise)

- New columns: "event_name_distance" (days between event and
  unemployment period)

- New columns: "event_name_match_quality" (overlap, nearest, or none)

- Synthetic unemployment periods if create_synthetic_unemployment = TRUE

Maintains original ordering by cf and temporal sequence.

## Details

The function implements two matching strategies:

- **Overlap matching**: Events that temporally overlap with unemployment
  periods

- **Nearest matching**: When no overlap exists, finds the nearest
  unemployment period

For each external event, the function:

- Creates a new column "event_name_attribute" with value 1 for matched
  periods

- Adds distance/quality metrics in additional columns

- Maintains all original vecshift data integrity

- Handles multiple events per person

**Synthetic Unemployment Creation**: When create_synthetic_unemployment
= TRUE, the function creates unemployment periods for persons in
external_events but not in vecshift_data:

- Creates periods up to synthetic_unemployment_duration days

- Uses max(fine) from main dataset as the reference endpoint

- Follows vecshift formatting conventions

**Memory Optimization**: The function includes several memory
optimization strategies:

- In-place modifications using data.table reference semantics

- Chunked processing for large datasets when memory_safe = TRUE

- Early filtering to reduce intermediate object sizes

- Efficient rolling joins for nearest neighbor matching

- Immediate cleanup of temporary objects

## Note

The function optimizes performance using data.table operations and
follows vecshift coding conventions. It handles edge cases such as:

- Events with only start dates (treated as single-day events)

- Multiple events per person

- Persons with no unemployment periods

- Date format consistency between datasets

- Proper integration with over_id consolidation logic

For very large datasets (\>1M records), consider using memory_safe =
TRUE to enable chunked processing that reduces peak memory usage.

## See also

[`vecshift`](https://gmontaletti.github.io/vecshift/reference/vecshift.md)
for the main temporal transformation function
[`add_unemployment_periods`](https://gmontaletti.github.io/vecshift/reference/add_unemployment_periods.md)
for adding unemployment periods

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)

# Create sample employment data
employment_dt <- data.table(
  id = 1:3,
  cf = c("ABC123", "ABC123", "DEF456"),
  INIZIO = as.Date(c("2023-01-01", "2023-07-01", "2023-03-01")),
  FINE = as.Date(c("2023-03-31", "2023-12-31", "2023-06-30")),
  prior = c(1, 1, 0)
)

# Transform to vecshift format
vecshift_result <- vecshift(employment_dt)

# Create external events (e.g., training programs)
training_events <- data.table(
  cf = c("ABC123", "DEF456", "GHI789"),
  event_name = c("training_program", "training_program", "training_program"),
  event_start = as.Date(c("2023-05-15", "2023-08-01", "2023-04-01")),
  event_end = as.Date(c("2023-05-25", "2023-08-15", "2023-04-15"))
)

# Add external events with overlap matching
result_with_events <- add_external_events(
  vecshift_data = vecshift_result,
  external_events = training_events,
  event_matching_strategy = "overlap",
  date_columns = c(start = "event_start", end = "event_end"),
  event_name_column = "event_name"
)

# Check the new attribute columns
print(result_with_events[, .(cf, arco, training_program_attribute,
                             training_program_distance, training_program_match_quality)])

# Add events with synthetic unemployment for missing persons
result_with_synthetic <- add_external_events(
  vecshift_data = vecshift_result,
  external_events = training_events,
  event_matching_strategy = "overlap",
  create_synthetic_unemployment = TRUE,
  synthetic_unemployment_duration = 365L
)

# Use nearest neighbor matching when overlap is not sufficient
result_nearest <- add_external_events(
  vecshift_data = vecshift_result,
  external_events = training_events,
  event_matching_strategy = "nearest"
)

# For very large datasets, use memory-safe mode
result_large <- add_external_events(
  vecshift_data = large_vecshift_result,
  external_events = large_training_events,
  event_matching_strategy = "overlap",
  memory_safe = TRUE,
  chunk_size = 5000L,
  progress = TRUE
)
} # }
```
