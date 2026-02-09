# Transform Employment Records into Temporal Segments (Core Function)

Processes employment contract records with temporal boundaries to create
continuous temporal segments that track employment status over time.
This core function handles the event-based transformation logic,
identifying unemployment periods (when no contracts are active), single
employment periods, and overlapping employment situations.

## Usage

``` r
vecshift(dt)
```

## Arguments

- dt:

  A data.table containing employment contract records with the following
  required columns:

  - `id`: Contract identifier (unique key for each employment contract)

  - `cf`: Person identifier (e.g., fiscal code)

  - `inizio`: Contract start date (Date or numeric)

  - `fine`: Contract end date (Date or numeric)

  - `prior`: Employment type indicator (0 or negative for part-time,
    positive for full-time)

## Value

A data.table with temporal segments containing:

- `cf`: Person identifier

- `inizio`: Segment start date

- `fine`: Segment end date

- `arco`: Number of overlapping contracts (0 = unemployment)

- `prior`: Employment type for the segment (0 = part-time, 1 =
  full-time)

- `id`: Contract ID (0 for unemployment periods)

- `durata`: Duration of the segment in days

## Details

The function implements precise date logic for temporal processing:

- Employment contracts are inclusive of both start and end dates

- A person works ON both inizio and fine dates

- End events are created at fine date

- Unemployment periods are identified (arco=0) and adjusted: inizio+1
  and fine-1

- This ensures correct temporal boundaries for all segments

The algorithm creates events for each contract start (+1) and end (-1 at
fine), then uses cumulative sums to track overlapping contracts.
Unemployment segments are identified (arco=0) and their dates are
adjusted to represent the actual unemployment period.

## Note

This function performs only the core temporal transformation. For
employment status classification, use
[`classify_employment_status`](https://gmontaletti.github.io/vecshift/reference/classify_employment_status.md)
separately or use
[`process_employment_pipeline`](https://gmontaletti.github.io/vecshift/reference/process_employment_pipeline.md)
for a complete processing pipeline.

## See also

[`classify_employment_status`](https://gmontaletti.github.io/vecshift/reference/classify_employment_status.md)
for status classification details
[`process_employment_pipeline`](https://gmontaletti.github.io/vecshift/reference/process_employment_pipeline.md)
for a complete processing pipeline

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
# Create sample employment data
dt <- data.table(
  id = 1:3,
  cf = c("ABC123", "ABC123", "DEF456"),
  inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
  fine = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
  prior = c(1, 0, 1)  # 1 = full-time, 0 = part-time
)

# Transform to temporal segments (pure transformation)
result <- vecshift(dt)
print(result)

# Apply status classification separately
result_with_status <- classify_employment_status(result)
print(result_with_status)

# Use complete pipeline for integrated processing
result_pipeline <- process_employment_pipeline(dt)
print(result_pipeline)
} # }
```
