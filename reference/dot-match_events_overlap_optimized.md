# Match Events Using Temporal Overlap Strategy (Fully Vectorized)

Fully vectorized implementation using data.table operations to eliminate
all nested loops. Uses foverlaps() with bulk updates and vectorized case
handling for optimal performance on large datasets.

## Usage

``` r
.match_events_overlap_optimized(
  vecshift_data,
  events_data,
  event_name_column,
  person_id_column,
  progress = FALSE
)
```

## Arguments

- vecshift_data:

  Main employment data with attribute columns initialized

- events_data:

  Prepared external events data

- event_name_column:

  Event name column (standardized)

- person_id_column:

  Person identifier column (standardized)

- progress:

  Show progress messages

## Value

Updated vecshift_data with overlap matches, same row count as input
(plus synthetics)
