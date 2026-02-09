# Match Events Using Nearest Neighbor Strategy (Fully Vectorized)

Fully vectorized nearest neighbor matching using data.table operations.
Eliminates all nested loops by using cross joins with distance
calculations and subsequent filtering for minimum distances per
event-person combination.

## Usage

``` r
.match_events_nearest_optimized(
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

Updated vecshift_data with nearest neighbor matches
