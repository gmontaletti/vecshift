# Match Events Using Nearest Neighbor Strategy (Chunked for Memory Safety)

Ultra-memory-safe chunked version that processes very small batches
person-by-person with aggressive memory management.

## Usage

``` r
.match_events_nearest_chunked(
  vecshift_data,
  events_data,
  event_name_column,
  person_id_column,
  chunk_size,
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

- chunk_size:

  Number of persons to process per chunk

- progress:

  Show progress messages

## Value

Updated vecshift_data with nearest neighbor matches
