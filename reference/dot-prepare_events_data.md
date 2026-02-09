# Prepare External Events Data for Matching

Internal helper function to standardize external events data for
temporal matching operations.

## Usage

``` r
.prepare_events_data(
  events_data,
  date_columns,
  event_name_column,
  person_id_column
)
```

## Arguments

- events_data:

  External events data.table

- date_columns:

  Named vector of date columns

- event_name_column:

  Event name column

- person_id_column:

  Person identifier column

## Value

Prepared events data.table with standardized columns
