# Create Synthetic Unemployment Periods for Missing Persons

Internal helper function to create synthetic unemployment periods for
persons present in external events but not in main employment data.

## Usage

``` r
.create_synthetic_unemployment(
  vecshift_data,
  events_data,
  person_id_column,
  max_duration,
  has_stato,
  date_columns
)
```

## Arguments

- vecshift_data:

  Main employment data

- events_data:

  External events data

- person_id_column:

  Person identifier column name

- max_duration:

  Maximum synthetic unemployment duration

- has_stato:

  Whether stato column exists

- date_columns:

  Named vector of date columns

## Value

Extended vecshift_data with synthetic unemployment periods
