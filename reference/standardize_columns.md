# Standardize Column Names for Employment Data

Maps custom column names to standardized internal names used by vecshift
functions. Allows flexibility in input data structure while maintaining
consistent processing.

## Usage

``` r
standardize_columns(dt, column_map, validate = TRUE)
```

## Arguments

- dt:

  Input data.table with employment records

- column_map:

  Named list mapping standard names to actual column names:

  - `id`: Contract identifier column name (required)

  - `cf`: Person identifier column name (required)

  - `inizio`: Start date column name (required)

  - `fine`: End date column name (required)

  - `prior`: Employment type column name (required)

- validate:

  Logical. If TRUE, validates the mapped columns

## Value

Data.table with standardized column names

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
# Data with custom column names
dt <- data.table(
  contract_id = 1:3,
  person_code = c("A001", "A001", "B002"),
  start_date = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
  end_date = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
  employment_type = c(1, 0, 1)
)

# Define column mapping
col_map <- list(
  id = "contract_id",
  cf = "person_code",
  inizio = "start_date",
  fine = "end_date",
  prior = "employment_type"
)

# Standardize column names
standardized_dt <- standardize_columns(dt, col_map)
} # }
```
