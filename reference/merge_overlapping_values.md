# Merge Overlapping Values for Employment Periods

Processes merged segment data to handle overlapping employment periods
(arco \> 1) by combining values from the previous row according to data
type-specific rules. This function operates on the output from
merge_original_columns() and handles the merging logic for overlapping
employment scenarios.

## Usage

``` r
merge_overlapping_values(segments_with_columns, columns)
```

## Arguments

- segments_with_columns:

  A data.table containing the output from merge_original_columns(),
  which includes temporal segments with merged columns. Must contain
  'cf', 'arco' columns and be ordered by person and time.

- columns:

  Character vector specifying which columns to process for overlapping
  periods. These columns must exist in segments_with_columns.

## Value

A data.table with the same structure as the input, but with overlapping
period values merged according to data type rules. All other columns and
rows remain unchanged.

## Details

When employment periods overlap (arco \> 1), this function combines
values from the previous row with the current row based on data type:

- **Character/Factor columns**: Values are combined with "-\>" separator
  showing transitions (e.g., "CompanyA-\>CompanyB")

- **Numeric columns**: Values are summed (e.g., overlapping salaries)

- **Multiple overlaps (arco \> 2)**: Chain all values in sequence for
  characters, continue summing for numerics

- **Factors**: Converted to character before processing

The function processes rows sequentially within each person (cf),
looking at the previous row's value when arco \> 1 and merging
accordingly. For arco = 1 or 0, values remain unchanged.

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)

# Create sample employment data with overlapping periods
original_dt <- data.table(
  id = 1:3,
  cf = c("PERSON001", "PERSON001", "PERSON001"),
  INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-06-01")),
  FINE = as.Date(c("2023-06-30", "2023-08-31", "2023-12-31")),
  prior = c(1, 1, 0),
  company = c("CompanyA", "CompanyB", "CompanyC"),
  salary = c(50000, 30000, 25000)
)

# Transform to segments and merge columns
segments <- vecshift(original_dt)
with_columns <- merge_original_columns(original_dt, segments, c("company", "salary"))

# Handle overlapping values
result <- merge_overlapping_values(with_columns, c("company", "salary"))
print(result)
} # }
```
