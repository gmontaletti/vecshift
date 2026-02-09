# Validate Employment Data Types and Formats

Performs comprehensive validation of employment data types, formats, and
basic logical consistency. Throws errors for critical issues and
warnings for potential problems. Includes validation for over_id and
duration calculation consistency.

## Usage

``` r
validate_employment_data_types(
  dt,
  strict = FALSE,
  validate_over_id = TRUE,
  validate_duration = TRUE
)
```

## Arguments

- dt:

  Data.table with standardized employment columns

- strict:

  Logical. If TRUE, throws errors for warnings

- validate_over_id:

  Logical. If TRUE, validates over_id column consistency

- validate_duration:

  Logical. If TRUE, validates duration calculation invariant

## Value

Invisible validation results list

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
library(vecshift)

# Create correctly typed data
good_data <- data.table(
  id = 1:3,
  cf = c("P001", "P001", "P002"),
  inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-02-01")),
  fine = as.Date(c("2023-03-31", "2023-06-30", "2023-05-31")),
  prior = c(1, 0, 1)
)

# Validate - should pass
validation_good <- validate_employment_data_types(good_data)
print(validation_good)

# Create data with type issues
bad_data <- data.table(
  id = 1:3,
  cf = c("P001", "P001", "P002"),
  inizio = c("2023-01-01", "2023-04-01", "2023-02-01"),  # Character, not Date!
  fine = as.Date(c("2023-03-31", "2023-06-30", "2023-05-31")),
  prior = c("1", "0", "1")  # Character, not numeric!
)

# Validate - should identify issues
validation_bad <- validate_employment_data_types(bad_data)
print(validation_bad)

# Check validation results
if (!validation_bad$is_valid) {
  cat("\nType errors detected:\n")
  print(validation_bad)
}
} # }
```
