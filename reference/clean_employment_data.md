# Clean Employment Data

Applies automatic data cleaning procedures to address common data
quality issues. Includes options for handling missing values,
duplicates, and logical inconsistencies.

## Usage

``` r
clean_employment_data(
  dt,
  remove_duplicates = TRUE,
  remove_invalid_dates = TRUE,
  remove_zero_duration = FALSE,
  fill_missing_prior = TRUE,
  verbose = TRUE
)
```

## Arguments

- dt:

  Data.table with employment records

- remove_duplicates:

  Logical. Remove duplicate records

- remove_invalid_dates:

  Logical. Remove records with invalid date ranges

- remove_zero_duration:

  Logical. Remove zero-duration contracts

- fill_missing_prior:

  Logical. Fill missing prior values with mode

- verbose:

  Logical. Print cleaning summary

## Value

Cleaned data.table with cleaning summary as attribute

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
library(vecshift)

# Create messy employment data
messy_data <- data.table(
  id = c(1:6, 3),  # Duplicate ID 3
  cf = c("P001", "P001", "P002", "P002", "P003", "P003", "P002"),
  inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-02-01",
                     "2023-06-15", "2023-03-01", "2023-05-01",
                     "2023-02-01")),  # Duplicate record
  fine = as.Date(c("2023-03-31", "2023-03-15", "2023-05-31",  # Invalid: fine < inizio!
                   "2023-08-31", "2023-03-01", "2023-07-31",  # Zero duration
                   "2023-05-31")),
  prior = c(1, NA, 0, 1, NA, 0, 0)  # Missing prior values
)

cat("Original data (", nrow(messy_data), "records):\n")
print(messy_data)

# Clean the data with defaults
clean_data <- clean_employment_data(messy_data, verbose = TRUE)

cat("\nCleaned data (", nrow(clean_data), "records):\n")
print(clean_data)

# Access cleaning summary
attr(clean_data, "cleaning_log")

# Custom cleaning - keep zero duration
clean_data2 <- clean_employment_data(
  messy_data,
  remove_duplicates = TRUE,
  remove_invalid_dates = TRUE,
  remove_zero_duration = FALSE,
  fill_missing_prior = TRUE,
  verbose = TRUE
)
} # }
```
