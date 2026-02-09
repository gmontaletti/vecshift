# Analyze Employment Status Patterns

Analyzes patterns in employment status classifications to provide
insights into employment dynamics, stability, and transition
frequencies.

## Usage

``` r
analyze_status_patterns(
  classified_data,
  person_col = "cf",
  include_transitions = TRUE
)
```

## Arguments

- classified_data:

  Data.table with employment segments and stato column

- person_col:

  Name of person identifier column (default: "cf")

- include_transitions:

  Logical. Include transition analysis

## Value

List with pattern analysis results

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
library(vecshift)

# Create and process employment data
employment_data <- data.table(
  id = 1:8,
  cf = rep(c("P001", "P002"), each = 4),
  inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01", "2023-10-01",
                     "2023-02-01", "2023-05-01", "2023-08-01", "2023-11-01")),
  fine = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30", "2023-12-31",
                   "2023-04-30", "2023-07-31", "2023-10-31", "2023-12-31")),
  prior = c(1, 0, 1, 0, 0, 1, 0, 1)
)

# Transform and classify
processed_data <- vecshift(employment_data)
classified_data <- classify_employment_status(processed_data)

# Analyze patterns without transitions
patterns_basic <- analyze_status_patterns(
  classified_data,
  person_col = "cf",
  include_transitions = FALSE
)
print(patterns_basic)

# Analyze patterns with transitions
patterns_full <- analyze_status_patterns(
  classified_data,
  person_col = "cf",
  include_transitions = TRUE
)
print(patterns_full)

# Access specific pattern metrics
patterns_full$status_distribution
patterns_full$transition_matrix
patterns_full$average_durations
} # }
```
