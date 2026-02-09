# Comprehensive Data Quality Assessment

Performs exhaustive data quality analysis including missing values,
duplicates, outliers, temporal patterns, and logical inconsistencies.
Generates detailed quality report with recommendations. Includes
validation for over_id consistency and duration calculation invariant.

## Usage

``` r
assess_data_quality(
  dt,
  person_col = "cf",
  start_col = "inizio",
  end_col = "fine",
  include_distributions = TRUE,
  include_temporal = TRUE,
  validate_over_id = TRUE,
  validate_duration = TRUE
)
```

## Arguments

- dt:

  Data.table with employment records (standardized columns)

- person_col:

  Name of person identifier column (default: "cf")

- start_col:

  Name of start date column (default: "inizio")

- end_col:

  Name of end date column (default: "fine")

- include_distributions:

  Logical. Include distribution analysis

- include_temporal:

  Logical. Include temporal pattern analysis

- validate_over_id:

  Logical. Include over_id consistency checks

- validate_duration:

  Logical. Include duration invariant validation

## Value

List with comprehensive quality assessment results

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
library(vecshift)

# Create sample employment data with quality issues
sample_data <- data.table(
  id = 1:10,
  cf = c(rep("P001", 4), rep("P002", 3), rep("P003", 3)),
  inizio = as.Date(c("2023-01-01", "2023-03-01", "2023-06-01", "2023-09-01",
                     "2023-02-01", "2023-05-01", "2023-08-01",
                     "2023-01-15", "2023-01-15", "2023-07-01")),
  fine = as.Date(c("2023-02-28", "2023-05-31", "2023-08-31", "2023-12-31",
                   "2023-04-30", "2023-07-31", "2023-10-31",
                   "2023-06-30", "2023-06-30", "2023-09-30")),
  prior = c(1, 0, 1, 0, 1, 1, 0, 1, 1, 0)
)

# Assess data quality
quality_report <- assess_data_quality(sample_data, person_col = "cf")

# View quality report
print(quality_report)

# Access specific quality metrics
quality_report$missing_values
quality_report$duplicates
quality_report$quality_score

# Quality assessment with processed data
processed_data <- vecshift(sample_data)
quality_post <- assess_data_quality(
  processed_data,
  person_col = "cf",
  validate_over_id = TRUE,
  validate_duration = TRUE
)
print(quality_post)
} # }
```
