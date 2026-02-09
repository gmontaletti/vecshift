# Get Pipeline Recommendations

Analyzes input data and provides recommendations for optimal pipeline
configuration based on data characteristics and available functions.

## Usage

``` r
get_pipeline_recommendations(data, target_operation = "analysis")
```

## Arguments

- data:

  A data.table containing employment data to analyze

- target_operation:

  Character string indicating the intended use:

  - `"analysis"`: For statistical analysis (default)

  - `"reporting"`: For business reporting

  - `"visualization"`: For data visualization

  - `"export"`: For data export/sharing

## Value

A list containing:

- `recommendations`: Named logical vector of recommended settings

- `merge_columns`: Suggested columns to merge (if any)

- `reasoning`: Character vector explaining the recommendations

- `warnings`: Any potential issues detected

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)

# Create sample data
sample_data <- data.table(
  id = 1:100,
  cf = sample(paste0("PERSON", 1:20), 100, replace = TRUE),
  inizio = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), 100),
  fine = inizio + sample(30:365, 100, replace = TRUE),
  prior = sample(c(0, 1), 100, replace = TRUE),
  company = sample(c("CompanyA", "CompanyB", "CompanyC"), 100, replace = TRUE),
  salary = sample(25000:80000, 100, replace = TRUE)
)

# Get recommendations for analysis
recs <- get_pipeline_recommendations(sample_data, "analysis")
print(recs)

# Apply recommendations
result <- process_employment_pipeline(
  original_data = sample_data,
  merge_columns = recs$merge_columns,
  apply_vecshift = recs$recommendations["apply_vecshift"],
  handle_overlaps = recs$recommendations["handle_overlaps"],
  collapse_consecutive = recs$recommendations["collapse_consecutive"]
)
} # }
```
