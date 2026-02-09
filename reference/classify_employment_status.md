# Apply Employment Status Classification

Classifies employment segments using flexible, customizable rules with
support for any numeric prior value system. Features sequence-based
overlap labeling that creates descriptive labels based on chronological
employment types. Optimized for high-performance processing with minimal
memory allocations.

## Usage

``` r
classify_employment_status(
  segments,
  rules = get_default_status_rules(),
  group_by = "cf",
  show_progress = FALSE
)
```

## Arguments

- segments:

  Data.table with temporal segments containing arco, prior, durata
  columns

- rules:

  List of classification rules (default: get_default_status_rules())

- group_by:

  Character vector of columns to group by for shift operations (default:
  "cf")

- show_progress:

  Logical indicating whether to show progress bar (default: FALSE)

## Value

Data.table with stato column containing employment status labels

## Details

\*\*Key Enhancements\*\*:

1\. \*\*Flexible Prior Value Mapping\*\*: No longer normalizes prior
values. Supports any numeric employment type codes through custom
prior_labels.

2\. \*\*Sequence-Based Overlap Labeling\*\*: For overlapping employment
periods, creates labels based on chronological sequence of employment
types. Example: prior values \[0, 1, 2\] in sequence -\>
"over_pt_ft_fixed"

3\. \*\*Unknown Prior Handling\*\*: For unmapped prior values, uses
numeric labels (e.g., "occ_5" for unknown prior value 5).

4\. \*\*Backward Compatibility\*\*: Maintains compatibility with
existing prior value conventions while supporting new flexible mapping.

\*\*Performance Optimizations\*\*: \* Vectorized data.table operations
replace all sapply() calls \* Pre-allocated memory for string
concatenation \* In-place modifications using := operator \* Optimized
grouping operations for large datasets \* Optional progress tracking for
long-running operations

\*\*Classification Logic\*\*: \* \*\*Unemployment\*\* (arco = 0):
Labeled based on duration threshold \* \*\*Single Employment\*\* (arco =
1): "occ\_" + employment type label \* \*\*Overlapping Employment\*\*
(arco \> 1): "over\_" + sequence of types

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
# Sample segments data with various prior values
segments <- data.table(
  cf = rep("A001", 6),
  inizio = 1:6,
  fine = 2:7,
  arco = c(0, 1, 2, 2, 1, 0),
  prior = c(NA, 1, 0, 2, 5, NA),  # Note: prior=5 is unmapped
  durata = rep(1, 6),
  over_id = c(0, 1, 2, 2, 3, 0)
)

# Apply default classification with progress tracking
classified <- classify_employment_status(segments, show_progress = TRUE)
print(classified$stato)
# Result: "disoccupato", "occ_ft", "over_pt_fixed", "over_pt_fixed", "occ_5", "disoccupato"

# Use custom prior labels for industry-specific codes
custom_rules <- create_custom_status_rules(
  prior_labels = list(
    "0" = "parttime",
    "1" = "fulltime",
    "2" = "contract",
    "5" = "apprentice"  # Map the previously unknown value
  )
)
classified_custom <- classify_employment_status(segments, custom_rules)
print(classified_custom$stato)
# Result: "disoccupato", "occ_fulltime", "over_parttime_contract",
#         "over_parttime_contract", "occ_apprentice", "disoccupato"

# Example showing sequence-based labeling with multiple overlap types
overlap_data <- data.table(
  cf = "B002",
  inizio = 1:4,
  fine = 2:5,
  arco = c(1, 3, 3, 2),
  prior = c(0, 1, 2, 3),
  durata = rep(1, 4),
  over_id = c(1, 2, 2, 2)
)

classified_overlap <- classify_employment_status(overlap_data)
print(classified_overlap$stato)
# Result: "occ_pt", "over_ft_fixed_temp", "over_ft_fixed_temp", "over_ft_fixed_temp"
} # }
```
