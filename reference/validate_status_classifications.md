# Validate Employment Status Classifications

Validates the consistency and logical correctness of employment status
classifications, checking for impossible combinations and missing
labels. Adapted for the enhanced flexible prior value system - validates
patterns rather than specific hardcoded values. Optimized for large
datasets.

## Usage

``` r
validate_status_classifications(
  classified_data,
  rules = get_default_status_rules()
)
```

## Arguments

- classified_data:

  Data.table with employment segments and status labels

- rules:

  Classification rules used (for validation reference, including
  prior_labels)

## Value

List with validation results and any detected issues:

- is_valid: Overall validation status

- validation_rate: Proportion of valid classifications

- missing_labels: Count of missing status labels

- impossible_combinations: Detailed breakdown of logical errors

- unexpected_statuses: Status labels that don't match expected patterns

- missing_critical_statuses: Critical status types that are missing

## Details

\*\*Enhanced Validation for Flexible Prior Values\*\*:

The validation has been updated to accommodate the flexible prior value
system: - No longer validates specific prior values (since they can be
any numeric code) - Focuses on structural consistency between arco and
stato patterns - Validates that status labels follow expected patterns
(disoccupato, occ\_, over\_) - Checks for missing critical status types
rather than exact label matches - Supports custom prior_labels through
rules parameter validation

\*\*Validation Checks\*\*: - Missing or empty status labels - Impossible
combinations (e.g., unemployment with arco \> 0) - Structural
inconsistencies between employment counts and status patterns -
Unexpected status label formats - Missing critical status categories

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
# Sample classified data with flexible prior values
classified_data <- data.table(
  cf = rep("A001", 4),
  inizio = 1:4,
  fine = 2:5,
  arco = c(0, 1, 2, 1),
  prior = c(NA, 2, 5, 1),  # Includes unmapped value 5
  durata = rep(1, 4),
  stato = c("disoccupato", "occ_fixed", "over_5_ft", "occ_ft")
)

# Validate with default rules
validation <- validate_status_classifications(classified_data)
print(validation$is_valid)
print(validation$validation_rate)

# Check for any issues
if (!validation$is_valid) {
  print(validation$impossible_combinations)
  print(validation$unexpected_statuses)
}

# Validate with custom rules that map prior value 5
custom_rules <- create_custom_status_rules(
  prior_labels = list("1" = "ft", "2" = "fixed", "5" = "intern")
)
validation_custom <- validate_status_classifications(classified_data, custom_rules)
print(validation_custom$is_valid)
} # }
```
