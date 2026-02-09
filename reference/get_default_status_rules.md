# Default Employment Status Classification Rules

Returns the default classification rules used by vecshift for employment
status. These rules include flexible prior value mappings and support
sequence-based overlap labeling. The rules can be modified to implement
custom business logic.

## Usage

``` r
get_default_status_rules()
```

## Value

List with default classification rules containing:

- unemployment: Rules for unemployment periods with duration thresholds

- prior_labels: Named list mapping prior values to employment type
  labels

- single_employment: Rules for single employment periods

- overlapping_employment: Rules for overlapping employment periods

## Details

The default rules include:

\*\*Prior Labels Mapping\*\*: Maps numeric prior values to employment
type labels: \* "-1", "0" -\> "pt" (part-time) \* "1" -\> "ft"
(full-time) \* "2" -\> "fixed" (fixed-term contract) \* "3" -\> "temp"
(temporary contract)

\*\*Unemployment Classification\*\*: Based on duration thresholds (8
days default)

\*\*Single Employment\*\*: Creates labels like "occ_ft", "occ_pt",
"occ_fixed"

\*\*Overlapping Employment\*\*: Uses sequence-based labeling to create
labels like "over_pt_ft_fixed" based on the chronological sequence of
employment types within the overlapping period.

## Examples

``` r
# View default rules
default_rules <- get_default_status_rules()
print(default_rules)
#> $unemployment
#> $unemployment$condition
#> [1] "arco == 0"
#> 
#> $unemployment$duration_threshold
#> [1] 8
#> 
#> $unemployment$short_label
#> [1] "disoccupato"
#> 
#> $unemployment$long_label
#> [1] "disoccupato"
#> 
#> 
#> $prior_labels
#> $prior_labels$`-1`
#> [1] "pt"
#> 
#> $prior_labels$`0`
#> [1] "pt"
#> 
#> $prior_labels$`1`
#> [1] "ft"
#> 
#> $prior_labels$`2`
#> [1] "fixed"
#> 
#> $prior_labels$`3`
#> [1] "temp"
#> 
#> 
#> $single_employment
#> $single_employment$full_time
#> $single_employment$full_time$condition
#> [1] "arco == 1 & prior == 1"
#> 
#> $single_employment$full_time$label
#> [1] "occ_ft"
#> 
#> 
#> $single_employment$part_time
#> $single_employment$part_time$condition
#> [1] "arco == 1 & prior == 0"
#> 
#> $single_employment$part_time$label
#> [1] "occ_pt"
#> 
#> 
#> 
#> $overlapping_employment
#> $overlapping_employment$pt_to_ft
#> $overlapping_employment$pt_to_ft$condition
#> [1] "arco > 1 & (prior > shift(prior, type = 'lag'))"
#> 
#> $overlapping_employment$pt_to_ft$label
#> [1] "over_pt_ft"
#> 
#> 
#> $overlapping_employment$ft_to_pt
#> $overlapping_employment$ft_to_pt$condition
#> [1] "arco > 1 & (prior < shift(prior, type = 'lag'))"
#> 
#> $overlapping_employment$ft_to_pt$label
#> [1] "over_ft_pt"
#> 
#> 
#> $overlapping_employment$pt_to_pt
#> $overlapping_employment$pt_to_pt$condition
#> [1] "arco > 1 & (prior == shift(prior, type = 'lag')) & prior == 0"
#> 
#> $overlapping_employment$pt_to_pt$label
#> [1] "over_pt_pt"
#> 
#> 
#> $overlapping_employment$ft_to_ft
#> $overlapping_employment$ft_to_ft$condition
#> [1] "default"
#> 
#> $overlapping_employment$ft_to_ft$label
#> [1] "over_ft_ft"
#> 
#> 
#> 

# Check prior labels mapping
print(default_rules$prior_labels)
#> $`-1`
#> [1] "pt"
#> 
#> $`0`
#> [1] "pt"
#> 
#> $`1`
#> [1] "ft"
#> 
#> $`2`
#> [1] "fixed"
#> 
#> $`3`
#> [1] "temp"
#> 

# Example: prior value 2 maps to "fixed"
print(default_rules$prior_labels["2"])  # "fixed"
#> $`2`
#> [1] "fixed"
#> 
```
