# Check Pipeline Function Availability

Utility function to check which pipeline functions are available in the
current environment. This helps users understand what pipeline steps can
be executed.

## Usage

``` r
check_pipeline_functions()
```

## Value

A named logical vector indicating which functions are available:

- `vecshift`: Core transformation function

- `merge_original_columns`: Column merging function

- `merge_overlapping_values`: Overlap handling function

- `merge_consecutive_employment`: Period consolidation with over_id

- `merge_consecutive_employment_fast`: Traditional consecutive merging

## Examples

``` r
if (FALSE) { # \dontrun{
# Check which functions are available
availability <- check_pipeline_functions()
print(availability)

# Use availability to conditionally run pipeline steps
if (availability["merge_original_columns"]) {
  # Can safely merge columns
}
} # }
```
