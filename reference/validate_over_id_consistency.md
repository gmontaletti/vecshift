# Validate over_id Column Consistency

Validates that over_id values are consistent with
employment/unemployment status and that overlapping periods share the
same over_id.

## Usage

``` r
validate_over_id_consistency(dt)
```

## Arguments

- dt:

  Data.table with over_id and arco columns

## Value

List with over_id validation results
