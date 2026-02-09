# Fast consolidation with consolidation_type support

High-performance version of merge_consecutive_employment with
consolidation_type support

## Usage

``` r
merge_consecutive_employment_fast_with_type(dt, consolidation_type = "both")
```

## Arguments

- dt:

  data.table object from vecshift() output

- consolidation_type:

  Character string specifying consolidation approach:

  - "both": Consolidate both overlapping (same over_id \> 0) and
    consecutive periods

  - "overlapping": Only consolidate periods with same over_id \> 0

  - "consecutive": Only consolidate consecutive periods (traditional
    approach)

  - "none": No consolidation, just add collapsed column and return

## Value

data.table with consolidated periods
