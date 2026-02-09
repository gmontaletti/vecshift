# Classify Employment Status with Consolidated Overlaps

Internal function that provides consolidated classification of
overlapping employment periods using over_id grouping. This creates
single status labels for entire overlapping employment periods rather
than segment-by-segment classification.

## Usage

``` r
.classify_with_consolidated_overlaps(segments, rules, group_by)
```

## Arguments

- segments:

  Data.table with employment segments including over_id column

- rules:

  Classification rules

- group_by:

  Character vector of grouping columns

## Value

Data.table with consolidated employment status classifications
