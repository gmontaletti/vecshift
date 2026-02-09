# Analyze Temporal Clustering Patterns

Detects patterns in contract timing such as seasonal employment, batch
processing indicators, or systematic data collection periods.

## Usage

``` r
analyze_temporal_clustering(dt, person_col = "cf", start_col = "inizio")
```

## Arguments

- dt:

  Data.table with employment records

- person_col:

  Person identifier column name

- start_col:

  Start date column name

## Value

List with temporal clustering analysis
