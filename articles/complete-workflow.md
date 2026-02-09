# Complete Workflow: From Raw Data to Analysis

``` r
library(vecshift)
library(data.table)
```

## Introduction

The vecshift package provides a comprehensive system for processing
temporal employment data, transforming raw employment records into
continuous time segments with employment status classifications and
overlap consolidation. This vignette walks through a complete workflow
from raw data preparation to advanced analysis, demonstrating best
practices and common patterns.

### What You Will Learn

This tutorial covers:

- Data validation and quality assessment
- Cleaning and standardization
- Core temporal transformation with vecshift()
- Employment status classification
- Quality validation and pattern analysis
- Advanced features (external events, merging consecutive periods)
- Pipeline processing for production workflows

### The Workflow Overview

    Raw Data → Validate → Clean → Standardize → vecshift() → Classify → Analyze

The vecshift workflow follows a systematic approach:

1.  **Prepare**: Assess data quality and standardize column names
2.  **Transform**: Apply vecshift() to create temporal segments with
    over_id
3.  **Classify**: Add employment status labels
4.  **Validate**: Ensure temporal consistency and classification
    integrity
5.  **Analyze**: Extract insights from employment patterns
6.  **Extend**: Apply advanced features as needed

## 1. Preparing Your Data

Let’s start by creating realistic employment data that includes common
patterns and challenges you might encounter in real-world datasets.

``` r
# 1. Create realistic employment data with various patterns -----
# This dataset represents 10 persons with 60 employment records over 2023
employment_raw <- data.table(
  id = 1:60,
  cf = c(
    # Person 001: Stable employment with one gap
    rep("PERSON001", 6),
    # Person 002: Multiple overlapping contracts
    rep("PERSON002", 8),
    # Person 003: Consecutive contracts, same type
    rep("PERSON003", 5),
    # Person 004: Part-time to full-time transition
    rep("PERSON004", 6),
    # Person 005: Seasonal worker with gaps
    rep("PERSON005", 7),
    # Person 006: Single long contract
    rep("PERSON006", 3),
    # Person 007: Multiple short contracts
    rep("PERSON007", 8),
    # Person 008: Overlapping with gap
    rep("PERSON008", 6),
    # Person 009: Consecutive with different types
    rep("PERSON009", 5),
    # Person 010: Mixed patterns
    rep("PERSON010", 6)
  ),
  inizio = as.Date(c(
    # PERSON001
    "2023-01-01", "2023-02-01", "2023-03-01", "2023-06-01", "2023-07-01", "2023-09-01",
    # PERSON002
    "2023-01-15", "2023-02-01", "2023-02-15", "2023-05-01", "2023-05-15", "2023-08-01", "2023-09-01", "2023-11-01",
    # PERSON003
    "2023-01-01", "2023-03-01", "2023-05-01", "2023-07-01", "2023-09-01",
    # PERSON004
    "2023-01-10", "2023-03-10", "2023-04-10", "2023-07-01", "2023-08-01", "2023-10-01",
    # PERSON005
    "2023-01-01", "2023-02-15", "2023-05-01", "2023-06-20", "2023-08-01", "2023-09-15", "2023-11-01",
    # PERSON006
    "2023-01-01", "2023-03-01", "2023-07-01",
    # PERSON007
    "2023-01-05", "2023-02-01", "2023-03-05", "2023-04-10", "2023-06-01", "2023-07-15", "2023-09-01", "2023-11-01",
    # PERSON008
    "2023-01-01", "2023-02-15", "2023-03-01", "2023-06-01", "2023-08-01", "2023-10-01",
    # PERSON009
    "2023-01-01", "2023-04-01", "2023-06-01", "2023-08-01", "2023-11-01",
    # PERSON010
    "2023-01-15", "2023-03-01", "2023-04-15", "2023-07-01", "2023-09-01", "2023-11-15"
  )),
  fine = as.Date(c(
    # PERSON001 (gap in April-May)
    "2023-01-31", "2023-02-28", "2023-03-31", "2023-06-30", "2023-08-31", "2023-12-31",
    # PERSON002 (overlapping contracts)
    "2023-03-31", "2023-04-30", "2023-06-30", "2023-07-31", "2023-10-31", "2023-10-31", "2023-12-31", "2023-12-31",
    # PERSON003 (consecutive, no gaps)
    "2023-02-28", "2023-04-30", "2023-06-30", "2023-08-31", "2023-12-31",
    # PERSON004 (PT to FT transition)
    "2023-02-28", "2023-03-31", "2023-06-30", "2023-07-31", "2023-09-30", "2023-12-31",
    # PERSON005 (seasonal with gaps)
    "2023-01-31", "2023-03-31", "2023-05-31", "2023-07-31", "2023-08-31", "2023-10-31", "2023-12-31",
    # PERSON006 (long stable contract)
    "2023-02-28", "2023-06-30", "2023-12-31",
    # PERSON007 (many short contracts)
    "2023-01-31", "2023-02-28", "2023-04-04", "2023-05-31", "2023-07-14", "2023-08-31", "2023-10-31", "2023-12-31",
    # PERSON008 (overlapping then gap)
    "2023-03-31", "2023-03-31", "2023-04-30", "2023-07-31", "2023-09-30", "2023-12-31",
    # PERSON009 (consecutive, varied types)
    "2023-03-31", "2023-05-31", "2023-07-31", "2023-10-31", "2023-12-31",
    # PERSON010 (mixed)
    "2023-02-28", "2023-04-14", "2023-06-30", "2023-08-31", "2023-11-14", "2023-12-31"
  )),
  prior = c(
    # PERSON001 (stable full-time)
    1, 1, 1, 1, 1, 1,
    # PERSON002 (mixed FT and PT, overlapping)
    1, 0, 1, 0, 1, 1, 0, 1,
    # PERSON003 (consecutive PT)
    0, 0, 0, 0, 0,
    # PERSON004 (PT to FT transition)
    0, 0, 0, 1, 1, 1,
    # PERSON005 (seasonal PT)
    0, 0, 0, 0, 0, 0, 0,
    # PERSON006 (stable FT)
    1, 1, 1,
    # PERSON007 (varied short contracts)
    0, 0, 1, 0, 0, 1, 0, 1,
    # PERSON008 (overlapping FT)
    1, 1, 1, 1, 1, 1,
    # PERSON009 (alternating FT/PT)
    1, 0, 1, 0, 1,
    # PERSON010 (mixed)
    0, 1, 0, 1, 0, 1
  )
)

cat("Created employment dataset with:\n")
#> Created employment dataset with:
cat("- Total records:", nrow(employment_raw), "\n")
#> - Total records: 60
cat("- Unique persons:", uniqueN(employment_raw$cf), "\n")
#> - Unique persons: 10
cat("- Date range:", format(min(employment_raw$inizio), "%Y-%m-%d"),
    "to", format(max(employment_raw$fine), "%Y-%m-%d"), "\n")
#> - Date range: 2023-01-01 to 2023-12-31
cat("- Full-time contracts:", sum(employment_raw$prior == 1), "\n")
#> - Full-time contracts: 32
cat("- Part-time contracts:", sum(employment_raw$prior == 0), "\n")
#> - Part-time contracts: 28
```

### Initial Data Overview

Before processing, let’s examine a sample of the data to understand its
structure:

``` r
# View sample records from different persons
cat("Sample records from PERSON001 (stable employment with gap):\n")
#> Sample records from PERSON001 (stable employment with gap):
print(employment_raw[cf == "PERSON001"])
#>       id        cf     inizio       fine prior
#>    <int>    <char>     <Date>     <Date> <num>
#> 1:     1 PERSON001 2023-01-01 2023-01-31     1
#> 2:     2 PERSON001 2023-02-01 2023-02-28     1
#> 3:     3 PERSON001 2023-03-01 2023-03-31     1
#> 4:     4 PERSON001 2023-06-01 2023-06-30     1
#> 5:     5 PERSON001 2023-07-01 2023-08-31     1
#> 6:     6 PERSON001 2023-09-01 2023-12-31     1

cat("\nSample records from PERSON002 (overlapping contracts):\n")
#> 
#> Sample records from PERSON002 (overlapping contracts):
print(employment_raw[cf == "PERSON002"][1:4])
#>       id        cf     inizio       fine prior
#>    <int>    <char>     <Date>     <Date> <num>
#> 1:     7 PERSON002 2023-01-15 2023-03-31     1
#> 2:     8 PERSON002 2023-02-01 2023-04-30     0
#> 3:     9 PERSON002 2023-02-15 2023-06-30     1
#> 4:    10 PERSON002 2023-05-01 2023-07-31     0

cat("\nSample records from PERSON004 (PT to FT transition):\n")
#> 
#> Sample records from PERSON004 (PT to FT transition):
print(employment_raw[cf == "PERSON004"])
#>       id        cf     inizio       fine prior
#>    <int>    <char>     <Date>     <Date> <num>
#> 1:    20 PERSON004 2023-01-10 2023-02-28     0
#> 2:    21 PERSON004 2023-03-10 2023-03-31     0
#> 3:    22 PERSON004 2023-04-10 2023-06-30     0
#> 4:    23 PERSON004 2023-07-01 2023-07-31     1
#> 5:    24 PERSON004 2023-08-01 2023-09-30     1
#> 6:    25 PERSON004 2023-10-01 2023-12-31     1

# Calculate basic statistics
employment_raw[, .(
  contracts = .N,
  first_date = min(inizio),
  last_date = max(fine),
  total_contract_days = sum(as.numeric(fine - inizio + 1))
), by = cf][1:5]
#>           cf contracts first_date  last_date total_contract_days
#>       <char>     <int>     <Date>     <Date>               <num>
#> 1: PERSON001         6 2023-01-01 2023-12-31                 304
#> 2: PERSON002         8 2023-01-15 2023-12-31                 838
#> 3: PERSON003         5 2023-01-01 2023-12-31                 365
#> 4: PERSON004         6 2023-01-10 2023-12-31                 338
#> 5: PERSON005         7 2023-01-01 2023-12-31                 288
```

## 2. Data Quality Assessment

Before processing employment data, it is essential to assess data
quality. The vecshift package provides comprehensive quality assessment
tools.

``` r
# 2. Assess data quality -----
quality_report <- assess_data_quality(employment_raw)

cat("Data Quality Assessment:\n")
#> Data Quality Assessment:
cat("========================\n\n")
#> ========================

cat("Basic Validation:\n")
#> Basic Validation:
cat("- Valid person IDs:", quality_report$basic_checks$valid_person_ids, "\n")
#> - Valid person IDs:
cat("- Valid dates:", quality_report$basic_checks$valid_dates, "\n")
#> - Valid dates:
cat("- Valid date ranges:", quality_report$basic_checks$valid_date_ranges, "\n")
#> - Valid date ranges:
cat("- Has duplicates:", quality_report$basic_checks$has_duplicates, "\n\n")
#> - Has duplicates:

cat("Date Issues:\n")
#> Date Issues:
cat("- Invalid ranges:", quality_report$date_issues$n_invalid_ranges, "\n")
#> - Invalid ranges:
cat("- Missing start dates:", quality_report$date_issues$n_missing_inizio, "\n")
#> - Missing start dates:
cat("- Missing end dates:", quality_report$date_issues$n_missing_fine, "\n\n")
#> - Missing end dates:

cat("Temporal Coverage:\n")
#> Temporal Coverage:
cat("- Total persons:", quality_report$temporal_coverage$n_persons, "\n")
#> - Total persons:
cat("- Total contracts:", quality_report$temporal_coverage$n_contracts, "\n")
#> - Total contracts:
cat("- Date range:", format(quality_report$temporal_coverage$date_range_start),
    "to", format(quality_report$temporal_coverage$date_range_end), "\n")
#> - Date range: NULL to NULL
cat("- Span (days):", quality_report$temporal_coverage$total_days_span, "\n\n")
#> - Span (days):

cat("Quality Score:\n")
#> Quality Score:
cat("- Overall score:", round(quality_report$quality_score$overall_score, 2), "\n")
#> - Overall score: 1
cat("- Production ready:", quality_report$quality_score$is_production_ready, "\n")
#> - Production ready: TRUE
```

The quality assessment confirms our data is clean and ready for
processing. In real scenarios, you might encounter issues that need
cleaning.

## 3. Standardizing Column Names

If your data uses different column names, use the standardization
function to map them to vecshift’s expected format:

``` r
# 3. Standardize column names (example with custom names) -----
# This example shows how to handle data with non-standard column names

# Suppose your data has these column names:
custom_data <- data.table(
  contract_id = 1:3,
  person_code = c("A001", "A001", "B002"),
  start_date = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
  end_date = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
  employment_type = c(1, 0, 1)
)

# Define the mapping
column_mapping <- list(
  id = "contract_id",
  cf = "person_code",
  inizio = "start_date",
  fine = "end_date",
  prior = "employment_type"
)

# Standardize the columns
standardized_data <- standardize_columns(custom_data, column_mapping, validate = TRUE)

cat("Columns standardized from custom names to vecshift format\n")
print(names(standardized_data))
```

Our dataset already uses standard column names, so we can proceed
directly to the transformation step.

## 4. Core Transformation with vecshift()

Now we apply the main vecshift transformation, which converts employment
contracts into continuous temporal segments with overlap detection and
consolidation identifiers.

``` r
# 4. Apply vecshift transformation -----
processed_data <- vecshift(employment_raw)

cat("Transformation complete!\n")
#> Transformation complete!
cat("========================\n\n")
#> ========================

cat("Input records:", nrow(employment_raw), "\n")
#> Input records: 60
cat("Output segments:", nrow(processed_data), "\n")
#> Output segments: 76
cat("Expansion factor:", round(nrow(processed_data) / nrow(employment_raw), 2), "x\n\n")
#> Expansion factor: 1.27 x

# Examine the output structure
cat("Output columns:\n")
#> Output columns:
print(names(processed_data))
#> [1] "id"      "cf"      "prior"   "arco"    "fine"    "inizio"  "over_id"
#> [8] "durata"

cat("\n\nSample output for PERSON001:\n")
#> 
#> 
#> Sample output for PERSON001:
print(processed_data[cf == "PERSON001"][1:10])
#>        id        cf prior  arco       fine     inizio over_id     durata
#>     <int>    <char> <num> <num>     <Date>     <Date>   <int> <difftime>
#>  1:     1 PERSON001     1     1 2023-01-31 2023-01-01       1    31 days
#>  2:     2 PERSON001     1     1 2023-02-28 2023-02-01       2    28 days
#>  3:     3 PERSON001     1     1 2023-03-31 2023-03-01       3    31 days
#>  4:     0 PERSON001     0     0 2023-05-31 2023-04-01       0    61 days
#>  5:     4 PERSON001     1     1 2023-06-30 2023-06-01       4    30 days
#>  6:     5 PERSON001     1     1 2023-08-31 2023-07-01       5    62 days
#>  7:     6 PERSON001     1     1 2023-12-31 2023-09-01       6   122 days
#>  8:    NA      <NA>    NA    NA       <NA>       <NA>      NA    NA days
#>  9:    NA      <NA>    NA    NA       <NA>       <NA>      NA    NA days
#> 10:    NA      <NA>    NA    NA       <NA>       <NA>      NA    NA days
```

### Understanding the Key Columns

Let’s explore what each column represents:

``` r
cat("Key Column Meanings:\n")
#> Key Column Meanings:
cat("===================\n\n")
#> ===================

cat("over_id: Consolidation identifier for continuous overlapping employment\n")
#> over_id: Consolidation identifier for continuous overlapping employment
cat("  - over_id = 0: Unemployment periods\n")
#>   - over_id = 0: Unemployment periods
cat("  - over_id > 0: Employment periods (same value = continuous overlapping employment)\n\n")
#>   - over_id > 0: Employment periods (same value = continuous overlapping employment)

cat("arco: Number of overlapping contracts at this point in time\n")
#> arco: Number of overlapping contracts at this point in time
cat("  - arco = 0: Unemployed\n")
#>   - arco = 0: Unemployed
cat("  - arco = 1: Single employment\n")
#>   - arco = 1: Single employment
cat("  - arco > 1: Multiple simultaneous employments\n\n")
#>   - arco > 1: Multiple simultaneous employments

cat("durata: Duration of this segment in days (corrected for temporal consistency)\n\n")
#> durata: Duration of this segment in days (corrected for temporal consistency)

cat("prior: Employment type indicator from original data\n")
#> prior: Employment type indicator from original data
cat("  - 0 or -1: Part-time\n")
#>   - 0 or -1: Part-time
cat("  - 1: Full-time\n\n")
#>   - 1: Full-time

# Show examples of each employment state
cat("Example: Unemployment period (arco = 0, over_id = 0):\n")
#> Example: Unemployment period (arco = 0, over_id = 0):
print(processed_data[arco == 0 & cf == "PERSON001"][1])
#>       id        cf prior  arco       fine     inizio over_id     durata
#>    <int>    <char> <num> <num>     <Date>     <Date>   <int> <difftime>
#> 1:     0 PERSON001     0     0 2023-05-31 2023-04-01       0    61 days

cat("\nExample: Single employment (arco = 1, over_id > 0):\n")
#> 
#> Example: Single employment (arco = 1, over_id > 0):
print(processed_data[arco == 1 & cf == "PERSON001"][1])
#>       id        cf prior  arco       fine     inizio over_id     durata
#>    <int>    <char> <num> <num>     <Date>     <Date>   <int> <difftime>
#> 1:     1 PERSON001     1     1 2023-01-31 2023-01-01       1    31 days

cat("\nExample: Overlapping employment (arco > 1, over_id > 0):\n")
#> 
#> Example: Overlapping employment (arco > 1, over_id > 0):
if (nrow(processed_data[arco > 1]) > 0) {
  print(processed_data[arco > 1][1])
} else {
  cat("No overlapping employment in this dataset\n")
}
#>       id        cf prior  arco       fine     inizio over_id     durata
#>    <int>    <char> <num> <num>     <Date>     <Date>   <int> <difftime>
#> 1:     8 PERSON002     0     2 2023-02-15 2023-02-01       7    14 days
```

### Key Metrics by Person

``` r
# Calculate key metrics for each person
person_metrics <- processed_data[, .(
  total_segments = .N,
  employment_segments = sum(arco > 0),
  unemployment_segments = sum(arco == 0),
  overlapping_segments = sum(arco > 1),
  total_days = as.numeric(sum(durata)),
  employment_days = as.numeric(sum(durata[arco > 0])),
  employment_rate = round(as.numeric(sum(durata[arco > 0])) / as.numeric(sum(durata)), 3),
  unique_over_ids = uniqueN(over_id[over_id > 0])
), by = cf]

cat("Employment Metrics by Person:\n")
#> Employment Metrics by Person:
print(person_metrics[1:10])
#>            cf total_segments employment_segments unemployment_segments
#>        <char>          <int>               <int>                 <int>
#>  1: PERSON001              7                   6                     1
#>  2: PERSON002             13                  13                     0
#>  3: PERSON003              5                   5                     0
#>  4: PERSON004              8                   6                     2
#>  5: PERSON005             11                   7                     4
#>  6: PERSON006              3                   3                     0
#>  7: PERSON007             10                   8                     2
#>  8: PERSON008              8                   7                     1
#>  9: PERSON009              5                   5                     0
#> 10: PERSON010              6                   6                     0
#>     overlapping_segments total_days employment_days employment_rate
#>                    <int>      <num>           <num>           <num>
#>  1:                    0        365             304           0.833
#>  2:                    9        351             351           1.000
#>  3:                    0        365             365           1.000
#>  4:                    0        356             338           0.949
#>  5:                    0        365             288           0.789
#>  6:                    0        365             365           1.000
#>  7:                    0        361             352           0.975
#>  8:                    2        365             334           0.915
#>  9:                    0        365             365           1.000
#> 10:                    0        351             351           1.000
#>     unique_over_ids
#>               <int>
#>  1:               6
#>  2:               1
#>  3:               5
#>  4:               6
#>  5:               7
#>  6:               3
#>  7:               8
#>  8:               4
#>  9:               5
#> 10:               6

cat("\n\nSummary Statistics:\n")
#> 
#> 
#> Summary Statistics:
cat("Average employment rate:", round(mean(person_metrics$employment_rate), 3), "\n")
#> Average employment rate: 0.946
cat("Persons with overlapping employment:", sum(person_metrics$overlapping_segments > 0), "\n")
#> Persons with overlapping employment: 2
cat("Persons with unemployment gaps:", sum(person_metrics$unemployment_segments > 0), "\n")
#> Persons with unemployment gaps: 5
```

## 5. Employment Status Classification

The vecshift package includes a flexible status classification system
that labels employment segments based on employment type and overlap
patterns.

``` r
# 5. Apply employment status classification -----
classified_data <- classify_employment_status(
  processed_data,
  group_by = "cf"
)

cat("Status Classification Complete!\n")
#> Status Classification Complete!
cat("================================\n\n")
#> ================================

# View status distribution
status_counts <- classified_data[, .N, by = stato][order(-N)]
cat("Status Distribution:\n")
#> Status Distribution:
print(status_counts)
#>          stato     N
#>         <char> <int>
#> 1:      occ_pt    29
#> 2:      occ_ft    26
#> 3: disoccupato    10
#> 4:  over_ft_pt     9
#> 5:     over_ft     2

cat("\n\nStatus meanings:\n")
#> 
#> 
#> Status meanings:
cat("- disoccupato: Unemployed\n")
#> - disoccupato: Unemployed
cat("- occ_ft: Employed full-time (single contract)\n")
#> - occ_ft: Employed full-time (single contract)
cat("- occ_pt: Employed part-time (single contract)\n")
#> - occ_pt: Employed part-time (single contract)
cat("- over_*: Overlapping employment with specific patterns\n")
#> - over_*: Overlapping employment with specific patterns

# Show examples with status labels
cat("\n\nSample records with status labels (PERSON004 - PT to FT transition):\n")
#> 
#> 
#> Sample records with status labels (PERSON004 - PT to FT transition):
print(classified_data[cf == "PERSON004", .(cf, inizio, fine, arco, prior, stato, durata)])
#>           cf     inizio       fine  arco prior       stato     durata
#>       <char>     <Date>     <Date> <num> <num>      <char> <difftime>
#> 1: PERSON004 2023-01-10 2023-02-28     1     0      occ_pt    50 days
#> 2: PERSON004 2023-03-01 2023-03-09     0     0 disoccupato     9 days
#> 3: PERSON004 2023-03-10 2023-03-31     1     0      occ_pt    22 days
#> 4: PERSON004 2023-04-01 2023-04-09     0     0 disoccupato     9 days
#> 5: PERSON004 2023-04-10 2023-06-30     1     0      occ_pt    82 days
#> 6: PERSON004 2023-07-01 2023-07-31     1     1      occ_ft    31 days
#> 7: PERSON004 2023-08-01 2023-09-30     1     1      occ_ft    61 days
#> 8: PERSON004 2023-10-01 2023-12-31     1     1      occ_ft    92 days
```

### Employment vs Unemployment Analysis

``` r
# Calculate employment statistics by status
employment_stats <- classified_data[, .(
  total_duration = as.numeric(sum(durata)),
  avg_segment_duration = round(as.numeric(mean(durata)), 1),
  min_duration = as.numeric(min(durata)),
  max_duration = as.numeric(max(durata)),
  n_segments = .N
), by = stato][order(-total_duration)]

cat("Duration Statistics by Status:\n")
#> Duration Statistics by Status:
print(employment_stats)
#>          stato total_duration avg_segment_duration min_duration max_duration
#>         <char>          <num>                <num>        <num>        <num>
#> 1:      occ_ft           1637                 63.0           18          184
#> 2:      occ_pt           1402                 48.3            1          122
#> 3:  over_ft_pt            330                 36.7           14           60
#> 4: disoccupato            196                 19.6            4           61
#> 5:     over_ft             44                 22.0           14           30
#>    n_segments
#>         <int>
#> 1:         26
#> 2:         29
#> 3:          9
#> 4:         10
#> 5:          2

# Calculate person-level employment metrics
person_employment <- classified_data[, .(
  total_days = as.numeric(sum(durata)),
  employment_days = as.numeric(sum(durata[stato != "disoccupato"])),
  unemployment_days = as.numeric(sum(durata[stato == "disoccupato"])),
  employment_rate = round(as.numeric(sum(durata[stato != "disoccupato"])) / as.numeric(sum(durata)), 3),
  primary_status = names(which.max(table(stato)))
), by = cf]

cat("\n\nPerson-Level Employment Summary:\n")
#> 
#> 
#> Person-Level Employment Summary:
print(person_employment[1:10])
#>            cf total_days employment_days unemployment_days employment_rate
#>        <char>      <num>           <num>             <num>           <num>
#>  1: PERSON001        365             304                61           0.833
#>  2: PERSON002        351             351                 0           1.000
#>  3: PERSON003        365             365                 0           1.000
#>  4: PERSON004        356             338                18           0.949
#>  5: PERSON005        365             288                77           0.789
#>  6: PERSON006        365             365                 0           1.000
#>  7: PERSON007        361             352                 9           0.975
#>  8: PERSON008        365             334                31           0.915
#>  9: PERSON009        365             365                 0           1.000
#> 10: PERSON010        351             351                 0           1.000
#>     primary_status
#>             <char>
#>  1:         occ_ft
#>  2:     over_ft_pt
#>  3:         occ_pt
#>  4:         occ_ft
#>  5:         occ_pt
#>  6:         occ_ft
#>  7:         occ_pt
#>  8:         occ_ft
#>  9:         occ_ft
#> 10:         occ_ft

cat("\n\nOverall Statistics:\n")
#> 
#> 
#> Overall Statistics:
cat("Average employment rate:", round(mean(person_employment$employment_rate), 3), "\n")
#> Average employment rate: 0.946
cat("Median employment rate:", round(median(person_employment$employment_rate), 3), "\n")
#> Median employment rate: 0.988
cat("Fully employed persons (rate = 1.0):", sum(person_employment$employment_rate == 1.0), "\n")
#> Fully employed persons (rate = 1.0): 5
cat("Persons with unemployment gaps:", sum(person_employment$unemployment_days > 0), "\n")
#> Persons with unemployment gaps: 5
```

## 6. Quality Validation

After classification, validate the integrity of the results to ensure
temporal consistency and proper classification.

``` r
# 6. Validate status classifications -----
validation <- validate_status_classifications(classified_data)

cat("Classification Validation Results:\n")
#> Classification Validation Results:
cat("===================================\n\n")
#> ===================================

cat("Overall validation:", ifelse(validation$is_valid, "PASSED", "FAILED"), "\n")
#> Overall validation: PASSED
cat("Total segments:", validation$total_segments, "\n")
#> Total segments:
cat("Missing labels:", validation$missing_labels, "\n")
#> Missing labels: 0
cat("Invalid labels:", validation$invalid_labels, "\n\n")
#> Invalid labels:

if (validation$total_impossible > 0) {
  cat("Impossible combinations detected:", validation$total_impossible, "\n")
  cat("Details:\n")
  for (issue in names(validation$impossible_combinations)) {
    count <- validation$impossible_combinations[[issue]]
    if (count > 0) {
      cat("  -", gsub("_", " ", issue), ":", count, "\n")
    }
  }
} else {
  cat("No impossible combinations detected - classification is consistent!\n")
}
#> No impossible combinations detected - classification is consistent!

# Verify temporal continuity for a sample person
cat("\n\nTemporal Continuity Check (PERSON001):\n")
#> 
#> 
#> Temporal Continuity Check (PERSON001):
person_segments <- classified_data[cf == "PERSON001"][order(inizio)]
person_segments[, gap_before := ifelse(.I > 1,
                                       as.numeric(inizio - shift(fine, 1)),
                                       NA)]
if (nrow(person_segments) > 1) {
  cat("All segments continuous:", all(person_segments$gap_before[2:nrow(person_segments)] == 1, na.rm = TRUE), "\n")
} else {
  cat("Single segment - no gaps to check\n")
}
#> All segments continuous: TRUE
```

## 7. Pattern Analysis

The package provides tools to analyze employment patterns, including
status transitions and duration distributions.

``` r
# 7. Analyze employment status patterns -----
patterns <- analyze_status_patterns(
  classified_data,
  person_col = "cf",
  include_transitions = TRUE
)

cat("Employment Pattern Analysis:\n")
#> Employment Pattern Analysis:
cat("=============================\n\n")
#> =============================

cat("Status Distribution:\n")
#> Status Distribution:
print(patterns$status_distribution)
#> 
#> disoccupato      occ_ft      occ_pt     over_ft  over_ft_pt 
#>          10          26          29           2           9

cat("\n\nTransition Matrix:\n")
#> 
#> 
#> Transition Matrix:
cat("(Shows transitions from row status to column status)\n")
#> (Shows transitions from row status to column status)
print(patterns$transition_matrix)
#> NULL

cat("\n\nAverage Durations by Status:\n")
#> 
#> 
#> Average Durations by Status:
print(patterns$average_durations)
#> NULL

cat("\n\nPattern Summary:\n")
#> 
#> 
#> Pattern Summary:
cat("- Total unique statuses:", patterns$n_unique_statuses, "\n")
#> - Total unique statuses:
cat("- Total transitions observed:", sum(patterns$transition_matrix, na.rm = TRUE), "\n")
#> - Total transitions observed: 0

# Most common status (handle both vector and data.frame formats)
if (is.data.frame(patterns$status_distribution)) {
  cat("- Most common status:", patterns$status_distribution$status[which.max(patterns$status_distribution$count)], "\n")
} else {
  cat("- Most common status:", names(which.max(patterns$status_distribution)), "\n")
}
#> - Most common status: occ_pt

# Most common transition
if (!is.null(patterns$transition_matrix) && sum(!is.na(patterns$transition_matrix)) > 0) {
  trans_mat <- patterns$transition_matrix
  max_idx <- which(trans_mat == max(trans_mat, na.rm = TRUE), arr.ind = TRUE)[1,]
  cat("- Most common transition:", paste(rownames(trans_mat)[max_idx[1]], "->", colnames(trans_mat)[max_idx[2]]), "\n")
}
```

### Detailed Transition Analysis

``` r
# Examine specific transition patterns
if (!is.null(patterns$transition_matrix)) {
  cat("\nKey Transition Patterns:\n\n")

  # Unemployment to employment transitions
  unemp_to_emp <- patterns$transition_matrix["disoccupato",
                                              grep("^occ_", colnames(patterns$transition_matrix), value = TRUE)]
  if (length(unemp_to_emp) > 0) {
    cat("From Unemployment to Employment:\n")
    print(unemp_to_emp[unemp_to_emp > 0])
  }

  # Employment to unemployment transitions
  emp_to_unemp <- patterns$transition_matrix[grep("^occ_", rownames(patterns$transition_matrix), value = TRUE),
                                             "disoccupato"]
  if (length(emp_to_unemp) > 0) {
    cat("\nFrom Employment to Unemployment:\n")
    print(emp_to_unemp[emp_to_unemp > 0])
  }
}
```

## 8. Advanced Features

The vecshift package provides several advanced capabilities for
specialized analysis scenarios.

### 8.1 Adding External Events

You can match external events (such as policy changes or economic
shocks) to employment segments to analyze their impact.

``` r
# 8.1 Adding external events -----
# Define external events that occurred during 2023
# Events are matched to unemployment periods in vecshift output
external_events <- data.table(
  cf = c("PERSON001", "PERSON002", "PERSON005"),
  event_start = as.Date(c("2023-04-15", "2023-03-01", "2023-04-15")),
  event_name = c("training_program", "policy_change", "subsidy_program")
)

cat("External Events:\n")
#> External Events:
print(external_events)
#>           cf event_start       event_name
#>       <char>      <Date>           <char>
#> 1: PERSON001  2023-04-15 training_program
#> 2: PERSON002  2023-03-01    policy_change
#> 3: PERSON005  2023-04-15  subsidy_program

# Match events to unemployment periods
data_with_events <- add_external_events(
  vecshift_data = classified_data,
  external_events = external_events,
  event_matching_strategy = "overlap",
  date_columns = c(start = "event_start"),
  event_name_column = "event_name",
  person_id_column = "cf"
)

cat("\nEvents have been matched to unemployment periods.\n")
#> 
#> Events have been matched to unemployment periods.
cat("New attribute columns added for each event type.\n")
#> New attribute columns added for each event type.

# Check which columns were added
event_cols <- grep("_attribute$", names(data_with_events), value = TRUE)
cat("Event attribute columns:", paste(event_cols, collapse = ", "), "\n")
#> Event attribute columns: training_program_attribute, policy_change_attribute, subsidy_program_attribute

# Show unemployment periods with matched events
if (length(event_cols) > 0) {
  unemployment_with_events <- data_with_events[arco == 0]
  for (col in event_cols) {
    if (sum(unemployment_with_events[[col]], na.rm = TRUE) > 0) {
      cat("\nUnemployment periods with", gsub("_attribute", "", col), ":\n")
      print(unemployment_with_events[get(col) == 1, .(cf, inizio, fine, durata)][1:3])
    }
  }
} else {
  cat("\nNo events matched to unemployment periods in this dataset.\n")
}
#> 
#> Unemployment periods with training_program :
#>           cf     inizio       fine     durata
#>       <char>     <Date>     <Date> <difftime>
#> 1: PERSON001 2023-04-01 2023-05-31    61 days
#> 2: PERSON005 2023-04-01 2023-04-30    30 days
#> 3:      <NA>       <NA>       <NA>    NA days
#> 
#> Unemployment periods with subsidy_program :
#>           cf     inizio       fine     durata
#>       <char>     <Date>     <Date> <difftime>
#> 1: PERSON001 2023-04-01 2023-05-31    61 days
#> 2: PERSON005 2023-04-01 2023-04-30    30 days
#> 3:      <NA>       <NA>       <NA>    NA days
```

### 8.2 Merging Consecutive Employment

When analyzing employment stability, you may want to consolidate
consecutive periods of the same employment type.

``` r
# 8.2 Merge consecutive employment periods -----
cat("Before merging consecutive periods:\n")
#> Before merging consecutive periods:
cat("Total segments:", nrow(classified_data), "\n")
#> Total segments: 76

merged_data <- merge_consecutive_employment(
  classified_data,
  consolidation_type = "both"  # Consolidate both overlapping and consecutive
)

cat("\nAfter merging consecutive periods:\n")
#> 
#> After merging consecutive periods:
cat("Total segments:", nrow(merged_data), "\n")
#> Total segments: 30
cat("Reduction:", nrow(classified_data) - nrow(merged_data), "segments\n")
#> Reduction: 46 segments
cat("Reduction percentage:", round((1 - nrow(merged_data) / nrow(classified_data)) * 100, 1), "%\n")
#> Reduction percentage: 60.5 %

# Check consolidation statistics
if ("collapsed" %in% names(merged_data)) {
  cat("\nConsolidation details:\n")
  cat("- Periods marked as collapsed:", sum(merged_data$collapsed, na.rm = TRUE), "\n")
}
#> 
#> Consolidation details:
#> - Periods marked as collapsed: 12

if ("n_periods" %in% names(merged_data)) {
  consolidated_summary <- merged_data[n_periods > 1, .(
    max_periods_merged = max(n_periods),
    avg_periods_merged = round(mean(n_periods), 1),
    total_consolidated_groups = .N
  )]
  cat("- Maximum periods merged:", consolidated_summary$max_periods_merged, "\n")
  cat("- Average periods merged:", consolidated_summary$avg_periods_merged, "\n")
  cat("- Total consolidated groups:", consolidated_summary$total_consolidated_groups, "\n")
}

# Compare segment counts by person
cat("\n\nPer-person segment reduction:\n")
#> 
#> 
#> Per-person segment reduction:
segment_comparison <- data.table(
  cf = unique(classified_data$cf),
  before = classified_data[, .N, by = cf][order(cf)]$N,
  after = merged_data[, .N, by = cf][order(cf)]$N
)
segment_comparison[, reduction := before - after]
print(segment_comparison[reduction > 0][1:5])
#>           cf before after reduction
#>       <char>  <int> <int>     <int>
#> 1: PERSON001      7     3         4
#> 2: PERSON002     13     1        12
#> 3: PERSON003      5     1         4
#> 4: PERSON004      8     5         3
#> 5: PERSON005     11     9         2
```

### 8.3 Understanding over_id Consolidation

The over_id system is a key innovation in vecshift that identifies
continuous overlapping employment periods.

``` r
# 8.3 Understanding over_id consolidation -----
cat("Understanding over_id:\n")
#> Understanding over_id:
cat("======================\n\n")
#> ======================

cat("over_id groups continuous employment periods, even with overlaps.\n")
#> over_id groups continuous employment periods, even with overlaps.
cat("Same over_id value = continuous overlapping employment period\n\n")
#> Same over_id value = continuous overlapping employment period

# Find persons with overlapping employment
overlapping_persons <- processed_data[arco > 1, unique(cf)]

if (length(overlapping_persons) > 0) {
  cat("Example of overlapping employment with over_id:\n")
  example_person <- overlapping_persons[1]
  example_data <- processed_data[cf == example_person][order(inizio)]
  print(example_data[, .(cf, inizio, fine, arco, over_id, durata)])

  cat("\n\nNotice how over_id groups overlapping periods together.\n")
  cat("This allows you to:\n")
  cat("- Identify continuous employment spans\n")
  cat("- Track employment intensity changes within a period\n")
  cat("- Consolidate periods while preserving overlap information\n")
} else {
  cat("This dataset has no overlapping employment periods.\n")
  cat("In datasets with overlaps, over_id would show:\n")
  cat("- Same over_id for all segments in a continuous employment period\n")
  cat("- Different over_id values for separate employment periods\n")
}
#> Example of overlapping employment with over_id:
#> Key: <cf, inizio, fine>
#>            cf     inizio       fine  arco over_id     durata
#>        <char>     <Date>     <Date> <num>   <int> <difftime>
#>  1: PERSON002 2023-01-15 2023-02-01     1       7    18 days
#>  2: PERSON002 2023-02-01 2023-02-15     2       7    14 days
#>  3: PERSON002 2023-02-15 2023-03-31     3       7    44 days
#>  4: PERSON002 2023-03-31 2023-04-30     2       7    30 days
#>  5: PERSON002 2023-04-30 2023-05-01     1       7     1 days
#>  6: PERSON002 2023-05-01 2023-05-15     2       7    14 days
#>  7: PERSON002 2023-05-15 2023-06-30     3       7    46 days
#>  8: PERSON002 2023-06-30 2023-07-31     2       7    31 days
#>  9: PERSON002 2023-07-31 2023-08-01     1       7     1 days
#> 10: PERSON002 2023-08-01 2023-09-01     2       7    31 days
#> 11: PERSON002 2023-09-01 2023-10-31     3       7    60 days
#> 12: PERSON002 2023-10-31 2023-11-01     1       7     1 days
#> 13: PERSON002 2023-11-01 2023-12-31     2       7    60 days
#> 
#> 
#> Notice how over_id groups overlapping periods together.
#> This allows you to:
#> - Identify continuous employment spans
#> - Track employment intensity changes within a period
#> - Consolidate periods while preserving overlap information

# Show over_id distribution
over_id_summary <- processed_data[over_id > 0, .(
  segments_in_group = .N,
  total_duration = as.numeric(sum(durata)),
  has_overlap = any(arco > 1)
), by = .(cf, over_id)][order(cf, over_id)]

cat("\n\nover_id Summary (employment periods only):\n")
#> 
#> 
#> over_id Summary (employment periods only):
print(over_id_summary[1:10])
#>            cf over_id segments_in_group total_duration has_overlap
#>        <char>   <int>             <int>          <num>      <lgcl>
#>  1: PERSON001       1                 1             31       FALSE
#>  2: PERSON001       2                 1             28       FALSE
#>  3: PERSON001       3                 1             31       FALSE
#>  4: PERSON001       4                 1             30       FALSE
#>  5: PERSON001       5                 1             62       FALSE
#>  6: PERSON001       6                 1            122       FALSE
#>  7: PERSON002       7                13            351        TRUE
#>  8: PERSON003       8                 1             59       FALSE
#>  9: PERSON003       9                 1             61       FALSE
#> 10: PERSON003      10                 1             61       FALSE
```

## 9. Pipeline Processing

For production workflows, use the integrated pipeline function that
combines all processing steps efficiently.

``` r
# 9. Integrated pipeline processing -----
# Check pipeline readiness
pipeline_check <- check_pipeline_functions()
cat("Pipeline Function Availability:\n")
print(pipeline_check)

# Get recommendations based on data characteristics
recommendations <- get_pipeline_recommendations(
  employment_raw,
  target_operation = "analysis"
)

cat("\n\nPipeline Recommendations:\n")
cat("Target operation:", recommendations$data_summary$target_operation, "\n")
cat("Recommended settings:\n")
print(recommendations$recommendations)

if (length(recommendations$warnings) > 0) {
  cat("\nWarnings:\n")
  for (warning in recommendations$warnings) {
    cat("-", warning, "\n")
  }
}

# Run the complete pipeline (example - we already processed the data above)
# In production, you would use this instead of individual steps:
pipeline_result <- process_employment_pipeline(
  original_data = employment_raw,
  apply_vecshift = TRUE,
  classify_status = TRUE,
  collapse_consecutive = TRUE,
  consolidate_periods = TRUE,
  consolidation_type = "both",
  validate_over_id = TRUE,
  show_progress = FALSE  # Set to TRUE to see progress bar
)

cat("\nPipeline execution complete!\n")
cat("Output rows:", nrow(pipeline_result), "\n")

# Access pipeline metadata
pipeline_steps <- attr(pipeline_result, "pipeline_steps")
cat("\nPipeline steps applied:\n")
print(pipeline_steps)

# Access validation results
validation_results <- attr(pipeline_result, "validation_results")
if (!is.null(validation_results$over_id)) {
  cat("\nover_id validation:",
      ifelse(validation_results$over_id$all_tests_passed, "PASSED", "FAILED"), "\n")
}
```

## 10. Best Practices and Next Steps

### Best Practices

1.  **Always validate input data** before processing
    - Use
      [`assess_data_quality()`](https://gmontaletti.github.io/vecshift/reference/assess_data_quality.md)
      to identify issues
    - Check for invalid date ranges, missing values, and duplicates
    - Clean data with
      [`clean_employment_data()`](https://gmontaletti.github.io/vecshift/reference/clean_employment_data.md)
      if needed
2.  **Use the pipeline functions** for production workflows
    - [`process_employment_pipeline()`](https://gmontaletti.github.io/vecshift/reference/process_employment_pipeline.md)
      handles the complete workflow
    - Get recommendations with
      [`get_pipeline_recommendations()`](https://gmontaletti.github.io/vecshift/reference/get_pipeline_recommendations.md)
    - Enable progress bars with `show_progress = TRUE` for long-running
      operations
3.  **Understand your consolidation strategy**
    - Use `consolidation_type = "both"` for comprehensive analysis
    - Use `consolidation_type = "overlapping"` to preserve consecutive
      distinctions
    - Use `consolidation_type = "consecutive"` for traditional merging
4.  **Leverage over_id for advanced analysis**
    - over_id identifies continuous employment periods
    - Use it to track employment stability and transitions
    - Consolidate periods while preserving temporal precision
5.  **Validate output after transformation**
    - Use
      [`validate_status_classifications()`](https://gmontaletti.github.io/vecshift/reference/validate_status_classifications.md)
      to check classification integrity
    - Verify temporal continuity for critical persons
    - Check the duration invariant with validation functions
6.  **Document custom status rules** for reproducibility
    - Create clear prior_labels mappings for industry-specific codes
    - Document unemployment thresholds and their business rationale
    - Use descriptive status labels that reflect your context

### Common Troubleshooting Scenarios

``` r
# Issue 1: Invalid date ranges
# Solution: Use data quality assessment and cleaning
problematic_data <- employment_raw
problematic_data[1, fine := inizio - 1]  # Create invalid range

quality_check <- assess_data_quality(problematic_data)
if (quality_check$date_issues$n_invalid_ranges > 0) {
  cat("Invalid date ranges detected - cleaning required\n")
  cleaned_data <- clean_employment_data(
    problematic_data,
    remove_invalid_dates = TRUE
  )
}

# Issue 2: Custom column names
# Solution: Use standardize_columns()
custom_cols <- list(
  id = "contract_id",
  cf = "person_code",
  inizio = "start_date",
  fine = "end_date",
  prior = "employment_type"
)
# standardized_data <- standardize_columns(your_data, custom_cols)

# Issue 3: Custom employment type codes
# Solution: Create custom status rules with prior_labels
custom_rules <- create_custom_status_rules(
  prior_labels = list(
    "0" = "pt",
    "1" = "ft",
    "2" = "fixed_term",
    "3" = "temporary",
    "4" = "apprentice"
  )
)
# classified_custom <- classify_employment_status(processed_data, rules = custom_rules)
```

### Next Steps

#### Advanced Analytics with longworkR

For analysis beyond temporal transformation, consider the companion
package longworkR:

- **Survival Analysis**: Analyze contract duration and termination
  patterns
- **Impact Evaluation**: Difference-in-differences, propensity score
  matching, event studies
- **Network Analysis**: Map employment transition networks and career
  pathways
- **Interactive Visualization**: Create dashboards with ggraph and g6r

See the longworkR package documentation at `../longworkR` for details.

#### Performance Optimization for Large Datasets

When working with large datasets (\>100,000 records):

``` r
# 1. Process in chunks by person or time period
large_data_chunk1 <- large_data[cf %in% person_ids[1:1000]]
result_chunk1 <- vecshift(large_data_chunk1)

# 2. Disable progress bars for batch processing
result <- process_employment_pipeline(
  large_data,
  show_progress = FALSE
)

# 3. Use data.table operations efficiently
setkey(processed_data, cf, inizio)  # Set keys for faster operations

# 4. Consider parallel processing for independent person-level analyses
# (Note: vecshift itself is already optimized at ~1.46M records/second)
```

#### Integration with Other Systems

``` r
# Export processed data
fwrite(classified_data, "processed_employment_data.csv")

# Integration with databases
# library(DBI)
# con <- dbConnect(...)
# dbWriteTable(con, "employment_segments", classified_data)

# Create summary reports
summary_report <- classified_data[, .(
  total_segments = .N,
  employment_rate = round(as.numeric(sum(durata[arco > 0])) / as.numeric(sum(durata)), 3),
  avg_segment_duration = round(as.numeric(mean(durata)), 1),
  has_overlaps = any(arco > 1)
), by = cf]

fwrite(summary_report, "employment_summary.csv")
```

## Summary

This vignette demonstrated a complete vecshift workflow:

1.  **Data Preparation**: Created realistic employment data with varied
    patterns
2.  **Quality Assessment**: Assessed data quality and validated input
3.  **Transformation**: Applied vecshift() to create temporal segments
    with over_id
4.  **Classification**: Added employment status labels with flexible
    rules
5.  **Validation**: Ensured temporal consistency and classification
    integrity
6.  **Analysis**: Extracted insights from employment patterns and
    transitions
7.  **Advanced Features**: Applied external events and period
    consolidation
8.  **Pipeline Processing**: Integrated workflow for production use

### Key Takeaways

- vecshift transforms employment contracts into continuous temporal
  segments
- over_id identifies and groups continuous overlapping employment
  periods
- The modular architecture separates core transformation from business
  logic
- Comprehensive validation ensures temporal and logical consistency
- The pipeline approach streamlines production workflows
- Advanced features support specialized analysis scenarios

### Further Reading

- **Understanding Date Logic**: See
  [`vignette("understanding-date-logic")`](https://gmontaletti.github.io/vecshift/articles/understanding-date-logic.md)
  for detailed date handling rules
- **Status Classification**: See
  [`vignette("status-classification")`](https://gmontaletti.github.io/vecshift/articles/status-classification.md)
  for advanced classification patterns
- **Function Reference**: See
  [`?vecshift`](https://gmontaletti.github.io/vecshift/reference/vecshift.md),
  [`?classify_employment_status`](https://gmontaletti.github.io/vecshift/reference/classify_employment_status.md),
  and related function documentation
- **longworkR Integration**: See the longworkR package for advanced
  analytics and visualization

The vecshift package provides a robust foundation for temporal
employment analysis, with the flexibility to adapt to diverse business
contexts while maintaining high performance and data integrity.
