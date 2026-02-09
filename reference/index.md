# Package index

## Core Transformation

Main function for event-based temporal transformation of employment data

- [`vecshift()`](https://gmontaletti.github.io/vecshift/reference/vecshift.md)
  : Transform Employment Records into Temporal Segments (Core Function)

## Pipeline Processing

Modular pipeline for comprehensive employment data processing

- [`process_employment_pipeline()`](https://gmontaletti.github.io/vecshift/reference/process_employment_pipeline.md)
  : Process Employment Data Through Complete Pipeline
- [`check_pipeline_functions()`](https://gmontaletti.github.io/vecshift/reference/check_pipeline_functions.md)
  : Check Pipeline Function Availability
- [`get_pipeline_recommendations()`](https://gmontaletti.github.io/vecshift/reference/get_pipeline_recommendations.md)
  : Get Pipeline Recommendations

## Employment Status Classification

Functions for classifying and analyzing employment status

- [`classify_employment_status()`](https://gmontaletti.github.io/vecshift/reference/classify_employment_status.md)
  : Apply Employment Status Classification
- [`create_custom_status_rules()`](https://gmontaletti.github.io/vecshift/reference/create_custom_status_rules.md)
  : Create Custom Status Classification Rules
- [`get_default_status_rules()`](https://gmontaletti.github.io/vecshift/reference/get_default_status_rules.md)
  : Default Employment Status Classification Rules
- [`analyze_status_patterns()`](https://gmontaletti.github.io/vecshift/reference/analyze_status_patterns.md)
  : Analyze Employment Status Patterns
- [`validate_status_classifications()`](https://gmontaletti.github.io/vecshift/reference/validate_status_classifications.md)
  : Validate Employment Status Classifications

## External Events & Unemployment

Functions for integrating external events and handling unemployment
periods

- [`add_external_events()`](https://gmontaletti.github.io/vecshift/reference/add_external_events.md)
  : Add External Event Attributes to Unemployment Periods
- [`add_unemployment_periods()`](https://gmontaletti.github.io/vecshift/reference/add_unemployment_periods.md)
  : Add Unemployment Periods at the Beginning and End of Employment
  Histories
- [`add_unemployment_tail()`](https://gmontaletti.github.io/vecshift/reference/add_unemployment_tail.md)
  : Add Unemployment Periods at the End of Employment Histories (Legacy)

## Employment Consolidation

Functions for merging and consolidating employment periods

- [`merge_consecutive_employment()`](https://gmontaletti.github.io/vecshift/reference/merge_consecutive_employment.md)
  : Merge Employment Periods with Multiple Consolidation Modes
- [`merge_consecutive_employment_fast()`](https://gmontaletti.github.io/vecshift/reference/merge_consecutive_employment_fast.md)
  : Alternative Fast Implementation Using Event-Based Approach
- [`merge_consecutive_employment_fast_with_type()`](https://gmontaletti.github.io/vecshift/reference/merge_consecutive_employment_fast_with_type.md)
  : Fast consolidation with consolidation_type support
- [`merge_original_columns()`](https://gmontaletti.github.io/vecshift/reference/merge_original_columns.md)
  : Merge Original Data Columns with Temporal Segments
- [`merge_overlapping_values()`](https://gmontaletti.github.io/vecshift/reference/merge_overlapping_values.md)
  : Merge Overlapping Values for Employment Periods

## Data Quality & Validation

Functions for assessing and improving data quality

- [`assess_data_quality()`](https://gmontaletti.github.io/vecshift/reference/assess_data_quality.md)
  : Comprehensive Data Quality Assessment
- [`validate_employment_data_types()`](https://gmontaletti.github.io/vecshift/reference/validate_employment_data_types.md)
  : Validate Employment Data Types and Formats
- [`clean_employment_data()`](https://gmontaletti.github.io/vecshift/reference/clean_employment_data.md)
  : Clean Employment Data
- [`data_quality`](https://gmontaletti.github.io/vecshift/reference/data_quality.md)
  : Data Quality and Validation Module

## Data Preparation

Functions for standardizing and preparing input data

- [`standardize_columns()`](https://gmontaletti.github.io/vecshift/reference/standardize_columns.md)
  : Standardize Column Names for Employment Data

## Module Documentation

Overview documentation for functional modules

- [`status_labeling`](https://gmontaletti.github.io/vecshift/reference/status_labeling.md)
  : Employment Status Classification Module

## Print Methods

S3 print methods for package objects

- [`print(`*`<employment_quality_report>`*`)`](https://gmontaletti.github.io/vecshift/reference/print.employment_quality_report.md)
  : Print Employment Quality Report
- [`print(`*`<employment_status_patterns>`*`)`](https://gmontaletti.github.io/vecshift/reference/print.employment_status_patterns.md)
  : Print Employment Status Patterns Analysis
- [`print(`*`<employment_status_validation>`*`)`](https://gmontaletti.github.io/vecshift/reference/print.employment_status_validation.md)
  : Print Employment Status Validation Results
