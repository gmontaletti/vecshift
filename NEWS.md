# vecshift 1.2.0

## Performance

* `merge_consecutive_employment()` is approximately 2.3x faster on typical
  workloads. The internal `.perform_aggregation()` helper no longer builds
  per-column aggregation expressions via `substitute()` + `eval()`. Instead
  it pre-computes group sizes, projects single-row groups directly without
  aggregation, and processes multi-row groups with vectorised `.SDcols`
  passes for numeric and character extras. Output column ordering and
  semantics are preserved exactly.
* `merge_original_columns()` now assigns `NA` placeholders for unemployment
  segments in a single batched `:=` call rather than one assignment per
  column.

## New features

* `process_employment_pipeline()` gains an optional `parallel = FALSE`
  parameter. When `TRUE`, the period-consolidation step is parallelised
  per-person via `future.apply::future_lapply()` (added to `Suggests`).
  Callers must set a `future::plan()` to actually use multiple workers;
  if `future.apply` is not installed, the function falls back to sequential
  processing with a warning.

# vecshift 1.1.0

## New features

* `classify_employment_status()` gains an `unemployment_duration_threshold`
  parameter as a convenience shortcut to override
  `rules$unemployment$duration_threshold` without constructing a full custom
  rule set.

## Robustness

* `vecshift()`, `classify_employment_status()`, and
  `merge_consecutive_employment()` now return correctly typed empty results
  when called on zero-row inputs instead of failing.
* Error messages from `vecshift()`, `process_employment_pipeline()`,
  `merge_original_columns()`, and `merge_overlapping_values()` now include
  remediation hints, the offending column class, and (where appropriate)
  the list of available columns.

## Performance

* `add_unemployment_periods()` defers its internal `copy()` until ordering
  must be mutated, and uses `.I`-based indexing instead of `.SD[1]` /
  `.SD[.N]` to extract the first and last record per person.

## Internal

* Extracted the repeated extra-column type detection block in
  `merge_consecutive_employment.R` into the internal helper
  `.classify_extra_columns()`.

# vecshift 1.0.5

## Performance

* Optimized `vecshift()` core and `merge_consecutive_employment()` for ~25% faster processing
* Resolved all test failures in `classify_employment_status` and test suite

# vecshift 1.0.4

## Bug Fixes

* Fixed critical bug in `add_external_events()` that caused "storage mode of IDate is somehow no longer integer" errors when mixing Date and IDate objects. Added explicit IDate conversions in `.prepare_events_data()` and `.create_synthetic_unemployment()` helper functions to ensure consistent date type handling throughout the pipeline. (#PR)

# vecshift 1.0.3

## Documentation

* Enhanced documentation with comprehensive examples
* Added complete workflow vignette
* Added pkgdown documentation website with GitHub Pages deployment

## Bug Fixes

* Corrected column parameter handling in `assess_data_quality()` and related functions

# vecshift 1.0.2

## Improvements

* Documentation and test improvements
* Enhanced test coverage

# vecshift 1.0.1

## Bug Fixes

* Prevented duplicate synthetic unemployment periods in `add_external_events()`

# vecshift 1.0.0

* Initial CRAN-ready release
* Core functionality for temporal employment data analysis
* Event-based processing with overlap detection
* Employment status classification
* External events integration
