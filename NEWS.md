# vecshift 2.0.0

## Breaking changes

* `vecshift()` now returns an object of class `c("vecshift_result",
  "data.table", "data.frame")` instead of a bare `data.table`. The result
  still inherits from `data.table`, so all data.table operations
  (subsetting, `:=`, joins, etc.) work unchanged. Code that asserts the
  exact class vector of the result must be updated.
* `add_external_events()` deprecates the `person_id_column` argument in
  favour of the new `person_col` argument. Passing `person_id_column`
  still works but emits a `.Deprecated()` warning. The argument will be
  removed in a future release.

## New features

* `vecshift()` gains explicit column-mapping parameters: `person_col`,
  `start_col`, `end_col`, `id_col`, and `type_col`. Defaults preserve the
  original Italian-labor names (`cf`, `inizio`, `fine`, `id`, `prior`),
  so existing code keeps working. Users with non-Italian datasets can
  now adopt the package without renaming columns externally.
* `vecshift()` gains a `granularity` parameter (default `"day"`). The
  values `"month"` and `"hour"` are reserved for future releases and
  currently raise a not-implemented error. The parameter is exposed
  today so that downstream code can adopt the API without breaking
  when finer granularities are added.
* New `vecshift_result` S3 class with dedicated `print()` and `summary()`
  methods. The summary verifies the duration invariant
  (`sum(durata) == elapsed_time` per person) and reports the status
  distribution when present.
* New helper `is_vecshift_result()` checks whether an object inherits
  from `vecshift_result`.
* New diagnostic function `validate_vecshift_input()` reports issues in
  an input data.table (missing columns, NA values, wrong date types,
  `end < start`, duplicate IDs) without modifying the data. Returns an
  object of class `vecshift_validation` with a dedicated print method.

## Metadata

* `vecshift()` results carry a `vecshift_metadata` attribute with the
  granularity, package version, and generation timestamp.

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
