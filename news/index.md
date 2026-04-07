# Changelog

## vecshift 1.1.0

### New features

- [`classify_employment_status()`](https://gmontaletti.github.io/vecshift/reference/classify_employment_status.md)
  gains an `unemployment_duration_threshold` parameter as a convenience
  shortcut to override `rules$unemployment$duration_threshold` without
  constructing a full custom rule set.

### Robustness

- [`vecshift()`](https://gmontaletti.github.io/vecshift/reference/vecshift.md),
  [`classify_employment_status()`](https://gmontaletti.github.io/vecshift/reference/classify_employment_status.md),
  and
  [`merge_consecutive_employment()`](https://gmontaletti.github.io/vecshift/reference/merge_consecutive_employment.md)
  now return correctly typed empty results when called on zero-row
  inputs instead of failing.
- Error messages from
  [`vecshift()`](https://gmontaletti.github.io/vecshift/reference/vecshift.md),
  [`process_employment_pipeline()`](https://gmontaletti.github.io/vecshift/reference/process_employment_pipeline.md),
  [`merge_original_columns()`](https://gmontaletti.github.io/vecshift/reference/merge_original_columns.md),
  and
  [`merge_overlapping_values()`](https://gmontaletti.github.io/vecshift/reference/merge_overlapping_values.md)
  now include remediation hints, the offending column class, and (where
  appropriate) the list of available columns.

### Performance

- [`add_unemployment_periods()`](https://gmontaletti.github.io/vecshift/reference/add_unemployment_periods.md)
  defers its internal
  [`copy()`](https://rdrr.io/pkg/data.table/man/copy.html) until
  ordering must be mutated, and uses `.I`-based indexing instead of
  `.SD[1]` / `.SD[.N]` to extract the first and last record per person.

### Internal

- Extracted the repeated extra-column type detection block in
  `merge_consecutive_employment.R` into the internal helper
  `.classify_extra_columns()`.

## vecshift 1.0.5

### Performance

- Optimized
  [`vecshift()`](https://gmontaletti.github.io/vecshift/reference/vecshift.md)
  core and
  [`merge_consecutive_employment()`](https://gmontaletti.github.io/vecshift/reference/merge_consecutive_employment.md)
  for ~25% faster processing
- Resolved all test failures in `classify_employment_status` and test
  suite

## vecshift 1.0.4

### Bug Fixes

- Fixed critical bug in
  [`add_external_events()`](https://gmontaletti.github.io/vecshift/reference/add_external_events.md)
  that caused “storage mode of IDate is somehow no longer integer”
  errors when mixing Date and IDate objects. Added explicit IDate
  conversions in
  [`.prepare_events_data()`](https://gmontaletti.github.io/vecshift/reference/dot-prepare_events_data.md)
  and
  [`.create_synthetic_unemployment()`](https://gmontaletti.github.io/vecshift/reference/dot-create_synthetic_unemployment.md)
  helper functions to ensure consistent date type handling throughout
  the pipeline. (#PR)

## vecshift 1.0.3

### Documentation

- Enhanced documentation with comprehensive examples
- Added complete workflow vignette
- Added pkgdown documentation website with GitHub Pages deployment

### Bug Fixes

- Corrected column parameter handling in
  [`assess_data_quality()`](https://gmontaletti.github.io/vecshift/reference/assess_data_quality.md)
  and related functions

## vecshift 1.0.2

### Improvements

- Documentation and test improvements
- Enhanced test coverage

## vecshift 1.0.1

### Bug Fixes

- Prevented duplicate synthetic unemployment periods in
  [`add_external_events()`](https://gmontaletti.github.io/vecshift/reference/add_external_events.md)

## vecshift 1.0.0

- Initial CRAN-ready release
- Core functionality for temporal employment data analysis
- Event-based processing with overlap detection
- Employment status classification
- External events integration
