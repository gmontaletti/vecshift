# Changelog

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
