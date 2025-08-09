# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The `vecshift` package is an R package that performs temporal data analysis operations on employment/labor data. The main function `vecshift()` processes data.table objects containing employment records with start/end dates and transforms them into continuous temporal segments with employment status classifications.

## Development Commands

### Build and Check Package
```bash
# Build the package
R CMD build .

# Check the package
R CMD check vecshift_*.tar.gz

# Install the package locally
R CMD INSTALL .

# Alternative using RStudio shortcuts (if in RStudio):
# Install: Cmd + Shift + B
# Check: Cmd + Shift + E
# Test: Cmd + Shift + T
```

### Managing Dependencies with renv
```r
# Restore project dependencies
renv::restore()

# Update renv lockfile after adding new dependencies
renv::snapshot()

# Add a new package dependency
install.packages("package_name")
renv::snapshot()
```

### Documentation
```r
# Generate documentation from roxygen comments (when added)
devtools::document()

# Build package documentation
devtools::build_manual()
```

## Core Architecture

### Main Function: vecshift()
Location: R/vecshift.R

The `vecshift()` function transforms employment records into temporal segments. It:
1. Takes a data.table with columns: id, cf (fiscal code), INIZIO (start date), FINE (end date), prior (employment type indicator)
2. Creates interval boundaries by splitting each record into start/end events
3. Calculates overlapping employment periods (`arco`)
4. Classifies each segment into employment states:
   - `disoccupato`: unemployed periods
   - `occ_ft`: full-time employment
   - `occ_pt`: part-time employment
   - `over_*`: overlapping employment situations

The function uses advanced data.table operations for efficient processing of large datasets.

### Key Dependencies
- **data.table**: Core data manipulation (required by vecshift function)
- **fst**: Fast serialization format (used in test.R for reading data)
- **renv**: Dependency management (version 1.1.4)

### Data Flow
1. Input: Employment records with temporal boundaries
2. Processing: Event-based transformation to detect overlaps and gaps
3. Output: Continuous temporal segments with employment status classification

## Testing Approach

Currently, testing is done through the test.R script which:
1. Loads sample data from an FST file
2. Applies the vecshift transformation
3. Performs aggregations to verify results

To run tests:
```r
source("test.R")
```

## Implementation Architecture

### Current Implementations
As of the modular refactoring, the package provides two implementations:

1. **`vecshift_fast()`**: High-performance monolithic implementation
   - Location: R/vecshift.R  
   - Performance: 1.46M records/second on large datasets (3M+ records)
   - Use case: Production workloads, large-scale processing
   - Trade-off: Less maintainable, harder to extend

2. **`vecshift()`**: Modular implementation with helper functions
   - Location: R/vecshift_modular.R
   - Performance: 132K records/second (11x slower than fast version)
   - Use case: Development, debugging, extending functionality
   - Benefits: Better maintainability, easier to test and extend

### Future Development Guidelines

**Critical Performance Consideration:**
Future refactoring should preserve the speed of the core event generation logic from `vecshift_fast()`. The event-based transformation (splitting contracts into start/end events and calculating cumulative overlaps) is the most computationally intensive part and should remain optimized.

**Recommended Approach for Future Enhancements:**
- **Core Engine**: Keep the fast event generation from `vecshift_fast()` as the foundational engine
- **Modular Rules**: Add business logic, validation, and classification rules as separate modular components
- **Integration Layer**: Use modular approach for data integration, output formatting, and extended features
- **Performance Testing**: Always benchmark against the current `vecshift_fast()` baseline (1.46M records/second)

**Extension Points for Modularity:**
- Input validation and data quality checks
- Employment classification rules and custom states  
- Output formatting and export functions
- Integration with other temporal analysis packages
- Visualization and reporting components

**Performance Benchmarks:**
- Small datasets (<10K records): Both implementations perform similarly
- Medium datasets (10K-100K records): Fast version ~2.4x faster
- Large datasets (1M+ records): Fast version ~11x faster
- Memory usage: Modular version uses ~1.5x more memory

This hybrid approach ensures production performance while enabling future extensibility and maintainability.

## Important Notes

- The package uses renv for dependency management - always restore the environment before development
- The vecshift function relies heavily on data.table syntax and operations
- Prior values: 0 or -1 indicate part-time, positive values indicate full-time employment
- The function handles overlapping employment periods (multiple concurrent jobs)