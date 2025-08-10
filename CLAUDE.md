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

The `vecshift()` function transforms employment records into temporal segments using a modular architecture:

1. **Core Transformation**: Takes a data.table with columns: id, cf (fiscal code), INIZIO (start date), FINE (end date), prior (employment type indicator)
2. **Event Processing**: Creates interval boundaries by splitting each record into start/end events
3. **Temporal Logic**: Creates end events at FINE, then adjusts unemployment periods (inizio+1, fine-1)
4. **Overlap Calculation**: Calculates overlapping employment periods (`arco`)
5. **Status Classification** (optional): Delegates to `classify_employment_status()` for employment state labeling

**Key Parameters:**
- `classify_status` (default: TRUE): Apply employment status classification
- `status_rules` (default: NULL): Custom classification rules

**Employment States (when classified):**
- `disoccupato`: unemployed periods
- `occ_ft`: full-time employment
- `occ_pt`: part-time employment
- `over_*`: overlapping employment situations

### Status Classification Module
Location: R/status_labeling.R

The employment status attribution is handled by a dedicated module:
- `classify_employment_status()`: Applies status labels to temporal segments
- `get_default_status_rules()`: Returns default classification rules
- `create_custom_status_rules()`: Creates custom classification schemes
- `analyze_status_patterns()`: Analyzes employment patterns
- `validate_status_classifications()`: Validates classification integrity

This separation ensures:
- Core performance remains optimized (~1.46M records/second)
- Status rules can be customized without modifying core logic
- Clear separation of concerns for maintainability

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

### Current Implementation
The package now provides a unified architecture that combines performance with modularity:

**`vecshift()`**: Main function with modular status classification
- Location: R/vecshift.R  
- Performance: 1.46M records/second on large datasets (3M+ records)
- Features:
  - High-performance core event processing
  - Optional status classification via `classify_status` parameter
  - Support for custom status rules via `status_rules` parameter
  - Clean separation between temporal logic and business rules

### Supporting Implementations

1. **`vecshift_integrated()`**: Full pipeline with all modules
   - Location: R/vecshift_integrated.R
   - Use case: When data quality assessment and cleaning are needed
   - Features: Quality reports, automatic cleaning, validation

### Future Development Guidelines

**Critical Performance Consideration:**
The core event generation logic in `vecshift()` is highly optimized. The event-based transformation (splitting contracts into start/end events and calculating cumulative overlaps) is the most computationally intensive part and should remain optimized.

**Recommended Approach for Future Enhancements:**
- **Core Engine**: Keep the fast event generation from `vecshift()` as the foundational engine
- **Modular Rules**: Add business logic, validation, and classification rules as separate components
- **Integration Layer**: Use the integrated system for data quality, cleaning, and extended features
- **Performance Testing**: Always benchmark against the current baseline (1.46M records/second)

**Extension Points:**
- Input validation and data quality checks (via `vecshift_integrated()`)
- Employment classification rules and custom states (via `status_labeling.R`)
- Output formatting and export functions
- Integration with other temporal analysis packages
- Visualization and reporting components

## Date Logic and Temporal Processing

### Overview of Date Logic
The vecshift package implements precise date logic for employment period calculations that maintains temporal accuracy while keeping the implementation simple and efficient.

### Core Date Logic Rules

#### 1. Contract Period Inclusivity
Employment contracts define **inclusive** date ranges:
- **Contract Duration**: From INIZIO to FINE (both days inclusive)
- **Working Days**: Person works ON both the start date (INIZIO) and end date (FINE)

**Example:**
```
Contract: INIZIO = 2023-01-01, FINE = 2023-01-31
Working Days: January 1st through January 31st (31 days total)
```

#### 2. Event Processing
The approach creates events as follows:
- End events are created at FINE
- Unemployment periods are identified (arco = 0)
- Unemployment dates are then adjusted: inizio+1 and fine-1
- This maintains correct temporal boundaries

**Example:**
```
Contract A: INIZIO = 2023-01-01, FINE = 2023-01-31
Contract B: INIZIO = 2023-02-05, FINE = 2023-02-28

Events created:
- 2023-01-01: +1 (start A)
- 2023-01-31: -1 (end A)
- 2023-02-05: +1 (start B)
- 2023-02-28: -1 (end B)

After processing:
- Employment A: Jan 1 - Jan 31
- Unemployment: Feb 1 - Feb 4 (adjusted from raw segment)
- Employment B: Feb 5 - Feb 28
```

#### 3. Event-Based Transformation
Each employment contract generates exactly two events:

1. **Start Event**: 
   - Date: INIZIO
   - Value: +1 (employment begins)
   - Type: Original contract type (prior value)

2. **End Event**:
   - Date: FINE
   - Value: -1 (employment ends)  
   - Type: 0 (marks end of employment)

**Cumulative Sum Interpretation:**
- arco = 0: Unemployment (no active contracts)
- arco = 1: Single employment (one active contract)
- arco > 1: Multiple employment (overlapping contracts)

### Duration Calculations

Duration calculations depend on employment status and account for the event structure:

#### Employment Duration (arco >= 1)
```
durata = fine - inizio
```
Standard date difference for employment segments.

#### Unemployment Duration (arco = 0) 
```
durata = fine - inizio - 1
```
The -1 adjustment accounts for the fact that unemployment periods use exclusive end dates in the event structure.

### Practical Examples

#### Example 1: Consecutive Contracts (No Unemployment Gap)
```
Data:
Contract 1: INIZIO = 2023-01-01, FINE = 2023-03-31 (90 days)
Contract 2: INIZIO = 2023-04-01, FINE = 2023-06-30 (91 days)

Generated Events:
2023-01-01: +1 (start contract 1)
2023-04-01: -1 (end contract 1, unemployment would start)
2023-04-01: +1 (start contract 2, same day - no unemployment)
2023-07-01: -1 (end contract 2)

Result: No unemployment period between contracts
```

#### Example 2: Gap Between Contracts
```
Data:
Contract 1: INIZIO = 2023-01-01, FINE = 2023-02-28 (59 days)
Contract 2: INIZIO = 2023-04-01, FINE = 2023-05-31 (61 days)

Generated Events:
2023-01-01: +1 (start contract 1) 
2023-03-01: -1 (end contract 1, unemployment starts)
2023-04-01: +1 (start contract 2, unemployment ends)
2023-06-01: -1 (end contract 2)

Unemployment Period: March 1 - March 31 (31 days)
```

#### Example 3: Overlapping Contracts
```
Data:
Contract 1: INIZIO = 2023-01-01, FINE = 2023-06-30
Contract 2: INIZIO = 2023-04-01, FINE = 2023-09-30

Generated Events:
2023-01-01: +1 (start contract 1, arco=1)
2023-04-01: +1 (start contract 2, arco=2) 
2023-07-01: -1 (end contract 1, arco=1)
2023-10-01: -1 (end contract 2, arco=0)

Overlapping Period: April 1 - June 30 (arco=2, multiple employment)
```

### Architecture Components

The package implements functionality through specialized modules:

#### R/data_quality.R 
- Input validation and data quality assessment
- Detects invalid date ranges, overlaps, and temporal inconsistencies

#### R/status_labeling.R
- Employment status classification based on arco values and prior types
- Maps overlapping periods to appropriate labels (occ_ft, occ_pt, over_*)
- Provides customizable classification rules

#### R/vecshift_integrated.R
- Full pipeline with data quality assessment
- Automatic data cleaning options
- Comprehensive processing with all modules

### Data Quality Considerations

#### Common Date Issues
1. **Invalid Ranges**: FINE < INIZIO
2. **Zero Duration**: FINE = INIZIO (handled correctly as 1-day contracts)
3. **Overlapping Contracts**: Multiple contracts active simultaneously
4. **Date Format Inconsistencies**: Mixed Date, numeric, and character formats

#### Validation Functions
- `validate_date_consistency()`: Detects logical inconsistencies
- Quality assessment provides person-level employment statistics
- Temporal coverage analysis identifies data gaps and employment patterns

### Performance

The vecshift function provides:
- **Optimized Core**: High-performance event generation (~1.46M records/second)
- **Optional Validation**: Use `vecshift_integrated()` for full data quality checks
- **Flexible Classification**: Customizable status rules without performance impact

## Important Notes

- The package uses renv for dependency management - always restore the environment before development
- The vecshift function relies heavily on data.table syntax and operations
- Prior values: 0 or -1 indicate part-time, positive values indicate full-time employment
- The function handles overlapping employment periods (multiple concurrent jobs)
- **Date Logic**: Creates end events at FINE and adjusts unemployment periods afterward (inizio+1, fine-1)