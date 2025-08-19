# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The `vecshift` package is an R package that performs temporal data analysis operations on employment/labor data. The main function `vecshift()` processes data.table objects containing employment records with start/end dates and transforms them into continuous temporal segments with employment status classifications and overlap consolidation identifiers (`over_id`).

**Note**: Analytics and visualization capabilities have been moved to the companion package `longworkR` located at `../longworkR`.

## Package Split Architecture

The project is now split into two complementary packages:

### vecshift (Core Data Transformation)
**Location**: Current directory
**Purpose**: Core temporal data processing and transformation
**Key Components**:
- `vecshift()`: Main transformation function
- `classify_employment_status()`: Employment status classification
- `add_external_events()`: External event integration
- `add_unemployment_tail()`: Unemployment period handling
- `merge_consecutive_employment()`: Employment consolidation
- `process_employment_pipeline()`: Pipeline processing
- Data quality and validation functions

### longworkR (Analytics & Visualization)
**Location**: `../longworkR`
**Purpose**: Advanced analytics and visualization for vecshift output
**Key Components**:
- **Survival Analysis**: Contract survival analysis and visualization
- **Impact Evaluation**: DiD, PSM, event studies, RDD
- **Network Analysis**: Employment transition networks
- **Visualization**: ggraph, g6r, and interactive visualizations
- **Advanced Analytics**: Consolidated period analysis, transition matrices

## Development Commands

### Build and Check vecshift Package
```bash
# Build the package
R CMD build .

# Check the package
R CMD check vecshift_*.tar.gz

# Install the package locally
R CMD INSTALL .
```

### Build and Check longworkR Package
```bash
# Navigate to longworkR directory
cd ../longworkR

# Build the package
R CMD build .

# Check the package
R CMD check longworkR_*.tar.gz

# Install the package locally
R CMD INSTALL .
```

### Managing Dependencies with renv
```r
# For vecshift
renv::restore()
renv::snapshot()

# For longworkR (separate renv environment)
cd ../longworkR
renv::init()
renv::install("../vecshift")  # Install local vecshift
renv::snapshot()
```

## Core Architecture

### Main Function: vecshift()
Location: R/vecshift.R

The `vecshift()` function transforms employment records into temporal segments using a modular architecture:

1. **Core Transformation**: Takes a data.table with columns: id, cf (fiscal code), INIZIO (start date), FINE (end date), prior (employment type indicator)
2. **Event Processing**: Creates interval boundaries by splitting each record into start/end events
3. **Temporal Logic**: Creates end events at FINE, then adjusts unemployment periods (inizio+1, fine-1)
4. **Overlap Calculation**: Calculates overlapping employment periods (`arco`)
5. **Consolidation Assignment**: Generates `over_id` to identify continuous overlapping employment periods
6. **Duration Correction**: Ensures mathematical invariant that elapsed time equals sum of durations by cf
7. **Status Classification** (optional): Delegates to `classify_employment_status()` for employment state labeling

**Key Parameters:**
- `classify_status` (default: TRUE): Apply employment status classification
- `status_rules` (default: NULL): Custom classification rules

**Output Columns:**
- `over_id`: Consolidation identifier for overlapping employment periods
  - `over_id = 0`: Unemployment periods
  - `over_id > 0`: Employment periods (same value for contracts in continuous overlapping time)
- `durata`: Corrected duration ensuring temporal consistency
- `arco`: Number of overlapping contracts at any point in time

### Status Classification Module
Location: R/status_labeling.R

The employment status attribution is handled by a dedicated module:
- `classify_employment_status()`: Applies status labels to temporal segments
- `get_default_status_rules()`: Returns default classification rules
- `create_custom_status_rules()`: Creates custom classification schemes
- `analyze_status_patterns()`: Analyzes employment patterns
- `validate_status_classifications()`: Validates classification integrity

### Key Dependencies
- **data.table**: Core data manipulation (required by vecshift function)
- **renv**: Dependency management

### Data Flow
1. Input: Employment records with temporal boundaries
2. Processing: Event-based transformation to detect overlaps and gaps
3. Output: Continuous temporal segments with employment status classification
4. Analytics: Use longworkR package for advanced analysis

## Working with Both Packages

### Typical Workflow
```r
# 1. Load packages
library(vecshift)
library(longworkR)

# 2. Process data with vecshift
processed_data <- vecshift(
  employment_data,
  classify_status = TRUE
)

# 3. Perform analytics with longworkR
transitions <- analyze_employment_transitions(
  processed_data,
  consolidation_type = "both"
)

# 4. Visualize with longworkR
plot_transitions_network(transitions)
```

### Development Workflow
When developing features that span both packages:

1. **Core Changes**: Modify vecshift for data processing changes
2. **Analytics Changes**: Modify longworkR for new analysis methods
3. **Testing**: Test integration between packages
4. **Documentation**: Update both package docs as needed

## Important Notes

- The package split maintains backward compatibility through clear dependencies
- vecshift focuses on performance-critical data transformation
- longworkR provides flexible analytics without impacting core performance
- Both packages use renv for dependency management
- Prior values: 0 or -1 indicate part-time, positive values indicate full-time employment
- The function handles overlapping employment periods (multiple concurrent jobs)
- **over_id Innovation**: Unique consolidation system for continuous employment period analysis
- **Duration Invariant**: Mathematical guarantee that elapsed_time = sum(durata) by person
- **Date Logic**: Creates end events at FINE and adjusts unemployment periods afterward (inizio+1, fine-1)

## Migration Notes

If you have existing code using the combined package:

1. Install both packages: `vecshift` and `longworkR`
2. Add `library(longworkR)` to scripts using analytics functions
3. All core data processing functions remain in `vecshift`
4. All visualization and analytics functions are now in `longworkR`
5. Function names and signatures remain unchanged

## Reference Directory

Use `../reference/vecshift` directory to store artifacts like:
- Todo lists
- .md documents
- Test R scripts not needed to compile the package
- Development notes and scratch files

When using agent-r-project-maintainer, instruct it to move these files there instead of deleting them.