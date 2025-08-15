# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The `vecshift` package is an R package that performs temporal data analysis operations on employment/labor data. The main function `vecshift()` processes data.table objects containing employment records with start/end dates and transforms them into continuous temporal segments with employment status classifications and overlap consolidation identifiers (`over_id`).

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


### Future Development Guidelines

**Critical Performance Consideration:**
The core event generation logic in `vecshift()` is highly optimized. The event-based transformation (splitting contracts into start/end events and calculating cumulative overlaps) is the most computationally intensive part and should remain optimized.

**Recommended Approach for Future Enhancements:**
- **Core Engine**: Keep the fast event generation from `vecshift()` as the foundational engine
- **Modular Rules**: Add business logic, validation, and classification rules as separate components
- **Integration Layer**: Use the integrated system for data quality, cleaning, and extended features
- **Performance Testing**: Always benchmark against the current baseline (1.46M records/second)

**Extension Points:**

- Output formatting and export functions
- Integration with other temporal analysis packages
- Visualization and reporting components

## Overlap Consolidation with over_id

### Overview of over_id Functionality

The `over_id` column provides a unique identifier for continuous employment periods, enabling powerful consolidation and analysis capabilities.

#### Core Concept
**over_id** assigns the same identifier to all employment contracts that belong to the same continuous overlapping time period:
- **over_id = 0**: Unemployment periods (no active contracts)
- **over_id > 0**: Employment periods where the same value indicates contracts belonging to the same continuous overlapping employment period

#### Mathematical Invariant: Duration Consistency
The package ensures temporal accuracy through a critical mathematical property:

```
For each person (cf): elapsed_time = sum(durata)

Where:
- elapsed_time = last(FINE) - first(INIZIO) + 1
- sum(durata) = total duration across all segments
```

This invariant guarantees that the sum of all segment durations exactly equals the total elapsed time from first employment start to last employment end.

### Practical Examples

#### Example 1: Simple Overlapping Contracts
```
Input Contracts:
Contract A: 2023-01-01 to 2023-06-30
Contract B: 2023-04-01 to 2023-09-30

Generated Segments:
Segment 1: 2023-01-01 to 2023-03-31, over_id=1, arco=1 (A only)
Segment 2: 2023-04-01 to 2023-06-30, over_id=1, arco=2 (A+B overlap)
Segment 3: 2023-07-01 to 2023-09-30, over_id=1, arco=1 (B only)

All segments share over_id=1 because they form one continuous employment period.
```

#### Example 2: Gap Between Employment Periods
```
Input Contracts:
Contract A: 2023-01-01 to 2023-03-31
Contract B: 2023-06-01 to 2023-08-31

Generated Segments:
Segment 1: 2023-01-01 to 2023-03-31, over_id=1, arco=1 (Employment A)
Segment 2: 2023-04-01 to 2023-05-31, over_id=0, arco=0 (Unemployment)
Segment 3: 2023-06-01 to 2023-08-31, over_id=2, arco=1 (Employment B)

Two distinct employment periods (over_id=1 and over_id=2) separated by unemployment.
```

#### Example 3: Complex Overlapping Pattern
```
Input Contracts:
Contract A: 2023-01-01 to 2023-04-30
Contract B: 2023-02-01 to 2023-06-30
Contract C: 2023-05-01 to 2023-07-31
Contract D: 2023-10-01 to 2023-12-31

Generated Segments:
Segment 1: 2023-01-01 to 2023-01-31, over_id=1, arco=1 (A)
Segment 2: 2023-02-01 to 2023-04-30, over_id=1, arco=2 (A+B)
Segment 3: 2023-05-01 to 2023-06-30, over_id=1, arco=2 (B+C)
Segment 4: 2023-07-01 to 2023-07-31, over_id=1, arco=1 (C)
Segment 5: 2023-08-01 to 2023-09-30, over_id=0, arco=0 (Unemployment)
Segment 6: 2023-10-01 to 2023-12-31, over_id=2, arco=1 (D)

Contracts A, B, C form one continuous period (over_id=1), while D is separate (over_id=2).
```

### Benefits of over_id Consolidation

1. **Simplified Analysis**: Group related employment periods for career progression analysis
2. **Accurate Duration Calculation**: Eliminate double-counting in overlapping periods
3. **Transition Analysis**: Clean identification of true employment-to-unemployment transitions
4. **Visualization**: Better network graphs showing consolidated employment states
5. **Performance**: Faster aggregations using pre-computed consolidation groups

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


### Data Quality Considerations

#### Common Date Issues
1. **Invalid Ranges**: FINE < INIZIO
2. **Zero Duration**: FINE = INIZIO (handled correctly as 1-day contracts)
3. **Overlapping Contracts**: Multiple contracts active simultaneously
4. **Date Format Inconsistencies**: Mixed Date, numeric, and character formats
5. **Duration Inconsistency**: Sum of durations ≠ elapsed time (automatically corrected by over_id processing)

#### Validation Functions
- `validate_date_consistency()`: Detects logical inconsistencies
- `validate_over_id_integrity()`: Verifies over_id assignments and duration calculations
- Quality assessment provides person-level employment statistics including over_id patterns
- Temporal coverage analysis identifies data gaps and employment patterns

### Performance

The vecshift function provides:
- **Optimized Core**: High-performance event generation
- **Flexible Classification**: Customizable status rules without performance impact

## Visualization with ggraph and Network Analysis

### Overview of Network Visualization Libraries

The vecshift package supports advanced visualization of employment transitions using modern R graph libraries:

#### Core Libraries
- **ggraph**: Grammar of graphics for network visualization (extends ggplot2) - Static visualizations
- **tidygraph**: Tidy manipulation of graph/network data structures
- **g6r**: Interactive network visualization using Ant Design G6 JavaScript library - Interactive visualizations
- **networkD3**: Interactive Sankey diagrams and network visualizations
- **viridis/RColorBrewer**: Colorblind-friendly palettes for accessibility

#### Key Visualization Types for Employment Transitions

1. **Network Diagrams**: Show overall transition structure and relationships
   - Layout algorithms: Fruchterman-Reingold ("fr"), Kamada-Kawai ("kk"), Sugiyama (hierarchical)
   - Best for: Understanding complex transition patterns and central employment states

2. **Sankey/Alluvial Diagrams**: Emphasize flow quantities and conservation
   - Shows volume of transitions between states
   - Best for: Visualizing career progression and flow patterns

3. **Arc Diagrams**: Simplified linear view of connections
   - States arranged linearly with arcs showing transitions
   - Best for: Clear presentation with fewer states

## Enhanced Analysis Functions with over_id Support

### New Analysis Functions

The package now includes three powerful new analysis functions that leverage over_id consolidation:

#### 1. analyze_consolidated_periods()
Analyzes consolidated employment periods using over_id groupings:

```r
consolidated_analysis <- analyze_consolidated_periods(
  pipeline_result = processed_data,
  consolidation_type = "both",  # "overlapping", "consecutive", "both", "none"
  min_employment_duration = 30,
  min_unemployment_duration = 7
)
```

**Output includes:**
- `over_id`: Consolidation period identifier
- `period_type`: "employment" or "unemployment"
- `total_duration`: Total days in consolidated period
- `contract_count`: Number of original contracts in period
- `max_overlap`: Maximum concurrent contracts
- `start_date`, `end_date`: Period boundaries
- `employment_types`: Summary of employment types in period

#### 2. create_consolidated_transition_matrix()
Creates transition matrices based on consolidated periods:

```r
transition_matrix <- create_consolidated_transition_matrix(
  pipeline_result = processed_data,
  transition_variable = "employment_type",
  consolidation_type = "both",
  output_probabilities = TRUE  # Convert counts to transition probabilities
)
```

**Features:**
- Transitions between consolidated periods rather than individual segments
- Eliminates artificial transitions within overlapping employment
- More accurate representation of true career movements
- Supports probability calculation for Markov chain analysis

#### 3. analyze_employment_overlaps()
Detailed analysis of overlapping employment patterns:

```r
overlap_analysis <- analyze_employment_overlaps(
  pipeline_result = processed_data,
  min_overlap_duration = 7,
  employment_type_variable = "prior"
)
```

**Output includes:**
- `over_id`: Consolidation period identifier
- `overlap_segments`: Number of segments with arco > 1
- `max_concurrent_contracts`: Peak overlap level
- `overlap_duration`: Total days with multiple contracts
- `overlap_percentage`: Percentage of employment period with overlaps
- `dominant_employment_type`: Most common employment type
- `employment_type_diversity`: Number of different employment types

### Enhanced analyze_employment_transitions()

The core transition analysis function now supports consolidation parameters:

```r
transitions <- analyze_employment_transitions(
  pipeline_result = your_data,
  transition_variable = "employment_type",
  min_unemployment_duration = 7,
  max_unemployment_duration = 365,
  consolidation_type = "both",        # NEW: Consolidation approach
  use_consolidated_periods = TRUE     # NEW: Use over_id for grouping
)
```

#### Consolidation Types Explained

| Type | Description | Use Case |
|------|-------------|----------|
| **"overlapping"** | Merge only overlapping contracts | Focus on concurrent employment analysis |
| **"consecutive"** | Merge only consecutive contracts | Analyze career continuity without gaps |
| **"both"** | Merge both overlapping and consecutive | Complete consolidation for career analysis |
| **"none"** | No consolidation (original behavior) | Detailed segment-level analysis |

### Working with analyze_employment_transitions() Output

The function provides two output formats suitable for visualization:

1. **Data.table format** (default):
   - Columns: from, to, weight, transition_duration, [statistics columns]
   - Direct conversion to tidygraph objects for ggraph visualization

2. **Matrix format** (output_transition_matrix = TRUE):
   - Square transition matrix with states as row/column names
   - Zero-filled for non-existent transitions
   - Suitable for heatmaps and matrix-based visualizations

### Accessibility Guidelines for Graph Visualization

#### Color Palettes
- **Primary**: viridis (perceptually uniform, colorblind-safe)
- **Alternative**: Okabe-Ito palette (optimized for all colorblindness types)
- **Qualitative**: RColorBrewer Set2/Set3 for categorical data

#### Design Principles
1. **Redundant Encoding**: Use both color AND shape/size for critical information
2. **Contrast**: Ensure WCAG AA compliance (4.5:1 for normal text, 3:1 for graphics)
3. **Clear Labels**: Use repel algorithms to prevent overlapping text
4. **Legends**: Position prominently with sufficient size for readability

### Example Visualization Workflow with over_id Consolidation

```r
# Load required libraries
library(vecshift)
library(ggraph)
library(tidygraph)
library(viridis)

# Analyze transitions with consolidation
transitions <- analyze_employment_transitions(
  pipeline_result = your_data,
  transition_variable = "employment_type",
  min_unemployment_duration = 7,
  max_unemployment_duration = 365,
  consolidation_type = "both",        # Use over_id consolidation
  use_consolidated_periods = TRUE
)

# Alternative: Analyze consolidated periods directly
consolidated <- analyze_consolidated_periods(
  pipeline_result = your_data,
  consolidation_type = "both"
)

# Create network visualization
tg <- tbl_graph(
  nodes = data.frame(name = unique(c(transitions$from, transitions$to))),
  edges = transitions,
  directed = TRUE
)

# Visualize with ggraph - cleaner due to consolidation
ggraph(tg, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = weight), 
                 arrow = arrow(length = unit(3, "mm"))) +
  geom_node_point(aes(size = degree), color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_alpha_continuous(range = c(0.3, 1)) +
  scale_size_continuous(range = c(3, 8)) +
  theme_graph() +
  labs(title = "Employment Transitions (Consolidated Periods)",
       subtitle = "Based on over_id consolidation")
```

### Layout Algorithm Selection Guide

| Layout | Best For | Characteristics |
|--------|----------|----------------|
| Fruchterman-Reingold ("fr") | General networks | Natural clustering, balanced spacing |
| Kamada-Kawai ("kk") | Small-medium networks | Preserves graph distances |
| Sugiyama | Hierarchical/temporal | Shows directionality and levels |
| Circle | Cyclical patterns | Equal prominence to all nodes |
| Linear/Arc | Simple transitions | Clear, minimal crossing |

### Performance Considerations

- For large networks (>100 nodes): Use data filtering (min_weight_threshold)
- For interactive exploration: Consider networkD3 or plotly backends
- For publication: Use high-resolution ggraph with careful aesthetic mapping

## Interactive Visualization with g6r

### Overview of g6r for Employment Transitions

g6r provides interactive, web-based network visualizations through R, leveraging the powerful Ant Design G6 JavaScript library. It's specifically designed for Shiny applications and interactive dashboards.

#### Key g6r Features
- **20+ Layout Algorithms**: Force-directed, circular, hierarchical (dagre), radial, concentric, grid
- **15+ Interactive Behaviors**: Zoom, pan, drag nodes, brush select, hover tooltips
- **17+ Plugins**: Minimap, edge bundling, menu system, fisheye distortion
- **Shiny Integration**: Native support with renderG6() and g6Output()
- **Real-time Updates**: Proxy functions for dynamic data changes
- **Touch Support**: Mobile-responsive interactions

### g6r vs Static Visualization Decision Guide

| Use Case | Recommendation | Reasoning |
|----------|---------------|-----------|
| **Exploration & Analysis** | g6r | Interactive filtering, drilling down into patterns |
| **Presentation/Reports** | ggraph | Clean, publication-ready static images |
| **Dashboard Applications** | g6r | Real-time updates, user interaction |
| **Large Networks (>1000 nodes)** | ggraph | Better performance for complex layouts |
| **Mobile/Touch Devices** | g6r | Native touch interaction support |
| **Print/PDF Output** | ggraph | Vector graphics, consistent formatting |

### g6r Integration with vecshift

#### Basic Interactive Visualization
```r
library(g6R)
library(vecshift)

# Process employment data with consolidation
transitions <- analyze_employment_transitions(
  pipeline_result = your_data,
  transition_variable = "company",
  max_unemployment_duration = 365,
  consolidation_type = "both",        # Use over_id consolidation
  use_consolidated_periods = TRUE
)

# Convert to g6r format
g6_data <- convert_transitions_to_g6r(
  transition_data = transitions,
  node_size_metric = "total_degree",
  edge_width_metric = "weight"
)

# Create interactive visualization
plot_interactive_transitions(
  transitions,
  layout = "force",           # or "circular", "dagre", "radial"
  accessibility_mode = TRUE,  # High contrast, colorblind-safe
  edge_bundling = TRUE,       # Cleaner appearance for dense networks
  height = "600px"
)
```

#### Advanced Shiny Integration
```r
# Launch complete employment dashboard
run_employment_dashboard()

# Or use modular components in custom Shiny apps
ui <- fluidPage(
  interactive_transitions_module()$ui("employment")
)

server <- function(input, output, session) {
  data <- reactive({ your_transition_data })
  interactive_transitions_module()$server("employment", data)
}
```

### Layout Selection for Employment Data

#### Force-Directed Layout ("force")
- **Best for**: General exploration, natural clustering
- **Characteristics**: Nodes repel, connected nodes attract
- **Use when**: Want to see natural groupings and communities
- **Performance**: Moderate (good for <500 nodes)

#### Circular Layout ("circular") 
- **Best for**: Equal emphasis on all employment states
- **Characteristics**: Nodes arranged in circle, transitions as chords
- **Use when**: Comparing transition volumes between states
- **Performance**: Excellent (handles 1000+ nodes well)

#### Hierarchical Layout ("dagre")
- **Best for**: Career progression, temporal flows
- **Characteristics**: Clear directional hierarchy
- **Use when**: Showing advancement patterns or skill progression
- **Performance**: Good (optimized for directed graphs)

#### Radial Layout ("radial")
- **Best for**: Hub-and-spoke patterns, central employers
- **Characteristics**: Central nodes with radiating connections
- **Use when**: Analyzing major employers or key transition points
- **Performance**: Good (clear even with many connections)

### Accessibility in Interactive Visualizations

#### Colorblind-Friendly Features
```r
# High contrast mode automatically enabled
plot_interactive_transitions(
  transitions,
  accessibility_mode = TRUE,
  color_palette = "viridis"  # Default colorblind-safe
)

# Test accessibility compliance
accessibility_tests <- test_g6r_accessibility(transitions)
# Generates: colorblind_safe, high_contrast, large_elements versions
```

#### Keyboard Navigation Support
- **Tab Navigation**: Move between interactive elements
- **Arrow Keys**: Pan the visualization
- **+/- Keys**: Zoom in/out
- **Space**: Reset view
- **Enter**: Select/activate focused element

#### Screen Reader Compatibility
- Semantic HTML structure for assistive technologies
- Alt-text for visual elements
- Structured data tables as fallback
- Keyboard shortcuts documented in help system

### Performance Optimization for Large Datasets

#### Data Filtering Strategies
```r
# Filter by minimum transition frequency
large_transitions <- transitions[weight >= 10]

# Focus on specific time periods or employment types
recent_transitions <- transitions[
  transition_duration <= 180  # Max 6 months unemployment
]

# Use hierarchical sampling for very large datasets
sampled_data <- transitions[sample(.N, min(.N, 1000))]
```

#### Layout Performance Guide
| Dataset Size | Recommended Layouts | Avoid | Expected Load Time |
|--------------|-------------------|-------|-------------------|
| <100 nodes | Any layout | None | <1 second |
| 100-500 nodes | Force, Circular, Radial | Complex hierarchical | 1-3 seconds |
| 500-1000 nodes | Circular, Grid | Force-directed | 3-8 seconds |
| >1000 nodes | Grid, Preset positions | Force, Dagre | 8+ seconds |

### Interactive Features for Employment Analysis

#### Real-time Filtering
- **Company Size**: Filter by number of employees
- **Transition Frequency**: Show only significant flows
- **Geographic Region**: Focus on specific areas
- **Time Period**: Analyze seasonal patterns
- **Employment Type**: Full-time vs part-time transitions

#### Dynamic Grouping
- **Sector Clustering**: Group by industry classification
- **Skill Level**: Aggregate by required qualifications
- **Company Hierarchy**: Parent/subsidiary relationships
- **Regional Analysis**: Geographic proximity groupings

#### Export and Sharing
- **Static Export**: High-resolution PNG/SVG for reports
- **Data Export**: Filtered transition tables as CSV
- **Interactive Sharing**: Shareable dashboard URLs
- **Embedded Widgets**: Integration with external websites



## Impact Evaluation Framework

### Overview of Impact Evaluation Modules

The vecshift package includes a comprehensive impact evaluation framework designed for rigorous causal inference analysis of employment interventions. The framework is built on seven interconnected modules that provide end-to-end functionality from event identification to report generation.

#### Core Modules

**1. Event Identification (`impact_evaluation.R`)**
- `identify_treatment_events()`: Flexible treatment event identification with multiple condition support
- `create_treatment_control_groups()`: Treatment and control group creation with matching capabilities
- `assess_treatment_event_quality()`: Diagnostic tools for event identification quality

**2. Matching and Control Group Selection (`impact_matching.R`)**  
- `propensity_score_matching()`: Advanced PSM with multiple algorithms (nearest, optimal, genetic)
- `coarsened_exact_matching()`: CEM implementation with automatic binning
- `assess_balance()`: Comprehensive balance assessment with distributional tests
- `assess_match_quality()`: Matching quality diagnostics and recommendations

**3. Causal Inference Estimation (`impact_estimation.R`)**
- `difference_in_differences()`: DiD estimation with parallel trends testing  
- `event_study_design()`: Event study analysis with flexible time windows
- `synthetic_control_method()`: Synthetic control wrapper using augsynth
- `aggregate_treatment_effects()`: Multi-outcome effect aggregation with multiple testing corrections

**4. Impact Metrics Calculation (`impact_metrics.R`)**
- `calculate_employment_stability_metrics()`: Employment stability and turnover measures
- `calculate_contract_quality_metrics()`: Contract type transitions and quality improvements  
- `calculate_career_complexity_metrics()`: Concurrent employment and career fragmentation
- `calculate_transition_pattern_metrics()`: Job search and transition frequency analysis
- `calculate_comprehensive_impact_metrics()`: Unified metric calculation interface

**5. Regression Discontinuity Design (`impact_rdd.R`)**
- RDD-specific functions for threshold-based interventions
- Local polynomial estimation with optimal bandwidth selection
- Robustness checks and sensitivity analysis

**6. Visualization (`impact_visualization.R`)**
- Impact-specific visualization functions
- Event study plots, balance plots, and diagnostic visualizations
- Integration with existing vecshift visualization framework

**7. Reporting (`impact_reporting.R`)**
- Automated report generation for impact evaluations
- Standardized output formats for policy reports
- Integration with R Markdown and publication workflows

### Typical Impact Evaluation Workflow

#### Phase 1: Event Identification and Data Preparation
```r
# 1. Identify treatment events (e.g., permanent contract transitions)
treatment_events <- identify_treatment_events(
  data = employment_data,
  treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
  event_window = c(-365, 730),
  min_pre_period = 180,
  multiple_events = "first"
)

# 2. Assess event identification quality
event_quality <- assess_treatment_event_quality(
  event_data = treatment_events,
  assessment_variables = c("age", "sector", "education", "prior_employment")
)

# 3. Create treatment and control groups
groups <- create_treatment_control_groups(
  event_data = treatment_events,
  matching_variables = c("age", "education", "sector", "region"),
  control_ratio = 2,
  exclude_future_treated = TRUE
)
```

#### Phase 2: Matching and Balance Assessment  
```r
# 4. Propensity score matching
ps_match <- propensity_score_matching(
  data = groups,
  matching_variables = c("age", "education", "prior_wage", "employment_history"),
  exact_match_vars = c("gender", "region"),
  method = "nearest",
  caliper = 0.1
)

# 5. Assess balance and match quality
balance_results <- assess_balance(
  matched_data = ps_match$matched_data,
  balance_variables = c("age", "education", "prior_wage", "sector")
)

match_quality <- assess_match_quality(
  matching_result = ps_match,
  diagnostic_plots = TRUE
)
```

#### Phase 3: Impact Estimation
```r
# 6. Difference-in-differences estimation
did_results <- difference_in_differences(
  data = ps_match$matched_data,
  outcome_vars = c("employment_rate", "wage_growth", "job_stability"),
  control_vars = c("age", "education"),
  parallel_trends_test = TRUE,
  cluster_var = "region"
)

# 7. Event study analysis (for dynamic effects)
event_study <- event_study_design(
  data = ps_match$matched_data,
  outcome_vars = c("employment_rate", "contract_quality_index"),
  event_window = c(-12, 24),
  time_unit = "months"
)

# 8. Calculate impact metrics
impact_metrics <- calculate_comprehensive_impact_metrics(
  data = ps_match$matched_data,
  metrics = c("stability", "quality", "complexity", "transitions"),
  output_format = "wide"
)
```

#### Phase 4: Results Aggregation and Reporting
```r
# 9. Aggregate treatment effects across outcomes
aggregated_effects <- aggregate_treatment_effects(
  estimation_results = list(did_results, event_study),
  aggregation_method = "meta_analysis",
  multiple_testing_correction = "holm"
)

# 10. Generate comprehensive impact report
impact_report <- generate_impact_evaluation_report(
  estimation_results = list(did_results, event_study),
  match_quality = match_quality,
  balance_assessment = balance_results,
  impact_metrics = impact_metrics,
  output_format = "html"
)
```

### Integration with Existing Vecshift Functions

The impact evaluation framework seamlessly integrates with core vecshift functionality:

#### Data Pipeline Integration
- All impact functions work directly with `vecshift()` output, leveraging over_id consolidation
- Automatic handling of temporal employment segments and over_id-based employment definitions
- Integration with existing data quality assessment and cleaning functions

#### Metric Calculation Enhancement
- Impact metrics utilize vecshift's temporal logic and consolidation features
- Employment stability metrics account for over_id-based employment periods
- Contract quality metrics leverage vecshift's employment classification system

#### Visualization Integration  
- Impact visualizations use vecshift's theme system and color palettes
- Event study plots integrate with existing ggraph transition visualization
- Balance plots and diagnostic visualizations follow vecshift design principles

### Example Use Cases

#### Use Case 1: Permanent Contract Policy Evaluation (COD_TIPOLOGIA_CONTRATTUALE == "C.01.00")
```r
# Evaluate impact of permanent contract transitions on employment stability
permanent_contract_study <- identify_treatment_events(
  data = vecshift_output,
  treatment_conditions = list(
    "COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'",
    "prior <= 0"  # Previously temporary/part-time
  ),
  event_window = c(-365, 730),
  require_employment_before = TRUE
)

# Outcomes: employment_rate, job_turnover, wage_growth, contract_stability
```

#### Use Case 2: Training Program Impact Assessment
```r
# Identify training program participation
training_events <- identify_treatment_events(
  data = vecshift_output,
  treatment_conditions = list(
    function(dt) dt$training_program_participation == 1
  ),
  event_window = c(-180, 365),
  min_pre_period = 90
)

# Focus on career complexity and transition pattern changes
```

#### Use Case 3: Regional Employment Policy Evaluation  
```r
# Multi-region policy rollout analysis
regional_policy <- identify_treatment_events(
  data = vecshift_output,
  treatment_conditions = list(
    list(column = "policy_region", operator = "==", value = "treatment_region"),
    list(column = "policy_start_date", operator = "<=", value = as.Date("2020-01-01"))
  ),
  event_window = c(-730, 1095)  # 2 years before, 3 years after
)

# Use synthetic control method for policy regions
```

### Best Practices and Recommendations

#### Event Identification
1. **Multiple Conditions**: Use multiple complementary conditions to improve treatment precision
2. **Minimum Periods**: Ensure adequate pre/post observation periods (recommended: 90+ days each)
3. **Quality Assessment**: Always run `assess_treatment_event_quality()` before proceeding
4. **Employment History**: Consider `require_employment_before = TRUE` for job-related interventions

#### Matching Strategy
1. **Variable Selection**: Include variables that predict both treatment and outcomes  
2. **Balance Assessment**: Prioritize balance over sample size retention
3. **Common Support**: Check and enforce common support regions
4. **Multiple Methods**: Compare PSM and CEM results for robustness

#### Estimation Approach  
1. **Parallel Trends**: Test parallel trends assumption in DiD analysis
2. **Dynamic Effects**: Use event studies to understand effect evolution
3. **Multiple Outcomes**: Analyze multiple complementary outcomes
4. **Robustness Checks**: Vary specifications and time windows

#### Metrics and Interpretation
1. **Comprehensive Metrics**: Calculate stability, quality, complexity, and transition metrics
2. **Economic Significance**: Focus on substantively meaningful effect sizes
3. **Multiple Testing**: Apply appropriate corrections for multiple outcomes
4. **Heterogeneity**: Investigate effect heterogeneity across subgroups

### Performance and Scalability

The impact evaluation framework is designed for large-scale employment datasets:

- **Optimized for Large Data**: All functions use data.table for efficient processing
- **Modular Design**: Can run individual components independently  
- **Memory Efficient**: Streaming approaches for very large datasets
- **Parallel Processing**: Support for multi-core estimation where available

Expected performance on typical datasets:
- Event identification: ~100K observations/second
- Propensity score matching: ~10K observations/second  
- DiD estimation: ~500K observations/second
- Comprehensive metrics: ~50K observations/second

The framework automatically scales computational approaches based on data size and provides progress indicators for long-running operations.

## Important Notes

- The package uses renv for dependency management - always restore the environment before development
- The vecshift function relies heavily on data.table syntax and operations
- Prior values: 0 or -1 indicate part-time, positive values indicate full-time employment
- The function handles overlapping employment periods (multiple concurrent jobs)
- **over_id Innovation**: Unique consolidation system for continuous employment period analysis
- **Duration Invariant**: Mathematical guarantee that elapsed_time = sum(durata) by person
- **Date Logic**: Creates end events at FINE and adjusts unemployment periods afterward (inizio+1, fine-1)
- **Consolidation Types**: Choose appropriate level ("overlapping", "consecutive", "both", "none") for your analysis needs
- **Visualization**: ggraph and tidygraph provide comprehensive network visualization capabilities for transition analysis, enhanced by consolidation
- **Impact Evaluation**: Comprehensive framework for causal inference with employment data, supporting multiple identification strategies and robust inference methods
- use "../reference/vecshift" directory to store artefacts like: todo lists, .md documents test R scripts not needed to compile the package. When using agent-r-project-maintainer istruct it to move these file there insteadd of deleting