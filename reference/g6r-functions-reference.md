# g6r Functions Reference for vecshift Package

This document provides a comprehensive reference for all g6r integration functions added to the vecshift package for interactive employment transition visualization.

## Overview

The g6r integration transforms vecshift from a data processing package into a complete interactive employment analysis platform. These functions leverage the Ant Design G6 JavaScript library through the g6R package to create web-based, interactive visualizations suitable for research, policy analysis, and business intelligence applications.

## Core Data Conversion & Visualization Functions

### 1. `convert_transitions_to_g6r()`

**Purpose**: Converts vecshift transition data (data.table format) to g6r-compatible format

**Parameters**:
- `transition_data`: Output from `analyze_employment_transitions()`
- `node_size_metric`: Metric for node sizing ("total_degree", "in_degree", "out_degree")
- `edge_width_metric`: Metric for edge width ("weight", "transition_duration")
- `min_weight_threshold`: Minimum transition weight to include (default: 1)

**Returns**: List with `nodes` and `edges` data.frames formatted for g6r

**Usage**:
```r
g6_data <- convert_transitions_to_g6r(
  transition_data = transitions,
  node_size_metric = "total_degree",
  edge_width_metric = "weight"
)
```

### 2. `plot_interactive_transitions()`

**Purpose**: Main function for creating interactive employment transition visualizations

**Parameters**:
- `transition_data`: Output from `analyze_employment_transitions()`
- `layout`: Layout algorithm ("force", "circular", "dagre", "radial", "concentric", "grid")
- `node_size_metric`: Metric for node sizing
- `edge_width_metric`: Metric for edge thickness
- `accessibility_mode`: Enable high contrast and colorblind-friendly features (default: TRUE)
- `edge_bundling`: Enable edge bundling for cleaner appearance (default: FALSE)
- `height`: Visualization height (default: "600px")
- `width`: Visualization width (default: "100%")
- `color_palette`: Color palette ("viridis", "okabe_ito", "set2")
- `min_weight_threshold`: Minimum weight to display transitions

**Returns**: Interactive g6r visualization object

**Usage**:
```r
plot_interactive_transitions(
  transitions,
  layout = "force",
  accessibility_mode = TRUE,
  edge_bundling = TRUE,
  height = "800px"
)
```

### 3. `generate_g6r_demo_data()`

**Purpose**: Creates realistic sample employment data for testing and demonstrations

**Parameters**:
- `n_persons`: Number of persons to generate (default: 100)
- `n_companies`: Number of companies (default: 10)
- `start_date`: Start date for employment periods (default: "2020-01-01")
- `end_date`: End date for employment periods (default: "2023-12-31")
- `overlap_probability`: Probability of overlapping contracts (default: 0.1)
- `unemployment_probability`: Probability of unemployment gaps (default: 0.3)

**Returns**: data.table with employment records compatible with vecshift processing

**Usage**:
```r
demo_data <- generate_g6r_demo_data(
  n_persons = 50,
  n_companies = 8,
  overlap_probability = 0.15
)
```

## Advanced Visualization Functions

### 4. `compare_transitions_between_groups()`

**Purpose**: Creates side-by-side comparisons of transition patterns between different groups

**Parameters**:
- `transition_data_list`: Named list of transition data.tables
- `layout`: Layout algorithm to use for all comparisons
- `group_names`: Names for each group (optional, uses list names if not provided)
- `accessibility_mode`: Enable accessibility features
- `sync_scales`: Synchronize node and edge scales across groups

**Returns**: List of g6r visualization objects

**Usage**:
```r
compare_transitions_between_groups(
  list(
    "Group A" = transitions_a,
    "Group B" = transitions_b
  ),
  layout = "circular",
  sync_scales = TRUE
)
```

### 5. `plot_transitions_over_time()`

**Purpose**: Foundation for time-based animation of transitions (shows evolution over time periods)

**Parameters**:
- `pipeline_result`: Full pipeline result with temporal information
- `time_variable`: Variable containing time/date information
- `period_length`: Length of time periods for aggregation ("month", "quarter", "year")
- `transition_variable`: Variable to analyze for transitions
- `layout`: Layout algorithm
- `animation_speed`: Speed of time progression (milliseconds per frame)

**Returns**: Animated g6r visualization with time controls

**Usage**:
```r
plot_transitions_over_time(
  pipeline_result = full_data,
  time_variable = "inizio",
  period_length = "quarter",
  transition_variable = "company"
)
```

### 6. `visualize_transition_matrix_g6r()`

**Purpose**: Converts matrix output from `analyze_employment_transitions(output_transition_matrix = TRUE)` to interactive g6r visualization

**Parameters**:
- `transition_matrix`: Square transition matrix with states as row/column names
- `layout`: Layout algorithm ("circular" recommended for matrices)
- `color_palette`: Color palette for visualization
- `accessibility_mode`: Enable accessibility features

**Returns**: Interactive g6r network visualization

**Usage**:
```r
# Get matrix output
matrix_result <- analyze_employment_transitions(
  pipeline_result = data,
  output_transition_matrix = TRUE
)

# Visualize
visualize_transition_matrix_g6r(
  matrix_result,
  layout = "circular"
)
```

## Testing & Quality Assurance Functions

### 7. `test_g6r_accessibility()`

**Purpose**: Comprehensive accessibility testing suite that generates multiple accessible versions

**Parameters**:
- `transition_data`: Transition data to test
- `test_types`: Types of accessibility tests ("colorblind", "high_contrast", "large_elements")
- `generate_reports`: Generate detailed accessibility reports (default: TRUE)

**Returns**: List containing accessibility test results and alternative visualizations

**Usage**:
```r
accessibility_tests <- test_g6r_accessibility(transitions)
# Returns: $colorblind_safe, $high_contrast, $large_elements, $reports
```

### 8. `validate_g6r_data()`

**Purpose**: Data validation for g6r input formats with helpful error messages

**Parameters**:
- `nodes`: Node data.frame
- `edges`: Edge data.frame
- `strict_mode`: Enable strict validation (default: FALSE)

**Returns**: List with validation results and suggested fixes

**Usage**:
```r
validation_results <- validate_g6r_data(
  nodes = node_data,
  edges = edge_data,
  strict_mode = TRUE
)
```

## Shiny Integration Functions

### 9. `interactive_transitions_module()`

**Purpose**: Complete Shiny module for employment transition visualization with both UI and server components

**Returns**: List with `ui` and `server` functions for modular Shiny integration

**Usage**:
```r
# In Shiny app
ui <- fluidPage(
  interactive_transitions_module()$ui("employment")
)

server <- function(input, output, session) {
  data <- reactive({ your_transition_data })
  interactive_transitions_module()$server("employment", data)
}

shinyApp(ui, server)
```

### 10. `run_employment_dashboard()`

**Purpose**: Launches a complete, full-featured Shiny dashboard for employment analysis

**Parameters**:
- `data`: Optional pre-loaded data (if NULL, uses demo data)
- `port`: Port number for Shiny app (default: auto)
- `launch_browser`: Launch in browser automatically (default: TRUE)

**Returns**: Starts Shiny application (no return value)

**Usage**:
```r
# Launch with demo data
run_employment_dashboard()

# Launch with your data
run_employment_dashboard(data = your_employment_data)
```

## Utility & Helper Functions

### 11. `update_g6r_layout()`

**Purpose**: Dynamic layout switching for existing visualizations without data reload

**Parameters**:
- `g6r_object`: Existing g6r visualization object
- `new_layout`: New layout algorithm
- `animate_transition`: Animate the layout change (default: TRUE)
- `transition_duration`: Duration of animation in milliseconds

**Returns**: Updated g6r visualization

**Usage**:
```r
updated_viz <- update_g6r_layout(
  existing_visualization,
  new_layout = "circular",
  animate_transition = TRUE
)
```

### 12. `export_g6r_visualization()`

**Purpose**: Export functionality for interactive visualizations (static images and data)

**Parameters**:
- `g6r_object`: g6r visualization to export
- `format`: Export format ("png", "svg", "pdf", "csv", "json")
- `filename`: Output filename (optional)
- `width`: Image width for static exports (default: 1200)
- `height`: Image height for static exports (default: 800)
- `resolution`: DPI for image exports (default: 300)

**Returns**: File path of exported content

**Usage**:
```r
# Export static image
export_g6r_visualization(
  visualization,
  format = "png",
  filename = "employment_transitions.png",
  resolution = 300
)

# Export data
export_g6r_visualization(
  visualization,
  format = "csv",
  filename = "transition_data.csv"
)
```

## Layout Selection Guide

### Layout Characteristics for Employment Data

| Layout | Best For | Performance | Use Cases |
|--------|----------|-------------|-----------|
| **Force ("force")** | General exploration, natural clustering | Moderate (<500 nodes) | Understanding community structures |
| **Circular ("circular")** | Equal emphasis, flow comparison | Excellent (1000+ nodes) | Transition volume analysis |
| **Hierarchical ("dagre")** | Career progression, temporal flows | Good (directed graphs) | Advancement patterns |
| **Radial ("radial")** | Hub-and-spoke, central employers | Good (many connections) | Major employer analysis |
| **Concentric ("concentric")** | Importance-based positioning | Good | Skill level progressions |
| **Grid ("grid")** | Systematic organization | Excellent (large datasets) | Structured comparisons |

## Performance Guidelines

### Dataset Size Recommendations

| Dataset Size | Nodes | Edges | Recommended Layouts | Expected Load Time |
|--------------|-------|-------|-------------------|-------------------|
| Small | <50 | <100 | Any layout | <1 second |
| Medium | 50-200 | 100-500 | Force, Circular, Radial | 1-3 seconds |
| Large | 200-1000 | 500-2000 | Circular, Grid | 3-8 seconds |
| Very Large | >1000 | >2000 | Grid, Preset positions | 8+ seconds |

### Performance Optimization Strategies

```r
# Filter by minimum transition frequency
large_transitions <- transitions[weight >= 10]

# Focus on specific time periods
recent_transitions <- transitions[transition_duration <= 180]

# Use hierarchical sampling for very large datasets  
sampled_data <- transitions[sample(.N, min(.N, 1000))]
```

## Accessibility Features

### Colorblind-Friendly Design
- **Default Palettes**: viridis (perceptually uniform), Okabe-Ito (colorblind-optimized)
- **High Contrast Mode**: WCAG AA compliant contrast ratios
- **Redundant Encoding**: Color + shape + size for critical information

### Keyboard Navigation
- **Tab**: Navigate between elements
- **Arrow Keys**: Pan visualization
- **+/-**: Zoom in/out
- **Space**: Reset view
- **Enter**: Activate focused element

### Screen Reader Support
- Semantic HTML structure
- Alt-text for visual elements
- Data table fallbacks
- Documented keyboard shortcuts

## Integration with vecshift Pipeline

### Complete Workflow Example

```r
library(vecshift)
library(g6R)
library(data.table)

# 1. Generate or load employment data
employment_data <- generate_g6r_demo_data(n_persons = 100, n_companies = 12)

# 2. Process through vecshift pipeline
pipeline_result <- process_employment_pipeline(
  original_data = employment_data,
  merge_columns = c("company", "sector")
)

# 3. Analyze transitions with new parameters
transitions <- analyze_employment_transitions(
  pipeline_result = pipeline_result,
  transition_variable = "company",
  statistics_variables = "sector",
  min_unemployment_duration = 7,
  max_unemployment_duration = 365,  # New parameter
  output_transition_matrix = FALSE   # New parameter
)

# 4. Create interactive visualization
interactive_plot <- plot_interactive_transitions(
  transition_data = transitions,
  layout = "force",
  accessibility_mode = TRUE,
  edge_bundling = TRUE,
  height = "700px"
)

# 5. Launch full dashboard for exploration
run_employment_dashboard(data = employment_data)

# 6. Export results
export_g6r_visualization(
  interactive_plot,
  format = "png",
  filename = "company_transitions.png",
  resolution = 300
)
```

## File Organization

### Implementation Files
- **`R/interactive_transitions_g6r.R`**: Main implementation with all 12 functions
- **`inst/examples/g6r_employment_dashboard.R`**: Complete Shiny dashboard
- **`inst/examples/g6r_comprehensive_example.R`**: Usage examples and demonstrations
- **`vignettes/g6r-interactive-transitions.Rmd`**: Tutorial vignette with step-by-step guide

### Documentation Files
- **`man/`**: Standard R documentation for each function
- **`reference/g6r-functions-reference.md`**: This comprehensive reference (current file)
- **`README_g6r_integration.md`**: Integration overview and quick start guide
- **`inst/doc/g6r_quick_reference.md`**: Quick reference card for common tasks

## Dependencies

### Required Packages
- **g6R**: Core interactive visualization library
- **data.table**: Data manipulation (already required by vecshift)
- **viridis**: Colorblind-friendly palettes
- **jsonlite**: Data serialization for JavaScript integration

### Optional Packages (for enhanced features)
- **shiny, shinydashboard**: Dashboard functionality
- **DT**: Enhanced data tables in dashboards  
- **plotly**: Additional interactive features
- **igraph**: Network analysis integration
- **htmlwidgets**: Custom widget development

## Version Compatibility

- **R**: >= 4.0.0
- **g6R**: >= 1.0.0
- **vecshift**: >= 0.3.1 (includes new analyze_employment_transitions parameters)
- **data.table**: >= 1.14.0

## Support and Troubleshooting

### Common Issues

1. **Performance with large datasets**: Use filtering and appropriate layouts
2. **Accessibility compliance**: Always test with `test_g6r_accessibility()`
3. **Shiny integration**: Use reactive data and proper module structure
4. **Export quality**: Adjust resolution and dimensions for publication needs

### Best Practices

1. **Always enable accessibility_mode** for public-facing visualizations
2. **Test with multiple layout algorithms** to find the best representation
3. **Use appropriate filtering** for large datasets to maintain performance
4. **Document custom color palettes** for consistency across visualizations
5. **Export both static and interactive versions** for different use cases

---

*This reference document covers all g6r integration functions for the vecshift package. For additional examples and tutorials, see the vignettes and example files in the inst/ directory.*