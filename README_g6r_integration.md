# Interactive Employment Transitions with g6r

This document provides a comprehensive overview of the g6r integration with the vecshift package, enabling interactive network visualizations of employment transitions.

## Overview

The g6r integration adds powerful interactive visualization capabilities to the vecshift package by leveraging the G6 JavaScript visualization library through the g6R R package. This integration enables users to create sophisticated, web-based network diagrams for exploring employment transition patterns.

## Key Features

### 🎯 Interactive Network Visualizations
- **20+ Layout Algorithms**: Force-directed, circular, hierarchical, radial, concentric, and more
- **Real-time Interactivity**: Zoom, pan, drag nodes, select elements
- **Multiple View Modes**: Network view, matrix view, timeline analysis
- **Performance Optimized**: Handles large datasets with 20,000+ nodes

### 🎨 Accessibility & Design
- **Colorblind-Friendly Palettes**: Default viridis and custom accessible color schemes
- **High Contrast Mode**: Enhanced visibility for accessibility compliance
- **Redundant Encoding**: Size + color coding for critical information
- **Responsive Design**: Works across desktop and mobile devices

### 🔧 Shiny Integration
- **Complete Shiny Modules**: Pre-built components for dashboard development
- **Dynamic Updates**: Real-time filtering and visualization updates
- **Export Capabilities**: Download data and visualizations
- **Professional Dashboards**: Production-ready employment analysis interfaces

## Installation Requirements

```r
# Install g6R package (required for interactive features)
install.packages("g6R")

# Or install development version for latest features
pak::pak("cynkra/g6R")

# Ensure vecshift is installed
pak::pak("gmontaletti/vecshift")  # Replace with actual repo

# Optional packages for enhanced functionality
install.packages(c("viridis", "shiny", "shinydashboard", "DT", "plotly"))
```

## Quick Start Guide

### 1. Basic Interactive Visualization

```r
library(vecshift)
library(g6R)
library(data.table)

# Generate sample data
demo_data <- generate_g6r_demo_data(n_persons = 50, n_companies = 8)

# Process through vecshift pipeline
pipeline_result <- process_employment_pipeline(
  original_data = demo_data,
  merge_columns = c("company", "salary", "region")
)

# Analyze employment transitions
transitions <- analyze_employment_transitions(
  pipeline_result = pipeline_result,
  transition_variable = "company",
  statistics_variables = c("salary", "region")
)

# Create interactive visualization
plot_interactive_transitions(
  transitions_data = transitions,
  layout = "force",
  show_minimap = TRUE,
  accessibility_mode = FALSE
)
```

### 2. Advanced Customization

```r
# Custom visualization with specific styling
plot_interactive_transitions(
  transitions_data = transitions,
  layout = "circular",
  node_size_metric = "in_degree",           # Size by incoming transitions
  edge_width_metric = "transition_duration", # Width by unemployment duration
  min_weight_threshold = 3,                 # Focus on significant transitions
  accessibility_mode = TRUE,                # High contrast colors
  edge_bundling = TRUE,                     # Clean edge visualization
  height = "800px"
)
```

### 3. Shiny Dashboard

```r
library(shiny)

# Launch pre-built dashboard
run_employment_dashboard()

# Or use module in custom app
ui <- fluidPage(
  titlePanel("Employment Transitions Explorer"),
  interactive_transitions_module()$ui("transitions")
)

server <- function(input, output, session) {
  transition_data <- reactive({
    analyze_employment_transitions(your_pipeline_result)
  })
  
  interactive_transitions_module()$server("transitions", transition_data)
}

shinyApp(ui, server)
```

## Core Functions

### Data Conversion Functions

| Function | Purpose | Key Features |
|----------|---------|--------------|
| `convert_transitions_to_g6r()` | Convert transition data to g6r format | Node/edge data preparation, metric calculation |
| `generate_g6r_demo_data()` | Create realistic sample data | Configurable parameters, career patterns |

### Visualization Functions

| Function | Purpose | Key Features |
|----------|---------|--------------|
| `plot_interactive_transitions()` | Main visualization function | Multiple layouts, interactive behaviors |
| `plot_transitions_over_time()` | Time-based animation foundation | Multi-period analysis, temporal patterns |
| `compare_transitions_between_groups()` | Side-by-side group comparisons | Regional, demographic comparisons |

### Shiny Integration

| Function | Purpose | Key Features |
|----------|---------|--------------|
| `interactive_transitions_module()` | Complete Shiny module | UI/server components, reactive updates |
| `run_employment_dashboard()` | Launch full dashboard | Multi-view interface, export options |

### Utility Functions

| Function | Purpose | Key Features |
|----------|---------|--------------|
| `test_g6r_accessibility()` | Accessibility testing | Colorblind simulation, contrast testing |

## Layout Options Comparison

| Layout | Best For | Performance | Use Case |
|--------|----------|-------------|----------|
| **Force-directed** | Organic exploration | Medium | General purpose, natural clustering |
| **Circular** | Relationship comparison | High | Equal emphasis on all nodes |
| **Hierarchical (Dagre)** | Directed flows | High | Clear directional patterns |
| **Radial** | Central node emphasis | High | Hub-and-spoke patterns |
| **Concentric** | Importance visualization | High | Centrality-based layouts |
| **Grid** | Organized positioning | High | Structured, systematic view |

## Accessibility Features

### Colorblind-Friendly Design
- **Default Palettes**: Viridis (perceptually uniform, colorblind-safe)
- **High Contrast Mode**: Black/white/primary colors with enhanced contrast
- **Custom Palettes**: Support for any colorblind-friendly scheme
- **Redundant Encoding**: Size + color for critical information

### WCAG Compliance
- **Contrast Ratios**: Meets WCAG AA standards (4.5:1 minimum)
- **Scalable Elements**: Readable at different zoom levels
- **Keyboard Navigation**: Accessible through standard web interactions
- **Screen Reader Support**: Semantic HTML structure

### Testing Tools
```r
# Test different accessibility scenarios
accessibility_tests <- test_g6r_accessibility(
  transitions_data = transitions,
  test_scenarios = c("colorblind", "high_contrast", "large_nodes")
)

# View different modes
accessibility_tests$colorblind_safe    # Deuteranopia-safe colors
accessibility_tests$high_contrast      # Maximum contrast design
accessibility_tests$large_elements     # Enhanced visibility
```

## Performance Optimization

### For Large Datasets
1. **Filter Data**: Use `min_weight_threshold` to focus on significant transitions
2. **Choose Efficient Layouts**: Circular and grid layouts perform better than force-directed
3. **Enable Edge Bundling**: Reduces visual complexity and rendering load
4. **Optimize Plugins**: Disable unnecessary plugins for large networks

```r
# Performance-optimized visualization
plot_interactive_transitions(
  transitions_data = large_transitions,
  layout = "circular",              # Faster than force-directed
  min_weight_threshold = 5,         # Focus on major transitions
  edge_bundling = TRUE,             # Reduce visual complexity
  show_minimap = FALSE,             # Reduce rendering load
  animation_duration = 0            # Disable animations for speed
)
```

### Shiny Performance
1. **Reactive Caching**: Cache expensive computations
2. **Progressive Loading**: Start with filtered data
3. **Debounced Inputs**: Prevent excessive re-rendering
4. **Lazy Loading**: Load visualizations on tab activation

## Integration with Other Packages

### With igraph
```r
# Convert g6r data for network analysis
library(igraph)

g6_data <- convert_transitions_to_g6r(transitions)
edges_igraph <- g6_data$edges[, c("source", "target", "weight")]

# Create igraph network
network <- graph_from_data_frame(
  d = edges_igraph, 
  vertices = g6_data$nodes,
  directed = TRUE
)

# Calculate centrality measures
centrality_measures <- data.frame(
  node = V(network)$name,
  betweenness = betweenness(network, normalized = TRUE),
  closeness = closeness(network, normalized = TRUE)
)
```

### With ggraph (Static Fallback)
```r
# Create static version for reports
library(ggraph)

# Convert g6r data to ggraph format
graph_tbl <- tbl_graph(
  nodes = g6_data$nodes,
  edges = g6_data$edges
)

# Static visualization
ggraph(graph_tbl, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.6) +
  geom_node_point(aes(size = value, color = color)) +
  theme_graph()
```

## Use Cases and Examples

### 1. Regional Employment Analysis
```r
# Compare employment patterns across regions
north_data <- employment_data[region == "North"]
south_data <- employment_data[region == "South"]

regional_comparison <- compare_transitions_between_groups(
  transitions_list = list(
    "North Region" = analyze_employment_transitions(process_north_data),
    "South Region" = analyze_employment_transitions(process_south_data)
  ),
  layout = "circular"
)
```

### 2. Sector Mobility Analysis
```r
# Analyze transitions between employment sectors
sector_transitions <- analyze_employment_transitions(
  pipeline_result = pipeline_result,
  transition_variable = "sector",
  statistics_variables = c("salary", "company_size"),
  min_unemployment_duration = 30  # Focus on longer gaps
)

plot_interactive_transitions(
  sector_transitions,
  layout = "dagre",  # Hierarchical for clear flow direction
  node_size_metric = "total_degree",
  show_tooltip = TRUE
)
```

### 3. Time Series Analysis
```r
# Track how transition patterns change over time
quarterly_results <- list(
  "Q1 2023" = process_employment_pipeline(q1_data),
  "Q2 2023" = process_employment_pipeline(q2_data),
  "Q3 2023" = process_employment_pipeline(q3_data),
  "Q4 2023" = process_employment_pipeline(q4_data)
)

time_based_plot <- plot_transitions_over_time(
  pipeline_results = quarterly_results,
  time_periods = names(quarterly_results),
  transition_variable = "company",
  animation_speed = 2000
)
```

## Advanced Features

### Custom JavaScript Integration
The g6r package supports custom JavaScript for advanced interactions. This enables:
- **Custom Event Handlers**: Click, hover, selection events
- **Dynamic Filtering**: Real-time data filtering based on user interaction
- **Animation Controls**: Custom animation sequences and timing
- **Export Functions**: Custom export formats and styling

### API Integration
For dynamic data sources:
- **REST API Connections**: Live data updates from employment databases
- **WebSocket Integration**: Real-time transition monitoring
- **Database Connections**: Direct connection to HR systems
- **File Watchers**: Automatic updates when data files change

## Troubleshooting

### Common Issues

#### 1. g6R Installation Problems
```r
# If standard installation fails
install.packages("g6R", dependencies = TRUE)

# For development version
remotes::install_github("cynkra/g6R")

# Check installation
if (requireNamespace("g6R", quietly = TRUE)) {
  message("g6R is available")
} else {
  message("Install g6R with: install.packages('g6R')")
}
```

#### 2. Empty Visualizations
```r
# Check your data has transitions
check_transitions <- function(transitions_data) {
  if (is.matrix(transitions_data)) {
    total <- sum(transitions_data)
    message("Matrix: ", total, " total transitions")
  } else {
    message("Data.table: ", nrow(transitions_data), " patterns, ", 
           sum(transitions_data$weight, na.rm = TRUE), " total transitions")
  }
  
  if (total == 0) {
    message("No transitions found. Check min_unemployment_duration parameter.")
  }
}

check_transitions(your_transitions)
```

#### 3. Performance Issues
```r
# For large datasets, optimize:
optimized_transitions <- transitions[weight >= 3]  # Filter significant transitions

plot_interactive_transitions(
  optimized_transitions,
  layout = "circular",        # Faster than force-directed  
  show_minimap = FALSE,       # Reduce rendering
  edge_bundling = FALSE,      # Disable for performance
  animation_duration = 0      # No animations
)
```

### Performance Benchmarks

| Dataset Size | Nodes | Edges | Recommended Layout | Expected Load Time |
|--------------|-------|-------|-------------------|-------------------|
| Small | <50 | <100 | Any layout | <1 second |
| Medium | 50-200 | 100-500 | Force/Circular | 1-3 seconds |
| Large | 200-1000 | 500-2000 | Circular/Grid | 3-10 seconds |
| Very Large | 1000+ | 2000+ | Grid only | 10+ seconds |

## Future Development

### Planned Features
- **Enhanced Animations**: Smooth time-based transitions
- **Statistical Overlays**: Network metrics visualization
- **Clustering Integration**: Automatic community detection
- **Export Enhancements**: High-resolution image export
- **Mobile Optimization**: Touch-friendly interactions

### Community Contributions
The g6r integration is designed for extensibility. Contributors can:
- **Add Layout Algorithms**: Implement new positioning strategies
- **Create Plugins**: Develop specialized interaction modes
- **Build Templates**: Design domain-specific dashboards
- **Improve Accessibility**: Enhance inclusive design features

## Resources

### Documentation
- **Main Package**: [vecshift documentation](link-to-docs)
- **g6R Package**: https://cynkra.github.io/g6R/
- **G6 JavaScript Library**: https://g6.antv.antgroup.com/en
- **Vignette**: `vignette("g6r-interactive-transitions", package = "vecshift")`

### Support
- **Issues**: Report bugs on GitHub
- **Discussions**: Join community discussions
- **Examples**: See `inst/examples/` for complete applications
- **Tutorials**: Video tutorials and workshops

### Citation
When using the g6r integration in research, please cite both packages:
```r
citation("vecshift")
citation("g6R")
```

This integration represents a significant advancement in employment data visualization, bringing professional-grade interactive capabilities to the R ecosystem for labor market analysis.