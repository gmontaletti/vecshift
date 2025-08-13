# Ggraph-Based Network Visualizations for Employment Transitions

## Overview

This implementation provides comprehensive ggraph and tidygraph-based visualization functions for analyzing employment transitions from vecshift output. The functions create accessible, publication-ready network visualizations with support for multiple layouts, colorblind-friendly palettes, and WCAG AA compliance.

## Key Features

### 🎯 **Four Main Visualization Types**
1. **Network Diagrams** - Traditional network layouts with multiple algorithms
2. **Heatmaps** - Matrix-based transition intensity visualizations  
3. **Circular Layouts** - Chord diagrams, arc diagrams, and wheel layouts
4. **Hierarchical Layouts** - Tree structures and directed flow visualizations

### 🎨 **Accessibility-First Design**
- **Colorblind-friendly palettes**: Viridis, Okabe-Ito, ColorBrewer
- **High contrast mode**: WCAG AA compliant contrast ratios
- **Redundant encoding**: Size + color, position + color for critical information
- **Screen reader support**: Proper alt-text and semantic structure
- **Black & white compatibility**: Full functionality in grayscale

### 🔧 **Advanced Customization**
- **10+ layout algorithms**: Fruchterman-Reingold, Kamada-Kawai, circular, hierarchical
- **Flexible node sizing**: Degree, strength, betweenness, closeness centrality
- **Edge styling**: Weight-based width, transparency, directional arrows
- **Color mapping**: Community detection, centrality measures, custom groupings

## Main Functions

### Core Visualization Functions

#### `plot_transitions_network()`
Creates network visualizations using ggraph with extensive customization options.

```r
# Basic network plot
plot_transitions_network(
  transitions_data = transitions,
  layout = "fr",                    # Fruchterman-Reingold layout
  node_size_var = "strength",       # Size nodes by weighted degree
  edge_width_var = "weight",        # Edge width by transition count
  node_color_var = "community",     # Color by community detection
  palette = "viridis",              # Colorblind-friendly palette
  accessibility_mode = TRUE,        # Enable high contrast
  show_labels = TRUE                # Show node labels
)
```

#### `plot_transitions_heatmap()`
Creates matrix heatmaps showing transition intensities.

```r
# Transition probability heatmap
plot_transitions_heatmap(
  transitions_data = transitions,
  cell_value = "both",              # Show counts and percentages
  normalize = "row",                # Row-normalize for probabilities
  color_scale = "gradient",         # Continuous color scale
  accessibility_mode = TRUE,        # High contrast mode
  text_color = "auto"               # Automatic text color selection
)
```

#### `plot_transitions_circular()`
Creates circular layouts emphasizing flow patterns.

```r
# Chord diagram
plot_transitions_circular(
  transitions_data = transitions,
  circular_type = "chord",          # Chord diagram style
  node_order = "frequency",         # Order by connection frequency
  show_flow_direction = TRUE,       # Show directional arrows
  edge_curve = 0.1,                # Edge curvature
  accessibility_mode = FALSE
)
```

#### `plot_transitions_hierarchical()`
Creates hierarchical layouts showing employment progression.

```r
# Sugiyama hierarchical layout
plot_transitions_hierarchical(
  transitions_data = transitions,
  hierarchy_type = "sugiyama",      # Sugiyama algorithm
  layout_direction = "vertical",    # Top-to-bottom layout
  node_color_var = "level",         # Color by hierarchy level
  show_levels = TRUE,               # Highlight hierarchy levels
  show_node_labels = TRUE
)
```

### Analysis and Utility Functions

#### `analyze_transitions_network()`
Provides comprehensive network analysis including centrality measures and community detection.

```r
# Network structure analysis
network_analysis <- analyze_transitions_network(
  transitions_data = transitions,
  directed = TRUE,
  compute_communities = TRUE
)

# Returns: centrality measures, community structure, network statistics
```

#### `create_accessibility_report()`
Generates accessibility compliance reports with recommendations.

```r
# Accessibility assessment
report <- create_accessibility_report(
  transitions_data = transitions,
  layout = "fr",
  palette = "viridis"
)

# Returns: WCAG compliance score, issues, recommendations
```

## Layout Algorithm Guide

| Layout | Best For | Characteristics | Max Nodes |
|--------|----------|-----------------|-----------|
| **Fruchterman-Reingold ("fr")** | General exploration | Natural clustering, balanced spacing | 50 |
| **Kamada-Kawai ("kk")** | Distance preservation | Preserves graph distances accurately | 30 |
| **Circular ("circle")** | Equal node emphasis | Nodes arranged in circle, clear comparison | 20 |
| **Sugiyama** | Directed flows | Hierarchical levels, career progression | 80 |
| **Tree** | Hierarchical data | Clear parent-child relationships | 100 |

## Color Palette Guide

### Accessibility-Optimized Palettes

1. **Viridis** (Recommended)
   - Perceptually uniform
   - Colorblind-safe for all types
   - High contrast ratios

2. **Okabe-Ito** 
   - Designed specifically for colorblindness
   - 8 distinct colors
   - Excellent for categorical data

3. **ColorBrewer Set2/Set3**
   - Tested for print and screen
   - Good for qualitative data
   - WCAG AA compliant

### Usage Guidelines

```r
# For maximum accessibility
plot_transitions_network(transitions, 
                        palette = "viridis", 
                        accessibility_mode = TRUE,
                        use_bw = TRUE)

# For publication (color)
plot_transitions_network(transitions,
                        palette = "okabe_ito",
                        accessibility_mode = FALSE)

# For presentation
plot_transitions_network(transitions,
                        palette = "viridis",
                        node_size_range = c(4, 16),  # Larger nodes
                        label_size = 4)              # Larger labels
```

## Integration with Vecshift Ecosystem

### Seamless Workflow Integration

```r
# Complete workflow example
# 1. Generate/load employment data
sample_data <- create_sample_employment_data(n_people = 30)

# 2. Process through vecshift pipeline  
pipeline_result <- process_employment_pipeline(
  sample_data,
  merge_columns = c("prior"),
  classify_status = TRUE
)

# 3. Analyze transitions
transitions <- analyze_employment_transitions(
  pipeline_result,
  transition_variable = "stato",
  min_unemployment_duration = 7
)

# 4. Create visualizations
network_plot <- plot_transitions_network(transitions)
heatmap_plot <- plot_transitions_heatmap(transitions)
```

### Theme Integration

The functions fully integrate with the existing vecshift theme system:

```r
# Use vecshift theme globally
set_vecshift_theme(base_size = 14, grid = "major")

# All ggraph functions will use vecshift theme
plot <- plot_transitions_network(transitions)

# Reset if needed
reset_default_theme()
```

## Example Use Cases

### 1. Exploratory Data Analysis
```r
# Quick exploration with recommendations
recommendations <- get_visualization_recommendations(
  transitions, 
  use_case = "exploration"
)

print(recommendations$recommended_plot)
```

### 2. Publication-Ready Figures
```r
# High-quality figure for academic publication
publication_plot <- plot_transitions_hierarchical(
  transitions,
  hierarchy_type = "sugiyama", 
  node_color_var = "level",
  palette = "viridis",
  accessibility_mode = FALSE,
  title = "Employment State Transitions",
  subtitle = "Hierarchical analysis of career progression patterns"
)

# Save high-resolution version
ggsave("employment_transitions.png", publication_plot,
       width = 10, height = 8, dpi = 300, bg = "white")
```

### 3. Accessibility-Compliant Dashboards
```r
# Dashboard with full accessibility
accessible_dashboard <- demonstrate_accessibility_features(transitions)

# Check compliance
accessibility_report <- create_accessibility_report(
  transitions, 
  palette = "viridis"
)

# Should achieve >80% accessibility score
```

### 4. Comparative Analysis
```r
# Compare different layouts
layout_comparison <- compare_layout_algorithms(
  transitions,
  layouts = c("fr", "kk", "circle", "sugiyama")
)

# Display side by side or create grid
gridExtra::grid.arrange(grobs = layout_comparison, ncol = 2)
```

## Technical Implementation

### Dependencies
- **ggraph (≥ 2.0.0)**: Network visualization grammar
- **tidygraph (≥ 1.2.0)**: Tidy graph data manipulation
- **igraph (≥ 1.2.0)**: Graph algorithms and metrics
- **ggplot2 (≥ 3.3.0)**: Base plotting system
- **ggrepel**: Label positioning (optional)
- **viridis**: Colorblind-friendly palettes (optional)

### Performance Considerations
- **Small networks** (<20 nodes): All layouts perform well
- **Medium networks** (20-50 nodes): Prefer circular or hierarchical layouts
- **Large networks** (>50 nodes): Use heatmaps or filter data
- **Dense networks** (density >0.3): Consider heatmap visualization

### Error Handling
- Comprehensive input validation
- Graceful degradation for missing packages
- Informative error messages with suggestions
- Fallback options for unsupported features

## Future Enhancements

### Planned Features
1. **Interactive versions** using plotly/htmlwidgets
2. **Animation support** for temporal transitions
3. **Export utilities** for various formats (SVG, PDF, etc.)
4. **Custom layout algorithms** specific to employment data
5. **Advanced community detection** methods
6. **Multi-level network analysis** for complex hierarchies

### Extensibility
The modular design allows easy extension:
- Custom layout algorithms
- Additional centrality measures
- New accessibility features
- Integration with other R packages

## Getting Started

### Installation
The functions are included in the vecshift package. Ensure you have the required dependencies:

```r
# Install vecshift if not already installed
# install.packages("vecshift")

# Install optional dependencies for full functionality
install.packages(c("ggraph", "tidygraph", "igraph", "viridis", "ggrepel"))
```

### Quick Start
```r
library(vecshift)

# Run the demo script for comprehensive examples
source("demo_ggraph_transitions.R")

# Or create a simple example
examples <- create_transitions_visualization_examples()
print(examples$network)
```

### Best Practices
1. **Start with recommendations**: Use `get_visualization_recommendations()`
2. **Test accessibility**: Always check with `accessibility_mode = TRUE`
3. **Consider your audience**: Choose appropriate complexity level
4. **Use consistent themes**: Set vecshift theme for brand consistency
5. **Export high-quality**: Use 300 DPI for publications

This implementation provides a comprehensive, accessible, and professional solution for visualizing employment transitions using modern network visualization techniques while maintaining full integration with the vecshift ecosystem.