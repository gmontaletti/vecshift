# g6r Quick Reference Guide

## Installation
```r
install.packages("g6R")  # Required for interactive features
library(vecshift)
library(g6R)
```

## Basic Usage

### 1. Data to Visualization Pipeline
```r
# Generate or load employment data
data <- generate_g6r_demo_data(n_persons = 50, n_companies = 8)

# Process through vecshift
result <- process_employment_pipeline(data, merge_columns = "company")

# Analyze transitions
transitions <- analyze_employment_transitions(result, transition_variable = "company")

# Create interactive visualization
plot_interactive_transitions(transitions, layout = "force")
```

### 2. Layout Options
| Layout | Best For | Performance |
|--------|----------|-------------|
| `"force"` | Natural clustering | Medium |
| `"circular"` | Equal emphasis | High |
| `"dagre"` | Directional flows | High |
| `"radial"` | Central hubs | High |
| `"concentric"` | Importance levels | High |

### 3. Customization Parameters
```r
plot_interactive_transitions(
  transitions,
  layout = "circular",
  node_size_metric = "total_degree",        # "in_degree", "out_degree", "weight"
  edge_width_metric = "weight",            # "transition_duration", custom
  min_weight_threshold = 3,                # Filter rare transitions
  accessibility_mode = TRUE,               # High contrast colors
  edge_bundling = TRUE,                    # Clean dense networks
  show_minimap = TRUE,                     # Navigation aid
  height = "600px"
)
```

## Accessibility
```r
# Colorblind-friendly visualization
plot_interactive_transitions(
  transitions,
  accessibility_mode = TRUE,
  node_color_palette = c("#000000", "#E69F00", "#56B4E9", "#009E73")
)

# Test accessibility features
test_g6r_accessibility(transitions, test_scenarios = "all")
```

## Shiny Integration
```r
# Launch full dashboard
run_employment_dashboard()

# Use module in custom app
ui <- fluidPage(
  interactive_transitions_module()$ui("transitions")
)

server <- function(input, output, session) {
  data <- reactive({ analyze_employment_transitions(your_data) })
  interactive_transitions_module()$server("transitions", data)
}

shinyApp(ui, server)
```

## Performance Optimization
```r
# For large datasets
plot_interactive_transitions(
  large_transitions,
  layout = "circular",              # Faster than force
  min_weight_threshold = 5,         # Focus on major transitions
  edge_bundling = TRUE,             # Reduce visual complexity
  show_minimap = FALSE,             # Reduce rendering load
  animation_duration = 0            # Disable animations
)
```

## Data Export
```r
# Convert to g6r format for inspection
g6_data <- convert_transitions_to_g6r(transitions)

# Export data
write.csv(g6_data$nodes, "nodes.csv", row.names = FALSE)
write.csv(g6_data$edges, "edges.csv", row.names = FALSE)
```

## Common Patterns

### Group Comparison
```r
# Compare regions/demographics
comparison <- compare_transitions_between_groups(
  list("North" = north_transitions, "South" = south_transitions),
  layout = "circular"
)
```

### Matrix Visualization
```r
# Create transition matrix
matrix_data <- analyze_employment_transitions(
  result, 
  transition_variable = "company",
  output_transition_matrix = TRUE
)

plot_interactive_transitions(matrix_data, layout = "circular")
```

### Integration with igraph
```r
# Convert for network analysis
library(igraph)
g6_data <- convert_transitions_to_g6r(transitions)
network <- graph_from_data_frame(g6_data$edges, vertices = g6_data$nodes, directed = TRUE)
```

## Troubleshooting

### Empty Visualization
```r
# Check data has transitions
if (nrow(transitions) == 0) {
  message("No transitions found - check min_unemployment_duration parameter")
}
```

### Performance Issues
```r
# Optimize for large data
transitions_filtered <- transitions[weight >= 3]
# Use "circular" or "grid" layouts
# Disable animations and some plugins
```

### Package Not Found
```r
if (!requireNamespace("g6R", quietly = TRUE)) {
  install.packages("g6R")
}
```

## Key Functions Quick Reference

| Function | Purpose |
|----------|---------|
| `plot_interactive_transitions()` | Main visualization function |
| `convert_transitions_to_g6r()` | Data format conversion |
| `generate_g6r_demo_data()` | Create sample data |
| `interactive_transitions_module()` | Shiny module |
| `run_employment_dashboard()` | Launch full dashboard |
| `test_g6r_accessibility()` | Accessibility testing |
| `compare_transitions_between_groups()` | Group comparisons |

## Resources
- Comprehensive guide: `README_g6r_integration.md`
- Full example: `inst/examples/g6r_comprehensive_example.R`  
- Vignette: `vignette("g6r-interactive-transitions")`
- g6R documentation: https://cynkra.github.io/g6R/