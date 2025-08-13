# Comprehensive Guide to ggraph Visualizations in vecshift

This guide provides a complete overview of the static network visualization functions available in the vecshift package using ggraph and tidygraph.

## Quick Reference

### Main Functions

| Function | Purpose | Best For |
|----------|---------|----------|
| `plot_transitions_network()` | General network layouts | Exploration, presentations |
| `plot_transitions_heatmap()` | Matrix visualizations | Quantitative analysis |
| `plot_transitions_circular()` | Circular/flow layouts | Flow patterns, relationships |
| `plot_transitions_hierarchical()` | Tree/hierarchy layouts | Career progression |

### Analysis Functions

| Function | Purpose | Output |
|----------|---------|---------|
| `analyze_transitions_network()` | Network metrics | List with centrality, community analysis |
| `create_accessibility_report()` | Accessibility assessment | Compliance score and recommendations |

### Example Functions

| Function | Purpose | Output |
|----------|---------|---------|
| `create_transitions_visualization_examples()` | Complete showcase | List of all visualization types |
| `demonstrate_accessibility_features()` | A11y comparisons | Standard vs accessible versions |
| `compare_layout_algorithms()` | Layout comparison | Multiple layouts for same data |
| `get_visualization_recommendations()` | Smart suggestions | Recommended plot based on data characteristics |

## Layout Algorithms Guide

### Force-Directed Layouts
- **`"fr"` (Fruchterman-Reingold)**: Natural clustering, good for medium networks
- **`"kk"` (Kamada-Kawai)**: Distance-preserving, ideal for smaller networks  
- **`"gem"` (GEM)**: Fast spring layout with good node separation
- **`"graphopt"`**: Emphasizes community structure

### Structured Layouts  
- **`"circle"`**: Equal emphasis on all nodes, good for comparisons
- **`"grid"`**: Systematic arrangement, good for ordered data
- **`"mds"`**: Multidimensional scaling, preserves relationships

### Specialized Layouts
- **Circular**: `plot_transitions_circular()` with `"chord"`, `"arc"`, `"wheel"`
- **Hierarchical**: `plot_transitions_hierarchical()` with `"sugiyama"`, `"tree"`, `"dendrogram"`

## Color Palettes

### Built-in Palettes
- **`"viridis"`**: Colorblind-friendly, perceptually uniform
- **`"okabe_ito"`**: Optimized for all colorblind types  
- **`"employment"`**: Vecshift employment-specific colors
- **`"main"`**: Vecshift main palette
- **`"colorbrewer_set2"`**: Qualitative, colorblind-safe

### Accessibility Options
- **`accessibility_mode = TRUE`**: High contrast mode
- **`use_bw = TRUE`**: Black and white version
- **`palette = "viridis"`**: Always colorblind-safe

## Node and Edge Customization

### Node Sizing Variables
- **`"degree"`**: Total number of connections
- **`"strength"`**: Weighted degree (connection strength)  
- **`"in_degree"`**: Incoming connections
- **`"out_degree"`**: Outgoing connections
- **`"betweenness"`**: Bridge positions in network
- **`"closeness"`**: Average distance to all other nodes
- **`"fixed"`**: All nodes same size

### Node Coloring Variables  
- **`"community"`**: Detected communities (Louvain algorithm)
- **`"degree"`**: Connection count (continuous scale)
- **`"status"`**: Employment status (if detectable from names)
- **`"level"`**: Hierarchy level (hierarchical layouts)
- **`"fixed"`**: Single color for all nodes

### Edge Styling
- **`edge_width_var = "weight"`**: Width by transition frequency
- **`edge_width_var = "transition_duration"`**: Width by unemployment duration
- **`show_edge_labels = TRUE`**: Display transition counts
- **`edge_alpha`**: Transparency (0-1)

## Data Format Support

### Data.table Format (Default)
```r
# Required columns: from, to, weight
transitions <- analyze_employment_transitions(pipeline_result, 
                                            transition_variable = "company")
plot_transitions_network(transitions)
```

### Matrix Format
```r  
# Adjacency matrix with row/column names as states
matrix <- analyze_employment_transitions(pipeline_result, 
                                       output_transition_matrix = TRUE)
plot_transitions_network(matrix, input_format = "matrix")
```

## Accessibility Features

### Automatic Accessibility
- All color palettes tested for colorblind compatibility
- High contrast mode available (`accessibility_mode = TRUE`)
- Black and white fallback (`use_bw = TRUE`)
- WCAG AA compliance checking (`create_accessibility_report()`)

### Manual Accessibility  
- Redundant encoding (size + color + shape)
- Clear labeling (`show_labels = TRUE`, `label_repel = TRUE`)
- Sufficient contrast ratios
- Alternative text in titles/subtitles

## Performance Guidelines

### Small Networks (≤10 nodes)
- Any layout works well
- Recommended: `"kk"` or `"circle"`
- Enable all features: labels, edge labels, etc.

### Medium Networks (10-25 nodes)
- Most layouts work
- Recommended: `"fr"` or `"circle"`  
- Consider `label_repel = TRUE`

### Large Networks (≥25 nodes)
- Choose carefully: `"circle"` or heatmap
- Filter data: `min_edge_weight >= 2`
- Disable edge labels: `show_edge_labels = FALSE`
- Consider `plot_transitions_heatmap()` instead

## Integration with Vecshift Pipeline

### Complete Workflow
```r
# 1. Process employment data
pipeline_result <- process_employment_pipeline(
  original_data = employment_data,
  merge_columns = c("company", "salary")
)

# 2. Analyze transitions
transitions <- analyze_employment_transitions(
  pipeline_result = pipeline_result,
  transition_variable = "company",
  statistics_variables = "salary"
)

# 3. Create visualization
network_plot <- plot_transitions_network(
  transitions_data = transitions,
  layout = "fr",
  palette = "employment"
)

# 4. Export for publication
ggsave("employment_network.png", network_plot, 
       width = 12, height = 8, dpi = 300)
```

### Smart Recommendations
```r
# Get automated recommendations based on data characteristics
recommendations <- get_visualization_recommendations(
  transitions_data = transitions,
  use_case = "publication"  # or "exploration", "presentation", "accessibility"
)

# Use recommended plot
print(recommendations$recommended_plot)

# View explanation
cat(recommendations$explanation)
```

## Export and Publication

### High-Resolution Export
```r
# PNG for presentations (300 DPI)
ggsave("network.png", plot, width = 12, height = 8, dpi = 300, bg = "white")

# PDF for publications (vector format)  
ggsave("network.pdf", plot, width = 12, height = 8, device = "pdf")

# SVG for web use
ggsave("network.svg", plot, width = 12, height = 8, device = "svg")
```

### Consistent Styling
All ggraph functions use vecshift's theme system:
- Consistent fonts and sizes
- Professional color palettes  
- Publication-ready defaults
- Accessibility compliance

## Advanced Customization

### ggplot2 Integration
```r
# All functions return ggplot objects, so you can customize further
plot <- plot_transitions_network(transitions) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom"
  ) +
  labs(caption = "Data source: Company HR records")
```

### Multi-panel Figures
```r
# Combine multiple visualizations
library(gridExtra)

network_plot <- plot_transitions_network(transitions, layout = "circle")
heatmap_plot <- plot_transitions_heatmap(transitions, normalize = "row")

combined <- grid.arrange(network_plot, heatmap_plot, ncol = 2)
```

## Troubleshooting

### Package Dependencies
Required packages will be automatically checked:
- `ggraph` (≥ 2.0.0)
- `tidygraph` (≥ 1.2.0) 
- `igraph` (≥ 1.2.0)
- `ggplot2` (≥ 3.3.0)

### Common Issues
1. **Empty plots**: Check `min_edge_weight` parameter
2. **Overlapping labels**: Use `label_repel = TRUE`  
3. **Performance issues**: Filter data or use simpler layouts
4. **Accessibility concerns**: Run `create_accessibility_report()`

### Error Messages
- "No nodes found": Increase `min_edge_weight` or check data
- "Package X required": Install missing dependencies
- "Empty transition matrix": Check analysis parameters

For more detailed examples and tutorials, see the package vignettes:
- "Static Network Visualizations with ggraph" 
- "Interactive Employment Transitions with g6r"
- "Vecshift Visualization Themes"