# Demonstration Script for Ggraph-Based Transition Visualizations
# This script shows how to use the new ggraph network visualization functions
# for employment transitions analysis in the vecshift package

# Load required libraries
library(vecshift)  # Make sure vecshift package is installed
library(data.table)

# Optional: Set vecshift theme as default
# set_vecshift_theme(base_size = 12)

# Step 1: Generate sample employment data =====================================
cat("Step 1: Generating sample employment data...\n")

# Create synthetic employment data with various patterns
sample_data <- create_sample_employment_data(
  n_people = 100,
  n_periods = 4,
  date_range = c("2022-01-01", "2024-12-31"),
  include_overlaps = TRUE,
  include_gaps = TRUE,
  seed = 123
)



cat("Generated data for", length(unique(sample_data$cf)), "people with",
    nrow(sample_data), "employment periods\n")

# Step 2: Process data through vecshift pipeline =============================
cat("Step 2: Processing data through vecshift pipeline...\n")

# Apply vecshift transformation with status classification
pipeline_result <- process_employment_pipeline(
  original_data = sample_data,
  merge_columns = c("prior"),
  classify_status = TRUE,
  show_progress = FALSE
)

cat("Pipeline result:", nrow(pipeline_result), "temporal segments\n")

# Step 3: Analyze employment transitions =====================================
cat("Step 3: Analyzing employment transitions...\n")

# Analyze transitions between employment states
transitions <- analyze_employment_transitions(
  pipeline_result = pipeline_result,
  transition_variable = "stato",  # Employment status
  min_unemployment_duration = 7,  # At least 1 week of unemployment
  show_progress = FALSE
)

cat("Found", nrow(transitions), "unique transition patterns\n")
print(head(transitions))

# Step 4: Create network visualizations ======================================
cat("\nStep 4: Creating network visualizations...\n")

# 4.1 Basic network visualization (Fruchterman-Reingold layout)
cat("  4.1 Creating basic network visualization...\n")

network_plot <- plot_transitions_network(
  transitions_data = transitions,
  layout = "fr",
  node_size_var = "strength",
  edge_width_var = "weight",
  node_color_var = "community",
  palette = "viridis",
  show_labels = TRUE,
  title = "Employment Transitions Network",
  subtitle = "Fruchterman-Reingold layout showing employment state relationships"
)

print(network_plot)

# 4.2 Alternative layout (Kamada-Kawai)
cat("  4.2 Creating Kamada-Kawai layout...\n")

network_kk <- plot_transitions_network(
  transitions_data = transitions,
  layout = "kk",
  node_size_var = "degree",
  edge_width_var = "weight",
  node_color_var = "community",
  palette = "okabe_ito",
  show_labels = TRUE,
  title = "Employment Transitions Network (Kamada-Kawai)",
  subtitle = "Layout optimized for distance preservation"
)

print(network_kk)

# Step 5: Create heatmap visualizations =====================================
cat("\nStep 5: Creating heatmap visualizations...\n")

# 5.1 Basic heatmap with counts and percentages
heatmap_plot <- plot_transitions_heatmap(
  transitions_data = transitions,
  cell_value = "both",
  normalize = "row",
  palette = "viridis",
  title = "Employment Transitions Heatmap",
  subtitle = "Row-normalized probabilities with counts and percentages"
)

print(heatmap_plot)

# 5.2 Raw counts heatmap
heatmap_raw <- plot_transitions_heatmap(
  transitions_data = transitions,
  cell_value = "weight",
  normalize = "none",
  color_scale = "gradient",
  palette = "viridis",
  title = "Employment Transitions Heatmap (Raw Counts)",
  subtitle = "Absolute transition frequencies"
)

print(heatmap_raw)

# Step 6: Create circular visualizations ====================================
cat("\nStep 6: Creating circular visualizations...\n")

# 6.1 Chord diagram
circular_plot <- plot_transitions_circular(
  transitions_data = transitions,
  circular_type = "chord",
  node_order = "frequency",
  show_flow_direction = TRUE,
  palette = "viridis",
  title = "Employment Transitions Chord Diagram",
  subtitle = "Circular layout emphasizing flow patterns"
)

print(circular_plot)

# 6.2 Arc diagram
arc_plot <- plot_transitions_circular(
  transitions_data = transitions,
  circular_type = "arc",
  node_order = "alphabetical",
  palette = "okabe_ito",
  title = "Employment Transitions Arc Diagram",
  subtitle = "Linear arrangement with arced connections"
)

print(arc_plot)

# Step 7: Create hierarchical visualizations ================================
cat("\nStep 7: Creating hierarchical visualizations...\n")

# 7.1 Sugiyama layout (directed hierarchy)
hierarchical_plot <- plot_transitions_hierarchical(
  transitions_data = transitions,
  hierarchy_type = "sugiyama",
  layout_direction = "vertical",
  node_color_var = "level",
  show_levels = TRUE,
  palette = "viridis",
  title = "Employment Transitions Hierarchy (Sugiyama)",
  subtitle = "Hierarchical layout showing employment progression"
)

print(hierarchical_plot)

# Step 8: Accessibility demonstrations ======================================
cat("\nStep 8: Demonstrating accessibility features...\n")

# 8.1 High-contrast network for accessibility
accessible_network <- plot_transitions_network(
  transitions_data = transitions,
  layout = "circle",
  accessibility_mode = TRUE,
  use_bw = TRUE,
  palette = "viridis",
  show_labels = TRUE,
  title = "Accessible Employment Transitions Network",
  subtitle = "High-contrast design optimized for colorblind accessibility"
)

print(accessible_network)

# 8.2 Create accessibility report
cat("  8.2 Generating accessibility report...\n")

accessibility_report <- create_accessibility_report(
  transitions_data = transitions,
  layout = "fr",
  palette = "viridis"
)

cat("Accessibility Score:", accessibility_report$accessibility_percentage, "%\n")
cat("Assessment:", accessibility_report$overall_assessment, "\n")

if (length(accessibility_report$recommendations) > 0) {
  cat("Recommendations:\n")
  for (rec in accessibility_report$recommendations) {
    cat("  -", rec, "\n")
  }
}

# Step 9: Network analysis ===================================================
cat("\nStep 9: Analyzing network structure...\n")

network_analysis <- analyze_transitions_network(
  transitions_data = transitions,
  directed = TRUE,
  compute_communities = TRUE
)

cat("Network Summary:\n")
cat("  - Nodes:", network_analysis$network_summary$nodes, "\n")
cat("  - Edges:", network_analysis$network_summary$edges, "\n")
cat("  - Density:", round(network_analysis$network_summary$density, 3), "\n")

if (!is.null(network_analysis$communities)) {
  cat("  - Communities:", network_analysis$communities$n_communities, "\n")
  cat("  - Modularity:", round(network_analysis$communities$modularity, 3), "\n")
}

cat("\nTop nodes by degree centrality:\n")
for (i in seq_along(network_analysis$centrality$degree$top_nodes)) {
  cat("  ", i, ".", network_analysis$centrality$degree$top_nodes[i], "\n")
}

# Step 10: Get visualization recommendations ================================
cat("\nStep 10: Getting visualization recommendations...\n")

recommendations <- get_visualization_recommendations(
  transitions_data = transitions,
  use_case = "exploration"
)

cat("Recommended visualization:", recommendations$recommended_visualization, "\n")
cat("Explanation:", recommendations$explanation, "\n")

# Display recommended plot
print(recommendations$recommended_plot)

# Step 11: Create comprehensive examples ====================================
cat("\nStep 11: Creating comprehensive visualization examples...\n")

# This creates all visualization types at once
all_examples <- create_transitions_visualization_examples(
  data = transitions,
  save_plots = FALSE  # Set to TRUE to save plots to disk
)

cat("Created", length(all_examples) - 1, "different visualizations\n")  # -1 for metadata
cat("Data summary:\n")
cat("  - Unique transitions:", all_examples$data_summary$n_transitions, "\n")
cat("  - Employment states:", all_examples$data_summary$n_states, "\n")
cat("  - Total weight:", all_examples$data_summary$total_weight, "\n")

# Display one final example
print(all_examples$network)

cat("\n=== Demonstration completed successfully! ===\n")
cat("You can now use these functions to create publication-ready\n")
cat("network visualizations of employment transitions with full\n")
cat("accessibility support and multiple layout options.\n")
