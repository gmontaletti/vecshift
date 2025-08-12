#' Comprehensive g6r Employment Transitions Example
#' 
#' @description
#' Complete example demonstrating all major features of the g6r integration
#' with vecshift for interactive employment transition analysis.
#' 
#' This script walks through:
#' 1. Data generation and processing
#' 2. Basic interactive visualizations
#' 3. Advanced customization options
#' 4. Accessibility features
#' 5. Comparison and time-based analysis
#' 6. Shiny dashboard integration
#' 7. Performance optimization techniques
#' 
#' @author Giampaolo Montaletti

# Required packages
required_packages <- c("vecshift", "g6R", "data.table", "shiny", "viridis", "DT")

# Check and load packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, "...")
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Load the g6r integration functions
source("R/interactive_transitions_g6r.R")

cat("=== G6R Employment Transitions Comprehensive Example ===\n\n")

# ============================================================================
# SECTION 1: DATA GENERATION AND PROCESSING
# ============================================================================

cat("1. Generating comprehensive employment dataset...\n")

# Create realistic employment data with multiple characteristics
employment_data <- generate_g6r_demo_data(
  n_persons = 100,           # Moderate size for demonstration
  n_companies = 15,          # Good variety of companies
  time_span_years = 3,       # 3-year period
  transition_probability = 0.35,  # Moderate job mobility
  seed = 123                 # Reproducible results
)

cat("   Generated", nrow(employment_data), "employment records for", 
    length(unique(employment_data$cf)), "persons\n")

# Process through vecshift pipeline with comprehensive column merging
cat("2. Processing data through vecshift pipeline...\n")

pipeline_result <- process_employment_pipeline(
  original_data = employment_data,
  merge_columns = c("company", "sector", "employment_type", "salary", 
                   "region", "skill_level", "career_mobility"),
  show_progress = FALSE
)

cat("   Pipeline result:", nrow(pipeline_result), "processed periods\n")

# ============================================================================
# SECTION 2: BASIC INTERACTIVE VISUALIZATIONS
# ============================================================================

cat("\n3. Creating basic interactive visualizations...\n")

# Analyze company transitions
company_transitions <- analyze_employment_transitions(
  pipeline_result = pipeline_result,
  transition_variable = "company",
  statistics_variables = c("salary", "sector", "skill_level"),
  min_unemployment_duration = 7,
  show_progress = FALSE
)

cat("   Found", nrow(company_transitions), "unique company transition patterns\n")

if (interactive()) {
  cat("   Displaying basic force-directed network...\n")
  
  # Basic interactive visualization
  basic_plot <- plot_interactive_transitions(
    transitions_data = company_transitions,
    layout = "force",
    height = "600px"
  )
  
  print(basic_plot)
  readline("Press Enter to continue to layout comparisons...")
}

# ============================================================================
# SECTION 3: LAYOUT COMPARISON
# ============================================================================

cat("\n4. Demonstrating different layout algorithms...\n")

layouts_to_test <- c("force", "circular", "dagre", "radial", "concentric")
layout_plots <- list()

for (layout in layouts_to_test) {
  cat("   Creating", layout, "layout...\n")
  
  layout_plots[[layout]] <- plot_interactive_transitions(
    transitions_data = company_transitions,
    layout = layout,
    min_weight_threshold = 2,  # Focus on significant transitions
    height = "500px"
  )
}

if (interactive()) {
  cat("   Displaying layout comparison...\n")
  
  # Show different layouts (user can switch between them)
  for (layout in names(layout_plots)) {
    cat("   Showing", layout, "layout...\n")
    print(layout_plots[[layout]])
    readline(paste("Press Enter to see next layout (", layout, ")..."))
  }
}

# ============================================================================
# SECTION 4: SECTOR ANALYSIS
# ============================================================================

cat("\n5. Analyzing employment sector transitions...\n")

# Analyze sector mobility patterns
sector_transitions <- analyze_employment_transitions(
  pipeline_result = pipeline_result,
  transition_variable = "sector",
  statistics_variables = c("salary", "company", "skill_level"),
  min_unemployment_duration = 14,  # Focus on longer unemployment periods
  show_progress = FALSE
)

cat("   Found", nrow(sector_transitions), "unique sector transition patterns\n")

if (interactive()) {
  # Sector visualization with hierarchical layout (good for showing flows)
  cat("   Displaying sector transitions with hierarchical layout...\n")
  
  sector_plot <- plot_interactive_transitions(
    transitions_data = sector_transitions,
    layout = "dagre",           # Hierarchical layout for clear flows
    node_size_metric = "total_degree",
    edge_width_metric = "weight",
    show_tooltip = TRUE,
    height = "600px"
  )
  
  print(sector_plot)
  readline("Press Enter to continue to accessibility features...")
}

# ============================================================================
# SECTION 5: ACCESSIBILITY FEATURES
# ============================================================================

cat("\n6. Demonstrating accessibility features...\n")

# Test accessibility modes
if (interactive()) {
  cat("   Testing colorblind-friendly visualization...\n")
  
  accessible_plot <- plot_interactive_transitions(
    transitions_data = company_transitions,
    layout = "circular",
    accessibility_mode = TRUE,
    # Use high-contrast colorblind-friendly palette
    node_color_palette = c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                          "#CC79A7", "#F0E442", "#0072B2", "#D55E00"),
    show_labels = TRUE,
    height = "600px"
  )
  
  print(accessible_plot)
  readline("Press Enter to see accessibility test results...")
}

# Run comprehensive accessibility tests
accessibility_results <- test_g6r_accessibility(
  transitions_data = company_transitions,
  test_scenarios = "all"
)

cat("   Created", length(accessibility_results), "accessibility test visualizations:\n")
for (test_name in names(accessibility_results)) {
  cat("     -", test_name, "\n")
}

if (interactive()) {
  for (test_name in names(accessibility_results)) {
    cat("   Showing", test_name, "accessibility test...\n")
    print(accessibility_results[[test_name]])
    readline(paste("Press Enter to see next test (", test_name, ")..."))
  }
}

# ============================================================================
# SECTION 6: ADVANCED CUSTOMIZATION
# ============================================================================

cat("\n7. Advanced customization examples...\n")

# Custom visualization with specific focus
if (interactive()) {
  cat("   Creating custom visualization focusing on high-value transitions...\n")
  
  custom_plot <- plot_interactive_transitions(
    transitions_data = company_transitions,
    layout = "radial",                    # Emphasize central nodes
    node_size_metric = "in_degree",       # Size by incoming transitions
    edge_width_metric = "transition_duration",  # Width by unemployment duration
    min_weight_threshold = 3,             # Focus on frequent transitions
    edge_bundling = TRUE,                 # Clean edge visualization
    show_minimap = TRUE,                  # Navigation aid
    animation_duration = 1500,            # Smooth animations
    height = "700px"
  )
  
  print(custom_plot)
  readline("Press Enter to continue to group comparisons...")
}

# ============================================================================
# SECTION 7: GROUP COMPARISONS
# ============================================================================

cat("\n8. Creating group comparison visualizations...\n")

# Compare transitions by skill level
entry_level_data <- employment_data[skill_level == "Entry"]
senior_level_data <- employment_data[skill_level == "Senior"]

# Process each group separately
cat("   Processing entry-level employment data...\n")
entry_pipeline <- process_employment_pipeline(
  entry_level_data, 
  merge_columns = c("company", "sector", "salary"),
  show_progress = FALSE
)

cat("   Processing senior-level employment data...\n")
senior_pipeline <- process_employment_pipeline(
  senior_level_data,
  merge_columns = c("company", "sector", "salary"),
  show_progress = FALSE
)

# Analyze transitions for each group
entry_transitions <- analyze_employment_transitions(
  entry_pipeline, 
  transition_variable = "company",
  show_progress = FALSE
)

senior_transitions <- analyze_employment_transitions(
  senior_pipeline,
  transition_variable = "company", 
  show_progress = FALSE
)

cat("   Entry level:", nrow(entry_transitions), "transition patterns\n")
cat("   Senior level:", nrow(senior_transitions), "transition patterns\n")

# Create comparison visualizations
skill_comparison <- compare_transitions_between_groups(
  transitions_list = list(
    "Entry Level" = entry_transitions,
    "Senior Level" = senior_transitions
  ),
  layout = "circular",
  min_weight_threshold = 1
)

if (interactive()) {
  cat("   Displaying skill level comparison...\n")
  
  cat("   Entry Level transitions:\n")
  print(skill_comparison$`Entry Level`)
  readline("Press Enter to see Senior Level transitions...")
  
  cat("   Senior Level transitions:\n")
  print(skill_comparison$`Senior Level`)
  readline("Press Enter to continue to performance testing...")
}

# ============================================================================
# SECTION 8: PERFORMANCE TESTING
# ============================================================================

cat("\n9. Performance testing with larger datasets...\n")

# Generate larger dataset for performance testing
cat("   Generating large dataset for performance testing...\n")
large_data <- generate_g6r_demo_data(
  n_persons = 300,
  n_companies = 25,
  time_span_years = 2,
  transition_probability = 0.4,
  seed = 456
)

cat("   Large dataset:", nrow(large_data), "records for", 
    length(unique(large_data$cf)), "persons\n")

# Process large dataset
cat("   Processing large dataset...\n")
start_time <- Sys.time()

large_pipeline <- process_employment_pipeline(
  large_data,
  merge_columns = c("company", "sector", "employment_type", "salary"),
  show_progress = FALSE
)

processing_time <- difftime(Sys.time(), start_time, units = "secs")
cat("   Processing completed in", round(processing_time, 2), "seconds\n")

# Analyze transitions with performance optimization
start_time <- Sys.time()

large_transitions <- analyze_employment_transitions(
  large_pipeline,
  transition_variable = "company",
  min_unemployment_duration = 7,
  show_progress = FALSE
)

analysis_time <- difftime(Sys.time(), start_time, units = "secs")
cat("   Transition analysis completed in", round(analysis_time, 2), "seconds\n")
cat("   Found", nrow(large_transitions), "transition patterns\n")

if (interactive()) {
  cat("   Creating performance-optimized visualization...\n")
  
  # Performance-optimized visualization
  start_time <- Sys.time()
  
  performance_plot <- plot_interactive_transitions(
    transitions_data = large_transitions,
    layout = "circular",              # Fast layout
    min_weight_threshold = 5,         # Focus on major transitions
    edge_bundling = TRUE,             # Reduce complexity
    show_minimap = FALSE,             # Reduce rendering
    animation_duration = 0,           # No animations for speed
    height = "600px"
  )
  
  viz_time <- difftime(Sys.time(), start_time, units = "secs")
  cat("   Visualization created in", round(viz_time, 2), "seconds\n")
  
  print(performance_plot)
  readline("Press Enter to continue to matrix visualization...")
}

# ============================================================================
# SECTION 9: MATRIX VISUALIZATION
# ============================================================================

cat("\n10. Creating transition matrix visualizations...\n")

# Create transition matrix for circular visualization
transition_matrix <- analyze_employment_transitions(
  pipeline_result = pipeline_result,
  transition_variable = "sector",  # Use sector for cleaner matrix
  output_transition_matrix = TRUE,
  show_progress = FALSE
)

cat("   Created", nrow(transition_matrix), "x", ncol(transition_matrix), "transition matrix\n")

if (interactive()) {
  cat("   Displaying matrix as circular network...\n")
  
  matrix_plot <- plot_interactive_transitions(
    transitions_data = transition_matrix,
    layout = "circular",
    min_weight_threshold = 2,
    show_labels = TRUE,
    height = "600px"
  )
  
  print(matrix_plot)
  readline("Press Enter to continue to Shiny dashboard demo...")
}

# ============================================================================
# SECTION 10: SHINY INTEGRATION DEMO
# ============================================================================

cat("\n11. Shiny integration demonstration...\n")

if (interactive()) {
  cat("   Would you like to launch the interactive Shiny dashboard? (y/n): ")
  response <- readline()
  
  if (tolower(substr(response, 1, 1)) == "y") {
    cat("   Launching employment transitions dashboard...\n")
    cat("   This will open in your web browser.\n")
    cat("   Close the browser tab or press Ctrl+C to return to this script.\n")
    
    try({
      run_employment_dashboard()
    }, silent = TRUE)
    
    cat("   Dashboard closed.\n")
  } else {
    cat("   Skipping dashboard launch.\n")
  }
}

# ============================================================================
# SECTION 11: DATA EXPORT EXAMPLES
# ============================================================================

cat("\n12. Data export examples...\n")

# Convert data to g6r format for inspection
g6_data <- convert_transitions_to_g6r(
  transitions_data = company_transitions,
  min_weight_threshold = 2
)

cat("   Converted to g6r format:\n")
cat("     Nodes:", nrow(g6_data$nodes), "\n")
cat("     Edges:", nrow(g6_data$edges), "\n")

# Show data structure
cat("   Node data structure:\n")
print(head(g6_data$nodes, 3))

cat("   Edge data structure:\n")
print(head(g6_data$edges, 3))

# Export data (optional)
if (interactive()) {
  cat("   Export data to CSV files? (y/n): ")
  response <- readline()
  
  if (tolower(substr(response, 1, 1)) == "y") {
    write.csv(g6_data$nodes, "employment_transitions_nodes.csv", row.names = FALSE)
    write.csv(g6_data$edges, "employment_transitions_edges.csv", row.names = FALSE)
    write.csv(employment_data, "original_employment_data.csv", row.names = FALSE)
    
    cat("   Data exported to CSV files:\n")
    cat("     - employment_transitions_nodes.csv\n")
    cat("     - employment_transitions_edges.csv\n")
    cat("     - original_employment_data.csv\n")
  }
}

# ============================================================================
# SECTION 12: SUMMARY AND RECOMMENDATIONS
# ============================================================================

cat("\n13. Summary and recommendations...\n")

# Calculate summary statistics
total_persons <- length(unique(employment_data$cf))
total_records <- nrow(employment_data)
total_transitions <- sum(company_transitions$weight)
unique_companies <- length(unique(employment_data$company))
unique_sectors <- length(unique(employment_data$sector))

cat("   Dataset Summary:\n")
cat("     - Total persons:", total_persons, "\n")
cat("     - Total employment records:", total_records, "\n")
cat("     - Total transitions analyzed:", total_transitions, "\n")
cat("     - Unique companies:", unique_companies, "\n")
cat("     - Unique sectors:", unique_sectors, "\n")

# Performance recommendations based on data size
cat("\n   Performance Recommendations:\n")
if (total_transitions < 50) {
  cat("     - Dataset size: SMALL - All layouts and features recommended\n")
  cat("     - Suggested layout: force (for natural clustering)\n")
  cat("     - Enable: all plugins, animations, minimap\n")
} else if (total_transitions < 200) {
  cat("     - Dataset size: MEDIUM - Most layouts suitable\n")
  cat("     - Suggested layout: circular or force\n") 
  cat("     - Enable: most plugins, moderate animations\n")
} else {
  cat("     - Dataset size: LARGE - Use performance optimizations\n")
  cat("     - Suggested layout: circular or grid\n")
  cat("     - Disable: animations, some plugins\n")
  cat("     - Enable: edge bundling, higher weight thresholds\n")
}

cat("\n   Accessibility Recommendations:\n")
cat("     - Always use colorblind-friendly palettes\n")
cat("     - Enable redundant encoding (size + color)\n")
cat("     - Test with accessibility_mode = TRUE\n")
cat("     - Provide alternative text descriptions\n")

cat("\n   Next Steps:\n")
cat("     1. Explore the Shiny dashboard for interactive analysis\n")
cat("     2. Customize layouts and styling for your specific use case\n")
cat("     3. Integrate with your existing data pipelines\n")
cat("     4. Consider performance optimizations for production use\n")
cat("     5. Test accessibility features with your target audience\n")

cat("\n=== G6R Employment Transitions Example Completed ===\n")
cat("For more information, see:\n")
cat("- Vignette: vignette('g6r-interactive-transitions', package = 'vecshift')\n")
cat("- Dashboard example: run_employment_dashboard()\n")
cat("- Documentation: help('plot_interactive_transitions')\n\n")

# Clean up temporary variables
rm(list = ls()[!ls() %in% c("employment_data", "pipeline_result", 
                           "company_transitions", "g6_data")])

cat("Core objects retained in environment:\n")
cat("- employment_data: Original employment records\n")
cat("- pipeline_result: Processed vecshift output\n") 
cat("- company_transitions: Transition analysis results\n")
cat("- g6_data: g6r-formatted network data\n")