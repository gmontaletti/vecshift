#!/usr/bin/env Rscript
#
# Demo: Black & White Theme Improvements
# =====================================
# 
# This script demonstrates the improved black and white visualization capabilities
# of the vecshift package, including:
# - Lighter, more readable grayscale palette
# - Pattern specifications for fills
# - Line type patterns for time series
# - Enhanced accessibility features

# Load required functions
source("R/theme_vecshift.R")

# If ggplot2 is available, create visualizations
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  
  cat("=== Vecshift B&W Theme Improvements Demo ===\n\n")
  
  # 1. Compare old vs new B&W palettes
  cat("1. Palette Comparison:\n")
  old_bw <- c("#000000", "#2C2C2C", "#4A4A4A", "#6E6E6E", "#8C8C8C")
  new_bw <- vecshift_colors("main_bw", n = 5)
  
  cat("Old B&W palette:", paste(old_bw, collapse = " "), "\n")
  cat("New B&W palette:", paste(new_bw, collapse = " "), "\n\n")
  
  # 2. Test pattern specifications
  cat("2. Pattern Specifications:\n")
  emp_patterns <- vecshift_patterns("employment")
  cat("Employment patterns available:", length(emp_patterns), "\n")
  for (name in names(emp_patterns)) {
    pattern <- emp_patterns[[name]]
    cat(sprintf("  %s: %s (%s)\n", name, pattern$pattern, pattern$description))
  }
  cat("\n")
  
  # 3. Test line type specifications
  cat("3. Line Type Specifications:\n")
  line_types <- vecshift_linetypes(c("occ_ft", "occ_pt", "disoccupato"), style = "employment")
  for (name in names(line_types)) {
    cat(sprintf("  %s: %s\n", name, line_types[[name]]))
  }
  cat("\n")
  
  # 4. Create sample data for visualization
  cat("4. Creating Sample Visualizations...\n")
  
  # Sample employment data
  sample_data <- data.frame(
    month = rep(1:6, 3),
    employment_status = rep(c("occ_ft", "occ_pt", "disoccupato"), each = 6),
    count = c(
      c(850, 870, 860, 880, 875, 890),  # Full-time
      c(320, 310, 340, 330, 325, 335),  # Part-time
      c(180, 175, 185, 170, 165, 172)   # Unemployed
    )
  )
  
  # Bar chart with improved B&W palette
  p1 <- ggplot(sample_data, aes(x = factor(month), y = count, fill = employment_status)) +
    geom_col(position = "dodge", color = "black", size = 0.3, alpha = 0.8) +
    scale_fill_vecshift_bw("employment") +
    labs(
      title = "Employment by Month - Improved B&W Theme",
      subtitle = "Using lighter grays with solid borders for better distinction",
      x = "Month", y = "Count", fill = "Employment Status"
    ) +
    theme_vecshift() +
    theme(legend.position = "bottom")
  
  # Save the plot
  ggsave("vecshift_bw_demo_bars.png", p1, width = 10, height = 6, dpi = 300)
  
  # Time series with line patterns
  p2 <- ggplot(sample_data, aes(x = month, y = count, color = employment_status, 
                               linetype = employment_status)) +
    geom_line(size = 1.2) +
    geom_point(size = 2.5, alpha = 0.8) +
    scale_color_vecshift("main_bw") +
    scale_linetype_manual(values = line_types) +
    labs(
      title = "Employment Trends with Distinct Line Patterns",
      subtitle = "Combining color and line type for maximum accessibility",
      x = "Month", y = "Count",
      color = "Employment Status", linetype = "Employment Status"
    ) +
    theme_vecshift(grid = "major") +
    theme(legend.position = "bottom")
  
  # Save the plot
  ggsave("vecshift_bw_demo_lines.png", p2, width = 10, height = 6, dpi = 300)
  
  cat("Sample plots saved as vecshift_bw_demo_bars.png and vecshift_bw_demo_lines.png\n\n")
  
  # 5. Test accessibility
  cat("5. Accessibility Testing:\n")
  cat("Testing new main_bw palette:\n")
  test_vecshift_accessibility("main_bw")
  
} else {
  cat("ggplot2 not available - testing core functions only\n\n")
  
  # Test core functions without visualization
  cat("1. New B&W Colors:\n")
  new_bw <- vecshift_colors("main_bw", n = 5)
  print(new_bw)
  
  cat("\n2. Employment Patterns:\n")
  patterns <- vecshift_patterns("employment")
  print(names(patterns))
  
  cat("\n3. Line Types:\n")
  lines <- vecshift_linetypes(c("occ_ft", "occ_pt", "disoccupato"))
  print(lines)
}

cat("\n=== Demo Complete ===\n")
cat("Key improvements:\n")
cat("• Lighter, more readable grayscale values\n")
cat("• Pattern specifications for visual distinction\n")
cat("• Line type patterns for time series\n")
cat("• Enhanced accessibility with WCAG compliance\n")
cat("• Solid borders for improved pattern visibility\n")