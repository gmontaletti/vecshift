# Vecshift Theme System Examples
# This file demonstrates the comprehensive theme and color system

library(vecshift)

# 1. Basic Color Palettes ====================================================

cat("=== Vecshift Color System Examples ===\n\n")

# Main palette
cat("1. Main Dark-Toned Palette:\n")
main_colors <- vecshift_colors("main")
cat("Colors:", paste(main_colors[1:5], collapse = ", "), "\n\n")

# Employment-specific colors
cat("2. Employment Status Colors:\n")
emp_colors <- vecshift_colors("employment")
cat("Employment colors:\n")
for (i in seq_along(emp_colors)) {
  cat(sprintf("  %s: %s\n", names(emp_colors)[i], emp_colors[i]))
}
cat("\n")

# Desaturated version
cat("3. Desaturated Palette (for subtle distinctions):\n")
desat_colors <- vecshift_colors("desaturated", n = 5)
cat("Colors:", paste(desat_colors, collapse = ", "), "\n\n")

# Black and white for printing
cat("4. Black & White Palette (for printing):\n")
bw_colors <- vecshift_colors("bw", n = 6)
cat("Colors:", paste(bw_colors, collapse = ", "), "\n\n")

# 2. Color Accessibility Testing ============================================

cat("=== Accessibility Testing ===\n\n")

# Test main palette
cat("Testing main palette accessibility:\n")
test_vecshift_accessibility("main")

cat("\nTesting employment palette accessibility:\n")
test_vecshift_accessibility("employment")

# 3. Advanced Color Usage ===================================================

cat("\n=== Advanced Color Usage ===\n\n")

# Colors with transparency
cat("5. Colors with Transparency (alpha = 0.7):\n")
alpha_colors <- vecshift_colors("main", n = 3, alpha = 0.7)
cat("Colors:", paste(alpha_colors, collapse = ", "), "\n\n")

# Reversed colors
cat("6. Reversed Color Order:\n")
reversed_colors <- vecshift_colors("main", n = 4, reverse = TRUE)
cat("Colors:", paste(reversed_colors, collapse = ", "), "\n\n")

# Specific employment colors
cat("7. Specific Employment Status Colors:\n")
key_colors <- get_employment_colors(c("occ_ft", "occ_pt", "disoccupato"))
for (i in seq_along(key_colors)) {
  cat(sprintf("  %s: %s\n", names(key_colors)[i], key_colors[i]))
}
cat("\n")

# 4. ggplot2 Integration (if available) =====================================

if (requireNamespace("ggplot2", quietly = TRUE)) {
  
  cat("=== ggplot2 Integration Examples ===\n\n")
  
  library(ggplot2)
  library(data.table)
  
  # Create sample data
  sample_data <- data.table(
    month = rep(1:12, 3),
    employment_status = rep(c("occ_ft", "occ_pt", "disoccupato"), each = 12),
    count = c(
      rnorm(12, 1000, 100),  # Full-time
      rnorm(12, 300, 50),    # Part-time  
      rnorm(12, 150, 30)     # Unemployed
    )
  )
  
  cat("8. Creating ggplot2 visualizations with vecshift theme:\n")
  
  # Basic line plot with vecshift theme and colors
  p1 <- ggplot(sample_data, aes(x = month, y = count, color = employment_status)) +
    geom_line(size = 1.2) +
    geom_point(size = 2.5) +
    scale_color_vecshift("employment") +
    labs(
      title = "Employment Trends Over Time",
      subtitle = "Monthly employment counts by status - Vecshift Theme",
      x = "Month",
      y = "Count",
      color = "Employment Status"
    ) +
    theme_vecshift()
  
  # Save plot
  ggsave("employment_trends_vecshift.png", p1, width = 10, height = 6, dpi = 300)
  cat("   - Line plot saved as 'employment_trends_vecshift.png'\n")
  
  # Bar chart with different theme options
  p2 <- ggplot(sample_data, aes(x = month, y = count, fill = employment_status)) +
    geom_col(position = "dodge") +
    scale_fill_vecshift("employment") +
    labs(
      title = "Monthly Employment Distribution",
      subtitle = "Clean theme with major grid lines only",
      x = "Month",
      y = "Count",
      fill = "Employment Status"
    ) +
    theme_vecshift(grid = "major", base_size = 12)
  
  ggsave("employment_distribution_vecshift.png", p2, width = 10, height = 6, dpi = 300)
  cat("   - Bar chart saved as 'employment_distribution_vecshift.png'\n")
  
  # Black and white version for printing
  p3 <- ggplot(sample_data, aes(x = month, y = count, fill = employment_status)) +
    geom_col(position = "dodge", color = "black", size = 0.3) +
    scale_fill_vecshift("bw") +
    labs(
      title = "Employment Distribution (Print Version)",
      subtitle = "Black & white theme for printing",
      x = "Month",
      y = "Count",
      fill = "Employment Status"
    ) +
    theme_vecshift(grid = "major")
  
  ggsave("employment_distribution_bw.png", p3, width = 10, height = 6, dpi = 300)
  cat("   - B&W version saved as 'employment_distribution_bw.png'\n")
  
  cat("\nPlots created successfully! All plots use vecshift color palettes and themes.\n")
  
} else {
  cat("=== ggplot2 Not Available ===\n")
  cat("Install ggplot2 to see visualization examples:\n")
  cat("  install.packages('ggplot2')\n\n")
}

# 5. Color Palette Comparison ===============================================

cat("\n=== Palette Comparison ===\n\n")

palettes_to_compare <- c("main", "desaturated", "employment", "contracts")

cat("Comparing different palettes (first 5 colors):\n")
for (pal in palettes_to_compare) {
  colors <- vecshift_colors(pal, n = 5)
  cat(sprintf("%-12s: %s\n", tools::toTitleCase(pal), paste(colors, collapse = ", ")))
}

cat("\n=== Theme System Ready ===\n")
cat("The vecshift theme system is fully functional!\n")
cat("Use vecshift_colors() for color palettes and theme_vecshift() for ggplot2 themes.\n")
cat("All colors are accessibility-tested and professionally designed.\n")