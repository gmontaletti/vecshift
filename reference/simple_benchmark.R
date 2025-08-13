#!/usr/bin/env Rscript

# Simple Performance Benchmark for vecshift over_id functionality
# This script tests the core performance with manageable dataset sizes

library(data.table)
library(microbenchmark)

# Load vecshift functions from source (to get latest over_id functionality)
source("R/globals.R")
source("R/status_labeling.R")  
source("R/vecshift.R")
source("R/merge_consecutive_employment.R")

# Try to load additional functions if available
tryCatch({
  source("R/process_employment_pipeline.R")
  source("R/analyze_employment_transitions.R")
  source("R/merge_columns.R")
}, error = function(e) {
  cat("Some additional functions not available (expected for core benchmarks)\n")
})

# Configuration
cat("============================================================\n")
cat("VECSHIFT OVER_ID PERFORMANCE BENCHMARK\n")
cat("============================================================\n")

# Target performance: ~1.46M records/second baseline
target_performance <- 1460000

# Test dataset sizes
test_sizes <- c(1000, 10000, 50000)

# Function to generate test data
generate_test_data <- function(n_records, seed = 42) {
  set.seed(seed)
  
  n_people <- max(1, round(n_records * 0.1))  # 10% unique people
  people_ids <- paste0("PERSON", sprintf("%06d", 1:n_people))
  
  # Generate contracts
  dt <- data.table(
    id = 1:n_records,
    cf = sample(people_ids, n_records, replace = TRUE),
    prior = sample(c(0, 1), n_records, replace = TRUE, prob = c(0.3, 0.7))
  )
  
  # Generate dates with some overlaps
  base_start <- as.Date("2020-01-01")
  dt[, INIZIO := base_start + sample(0:1000, n_records, replace = TRUE)]
  dt[, FINE := INIZIO + sample(30:365, n_records, replace = TRUE)]
  
  # Add metadata for analysis functions
  dt[, company := sample(LETTERS[1:10], n_records, replace = TRUE)]
  dt[, salary := sample(25000:80000, n_records, replace = TRUE)]
  
  return(dt[order(cf, INIZIO)])
}

# Benchmark function
benchmark_performance <- function(test_data) {
  n_records <- nrow(test_data)
  cat(sprintf("Testing with %s records (%d people)...\n", 
              format(n_records, big.mark = ","), 
              length(unique(test_data$cf))))
  
  # Core vecshift benchmarks
  cat("  - vecshift() core function: ")
  bench_core <- microbenchmark(
    vecshift(test_data, classify_status = TRUE),
    times = 5,
    unit = "ms"
  )
  
  core_time <- median(bench_core$time) / 1000000000  # Convert to seconds
  core_performance <- n_records / core_time
  
  cat(sprintf("%.1f ms, %.0f records/sec\n", median(bench_core$time)/1000000, core_performance))
  
  # Test consolidation if we have over_id
  vecshift_output <- vecshift(test_data, classify_status = TRUE)
  
  if ("over_id" %in% names(vecshift_output)) {
    cat("  - merge_consecutive_employment(): ")
    bench_consolidation <- microbenchmark(
      merge_consecutive_employment(vecshift_output, consolidation_type = "both"),
      times = 5,
      unit = "ms"
    )
    
    cons_time <- median(bench_consolidation$time) / 1000000000
    cons_performance <- n_records / cons_time
    
    cat(sprintf("%.1f ms, %.0f records/sec\n", median(bench_consolidation$time)/1000000, cons_performance))
    
    # Test analysis functions if available
    tryCatch({
      pipeline_result <- process_employment_pipeline(
        test_data,
        merge_columns = c("company", "salary"),
        collapse_consecutive = FALSE,
        show_progress = FALSE
      )
      
      cat("  - analyze_employment_transitions(): ")
      bench_analysis <- microbenchmark(
        analyze_employment_transitions(
          pipeline_result,
          transition_variable = "company",
          use_consolidated_periods = FALSE,
          show_progress = FALSE
        ),
        times = 3,
        unit = "ms"
      )
      
      analysis_time <- median(bench_analysis$time) / 1000000000
      analysis_performance <- n_records / analysis_time
      
      cat(sprintf("%.1f ms, %.0f records/sec\n", median(bench_analysis$time)/1000000, analysis_performance))
      
    }, error = function(e) {
      cat("Analysis functions not available\n")
    })
  }
  
  return(list(
    n_records = n_records,
    core_performance = core_performance,
    core_time_ms = median(bench_core$time) / 1000000
  ))
}

# Run benchmarks
results <- list()

for (size in test_sizes) {
  cat(sprintf("\n--- Dataset Size: %s records ---\n", format(size, big.mark = ",")))
  
  test_data <- generate_test_data(size)
  result <- benchmark_performance(test_data)
  results[[as.character(size)]] <- result
  
  # Check against target
  efficiency_ratio <- result$core_performance / target_performance
  
  if (efficiency_ratio >= 1.0) {
    cat(sprintf("  ✅ PERFORMANCE: %.1f%% of target (GOOD)\n", efficiency_ratio * 100))
  } else if (efficiency_ratio >= 0.8) {
    cat(sprintf("  ⚠️  PERFORMANCE: %.1f%% of target (ACCEPTABLE)\n", efficiency_ratio * 100))
  } else {
    cat(sprintf("  ❌ PERFORMANCE: %.1f%% of target (NEEDS OPTIMIZATION)\n", efficiency_ratio * 100))
  }
}

# Summary
cat("\n============================================================\n")
cat("PERFORMANCE SUMMARY\n")
cat("============================================================\n")

# Create summary table
summary_data <- data.table(
  dataset_size = sapply(results, function(x) x$n_records),
  time_ms = sapply(results, function(x) x$core_time_ms),
  records_per_sec = sapply(results, function(x) x$core_performance),
  efficiency_pct = sapply(results, function(x) round(x$core_performance / target_performance * 100, 1))
)

print(summary_data)

# Overall assessment
avg_efficiency <- mean(summary_data$efficiency_pct)

cat("\n")
if (avg_efficiency >= 100) {
  cat("🎉 OVERALL ASSESSMENT: EXCELLENT - Performance exceeds target\n")
} else if (avg_efficiency >= 80) {
  cat("✅ OVERALL ASSESSMENT: GOOD - Performance meets expectations\n") 
} else if (avg_efficiency >= 60) {
  cat("⚠️  OVERALL ASSESSMENT: ACCEPTABLE - Some performance degradation\n")
} else {
  cat("❌ OVERALL ASSESSMENT: POOR - Significant performance issues\n")
}

cat(sprintf("Average Performance: %.1f%% of target (%.1fM records/second baseline)\n", 
            avg_efficiency, target_performance / 1000000))

# Test over_id functionality specifically
cat("\n--- Testing over_id Feature ---\n")
test_data_medium <- generate_test_data(10000)

# Generate with overlapping periods
set.seed(42)
test_data_medium[1:100, FINE := test_data_medium[1:100, INIZIO + 500]]  # Create overlaps

vecshift_result <- vecshift(test_data_medium)

if ("over_id" %in% names(vecshift_result)) {
  over_id_stats <- vecshift_result[, .(
    n_segments = .N,
    n_employment = sum(arco > 0),
    n_unemployment = sum(arco == 0),
    n_overlaps = sum(arco > 1),
    unique_over_ids = length(unique(over_id[over_id > 0]))
  )]
  
  cat("over_id functionality working:\n")
  cat(sprintf("  - Total segments: %d\n", over_id_stats$n_segments))
  cat(sprintf("  - Employment periods: %d\n", over_id_stats$n_employment))
  cat(sprintf("  - Unemployment periods: %d\n", over_id_stats$n_unemployment))
  cat(sprintf("  - Overlapping periods: %d\n", over_id_stats$n_overlaps))
  cat(sprintf("  - Unique over_id groups: %d\n", over_id_stats$unique_over_ids))
  
  # Test consolidation
  consolidated <- merge_consecutive_employment(vecshift_result, consolidation_type = "both")
  reduction_ratio <- (nrow(vecshift_result) - nrow(consolidated)) / nrow(vecshift_result) * 100
  
  cat(sprintf("  - Consolidation reduces records by %.1f%% (%d -> %d)\n",
              reduction_ratio, nrow(vecshift_result), nrow(consolidated)))
  
  cat("✅ over_id functionality validated\n")
} else {
  cat("❌ over_id column not found in vecshift output\n")
}

cat("\n============================================================\n")
cat("BENCHMARK COMPLETE\n")
cat("============================================================\n")

# Save timestamp
cat("Results generated on:", format(Sys.time()), "\n")