# Performance Comparison: vecshift_fast vs vecshift (modular)
# ===========================================================

library(data.table)
library(microbenchmark)

# Source both implementations
source("R/vecshift.R")
source("R/vecshift_modular.R")

# Create test datasets of various sizes
generate_test_data <- function(n_records, n_persons = NULL) {
  if (is.null(n_persons)) {
    n_persons <- ceiling(n_records / 10)  # Average 10 records per person
  }
  
  set.seed(123)  # For reproducible results
  
  data.table(
    id = 1:n_records,
    cf = sample(paste0("CF", 1:n_persons), n_records, replace = TRUE),
    INIZIO = as.Date("2020-01-01") + sample(0:1095, n_records, replace = TRUE),
    FINE = as.Date("2020-01-01") + sample(30:1460, n_records, replace = TRUE),
    prior = sample(c(-1, 0, 1), n_records, replace = TRUE)
  )
}

# Test datasets
small_data <- generate_test_data(100)
medium_data <- generate_test_data(1000)
large_data <- generate_test_data(10000)

cat("Performance Comparison Results\n")
cat("==============================\n\n")

# Small dataset test
cat("Small Dataset (100 records):\n")
cat("-----------------------------\n")
small_benchmark <- microbenchmark(
  fast = vecshift_fast(small_data),
  modular = vecshift(small_data),
  times = 100
)
print(small_benchmark)
cat("\n")

# Medium dataset test
cat("Medium Dataset (1,000 records):\n")
cat("--------------------------------\n")
medium_benchmark <- microbenchmark(
  fast = vecshift_fast(medium_data),
  modular = vecshift(medium_data),
  times = 50
)
print(medium_benchmark)
cat("\n")

# Large dataset test
cat("Large Dataset (10,000 records):\n")
cat("--------------------------------\n")
large_benchmark <- microbenchmark(
  fast = vecshift_fast(large_data),
  modular = vecshift(large_data),
  times = 10
)
print(large_benchmark)
cat("\n")

# Verify results are identical
cat("Result Verification:\n")
cat("-------------------\n")
result_fast <- vecshift_fast(medium_data)
result_modular <- vecshift(medium_data)

# Sort both results for comparison (they might be in different orders)
setorder(result_fast, cf, inizio)
setorder(result_modular, cf, inizio)

# Compare results
identical_results <- identical(result_fast, result_modular)
cat("Results identical:", identical_results, "\n")

if (!identical_results) {
  cat("Differences found:\n")
  
  # Check dimensions
  cat("Dimensions - Fast:", paste(dim(result_fast), collapse = "x"), 
      ", Modular:", paste(dim(result_modular), collapse = "x"), "\n")
  
  # Check column names
  if (!identical(names(result_fast), names(result_modular))) {
    cat("Column names differ:\n")
    cat("Fast:", paste(names(result_fast), collapse = ", "), "\n")
    cat("Modular:", paste(names(result_modular), collapse = ", "), "\n")
  }
  
  # Check for differences in key columns if dimensions match
  if (nrow(result_fast) == nrow(result_modular) && ncol(result_fast) == ncol(result_modular)) {
    common_cols <- intersect(names(result_fast), names(result_modular))
    for (col in common_cols) {
      if (!identical(result_fast[[col]], result_modular[[col]])) {
        cat("Column", col, "differs between implementations\n")
      }
    }
  }
} else {
  cat("✓ Both implementations produce identical results\n")
}

# Performance with validation disabled
cat("\nPerformance with validation disabled:\n")
cat("------------------------------------\n")
validation_benchmark <- microbenchmark(
  fast = vecshift_fast(medium_data),
  modular_with_validation = vecshift(medium_data, validate = TRUE),
  modular_no_validation = vecshift(medium_data, validate = FALSE),
  times = 50
)
print(validation_benchmark)

# Memory usage comparison
cat("\nMemory Usage Comparison:\n")
cat("------------------------\n")
library(pryr)

mem_fast <- object_size(vecshift_fast)
mem_modular <- object_size(vecshift)
mem_helpers <- object_size(validate_input) + object_size(create_employment_events) + 
              object_size(process_temporal_segments) + object_size(classify_employment_states)

cat("Fast version function size:", format(mem_fast), "\n")
cat("Modular version function size:", format(mem_modular), "\n")
cat("Helper functions size:", format(mem_helpers), "\n")
cat("Total modular approach size:", format(mem_modular + mem_helpers), "\n")

# Summary
cat("\nSummary:\n")
cat("--------\n")
fast_median <- median(large_benchmark$time[large_benchmark$expr == "fast"])
modular_median <- median(large_benchmark$time[large_benchmark$expr == "modular"])
speedup_ratio <- modular_median / fast_median

if (speedup_ratio > 1) {
  cat(sprintf("Fast version is %.2fx faster than modular version\n", speedup_ratio))
} else {
  cat(sprintf("Modular version is %.2fx faster than fast version\n", 1/speedup_ratio))
}

cat("Trade-offs:\n")
cat("- Fast version: Better performance, less readable code, harder to maintain\n")
cat("- Modular version: Better maintainability, easier debugging, additional features\n")