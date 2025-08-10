# Performance Comparison: vecshift with different options
# ========================================================

library(data.table)
library(microbenchmark)

# Source implementations
source("R/vecshift.R")
source("R/status_labeling.R")  # Required for status classification

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
  with_status = vecshift(small_data),
  no_status = vecshift(small_data, classify_status = FALSE),
  times = 100
)
print(small_benchmark)
cat("\n")

# Medium dataset test
cat("Medium Dataset (1,000 records):\n")
cat("--------------------------------\n")
medium_benchmark <- microbenchmark(
  with_status = vecshift(medium_data),
  no_status = vecshift(medium_data, classify_status = FALSE),
  times = 50
)
print(medium_benchmark)
cat("\n")

# Large dataset test
cat("Large Dataset (10,000 records):\n")
cat("--------------------------------\n")
large_benchmark <- microbenchmark(
  with_status = vecshift(large_data),
  no_status = vecshift(large_data, classify_status = FALSE),
  times = 10
)
print(large_benchmark)
cat("\n")

# Verify results are identical
cat("Result Verification:\n")
cat("-------------------\n")
result_with_status <- vecshift(medium_data)
result_no_status <- vecshift(medium_data, classify_status = FALSE)

# Sort both results for comparison (they might be in different orders)
setorder(result_with_status, cf, inizio)
setorder(result_no_status, cf, inizio)

# Compare core columns (excluding status column)
core_cols <- c("cf", "inizio", "fine", "arco", "prior", "id", "durata")
identical_results <- identical(result_with_status[, ..core_cols], result_no_status[, ..core_cols])
cat("Results identical:", identical_results, "\n")

if (!identical_results) {
  cat("Differences found:\n")
  
  # Check dimensions
  cat("Dimensions - With status:", paste(dim(result_with_status), collapse = "x"), 
      ", No status:", paste(dim(result_no_status), collapse = "x"), "\n")
  
  # Check column names
  if (!identical(names(result_with_status), names(result_no_status))) {
    cat("Column names differ:\n")
    cat("With status:", paste(names(result_with_status), collapse = ", "), "\n")
    cat("No status:", paste(names(result_no_status), collapse = ", "), "\n")
  }
  
  # Check for differences in core columns
  for (col in core_cols) {
    if (col %in% names(result_with_status) && col %in% names(result_no_status)) {
      if (!identical(result_with_status[[col]], result_no_status[[col]])) {
        cat("Column", col, "differs between implementations\n")
      }
    }
  }
} else {
  cat("✓ Core columns are identical between implementations\n")
  cat("  (Status column expected to differ)\n")
}

# Performance with validation disabled
cat("\nPerformance with validation disabled:\n")
cat("------------------------------------\n")
validation_benchmark <- microbenchmark(
  no_status = vecshift(medium_data, classify_status = FALSE),
  with_status = vecshift(medium_data, classify_status = TRUE),
  times = 50
)
print(validation_benchmark)

# Memory usage comparison
cat("\nMemory Usage Comparison:\n")
cat("------------------------\n")
library(pryr)

mem_vecshift <- object_size(vecshift)
mem_status <- object_size(classify_employment_status)

cat("Main vecshift function size:", format(mem_vecshift), "\n")
cat("Status classification function size:", format(mem_status), "\n")
cat("Total size:", format(mem_vecshift + mem_status), "\n")

# Summary
cat("\nSummary:\n")
cat("--------\n")
no_status_median <- median(large_benchmark$time[large_benchmark$expr == "no_status"])
with_status_median <- median(large_benchmark$time[large_benchmark$expr == "with_status"])
overhead_ratio <- with_status_median / no_status_median

cat(sprintf("Status classification overhead: %.2fx\n", overhead_ratio))
cat(sprintf("Performance difference: %.1f%%\n", (overhead_ratio - 1) * 100))

cat("Trade-offs:\n")
cat("- No status: Better performance, minimal output\n")
cat("- With status: Full employment classification, slight performance cost\n")