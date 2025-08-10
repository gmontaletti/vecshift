# Large Scale Performance Test: 1M CF with 3 contracts average
# ==============================================================

library(data.table)
library(microbenchmark)

# Source implementations
source("R/vecshift.R")
source("R/status_labeling.R")  # Required for status classification

#' Generate Large Scale Test Data
#'
#' Creates realistic employment data with specified number of fiscal codes
#' and average contracts per person, with realistic temporal overlaps and gaps
#'
#' @param n_cf Number of unique fiscal codes (persons)
#' @param avg_contracts Average number of contracts per person
#' @param seed Random seed for reproducibility
#' @return data.table with employment records
generate_large_test_data <- function(n_cf = 1000000, avg_contracts = 3, seed = 123) {
  set.seed(seed)
  
  cat("Generating large-scale test data...\n")
  cat("- Persons (cf):", format(n_cf, big.mark = ","), "\n")
  cat("- Average contracts per person:", avg_contracts, "\n")
  
  # Generate number of contracts per person (Poisson distribution around mean)
  contracts_per_person <- rpois(n_cf, lambda = avg_contracts)
  # Ensure at least 1 contract per person
  contracts_per_person <- pmax(contracts_per_person, 1)
  
  total_records <- sum(contracts_per_person)
  cat("- Total records:", format(total_records, big.mark = ","), "\n")
  
  # Create person identifiers
  cf_ids <- paste0("CF", sprintf("%07d", 1:n_cf))
  
  # Expand to record level
  cf_expanded <- rep(cf_ids, contracts_per_person)
  record_ids <- 1:total_records
  
  cat("Creating employment records...\n")
  
  # Generate realistic employment periods
  # Base period: 2020-2024 (4 years = 1461 days)
  base_date <- as.Date("2020-01-01")
  max_days <- 1461
  
  # Generate start dates
  start_dates <- base_date + sample(0:(max_days-365), total_records, replace = TRUE)
  
  # Generate contract durations (realistic distribution)
  # Mix of short-term (30-180 days) and long-term (180-365+ days)
  duration_type <- sample(c("short", "medium", "long"), total_records, 
                         replace = TRUE, prob = c(0.3, 0.4, 0.3))
  
  durations <- ifelse(duration_type == "short", 
                     sample(30:180, total_records, replace = TRUE),
                     ifelse(duration_type == "medium",
                           sample(180:365, total_records, replace = TRUE),
                           sample(365:730, total_records, replace = TRUE)))
  
  end_dates <- start_dates + durations
  
  # Generate employment types (prior)
  # 60% full-time (1), 30% part-time (0), 10% temporary (-1)
  prior_values <- sample(c(-1, 0, 1), total_records, 
                        replace = TRUE, prob = c(0.1, 0.3, 0.6))
  
  # Create the data.table
  dt <- data.table(
    id = record_ids,
    cf = cf_expanded,
    INIZIO = start_dates,
    FINE = end_dates,
    prior = prior_values
  )
  
  # Sort by person and start date
  setorder(dt, cf, INIZIO)
  
  cat("Data generation complete.\n")
  cat("Final dataset size:", format(nrow(dt), big.mark = ","), "records\n\n")
  
  return(dt)
}

#' Monitor Memory Usage
#'
#' @param description Description of the operation
monitor_memory <- function(description) {
  if (requireNamespace("pryr", quietly = TRUE)) {
    mem_used <- pryr::mem_used()
    cat(sprintf("Memory usage (%s): %s\n", description, format(mem_used)))
    return(mem_used)
  } else {
    cat("pryr package not available for memory monitoring\n")
    return(NULL)
  }
}

# Main Performance Test
# =====================

cat("Large Scale Performance Test\n")
cat("============================\n\n")

# Monitor initial memory
monitor_memory("before data generation")

# Generate test data
large_data <- generate_large_test_data(n_cf = 1000000, avg_contracts = 3)

# Monitor memory after data generation
monitor_memory("after data generation")

# Check data characteristics
cat("Dataset Characteristics:\n")
cat("------------------------\n")
cat("Total records:", format(nrow(large_data), big.mark = ","), "\n")
cat("Unique persons (cf):", format(large_data[, length(unique(cf))], big.mark = ","), "\n")
cat("Average contracts per person:", round(nrow(large_data) / large_data[, length(unique(cf))], 2), "\n")
cat("Date range:", format(range(large_data$INIZIO)), "\n")

# Check for overlaps per person
overlap_stats <- large_data[, {
  if (.N > 1) {
    setorder(.SD, INIZIO)
    overlaps <- 0
    for (i in 2:.N) {
      if (INIZIO[i] <= FINE[i-1]) overlaps <- overlaps + 1
    }
    list(contracts = .N, overlaps = overlaps)
  } else {
    list(contracts = .N, overlaps = 0)
  }
}, by = cf]

cat("Persons with overlapping contracts:", format(sum(overlap_stats$overlaps > 0), big.mark = ","), "\n")
cat("Total overlapping periods:", format(sum(overlap_stats$overlaps), big.mark = ","), "\n\n")

# Performance comparison
cat("Performance Comparison\n")
cat("======================\n")

monitor_memory("before performance test")

# Single run first to check for errors
cat("Running initial test...\n")
start_time <- Sys.time()
result_main <- vecshift(large_data)
fast_time <- difftime(Sys.time(), start_time, units = "secs")
cat("Fast version completed in:", round(as.numeric(fast_time), 2), "seconds\n")

monitor_memory("after fast version")

start_time <- Sys.time()
result_modular <- vecshift(large_data, verbose = TRUE)
modular_time <- difftime(Sys.time(), start_time, units = "secs")
cat("Modular version completed in:", round(as.numeric(modular_time), 2), "seconds\n")

monitor_memory("after modular version")

# Verify results consistency
cat("\nResult Verification:\n")
cat("--------------------\n")
cat("Main version output rows:", format(nrow(result_main), big.mark = ","), "\n")
cat("Modular version output rows:", format(nrow(result_modular), big.mark = ","), "\n")

# Compare a sample since full comparison might be memory intensive
sample_size <- min(10000, nrow(result_main))
if (nrow(result_main) == nrow(result_modular)) {
  # Sort both for comparison
  setorder(result_main, cf, inizio)
  setorder(result_modular, cf, inizio)
  
  # Compare samples
  sample_indices <- sample(1:nrow(result_main), sample_size)
  main_sample <- result_main[sample_indices]
  modular_sample <- result_modular[sample_indices]
  
  identical_sample <- identical(main_sample, modular_sample)
  cat("Sample comparison (", format(sample_size, big.mark = ","), "rows) identical:", identical_sample, "\n")
} else {
  cat("Row counts differ - detailed comparison needed\n")
}

# Performance summary
cat("\nPerformance Summary:\n")
cat("-------------------\n")
cat("Dataset size:", format(nrow(large_data), big.mark = ","), "records\n")
cat("Fast version time:", round(as.numeric(fast_time), 2), "seconds\n")
cat("Modular version time:", round(as.numeric(modular_time), 2), "seconds\n")
speedup <- as.numeric(modular_time) / as.numeric(fast_time)
cat("Speed ratio (modular/fast):", round(speedup, 2), "x\n")
cat("Records processed per second (fast):", format(round(nrow(large_data) / as.numeric(fast_time)), big.mark = ","), "\n")
cat("Records processed per second (modular):", format(round(nrow(large_data) / as.numeric(modular_time)), big.mark = ","), "\n")

# Memory efficiency test
cat("\nMemory Efficiency Test:\n")
cat("-----------------------\n")

# Test with validation disabled
cat("Testing modular version with validation disabled...\n")
start_time <- Sys.time()
result_modular_no_val <- vecshift(large_data, validate = FALSE, verbose = TRUE)
modular_no_val_time <- difftime(Sys.time(), start_time, units = "secs")
cat("Modular (no validation) time:", round(as.numeric(modular_no_val_time), 2), "seconds\n")

validation_overhead <- as.numeric(modular_time) - as.numeric(modular_no_val_time)
cat("Validation overhead:", round(validation_overhead, 2), "seconds\n")

# Chunk processing test for very large datasets
if (nrow(large_data) > 1000000) {
  cat("\nChunk Processing Test:\n")
  cat("---------------------\n")
  
  chunk_size <- 500000
  unique_cfs <- unique(large_data$cf)
  n_chunks <- ceiling(length(unique_cfs) / chunk_size)
  
  cat("Processing in", n_chunks, "chunks of", format(chunk_size, big.mark = ","), "persons each...\n")
  
  chunk_results <- list()
  start_time <- Sys.time()
  
  for (i in 1:n_chunks) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, length(unique_cfs))
    chunk_cfs <- unique_cfs[start_idx:end_idx]
    
    chunk_data <- large_data[cf %in% chunk_cfs]
    chunk_results[[i]] <- vecshift(chunk_data)
    
    cat("Completed chunk", i, "of", n_chunks, "\n")
  }
  
  chunk_time <- difftime(Sys.time(), start_time, units = "secs")
  final_result <- rbindlist(chunk_results)
  
  cat("Chunk processing completed in:", round(as.numeric(chunk_time), 2), "seconds\n")
  cat("Final result rows:", format(nrow(final_result), big.mark = ","), "\n")
}

monitor_memory("final")

cat("\nLarge Scale Test Complete!\n")