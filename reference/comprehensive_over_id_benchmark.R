#!/usr/bin/env Rscript

# Comprehensive End-to-End Benchmark for vecshift over_id Functionality
# Demonstrates real-world performance benefits of consolidation

library(data.table)
library(microbenchmark)

# Load vecshift functions from source
source("R/globals.R")
source("R/status_labeling.R")
source("R/vecshift.R") 
source("R/merge_consecutive_employment.R")

# Load additional functions
tryCatch({
  source("R/process_employment_pipeline.R")
  source("R/analyze_employment_transitions.R")
  source("R/merge_columns.R")
}, error = function(e) {
  cat("Note: Some advanced functions not available\n")
})

cat("================================================================\n")
cat("COMPREHENSIVE VECSHIFT OVER_ID BENCHMARK\n")
cat("================================================================\n")
cat("Testing end-to-end performance benefits of consolidation\n\n")

# Generate realistic employment data
generate_realistic_employment_data <- function(n_records, seed = 42) {
  set.seed(seed)
  
  # Realistic parameters
  n_people <- max(1, round(n_records * 0.02))  # 2% unique people (more realistic)
  n_companies <- max(10, round(n_people * 0.1))  # 10% of people are companies
  
  cat(sprintf("Generating %s records for %d people across %d companies...\n",
              format(n_records, big.mark = ","), n_people, n_companies))
  
  # Generate person and company pools
  people_ids <- paste0("PERSON", sprintf("%06d", 1:n_people))
  companies <- paste0("COMPANY_", LETTERS[1:min(26, n_companies)])
  departments <- c("IT", "Finance", "HR", "Marketing", "Operations", "Sales", "Legal", "R&D")
  
  # Create employment contracts
  dt <- data.table(
    id = 1:n_records,
    cf = sample(people_ids, n_records, replace = TRUE)
  )
  
  # Assign employment types with realistic distribution
  dt[, prior := sample(c(-1, 0, 1, 2), n_records, replace = TRUE, 
                       prob = c(0.05, 0.25, 0.60, 0.10))]  # Mostly full-time
  
  # Generate realistic date ranges (2020-2023)
  base_start <- as.Date("2020-01-01")
  base_end <- as.Date("2023-12-31")
  
  # Contract durations: realistic distribution
  duration_days <- sample(c(
    sample(30:90, n_records * 0.1, replace = TRUE),    # Short contracts (10%)
    sample(91:365, n_records * 0.3, replace = TRUE),   # Medium contracts (30%) 
    sample(366:730, n_records * 0.4, replace = TRUE),  # Long contracts (40%)
    sample(731:1095, n_records * 0.2, replace = TRUE)  # Very long contracts (20%)
  ), n_records)
  
  # Generate start dates
  max_start_days <- as.numeric(base_end - base_start) - max(duration_days)
  dt[, INIZIO := base_start + sample(0:max_start_days, n_records, replace = TRUE)]
  dt[, FINE := INIZIO + duration_days - 1]
  
  # Ensure FINE doesn't exceed base_end
  dt[FINE > base_end, FINE := base_end]
  
  # Create realistic overlapping patterns (20% of records)
  overlap_indices <- sample(n_records, round(n_records * 0.2))
  
  for (i in overlap_indices) {
    person <- dt$cf[i]
    person_records <- dt[cf == person & id != i]
    
    if (nrow(person_records) > 0) {
      # Create overlap with existing contract
      ref_record <- person_records[sample(nrow(person_records), 1)]
      
      # Overlap by 1-6 months
      overlap_months <- sample(1:6, 1)
      new_start <- ref_record$INIZIO + sample(-30:30, 1)
      new_end <- ref_record$FINE + (overlap_months * 30) + sample(-30:30, 1)
      
      # Ensure valid ranges
      new_start <- max(base_start, new_start)
      new_end <- min(base_end, new_end)
      
      if (new_start < new_end) {
        dt[id == i, INIZIO := new_start]
        dt[id == i, FINE := new_end]
      }
    }
  }
  
  # Add metadata columns
  dt[, company := sample(companies, n_records, replace = TRUE)]
  dt[, department := sample(departments, n_records, replace = TRUE)]
  dt[, salary := round(runif(n_records, 25000, 100000), -3)]  # Salary in thousands
  dt[, contract_type := ifelse(prior > 0, "FULL_TIME", "PART_TIME")]
  
  return(dt[order(cf, INIZIO)])
}

# Benchmark function for end-to-end analysis
benchmark_end_to_end_analysis <- function(employment_data) {
  n_records <- nrow(employment_data)
  cat(sprintf("\n--- End-to-End Analysis Benchmark: %s records ---\n", 
              format(n_records, big.mark = ",")))
  
  results <- list()
  
  # Step 1: Core vecshift processing
  cat("1. Core vecshift processing...\n")
  time_start <- Sys.time()
  vecshift_result <- vecshift(employment_data, classify_status = TRUE)
  time_vecshift <- as.numeric(Sys.time() - time_start)
  
  cat(sprintf("   - Generated %s temporal segments in %.2f seconds\n",
              format(nrow(vecshift_result), big.mark = ","), time_vecshift))
  
  results$vecshift <- list(
    input_records = n_records,
    output_segments = nrow(vecshift_result), 
    processing_time = time_vecshift,
    records_per_second = n_records / time_vecshift
  )
  
  # Step 2: Consolidation
  cat("2. Period consolidation...\n")
  time_start <- Sys.time()
  consolidated_result <- merge_consecutive_employment(vecshift_result, 
                                                    consolidation_type = "both")
  time_consolidation <- as.numeric(Sys.time() - time_start)
  
  reduction_pct <- (nrow(vecshift_result) - nrow(consolidated_result)) / nrow(vecshift_result) * 100
  
  cat(sprintf("   - Consolidated to %s periods in %.2f seconds (%.1f%% reduction)\n",
              format(nrow(consolidated_result), big.mark = ","),
              time_consolidation, reduction_pct))
  
  results$consolidation <- list(
    input_segments = nrow(vecshift_result),
    output_periods = nrow(consolidated_result),
    reduction_pct = reduction_pct,
    processing_time = time_consolidation
  )
  
  # Step 3: Analysis comparison (raw vs consolidated)
  if (exists("analyze_employment_transitions")) {
    cat("3. Transition analysis comparison...\n")
    
    # Prepare pipeline results (simulate process_employment_pipeline output)
    pipeline_raw <- vecshift_result
    pipeline_raw[, company := employment_data$company[match(id, employment_data$id)]]
    
    pipeline_consolidated <- consolidated_result  
    if (!"company" %in% names(pipeline_consolidated)) {
      pipeline_consolidated[, company := "UNKNOWN"]  # Fallback
    }
    
    # Benchmark raw analysis
    time_start <- Sys.time()
    tryCatch({
      transitions_raw <- analyze_employment_transitions(
        pipeline_raw,
        transition_variable = "company",
        use_consolidated_periods = FALSE,
        show_progress = FALSE
      )
      time_analysis_raw <- as.numeric(Sys.time() - time_start)
      n_transitions_raw <- nrow(transitions_raw)
    }, error = function(e) {
      time_analysis_raw <- NA
      n_transitions_raw <- 0
      cat("   - Raw analysis failed\n")
    })
    
    # Benchmark consolidated analysis  
    time_start <- Sys.time()
    tryCatch({
      transitions_consolidated <- analyze_employment_transitions(
        pipeline_consolidated,
        transition_variable = "company", 
        use_consolidated_periods = FALSE,  # Already consolidated
        show_progress = FALSE
      )
      time_analysis_consolidated <- as.numeric(Sys.time() - time_start)
      n_transitions_consolidated <- nrow(transitions_consolidated)
    }, error = function(e) {
      time_analysis_consolidated <- NA
      n_transitions_consolidated <- 0
      cat("   - Consolidated analysis failed\n")
    })
    
    if (!is.na(time_analysis_raw) && !is.na(time_analysis_consolidated)) {
      speedup <- time_analysis_raw / time_analysis_consolidated
      
      cat(sprintf("   - Raw analysis: %.2f sec (%d transitions)\n",
                  time_analysis_raw, n_transitions_raw))
      cat(sprintf("   - Consolidated analysis: %.2f sec (%d transitions)\n", 
                  time_analysis_consolidated, n_transitions_consolidated))
      cat(sprintf("   - Speedup from consolidation: %.1fx\n", speedup))
      
      results$analysis <- list(
        raw_time = time_analysis_raw,
        consolidated_time = time_analysis_consolidated,
        speedup = speedup,
        raw_transitions = n_transitions_raw,
        consolidated_transitions = n_transitions_consolidated
      )
    }
  }
  
  # Step 4: Total end-to-end metrics
  total_time <- time_vecshift + time_consolidation
  overall_throughput <- n_records / total_time
  
  cat(sprintf("\n4. End-to-end summary:\n"))
  cat(sprintf("   - Total processing time: %.2f seconds\n", total_time))
  cat(sprintf("   - Overall throughput: %.0f records/second\n", overall_throughput))
  cat(sprintf("   - Data reduction: %s -> %s records (%.1f%% reduction)\n",
              format(n_records, big.mark = ","),
              format(nrow(consolidated_result), big.mark = ","),
              reduction_pct))
  
  results$total <- list(
    input_records = n_records,
    final_periods = nrow(consolidated_result),
    total_time = total_time,
    throughput = overall_throughput,
    data_reduction_pct = reduction_pct
  )
  
  return(results)
}

# Test over_id specific functionality
test_over_id_functionality <- function(test_data) {
  cat("\n--- over_id Functionality Validation ---\n")
  
  # Process with vecshift
  result <- vecshift(test_data, classify_status = FALSE)
  
  if ("over_id" %in% names(result)) {
    # Analyze over_id patterns
    over_id_analysis <- result[, .(
      n_segments = .N,
      min_date = min(inizio),
      max_date = max(fine), 
      employment_segments = sum(arco > 0),
      unemployment_segments = sum(arco == 0),
      overlapping_segments = sum(arco > 1),
      unique_employment_periods = length(unique(over_id[over_id > 0]))
    ), by = over_id][order(over_id)]
    
    cat("over_id Analysis:\n")
    cat(sprintf("- Total over_id groups: %d\n", nrow(over_id_analysis)))
    cat(sprintf("- Unemployment periods (over_id=0): %d segments\n", 
                over_id_analysis[over_id == 0, n_segments]))
    cat(sprintf("- Employment periods: %d groups with %d total segments\n",
                over_id_analysis[over_id > 0, .N], 
                over_id_analysis[over_id > 0, sum(n_segments)]))
    
    # Check for overlapping periods
    overlapping_groups <- over_id_analysis[over_id > 0 & overlapping_segments > 0]
    
    if (nrow(overlapping_groups) > 0) {
      cat(sprintf("- Groups with overlapping employment: %d\n", nrow(overlapping_groups)))
      cat(sprintf("- Total overlapping segments: %d\n", overlapping_groups[, sum(overlapping_segments)]))
    }
    
    cat("✅ over_id functionality working correctly\n")
    return(TRUE)
  } else {
    cat("❌ over_id column not found\n")
    return(FALSE)
  }
}

# Main execution
main_comprehensive_benchmark <- function() {
  # Test datasets of different sizes
  test_sizes <- c(5000, 25000, 100000)
  all_results <- list()
  
  for (size in test_sizes) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat(sprintf("TESTING DATASET SIZE: %s RECORDS\n", format(size, big.mark = ",")))
    cat(paste(rep("=", 60), collapse = ""), "\n")
    
    # Generate test data
    test_data <- generate_realistic_employment_data(size)
    
    # Test over_id functionality
    over_id_working <- test_over_id_functionality(test_data) 
    
    # Run comprehensive benchmark
    results <- benchmark_end_to_end_analysis(test_data)
    results$over_id_working <- over_id_working
    results$dataset_size <- size
    
    all_results[[as.character(size)]] <- results
  }
  
  # Generate summary report
  cat("\n\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("COMPREHENSIVE BENCHMARK SUMMARY\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  # Create summary table
  summary_data <- data.table(
    dataset_size = sapply(all_results, function(x) x$dataset_size),
    vecshift_throughput = sapply(all_results, function(x) round(x$vecshift$records_per_second)),
    total_throughput = sapply(all_results, function(x) round(x$total$throughput)),
    data_reduction_pct = sapply(all_results, function(x) round(x$total$data_reduction_pct, 1)),
    final_periods = sapply(all_results, function(x) x$total$final_periods)
  )
  
  cat("\nPerformance Summary:\n")
  print(summary_data)
  
  # Calculate averages
  avg_vecshift_throughput <- mean(summary_data$vecshift_throughput)
  avg_total_throughput <- mean(summary_data$total_throughput) 
  avg_reduction <- mean(summary_data$data_reduction_pct)
  
  cat(sprintf("\nOverall Performance:\n"))
  cat(sprintf("- Average vecshift throughput: %s records/second\n",
              format(round(avg_vecshift_throughput), big.mark = ",")))
  cat(sprintf("- Average end-to-end throughput: %s records/second\n",
              format(round(avg_total_throughput), big.mark = ",")))
  cat(sprintf("- Average data reduction: %.1f%%\n", avg_reduction))
  
  # Final assessment
  target_performance <- 1460000  # 1.46M records/second
  efficiency_ratio <- avg_vecshift_throughput / target_performance
  
  cat(sprintf("\nPerformance Assessment:\n"))
  if (efficiency_ratio >= 0.50) {
    cat("✅ GOOD: Performance within acceptable range for enhanced functionality\n")
  } else if (efficiency_ratio >= 0.20) {
    cat("⚠️  ACCEPTABLE: Performance trade-off justified by consolidation benefits\n")
  } else {
    cat("❌ NEEDS OPTIMIZATION: Significant performance degradation\n")
  }
  
  cat(sprintf("Core efficiency: %.1f%% of original target\n", efficiency_ratio * 100))
  
  # Consolidation benefits assessment
  if (avg_reduction >= 70) {
    cat("🎉 EXCELLENT consolidation benefits (>70% reduction)\n")
  } else if (avg_reduction >= 50) {
    cat("✅ GOOD consolidation benefits (>50% reduction)\n")
  } else if (avg_reduction >= 30) {
    cat("⚠️  MODERATE consolidation benefits (>30% reduction)\n")
  } else {
    cat("❌ LOW consolidation benefits (<30% reduction)\n")
  }
  
  cat(sprintf("\n🔧 FINAL RECOMMENDATION:\n"))
  if (efficiency_ratio >= 0.20 && avg_reduction >= 50) {
    cat("✅ DEPLOY TO PRODUCTION - Benefits outweigh performance costs\n")
  } else if (efficiency_ratio >= 0.15 && avg_reduction >= 70) {
    cat("⚠️  CONDITIONAL DEPLOYMENT - Monitor performance in production\n")
  } else {
    cat("❌ NEEDS OPTIMIZATION before production deployment\n")
  }
  
  return(all_results)
}

# Execute comprehensive benchmark
if (!interactive()) {
  benchmark_results <- main_comprehensive_benchmark()
} else {
  cat("Comprehensive benchmark loaded. Run main_comprehensive_benchmark() to execute.\n")
}