# Test script to verify the updated analyze_employment_transitions function
# This script tests the new consolidated periods functionality

library(data.table)

# Source the necessary functions
source("R/vecshift.R")
source("R/merge_consecutive_employment.R") 
source("R/analyze_employment_transitions.R")

# Create test data with overlapping periods
test_data <- data.table(
  id = 1:8,
  cf = c("PERSON1", "PERSON1", "PERSON1", "PERSON1", 
         "PERSON2", "PERSON2", "PERSON2", "PERSON2"),
  INIZIO = as.Date(c("2023-01-01", "2023-01-15", "2023-04-01", "2023-06-01",
                     "2023-01-01", "2023-02-01", "2023-05-01", "2023-07-01")),
  FINE = as.Date(c("2023-03-31", "2023-03-15", "2023-05-31", "2023-08-31",
                   "2023-01-31", "2023-04-30", "2023-06-30", "2023-09-30")),
  prior = c(1, 0.5, 1, 0.5, 1, 1, 0.5, 1),
  company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD",
              "CompanyX", "CompanyY", "CompanyZ", "CompanyW"),
  salary = c(3000, 1500, 3500, 2000, 2800, 3200, 1800, 3800)
)

# Process with vecshift (assuming it creates over_id column)
print("Processing with vecshift...")
result <- vecshift(test_data)
print(head(result))

# Check if over_id column exists
if ("over_id" %in% names(result)) {
  print("over_id column found - testing consolidated analysis")
  
  # Test with consolidation
  transitions_consolidated <- analyze_employment_transitions(
    pipeline_result = result,
    transition_variable = "company",
    statistics_variables = c("salary"),
    use_consolidated_periods = TRUE,
    consolidation_type = "both",
    show_progress = TRUE
  )
  
  print("=== CONSOLIDATED TRANSITIONS ===")
  print(transitions_consolidated)
  
  # Test without consolidation for comparison
  transitions_original <- analyze_employment_transitions(
    pipeline_result = result,
    transition_variable = "company", 
    statistics_variables = c("salary"),
    use_consolidated_periods = FALSE,
    show_progress = TRUE
  )
  
  print("=== ORIGINAL (NON-CONSOLIDATED) TRANSITIONS ===")
  print(transitions_original)
  
  # Compare transition counts
  cat("\nComparison:\n")
  cat("Consolidated transitions:", nrow(transitions_consolidated), "\n")
  cat("Original transitions:", nrow(transitions_original), "\n")
  
} else {
  print("over_id column not found - testing basic functionality")
  
  # Test without consolidation
  transitions_basic <- analyze_employment_transitions(
    pipeline_result = result,
    transition_variable = "company",
    statistics_variables = c("salary"),
    use_consolidated_periods = FALSE,
    show_progress = TRUE
  )
  
  print("=== BASIC TRANSITIONS ===")
  print(transitions_basic)
}

print("Test completed!")