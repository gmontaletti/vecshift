# Test script to demonstrate the modified analyze_employment_transitions() function
# with mode/weighted mean calculations for "other columns"

library(data.table)

# Create sample employment data with transitions including additional columns
# This creates gaps between employment periods that will become unemployment periods
employment_data <- data.table(
  id = 1:6,
  cf = c(rep("PERSON001", 3), rep("PERSON002", 3)),
  INIZIO = as.Date(c(
    # Person 1: employment -> gap -> employment pattern
    "2023-01-01", "2023-06-01", # Gap from Apr to May creates unemployment
    # Person 2: employment -> gap -> employment pattern  
    "2023-02-01", "2023-07-01", # Gap from May to Jun creates unemployment
    # Add some additional records
    "2023-03-01", "2023-08-01"
  )),
  FINE = as.Date(c(
    # Person 1
    "2023-03-31", "2023-08-31", # Creates gap 2023-04-01 to 2023-05-31
    # Person 2
    "2023-04-30", "2023-09-30", # Creates gap 2023-05-01 to 2023-06-30
    # Additional
    "2023-05-31", "2023-10-31"
  )),
  prior = c(1, 1, 1, 1, 0, 0),
  # Transition columns (what we're analyzing)
  company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD", "CompanyE", "CompanyF"),
  sector = c("Tech", "Finance", "Health", "Tech", "Finance", "Education"),
  # Other columns (should get mode/weighted mean calculations)
  salary = c(50000, 60000, 55000, 70000, 35000, 40000),
  hours_per_week = c(40, 45, 40, 45, 25, 30),
  region = c("North", "North", "South", "North", "Central", "Central"),
  contract_type = c("Permanent", "Permanent", "Permanent", "Permanent", "Temp", "Temp")
)

print("Sample employment data:")
print(employment_data)

# Apply vecshift transformation first (mimicking pipeline)
library(vecshift)  # Load the package
result <- vecshift(employment_data, classify_status = FALSE)

# Manually merge the additional columns to simulate pipeline result
# (In a real pipeline, this would be done by the merge functionality)
additional_cols <- employment_data[, .(id, company, sector, salary, hours_per_week, region, contract_type)]
result <- merge(result, additional_cols, by = "id", all.x = TRUE)

# Add the merged columns attribute to simulate pipeline result
attr(result, "merged_columns") <- c("company", "sector", "salary", "hours_per_week", "region", "contract_type")

print("\nVecshift result (first few rows):")
print(head(result))

print("\nColumns in result:")
print(names(result))

# Test the modified function
print("\n=== Testing analyze_employment_transitions with other columns ===")

# Test with transition_columns specified
transitions <- analyze_employment_transitions(
  pipeline_result = result,
  transition_columns = c("company", "sector"),
  min_unemployment_duration = 1,
  return_list = FALSE,
  show_progress = TRUE
)

print("\nTransitions analysis result:")
print(transitions)

print("\nColumn names in transitions result:")
print(names(transitions))

# Expected additional columns:
# salary_from_mean, salary_to_mean (numeric)
# hours_per_week_from_mean, hours_per_week_to_mean (numeric) 
# region_from_mode, region_to_mode (character)
# contract_type_from_mode, contract_type_to_mode (character)

print("\n=== Testing with return_list = TRUE ===")

transitions_list <- analyze_employment_transitions(
  pipeline_result = result,
  transition_columns = c("company"),
  min_unemployment_duration = 1,
  return_list = TRUE,
  show_progress = FALSE
)

print("\nTransitions list result for 'company':")
print(transitions_list$company)

if (!is.null(transitions_list$company) && nrow(transitions_list$company) > 0) {
  print("\nColumn names in company transitions:")
  print(names(transitions_list$company))
}