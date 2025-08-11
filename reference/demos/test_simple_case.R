# Test very simple case to debug the other columns issue

library(data.table)
library(vecshift)

# Very simple test case
employment_data <- data.table(
  id = 1:4,
  cf = c("PERSON001", "PERSON001", "PERSON002", "PERSON002"),
  INIZIO = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01", "2023-07-01")),
  FINE = as.Date(c("2023-03-31", "2023-08-31", "2023-04-30", "2023-09-30")),
  prior = c(1, 1, 1, 1),
  company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD"),
  salary = c(50000, 60000, 55000, 70000)
)

result <- vecshift(employment_data, classify_status = FALSE)
additional_cols <- employment_data[, .(id, company, salary)]
result <- merge(result, additional_cols, by = "id", all.x = TRUE)

print("Result data:")
print(result)

# Test the function with explicit transition_columns, salary should be "other"
print("\n=== Testing with company as transition column, salary as other ===")
transitions <- analyze_employment_transitions(
  pipeline_result = result,
  transition_columns = "company",  # Only company, so salary should be "other"
  min_unemployment_duration = 1,
  return_list = FALSE,
  show_progress = TRUE
)

print("\nResult:")
print(transitions)
print("Column names:")
print(names(transitions))

# We should see: company transitions plus salary_from_mean and salary_to_mean