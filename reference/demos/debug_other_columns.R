# Debug script to check the other columns logic

library(data.table)
library(vecshift)

# Simple test case
employment_data <- data.table(
  id = 1:4,
  cf = c("PERSON001", "PERSON001", "PERSON002", "PERSON002"),
  INIZIO = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01", "2023-07-01")),
  FINE = as.Date(c("2023-03-31", "2023-08-31", "2023-04-30", "2023-09-30")),
  prior = c(1, 1, 1, 1),
  company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD"),
  salary = c(50000, 60000, 55000, 70000)
)

print("Original data:")
print(employment_data)

# Apply vecshift
result <- vecshift(employment_data, classify_status = FALSE)

# Merge additional columns
additional_cols <- employment_data[, .(id, company, salary)]
result <- merge(result, additional_cols, by = "id", all.x = TRUE)

print("\nResult after merge:")
print(result)

# Set transition_columns and other_columns manually for debugging
transition_columns <- "company"
standard_cols <- c("cf", "inizio", "fine", "arco", "prior", "durata", "id", "stato", 
                   "collapsed", "n_periods")
other_columns <- setdiff(names(result), c(transition_columns, standard_cols))

print(paste("\nTransition columns:", paste(transition_columns, collapse = ", ")))
print(paste("Other columns:", paste(other_columns, collapse = ", ")))
print(paste("All result columns:", paste(names(result), collapse = ", ")))

# Check if unemployment periods exist
print("\nUnemployment periods (arco = 0):")
print(result[arco == 0])

# Check transitions manually
dt <- copy(result)
setorder(dt, cf, inizio)

# Add shift operations
dt[, `:=`(
  prev_arco = shift(arco, n = 1, type = "lag"),
  next_arco = shift(arco, n = 1, type = "lead")
), by = cf]

# Add from/to values
dt[, from_company := shift(company, n = 1, type = "lag"), by = cf]
dt[, to_company := shift(company, n = 1, type = "lead"), by = cf]
dt[, from_salary := shift(salary, n = 1, type = "lag"), by = cf]
dt[, to_salary := shift(salary, n = 1, type = "lead"), by = cf]

print("\nData with shift operations:")
print(dt[, .(cf, inizio, fine, arco, prev_arco, next_arco, company, from_company, to_company, salary, from_salary, to_salary)])

# Identify transitions
dt[, is_transition_unemployment := (
  !is.na(prev_arco) & prev_arco >= 1 &
  !is.na(arco) & arco == 0 &
  !is.na(next_arco) & next_arco >= 1
)]

print("\nTransition unemployment periods:")
print(dt[is_transition_unemployment == TRUE])

# Test the function directly - set transition_columns to only company, leaving salary as "other"
# Don't set merged_columns attribute, let the function work with specified transition_columns

print("\n=== Testing analyze_employment_transitions ===")
transitions <- analyze_employment_transitions(
  pipeline_result = result,
  transition_columns = "company",
  min_unemployment_duration = 1,
  return_list = FALSE,
  show_progress = TRUE
)

print("\nTransitions result:")
print(transitions)
print("\nColumns in transitions:")
print(names(transitions))