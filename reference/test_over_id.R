# Test to understand over_id behavior
library(data.table)
source("/Users/giampaolomontaletti/Documents/funzioni/vecshift/R/vecshift.R")
source("/Users/giampaolomontaletti/Documents/funzioni/vecshift/R/status_labeling.R")

# Create test data with overlapping employment periods
dt <- data.table(
  id = 1:4,
  cf = c("A", "A", "A", "A"),
  INIZIO = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-05-01")),
  FINE = as.Date(c("2023-03-15", "2023-04-30", "2023-04-15", "2023-06-30")),
  prior = c(1, 1, 1, 1)
)

print("Input data:")
print(dt)

# Run vecshift without status classification to see raw over_id
result <- vecshift(dt, classify_status = FALSE)
print("\nVecshift output with over_id:")
print(result)

# Print column names to verify over_id is there
print("\nColumn names:")
print(names(result))