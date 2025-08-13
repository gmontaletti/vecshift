# Debug script for over_id functionality

library(data.table)

# Load functions from source
source("R/status_labeling.R")
source("R/vecshift.R")

# Test with simple overlapping data
test_data <- data.table(
  id = 1:2,
  cf = c("A", "A"),
  INIZIO = as.Date(c("2023-01-01", "2023-06-01")),
  FINE = as.Date(c("2023-08-31", "2023-10-31")), 
  prior = c(1, 1)
)

cat("Input data:\n")
print(test_data)

# Test vecshift with detailed debugging
cat("\n=== Testing vecshift with classify_status = FALSE ===\n")
result_no_status <- vecshift(test_data, classify_status = FALSE)
print(names(result_no_status))
print(result_no_status)

cat("\n=== Testing vecshift with classify_status = TRUE ===\n")
result_with_status <- vecshift(test_data, classify_status = TRUE)
print(names(result_with_status))  
print(result_with_status)

# Check if over_id exists in either case
if ("over_id" %in% names(result_no_status)) {
  cat("✅ over_id found in result WITHOUT status classification\n")
} else {
  cat("❌ over_id NOT found in result WITHOUT status classification\n")
}

if ("over_id" %in% names(result_with_status)) {
  cat("✅ over_id found in result WITH status classification\n")
} else {
  cat("❌ over_id NOT found in result WITH status classification\n")
}