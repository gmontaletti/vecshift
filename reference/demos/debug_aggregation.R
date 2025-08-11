# Debug the aggregation step specifically

library(data.table)
library(vecshift)
library(collapse)

# Simple test case - exactly like in the function
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

# Simulate exactly what happens in the function
dt <- copy(result)
setorder(dt, cf, inizio)

transition_columns <- "company"
other_columns <- "salary"

# Add shift operations exactly as in function
dt[, `:=`(
  prev_arco = shift(arco, n = 1, type = "lag"),
  next_arco = shift(arco, n = 1, type = "lead")
), by = cf]

# Add from/to values for transition columns
for (col in transition_columns) {
  dt[, paste0("from_", col) := shift(get(col), n = 1, type = "lag"), by = cf]
  dt[, paste0("to_", col) := shift(get(col), n = 1, type = "lead"), by = cf]
}

# Add from/to values for other columns
for (col in other_columns) {
  dt[, paste0("from_", col) := shift(get(col), n = 1, type = "lag"), by = cf]
  dt[, paste0("to_", col) := shift(get(col), n = 1, type = "lead"), by = cf]
}

dt[, from_durata := shift(durata, n = 1, type = "lag"), by = cf]
dt[, to_durata := shift(durata, n = 1, type = "lead"), by = cf]

# Identify transitions
dt[, is_transition_unemployment := (
  !is.na(prev_arco) & prev_arco >= 1 &
  !is.na(arco) & arco == 0 &
  !is.na(next_arco) & next_arco >= 1
)]

transition_periods <- dt[is_transition_unemployment == TRUE]

# Create col_subset exactly as in function
col <- "company"
from_col <- paste0("from_", col)
to_col <- paste0("to_", col)

base_cols <- c("cf", "durata", from_col, to_col, "from_durata", "to_durata")
other_from_cols <- paste0("from_", other_columns)
other_to_cols <- paste0("to_", other_columns)
all_other_cols <- c(other_from_cols, other_to_cols)
existing_other_cols <- intersect(all_other_cols, names(transition_periods))

col_subset <- transition_periods[, c(base_cols, existing_other_cols), with = FALSE]
setnames(col_subset, c(from_col, to_col), c("from", "to"))
col_subset[, variable := col]
col_subset <- col_subset[!is.na(from) & !is.na(to)]

col_data <- col_subset

print("col_data for aggregation:")
print(col_data)
print("col_data columns:")
print(names(col_data))

# Test base aggregation
base_agg <- col_data[, .(
  weight = .N,
  transition_duration = fmean(durata),
  from_mode = fmode(from),
  to_mode = fmode(to)
), by = .(from, to)]

print("Base aggregation result:")
print(base_agg)

# Test the other column condition and aggregation
other_col <- "salary"
other_from_col <- "from_salary"
other_to_col <- "to_salary"

print(paste("Length other_columns:", length(other_columns)))
print(paste("Other columns:", paste(other_columns, collapse = ", ")))
print(paste("other_from_col in names(col_data):", other_from_col %in% names(col_data)))
print(paste("other_to_col in names(col_data):", other_to_col %in% names(col_data)))
print(paste("is.numeric(salary):", is.numeric(result[[other_col]])))

condition_result <- (other_from_col %in% names(col_data) && other_to_col %in% names(col_data))
print(paste("Overall condition result:", condition_result))

if (condition_result) {
  print("Condition TRUE - attempting aggregation...")
  
  # Replicate the exact aggregation logic
  other_agg <- col_data[, .(
    salary_from_mean = {
      w <- as.numeric(from_durata[!is.na(get(other_from_col)) & !is.na(from_durata)])
      v <- as.numeric(get(other_from_col))[!is.na(get(other_from_col)) & !is.na(from_durata)]
      if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) weighted.mean(v, w = w) else fmean(as.numeric(get(other_from_col)))
    },
    salary_to_mean = {
      w <- as.numeric(to_durata[!is.na(get(other_to_col)) & !is.na(to_durata)])
      v <- as.numeric(get(other_to_col))[!is.na(get(other_to_col)) & !is.na(to_durata)]
      if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) weighted.mean(v, w = w) else fmean(as.numeric(get(other_to_col)))
    }
  ), by = .(from, to)]
  
  print("Other aggregation result:")
  print(other_agg)
  
  # Test merge
  final_agg <- merge(base_agg, other_agg, by = c("from", "to"), all.x = TRUE)
  print("Final merged aggregation:")
  print(final_agg)
  print("Final aggregation column names:")
  print(names(final_agg))
} else {
  print("Condition FALSE - aggregation skipped")
}