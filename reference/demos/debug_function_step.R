# Debug specific step of the function

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

result <- vecshift(employment_data, classify_status = FALSE)
additional_cols <- employment_data[, .(id, company, salary)]
result <- merge(result, additional_cols, by = "id", all.x = TRUE)

# Replicate the function logic step by step
transition_columns <- "company"
standard_cols <- c("cf", "inizio", "fine", "arco", "prior", "durata", "id", "stato", 
                   "collapsed", "n_periods")
other_columns <- setdiff(names(result), c(transition_columns, standard_cols))

print(paste("transition_columns:", paste(transition_columns, collapse = ", ")))
print(paste("other_columns:", paste(other_columns, collapse = ", ")))

dt <- copy(result)
setorder(dt, cf, inizio)

# Add shift operations
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

print("transition_periods columns:")
print(names(transition_periods))

print("transition_periods data:")
print(transition_periods)

# Now check the col_subset creation
col <- transition_columns[1]  # "company"
from_col <- paste0("from_", col)
to_col <- paste0("to_", col)

base_cols <- c("cf", "durata", from_col, to_col, "from_durata", "to_durata")
other_from_cols <- paste0("from_", other_columns)
other_to_cols <- paste0("to_", other_columns)
all_other_cols <- c(other_from_cols, other_to_cols)

print(paste("base_cols:", paste(base_cols, collapse = ", ")))
print(paste("other_from_cols:", paste(other_from_cols, collapse = ", ")))
print(paste("other_to_cols:", paste(other_to_cols, collapse = ", ")))
print(paste("all_other_cols:", paste(all_other_cols, collapse = ", ")))

existing_other_cols <- intersect(all_other_cols, names(transition_periods))
print(paste("existing_other_cols:", paste(existing_other_cols, collapse = ", ")))

col_subset <- transition_periods[, c(base_cols, existing_other_cols), with = FALSE]
print("col_subset columns:")
print(names(col_subset))
print("col_subset data:")
print(col_subset)

# Continue the processing as in the function
setnames(col_subset, c(from_col, to_col), c("from", "to"))
col_subset[, variable := col]
col_subset <- col_subset[!is.na(from) & !is.na(to)]

print("col_subset after processing:")
print(col_subset)
print("col_subset columns after processing:")
print(names(col_subset))

# Now test the aggregation logic
transitions_long <- col_subset
col_data <- transitions_long[variable == "company"]

print("col_data:")
print(col_data)
print("col_data columns:")
print(names(col_data))

# Check the condition that's failing
other_col <- "salary"
other_from_col <- "from_salary"
other_to_col <- "to_salary"

print(paste("Checking if", other_from_col, "in names(col_data):", other_from_col %in% names(col_data)))
print(paste("Checking if", other_to_col, "in names(col_data):", other_to_col %in% names(col_data)))

# Test the aggregation manually
library(collapse)

# Test base aggregation (this should work)
base_agg <- col_data[, .(
  weight = .N,
  transition_duration = fmean(durata),
  from_mode = fmode(from),
  to_mode = fmode(to)
), by = .(from, to)]

print("Base aggregation:")
print(base_agg)

# Test other column aggregation (this is where the issue might be)
other_agg <- col_data[, .(
  salary_from_mean = {
    w <- from_durata[!is.na(get(other_from_col)) & !is.na(from_durata)]
    v <- as.numeric(get(other_from_col))[!is.na(get(other_from_col)) & !is.na(from_durata)]
    if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) weighted.mean(v, w = w) else fmean(as.numeric(get(other_from_col)))
  },
  salary_to_mean = {
    w <- to_durata[!is.na(get(other_to_col)) & !is.na(to_durata)]
    v <- as.numeric(get(other_to_col))[!is.na(get(other_to_col)) & !is.na(to_durata)]
    if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) weighted.mean(v, w = w) else fmean(as.numeric(get(other_to_col)))
  }
), by = .(from, to)]

print("Other aggregation:")
print(other_agg)

# Test merge
final_agg <- merge(base_agg, other_agg, by = c("from", "to"), all.x = TRUE)
print("Final merged aggregation:")
print(final_agg)