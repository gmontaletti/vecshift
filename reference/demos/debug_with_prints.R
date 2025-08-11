# Create a debug version of the function with print statements

library(data.table)
library(vecshift)

# Simple test data
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

# Replicate the exact function steps
pipeline_result <- result
transition_columns <- "company"

print("=== DEBUGGING FUNCTION STEPS ===")

# Input validation (skip most of this)
dt <- copy(pipeline_result)
setorder(dt, cf, inizio)

# Determine other columns
standard_cols <- c("cf", "inizio", "fine", "arco", "prior", "durata", "id", "stato", 
                   "collapsed", "n_periods")
other_columns <- setdiff(names(pipeline_result), c(transition_columns, standard_cols))

print(paste("transition_columns:", paste(transition_columns, collapse = ", ")))
print(paste("other_columns:", paste(other_columns, collapse = ", ")))
print(paste("length(other_columns):", length(other_columns)))

# Add shift operations
dt[, `:=`(
  prev_arco = shift(arco, n = 1, type = "lag"),
  next_arco = shift(arco, n = 1, type = "lead"),
  next_next_arco = shift(arco, n = 2, type = "lead")
), by = cf]

# Add from/to values for transition columns
for (col in transition_columns) {
  dt[, paste0("from_", col) := shift(get(col), n = 1, type = "lag"), by = cf]
  dt[, paste0("to_", col) := shift(get(col), n = 1, type = "lead"), by = cf]
}

# Add from/to values for other columns
print(paste("Processing other_columns loop with", length(other_columns), "columns"))
for (col in other_columns) {
  print(paste("Processing other column:", col))
  dt[, paste0("from_", col) := shift(get(col), n = 1, type = "lag"), by = cf]
  dt[, paste0("to_", col) := shift(get(col), n = 1, type = "lead"), by = cf]
}

dt[, from_durata := shift(durata, n = 1, type = "lag"), by = cf]
dt[, to_durata := shift(durata, n = 1, type = "lead"), by = cf]

# Identify transitions
dt[, is_transition_unemployment := (
  !is.na(prev_arco) & prev_arco >= 1 &
  !is.na(arco) & arco == 0 &
  !is.na(next_arco) & next_arco >= 1 &
  !is.na(durata) & durata >= 1
)]

transition_periods <- dt[is_transition_unemployment == TRUE]

print(paste("Found", nrow(transition_periods), "transition periods"))
print("transition_periods columns:")
print(names(transition_periods))

# Process columns separately 
transitions_long_list <- list()

for (i in seq_along(transition_columns)) {
  col <- transition_columns[i]
  print(paste("Processing transition column", i, ":", col))
  
  from_col <- paste0("from_", col)
  to_col <- paste0("to_", col)
  
  base_cols <- c("cf", "durata", from_col, to_col, "from_durata", "to_durata")
  
  # Add other columns' from/to values
  other_from_cols <- paste0("from_", other_columns)
  other_to_cols <- paste0("to_", other_columns)
  all_other_cols <- c(other_from_cols, other_to_cols)
  
  print(paste("other_from_cols:", paste(other_from_cols, collapse = ", ")))
  print(paste("other_to_cols:", paste(other_to_cols, collapse = ", ")))
  print(paste("all_other_cols:", paste(all_other_cols, collapse = ", ")))
  
  # Filter to only include columns that exist in the dataset
  existing_other_cols <- intersect(all_other_cols, names(transition_periods))
  
  print(paste("existing_other_cols:", paste(existing_other_cols, collapse = ", ")))
  
  # Create subset with transition column data and all other columns
  all_cols <- c(base_cols, existing_other_cols)
  print(paste("all_cols for subset:", paste(all_cols, collapse = ", ")))
  
  col_subset <- transition_periods[, all_cols, with = FALSE]
  
  print("col_subset before renaming:")
  print(col_subset)
  print("col_subset columns before renaming:")
  print(names(col_subset))
  
  # Rename main transition columns for consistent processing
  setnames(col_subset, c(from_col, to_col), c("from", "to"))
  
  # Add variable identifier
  col_subset[, variable := col]
  
  # Remove rows where either from or to is NA
  col_subset <- col_subset[!is.na(from) & !is.na(to)]
  
  print("col_subset after processing:")
  print(col_subset)
  print("col_subset columns after processing:")
  print(names(col_subset))
  
  transitions_long_list[[col]] <- col_subset
}

# Combine all columns
transitions_long <- rbindlist(transitions_long_list, use.names = TRUE, fill = TRUE)

print("transitions_long after rbindlist:")
print(transitions_long)
print("transitions_long columns:")
print(names(transitions_long))

# Process aggregation for the transition column
col <- transition_columns[1]
col_data <- transitions_long[variable == col]

print(paste("Processing aggregation for column:", col))
print("col_data for aggregation:")
print(col_data)
print("col_data columns:")
print(names(col_data))

# Check other columns condition
print(paste("length(other_columns):", length(other_columns)))
if (length(other_columns) > 0) {
  print("other_columns length > 0, processing other columns...")
  
  for (other_col in other_columns) {
    print(paste("Processing other_col:", other_col))
    
    other_from_col <- paste0("from_", other_col)
    other_to_col <- paste0("to_", other_col)
    
    print(paste("Looking for", other_from_col, "and", other_to_col))
    print(paste("other_from_col in names(col_data):", other_from_col %in% names(col_data)))
    print(paste("other_to_col in names(col_data):", other_to_col %in% names(col_data)))
    
    condition_met <- (other_from_col %in% names(col_data) && other_to_col %in% names(col_data))
    print(paste("Condition met:", condition_met))
    
    if (condition_met) {
      print("Condition TRUE - should process aggregation")
      
      is_numeric_col <- is.numeric(pipeline_result[[other_col]])
      print(paste("is.numeric(", other_col, "):", is_numeric_col))
      
      # Try the actual aggregation
      library(collapse)
      
      print("Attempting base aggregation...")
      base_agg <- col_data[, .(
        weight = .N,
        transition_duration = fmean(durata),
        from_mode = fmode(from),
        to_mode = fmode(to)
      ), by = .(from, to)]
      
      print("Base aggregation result:")
      print(base_agg)
      
      print("Attempting other column aggregation...")
      
      # Test the exact aggregation logic from the function with corrected syntax
      from_col_name <- paste0(other_col, "_from_mean")
      to_col_name <- paste0(other_col, "_to_mean")
      
      other_agg <- col_data[, {
        from_value <- {
          w <- as.numeric(from_durata[!is.na(get(other_from_col)) & !is.na(from_durata)])
          v <- as.numeric(get(other_from_col))[!is.na(get(other_from_col)) & !is.na(from_durata)]
          if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) weighted.mean(v, w = w) else fmean(as.numeric(get(other_from_col)))
        }
        to_value <- {
          w <- as.numeric(to_durata[!is.na(get(other_to_col)) & !is.na(to_durata)])
          v <- as.numeric(get(other_to_col))[!is.na(get(other_to_col)) & !is.na(to_durata)]
          if(length(w) > 0 && sum(w, na.rm = TRUE) > 0) weighted.mean(v, w = w) else fmean(as.numeric(get(other_to_col)))
        }
        # Return as a named list
        result <- list(from_value, to_value)
        names(result) <- c(from_col_name, to_col_name)
        result
      }, by = .(from, to)]
      
      print("Other aggregation result:")
      print(other_agg)
      
      # Test the merge
      print("Attempting merge...")
      final_result <- merge(base_agg, other_agg, by = c("from", "to"), all.x = TRUE)
      
      print("Final merged result:")
      print(final_result)
      print("Final merged result columns:")
      print(names(final_result))
      
    } else {
      print("Condition FALSE - skipping aggregation")
    }
  }
} else {
  print("other_columns length is 0, skipping other columns processing")
}