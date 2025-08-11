# Test merge_original_columns functionality

library(data.table)

test_that("merge_original_columns works with single column", {
  # Arrange
  original_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-07-01")),
    FINE = as.Date(c("2023-03-31", "2023-12-31")),
    prior = c(1L, 0L),
    company = c("CompanyA", "CompanyB")
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  
  # Act
  result <- merge_original_columns(original_data, segments, "company")
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_true("company" %in% names(result))
  
  # Check that employment segments have company info
  employment_segments <- result[id > 0]
  expect_true(all(!is.na(employment_segments$company)))
  
  # Check that unemployment segments have NA company
  if (any(result$id == 0)) {
    unemployment_segments <- result[id == 0]
    expect_true(all(is.na(unemployment_segments$company)))
  }
})

test_that("merge_original_columns works with multiple columns", {
  # Arrange
  original_data <- data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON002"),
    INIZIO = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
    FINE = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
    prior = c(1L, 0L, 1L),
    company = c("CompanyA", "CompanyB", "CompanyC"),
    salary = c(50000, 25000, 60000),
    department = c("IT", "HR", "Finance")
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  
  # Act
  result <- merge_original_columns(
    original_data, 
    segments, 
    c("company", "salary", "department")
  )
  
  # Assert
  expect_s3_class(result, "data.table")
  expect_true(all(c("company", "salary", "department") %in% names(result)))
  
  # Check that all employment segments have complete information
  employment_segments <- result[id > 0]
  expect_equal(nrow(employment_segments[is.na(company)]), 0)
  expect_equal(nrow(employment_segments[is.na(salary)]), 0)
  expect_equal(nrow(employment_segments[is.na(department)]), 0)
})

test_that("merge_original_columns handles unemployment periods correctly", {
  # Arrange - create data that will generate unemployment periods
  original_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-07-01")),  # Gap: April-June
    FINE = as.Date(c("2023-03-31", "2023-12-31")),
    prior = c(1L, 1L),
    company = c("CompanyA", "CompanyB")
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  
  # Act
  result <- merge_original_columns(original_data, segments, "company")
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Check that we have unemployment periods
  expect_true(any(result$id == 0))
  
  # Check that unemployment periods have NA for company
  unemployment_segments <- result[id == 0]
  expect_true(all(is.na(unemployment_segments$company)))
  
  # Check that employment segments have company info
  employment_segments <- result[id > 0]
  expect_true(all(!is.na(employment_segments$company)))
})

test_that("merge_original_columns preserves temporal ordering", {
  # Arrange
  original_data <- data.table(
    id = c(3L, 1L, 2L),  # Deliberately out of order
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-09-01", "2023-01-01", "2023-05-01")),
    FINE = as.Date(c("2023-12-31", "2023-03-31", "2023-07-31")),
    prior = c(1L, 1L, 0L),
    company = c("CompanyC", "CompanyA", "CompanyB")
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  
  # Act
  result <- merge_original_columns(original_data, segments, "company")
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Check that result is ordered by cf and inizio
  expect_true(all(result$inizio == result[order(cf, inizio)]$inizio))
})

test_that("merge_original_columns validates input parameters", {
  # Arrange
  original_data <- data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2023-01-01"),
    FINE = as.Date("2023-12-31"),
    prior = 1L,
    company = "CompanyA"
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  
  # Act & Assert - Test various error conditions
  
  # Non-data.table inputs
  expect_error(
    merge_original_columns(as.data.frame(original_data), segments, "company"),
    "must be a data.table object"
  )
  
  expect_error(
    merge_original_columns(original_data, as.data.frame(segments), "company"),
    "must be a data.table object"
  )
  
  # Invalid columns parameter
  expect_error(
    merge_original_columns(original_data, segments, c()),
    "must be a non-empty character vector"
  )
  
  expect_error(
    merge_original_columns(original_data, segments, 123),
    "must be a non-empty character vector"
  )
  
  # Missing columns
  expect_error(
    merge_original_columns(original_data, segments, "nonexistent_column"),
    "Columns not found in original_data"
  )
  
  # Missing id column
  original_no_id <- copy(original_data)
  original_no_id[, id := NULL]
  expect_error(
    merge_original_columns(original_no_id, segments, "company"),
    "must contain an 'id' column"
  )
})

test_that("merge_original_columns handles different data types correctly", {
  # Arrange
  original_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-07-01")),
    FINE = as.Date(c("2023-03-31", "2023-12-31")),
    prior = c(1L, 0L),
    company = c("CompanyA", "CompanyB"),           # character
    salary = c(50000.5, 25000.75),                # numeric
    start_date = as.Date(c("2023-01-01", "2023-07-01")),  # Date
    is_permanent = c(TRUE, FALSE)                 # logical
  )
  
  # Create gap to generate unemployment
  original_data[1, FINE := as.Date("2023-05-31")]  # Creates unemployment gap
  
  segments <- vecshift(original_data, classify_status = FALSE)
  
  # Act
  result <- merge_original_columns(
    original_data, 
    segments, 
    c("company", "salary", "start_date", "is_permanent")
  )
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Check unemployment periods have appropriate NA types
  if (any(result$id == 0)) {
    unemployment <- result[id == 0]
    expect_true(all(is.na(unemployment$company)))
    expect_true(all(is.na(unemployment$salary)))
    expect_true(all(is.na(unemployment$start_date)))
    expect_true(all(is.na(unemployment$is_permanent)))
    
    # Verify NA types are correct
    expect_type(unemployment$company, "character")
    expect_type(unemployment$salary, "double")
    expect_s3_class(unemployment$start_date, "Date")
    expect_type(unemployment$is_permanent, "logical")
  }
})

test_that("merge_original_columns warns about column conflicts", {
  # Arrange
  original_data <- data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2023-01-01"),
    FINE = as.Date("2023-12-31"),
    prior = 1L,
    durata = 100  # This conflicts with segments$durata
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  
  # Act & Assert
  expect_warning(
    merge_original_columns(original_data, segments, "durata"),
    "Column name conflicts detected"
  )
})

# Tests for merge_overlapping_values function
test_that("merge_overlapping_values handles character columns correctly", {
  # Arrange - Create data with overlapping periods
  original_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01")),
    FINE = as.Date(c("2023-06-30", "2023-05-31")),  # Creates overlap
    prior = c(1L, 0L),
    company = c("CompanyA", "CompanyB")
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  with_columns <- merge_original_columns(original_data, segments, "company")
  
  # Act
  result <- merge_overlapping_values(with_columns, "company")
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Find overlapping segments (arco > 1)
  overlap_segments <- result[arco > 1]
  expect_gt(nrow(overlap_segments), 0)
  
  # Check that overlapping period has merged company names
  expect_true(any(grepl("CompanyA->CompanyB", overlap_segments$company)))
})

test_that("merge_overlapping_values handles numeric columns correctly", {
  # Arrange
  original_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01")),
    FINE = as.Date(c("2023-06-30", "2023-05-31")),  # Creates overlap
    prior = c(1L, 0L),
    salary = c(50000, 30000)
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  with_columns <- merge_original_columns(original_data, segments, "salary")
  
  # Act
  result <- merge_overlapping_values(with_columns, "salary")
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Find overlapping segments (arco > 1)
  overlap_segments <- result[arco > 1]
  expect_gt(nrow(overlap_segments), 0)
  
  # Check that overlapping period has summed salaries
  expect_true(any(overlap_segments$salary == 80000))  # 50000 + 30000
})

test_that("merge_overlapping_values handles multiple columns with different types", {
  # Arrange
  original_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01")),
    FINE = as.Date(c("2023-06-30", "2023-05-31")),  # Creates overlap
    prior = c(1L, 0L),
    company = c("CompanyA", "CompanyB"),
    salary = c(50000, 30000),
    department = c("IT", "HR")
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  with_columns <- merge_original_columns(original_data, segments, c("company", "salary", "department"))
  
  # Act
  result <- merge_overlapping_values(with_columns, c("company", "salary", "department"))
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Find overlapping segments (arco > 1)
  overlap_segments <- result[arco > 1]
  expect_gt(nrow(overlap_segments), 0)
  
  # Check character merging
  expect_true(any(grepl("CompanyA->CompanyB", overlap_segments$company)))
  expect_true(any(grepl("IT->HR", overlap_segments$department)))
  
  # Check numeric summing
  expect_true(any(overlap_segments$salary == 80000))
})

test_that("merge_overlapping_values handles factor columns", {
  # Arrange
  original_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01")),
    FINE = as.Date(c("2023-06-30", "2023-05-31")),  # Creates overlap
    prior = c(1L, 0L),
    level = factor(c("Junior", "Senior"))
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  with_columns <- merge_original_columns(original_data, segments, "level")
  
  # Act
  result <- merge_overlapping_values(with_columns, "level")
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Find overlapping segments (arco > 1)
  overlap_segments <- result[arco > 1]
  expect_gt(nrow(overlap_segments), 0)
  
  # Check factor merging (should be converted to character)
  expect_true(any(grepl("Junior->Senior", overlap_segments$level)))
})

test_that("merge_overlapping_values handles triple overlap (arco = 3)", {
  # Arrange - Three overlapping contracts
  original_data <- data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    FINE = as.Date(c("2023-12-31", "2023-12-31", "2023-12-31")),  # All end together
    prior = c(1L, 1L, 0L),
    company = c("CompanyA", "CompanyB", "CompanyC"),
    salary = c(50000, 30000, 20000)
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  with_columns <- merge_original_columns(original_data, segments, c("company", "salary"))
  
  # Act
  result <- merge_overlapping_values(with_columns, c("company", "salary"))
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Find maximum overlap segments (arco = 3)
  max_overlap_segments <- result[arco == 3]
  expect_gt(nrow(max_overlap_segments), 0)
  
  # Check chained character merging
  expect_true(any(grepl("CompanyA->CompanyB->CompanyC", max_overlap_segments$company)))
  
  # Check accumulated numeric summing
  expect_true(any(max_overlap_segments$salary == 100000))  # 50000 + 30000 + 20000
})

test_that("merge_overlapping_values leaves non-overlapping periods unchanged", {
  # Arrange - Mix of overlapping and non-overlapping periods
  original_data <- data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-08-01")),
    FINE = as.Date(c("2023-03-31", "2023-06-30", "2023-10-31")),  # No overlaps
    prior = c(1L, 0L, 1L),
    company = c("CompanyA", "CompanyB", "CompanyC")
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  with_columns <- merge_original_columns(original_data, segments, "company")
  
  # Act
  result <- merge_overlapping_values(with_columns, "company")
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Check that non-overlapping segments are unchanged
  non_overlap_segments <- result[arco <= 1 & id > 0]  # Exclude unemployment
  expect_true(all(non_overlap_segments$company %in% c("CompanyA", "CompanyB", "CompanyC")))
  expect_false(any(grepl("->", non_overlap_segments$company)))
})

test_that("merge_overlapping_values handles data with no overlaps", {
  # Arrange - No overlapping periods
  original_data <- data.table(
    id = c(1L, 2L),
    cf = c("PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-07-01")),  # No overlap
    FINE = as.Date(c("2023-03-31", "2023-09-30")),
    prior = c(1L, 0L),
    company = c("CompanyA", "CompanyB")
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  with_columns <- merge_original_columns(original_data, segments, "company")
  
  # Act
  result <- merge_overlapping_values(with_columns, "company")
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Check that result is identical to input (no overlaps to process)
  expect_equal(nrow(result), nrow(with_columns))
  expect_true(all(result$company[!is.na(result$company)] %in% c("CompanyA", "CompanyB")))
})

test_that("merge_overlapping_values validates input parameters", {
  # Arrange
  original_data <- data.table(
    id = 1L,
    cf = "PERSON001",
    INIZIO = as.Date("2023-01-01"),
    FINE = as.Date("2023-12-31"),
    prior = 1L,
    company = "CompanyA"
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  with_columns <- merge_original_columns(original_data, segments, "company")
  
  # Act & Assert - Test various error conditions
  
  # Non-data.table input
  expect_error(
    merge_overlapping_values(as.data.frame(with_columns), "company"),
    "must be a data.table object"
  )
  
  # Invalid columns parameter
  expect_error(
    merge_overlapping_values(with_columns, c()),
    "must be a non-empty character vector"
  )
  
  expect_error(
    merge_overlapping_values(with_columns, 123),
    "must be a non-empty character vector"
  )
  
  # Missing required columns
  test_data <- copy(with_columns)
  test_data[, cf := NULL]
  expect_error(
    merge_overlapping_values(test_data, "company"),
    "Missing required columns.*cf"
  )
  
  # Missing requested columns
  expect_error(
    merge_overlapping_values(with_columns, "nonexistent_column"),
    "Columns not found in segments_with_columns"
  )
})

test_that("merge_overlapping_values handles NA values correctly", {
  # Arrange - Include some NA values
  original_data <- data.table(
    id = c(1L, 2L, 3L),
    cf = c("PERSON001", "PERSON001", "PERSON001"),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-06-01")),
    FINE = as.Date(c("2023-08-31", "2023-07-31", "2023-09-30")),  # Creates overlaps
    prior = c(1L, 0L, 1L),
    company = c("CompanyA", NA_character_, "CompanyC"),
    salary = c(50000, 30000, NA_real_)
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  with_columns <- merge_original_columns(original_data, segments, c("company", "salary"))
  
  # Act
  result <- merge_overlapping_values(with_columns, c("company", "salary"))
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Check that NA values are handled gracefully (skipped)
  expect_true(any(is.na(result$company) | result$company == "CompanyA" | 
                  result$company == "CompanyC" | grepl("->", result$company)))
})

test_that("merge_overlapping_values processes multiple persons independently", {
  # Arrange - Two persons with different overlap patterns
  original_data <- data.table(
    id = c(1L, 2L, 3L, 4L),
    cf = c("PERSON001", "PERSON001", "PERSON002", "PERSON002"),
    INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-01-01", "2023-04-01")),
    FINE = as.Date(c("2023-06-30", "2023-05-31", "2023-06-30", "2023-05-31")),  # Both have overlaps
    prior = c(1L, 0L, 0L, 1L),
    company = c("CompanyA", "CompanyB", "CompanyX", "CompanyY"),
    salary = c(50000, 30000, 40000, 35000)
  )
  
  segments <- vecshift(original_data, classify_status = FALSE)
  with_columns <- merge_original_columns(original_data, segments, c("company", "salary"))
  
  # Act
  result <- merge_overlapping_values(with_columns, c("company", "salary"))
  
  # Assert
  expect_s3_class(result, "data.table")
  
  # Check that each person's overlaps are processed independently
  person1_overlaps <- result[cf == "PERSON001" & arco > 1]
  person2_overlaps <- result[cf == "PERSON002" & arco > 1]
  
  expect_gt(nrow(person1_overlaps), 0)
  expect_gt(nrow(person2_overlaps), 0)
  
  # Check that person-specific merges occurred
  if (nrow(person1_overlaps) > 0) {
    expect_true(any(grepl("CompanyA->CompanyB", person1_overlaps$company)))
    expect_true(any(person1_overlaps$salary == 80000))
  }
  
  if (nrow(person2_overlaps) > 0) {
    expect_true(any(grepl("CompanyX->CompanyY", person2_overlaps$company)))
    expect_true(any(person2_overlaps$salary == 75000))
  }
})