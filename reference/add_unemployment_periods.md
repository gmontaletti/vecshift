# Add Unemployment Periods at the Beginning and End of Employment Histories

Extends vecshift output by adding unemployment periods at the beginning
and/or end of employment histories. This creates complete temporal
coverage for analysis requiring uniform observation periods by:

- **Head**: Adding unemployment from min_date to first employment start

- **Tail**: Adding unemployment from last employment end to max_date

## Usage

``` r
add_unemployment_periods(
  vecshift_data,
  min_date = NULL,
  max_date = NULL,
  min_duration = 1L,
  add_head = FALSE,
  add_tail = TRUE
)
```

## Arguments

- vecshift_data:

  A data.table output from vecshift() containing temporal employment
  segments with required columns: cf, inizio, fine, arco, prior, id,
  over_id, durata. Optional: stato column for employment status.

- min_date:

  Date or numeric. The start date for unemployment heads (optional).
  Only used when add_head = TRUE. Individuals with first employment
  starting after this date will get unemployment heads added.

- max_date:

  Date or numeric. The end date for unemployment tails (optional). Only
  used when add_tail = TRUE. Individuals with last employment ending
  before this date will get unemployment tails added.

- min_duration:

  Integer. Minimum duration in days for unemployment periods to be
  added. Shorter periods are excluded (default: 1).

- add_head:

  Logical. Whether to add unemployment periods at the beginning of
  employment histories (default: FALSE). Requires min_date to be
  specified.

- add_tail:

  Logical. Whether to add unemployment periods at the end of employment
  histories (default: TRUE). Requires max_date to be specified.

## Value

A data.table with the same structure as input, extended with
unemployment tail segments where applicable. Maintains original ordering
by cf and temporal sequence.

## Details

The function can add unemployment periods at both ends of employment
histories:

**Head Unemployment** (when `add_head = TRUE`):

- Identifies individuals whose first employment starts after min_date

- Creates unemployment from min_date to (first_start - 1)

- Only added if duration \>= min_duration

**Tail Unemployment** (when `add_tail = TRUE`):

- Identifies individuals whose last employment ends before max_date

- Creates unemployment from (last_end + 1) to max_date

- Only added if duration \>= min_duration

All unemployment periods follow vecshift formatting:

- `arco = 0`: No active employment contracts

- `over_id = 0`: Unemployment period

- `id = 0`: No associated contract ID

- `prior = 0`: Standard for unemployment periods

- `durata`: Correctly calculated duration

- `stato = "disoccupato"`: Employment status (if present in input)

The function preserves the temporal invariant that elapsed time equals
sum of durations by person, ensuring consistency with vecshift's core
logic.

## Note

This function is optimized for performance using data.table operations
and follows vecshift coding conventions. It handles edge cases such as:

- Individuals already starting at/before min_date (no head added)

- Individuals already ending at/after max_date (no tail added)

- Very short unemployment periods (filtered by min_duration)

- Proper integration with over_id consolidation logic

- Maintains temporal ordering by cf and inizio

## See also

[`vecshift`](https://gmontaletti.github.io/vecshift/reference/vecshift.md)
for the main temporal transformation function
[`classify_employment_status`](https://gmontaletti.github.io/vecshift/reference/classify_employment_status.md)
for status classification

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
# Create sample employment data
dt <- data.table(
  id = 1:2,
  cf = c("ABC123", "DEF456"),
  INIZIO = as.Date(c("2023-03-01", "2023-06-01")),
  FINE = as.Date(c("2023-05-31", "2023-08-31")),
  prior = c(1, 0)
)

# Transform to vecshift format
result <- vecshift(dt)

# Add unemployment tails only (backward compatible)
extended_result <- add_unemployment_periods(
  result,
  max_date = as.Date("2024-12-31")
)

# Add unemployment heads only
extended_result <- add_unemployment_periods(
  result,
  min_date = as.Date("2022-01-01"),
  add_head = TRUE,
  add_tail = FALSE
)

# Add both head and tail unemployment
extended_result <- add_unemployment_periods(
  result,
  min_date = as.Date("2022-01-01"),
  max_date = as.Date("2024-12-31"),
  add_head = TRUE,
  add_tail = TRUE
)

# Check the added unemployment periods
print(extended_result[arco == 0])
} # }
```
