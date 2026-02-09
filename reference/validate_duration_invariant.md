# Validate Duration Calculation Invariant

Validates that the mathematical invariant holds: for each person,
sum(durata) should equal (max(fine) - min(inizio) + 1). This ensures
duration calculations are consistent.

## Usage

``` r
validate_duration_invariant(
  dt,
  person_col = "cf",
  start_col = "inizio",
  end_col = "fine"
)
```

## Arguments

- dt:

  Data.table with cf, inizio, fine, and durata columns

- person_col:

  Name of person identifier column (default: "cf")

- start_col:

  Name of start date column (default: "inizio")

- end_col:

  Name of end date column (default: "fine")

## Value

List with duration validation results
