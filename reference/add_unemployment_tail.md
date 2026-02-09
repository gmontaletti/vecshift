# Add Unemployment Periods at the End of Employment Histories (Legacy)

This function is maintained for backward compatibility. It calls the new
\codeadd_unemployment_periods() function with \codeadd_tail = TRUE and
\codeadd_head = FALSE.

## Usage

``` r
add_unemployment_tail(vecshift_data, max_date, min_tail_duration = 1L)
```

## Arguments

- vecshift_data:

  A data.table output from vecshift()

- max_date:

  Date or numeric. The end date for unemployment tails

- min_tail_duration:

  Integer. Minimum duration for unemployment tails (default: 1)

## Value

A data.table with unemployment tail periods added

## See also

\code\linkadd_unemployment_periods for the enhanced version with
head/tail options
