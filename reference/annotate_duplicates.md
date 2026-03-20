# Annotate Duplicate Concentration Records

Detects and annotates duplicate records in mapped ADNCA data. Exact
duplicates (same AVAL and time point within a group) are marked as
`DTYPE = "COPY"`. Time duplicates (same time point but different AVAL)
can be resolved via `time_duplicate_rows`.

## Usage

``` r
annotate_duplicates(dataset, time_duplicate_rows = NULL)
```

## Arguments

- dataset:

  A mapped data frame with standard ADNCA columns.

- time_duplicate_rows:

  Optional integer vector of row indices (in the mapped dataset) to mark
  as `"TIME DUPLICATE"`. These indices refer to row positions after
  mapping (1-based). Row order is preserved through `group_by`/`mutate`
  operations. When `NULL` and time duplicates are detected, an error is
  raised.

## Value

The dataset with a `DTYPE` column added. Raises an error of class
`"time_duplicate_error"` if unresolved time duplicates are found, with
the duplicate rows attached as `duplicate_data` in the condition.
