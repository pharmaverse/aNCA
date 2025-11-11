# Create PK Dose Dataset

This function creates a pharmacokinetic dose dataset from the provided
concentration data.

## Usage

``` r
format_pkncadose_data(
  pkncaconc_data,
  time_column = "AFRLT",
  rrlt_column = "ARRLT",
  group_columns
)
```

## Arguments

- pkncaconc_data:

  A data frame containing the concentration data.

- time_column:

  A character string specifying the time from first dose column.

- rrlt_column:

  A character string specifying the time since last dose column.

- group_columns:

  A character vector specifying the columns to group by.

## Value

A data frame containing the dose data.

## Details

The function performs the following steps:

- Arranges and groups the data by group_columns

- Selects the first row within each group (arranged by DOSNOA- a
  variable created in `format_pkncaconc_data`)

Note\*: This function is designed to work with the output of
`format_pkncaconc_data`.
