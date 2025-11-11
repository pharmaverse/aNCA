# Create PK Concentration Dataset

This function creates a pharmacokinetic concentration dataset from the
provided ADNCA data.

## Usage

``` r
format_pkncaconc_data(
  ADNCA,
  group_columns,
  time_column = "AFRLT",
  rrlt_column = "ARRLT",
  route_column = "ROUTE"
)
```

## Arguments

- ADNCA:

  A data frame containing the ADNCA data.

- group_columns:

  A character vector specifying the columns to group by.

- time_column:

  A character string specifying the time column.

- rrlt_column:

  A character string specifying the time since last dose column.

- route_column:

  A character string specifying the route column.

## Value

A data frame containing the filtered and processed concentration data.

## Details

The function performs the following steps:

- Checks for required columns and data.

- Filters out rows with EVID = 0 and PARAMCD containing "DOSE" (dosing
  data- not CDISC standard)

- Creates `DOSNOA` variable, sequential numbers based on time of dose

- Creates a 'std_route' column with PKNCA values "intravascular" or
  "extravascular" based on route_column (ROUTE, CDISC: C66729).

- Arranges the data by group_columns.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example usage:
  conc_data <- format_pkncaconc_data(ADNCA,
                                     group_columns,
                                     "AFRLT",
                                     "ROUTE")
} # }
```
