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
  route_column = "ROUTE",
  time_end_column = NULL,
  nca_exclude_reason_columns = NULL
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

- time_end_column:

  (Optional) A character string specifying the column for end time, used
  to calculate `CONCDUR`.

- nca_exclude_reason_columns:

  A character vector specifying the columns to indicate reasons for
  excluding records from NCA analysis.

## Value

A data frame containing the filtered and processed concentration data.
The following new columns are created:

- `DOSNOA`: Sequential dose number within each group, based on time of
  dose.

- `std_route`: Standardized route, either "intravascular" or
  "extravascular", derived from the route column.

- `nca_exclude`: Character column with concatenated exclusion reasons
  (if `nca_exclude_reason_columns` is provided), otherwise empty string.

- `CONCDUR`: (Optional) Sampling duration, calculated as
  `time_end_column - time_column` if `time_end_column` is provided.

## Details

The function performs the following steps:

- Checks for required columns and data.

- Filters out rows with EVID = 0 and PARAMCD containing "DOSE" (removes
  dosing data; not CDISC standard).

- Optionally creates `CONCDUR` if `time_end_column` is provided.

- Optionally creates `nca_exclude` by merging specified exclusion reason
  columns.

- Creates `DOSNOA` variable: sequential numbers based on time of dose
  within each group.

- Creates a `std_route` column with PKNCA values "intravascular" or
  "extravascular" based on the route column (ROUTE, CDISC: C66729).

- Arranges the data by group_columns and dose time.

## Examples

``` r
adnca <- read.csv(system.file("shiny/data/example-ADNCA.csv", package = "aNCA"))
conc_data <- format_pkncaconc_data(
  ADNCA = adnca,
  group_columns = c("STUDYID", "DOSETRT", "USUBJID", "PARAM"),
  time_column = "AFRLT",
  rrlt_column = "ARRLT",
  route_column = "ROUTE"
)
```
