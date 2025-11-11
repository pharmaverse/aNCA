# Create duplicates in concentration data with Pre-dose and Last Values for Dosing Cycles

This function duplicates and adjusts concentration data to ensure all
dosing cycles have complete pre-dose and last concentration values. It
is designed for use in pharmacokinetic analyses where dosing intervals
and concentration values need to be aligned for each dose.

## Usage

``` r
dose_profile_duplicates(
  conc_data,
  groups = c("USUBJID", "DOSNOA", "PARAM"),
  dosno = "DOSNOA",
  arrlt = "ARRLT",
  afrlt = "AFRLT",
  nrrlt = "NRRLT",
  nfrlt = "NFRLT"
)
```

## Arguments

- conc_data:

  A data frame containing concentration data.

- groups:

  A character vector of column names to use for grouping (e.g.,
  c("USUBJID", "PARAM", "DOSNOA")).

- dosno:

  Column name for the dose number (default: "DOSNOA").

- arrlt:

  Column name for time from the most recent dose.

- afrlt:

  Column name for time from the first dose.

- nrrlt:

  Column name for the numeric relative time.

- nfrlt:

  Column name for the nominal relative time.

## Value

A data frame with adjusted concentration data, including:

- Duplicated pre-dose values assigned to the previous dose.

- Duplicated last values assigned to the next dose if pre-dose values
  are missing, filtered so only samples within 4 hours of the next dose
  are included.

- Sorted by the grouping variables and relative time.

## Examples

``` r
# Example usage
conc_data <- data.frame(
USUBJID = c("001", "001", "001", "001", "001", "001", "001", "001", "001", "001"),
AVAL = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
DOSNOA = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3),
ARRLT = c(-1, 0, 1, -1, 0, 1, 2, 0, 1, 2),
AFRLT = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8),
NRRLT = c(-1, 0, 1, -1, 0, 1, 2, 0, 1, 2),
NFRLT = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8)
)
result <- dose_profile_duplicates(conc_data,
                            groups = c("USUBJID", "DOSNOA"), dosno = "DOSNOA")
```
