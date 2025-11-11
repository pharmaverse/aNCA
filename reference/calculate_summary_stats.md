# Calculate Summary Statistics

This function calculates various summary statistics for formatted output
of PKNCA::pk.nca().

## Usage

``` r
calculate_summary_stats(data, input_groups = "ATPTREF")
```

## Arguments

- data:

  A data frame containing results of Non Compartmental Analysis using
  PKNCA package. Assumes presence of columns: PPORRES, PPSTRES,
  PPSTRESU, PPTESTCD

- input_groups:

  A character vector specifying the columns to group by. Here. the
  hierarchical order matters Default is "PPSTRESU".

## Value

A data frame with summary statistics for each group and parameter.

## Details

The function calculates the following statistics for numeric variables:

- Geometric mean (`geomean`)

- Geometric coefficient of variation (`geocv`)

- Arithmetic mean (`mean`)

- Standard deviation (`sd`)

- Minimum value (`min`)

- Maximum value (`max`)

- Median value (`median`)

- Count of missing values (`count.missing`)

- Count (`count`)

The resulting summary statistics are rounded to three decimal places. If
units are different, they are standardized to the group's most frequent
first unit.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(
ATPTREF = c(1, 1, 1, 1, 1, 1),
PPTESTCD = c("A", "A", "B", "B", "C", "C"),
PPSTRES = c(10, 20, 5, 15, NA, 30),
PPSTRESU = c("mg/L", "mg/L", "ng/mL", "ng/mL", "µg/L", "µg/L")
)
calculate_summary_stats(data)
} # }
```
