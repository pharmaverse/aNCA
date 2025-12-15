# Create a Mean PK Line Plot

Create a Mean PK Line Plot

## Usage

``` r
process_data_mean(
  data,
  selected_analytes,
  selected_pcspec,
  profiles_selected = NULL,
  ylog_scale = FALSE,
  color_by = NULL,
  facet_by = NULL
)
```

## Arguments

- data:

  Raw data frame.

- selected_analytes, selected_pcspec, profiles_selected:

  Inputs for filtering.

- ylog_scale:

  Logical, whether to use a logarithmic scale for the y-axis.

- color_by, facet_by:

  Optional grouping variables to be included in summary.

## Value

`summarised_data` with Mean, SD, and CIs for the profiles selected.

## Examples

``` r
base_df <- expand.grid(
USUBJID = c("Subject1", "Subject2", "Subject3", "Subject4"),
PARAM = c("Analyte1"),
PCSPEC = c("Spec1"),
ATPTREF = 1,
NFRLT = 0:5,
AVALU = "ug/ml",
RRLTU = "hr"
)
set.seed(123)
base_df$AVAL <- rnorm(nrow(base_df), mean = 50, sd = 10)

result <- process_data_mean(
data = base_df,
selected_analytes = c("Analyte1"),
selected_pcspec = c("Spec1"),
profiles_selected = NULL,
ylog_scale = FALSE
)
```
