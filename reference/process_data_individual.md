# Process data for individual line plot

Creates a filtered data frame for individual spaghetti plots.

## Usage

``` r
process_data_individual(
  data,
  selected_usubjids,
  selected_analytes,
  selected_pcspec,
  profiles_selected = NULL,
  ylog_scale = FALSE
)
```

## Arguments

- data:

  Raw data frame.

- selected_usubjids, selected_analytes, selected_pcspec:

  Inputs for filters.

- profiles_selected:

  Optional profiles to filter on.

- ylog_scale:

  Logical, whether to use a logarithmic scale for the y-axis.

## Value

`processed_data` filtered for the spaghetti plots.

## See also

dose_profile_duplicates

## Examples

``` r
base_df <- expand.grid(
USUBJID = c("Subject1", "Subject2", "Subject3", "Subject4"),
PARAM = c("Analyte1"),
PCSPEC = c("Spec1"),
ATPTREF = 1,
AFRLT = 0:5
)
set.seed(123)
base_df$AVAL <- rnorm(nrow(base_df), mean = 50, sd = 10)

result <- process_data_individual(
data = base_df,
selected_usubjids = c("Subject1", "Subject2"),
selected_analytes = c("Analyte1"),
selected_pcspec = c("Spec1"),
profiles_selected = NULL,
ylog_scale = FALSE
)
```
