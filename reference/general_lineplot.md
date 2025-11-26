# Generate a General Line Plot for ADNCA Dataset

This function generates a line plot for an ADNCA dataset based on
user-selected analytes, subjects, and other parameters. The plot can be
customized to display data on a linear or logarithmic scale and can be
filtered by cycle.

## Usage

``` r
general_lineplot(
  data,
  selected_analytes,
  selected_pcspec,
  selected_usubjids,
  colorby_var = "USUBJID",
  facet_by = NULL,
  time_scale,
  yaxis_scale,
  show_threshold = FALSE,
  threshold_value = 0,
  show_dose = FALSE,
  cycle = NULL,
  palette = NULL
)
```

## Arguments

- data:

  A data frame containing the ADNCA dataset.

- selected_analytes:

  A character vector of selected analytes to be included in the plot.

- selected_pcspec:

  A character vector of selected matrix to be included in the plot.

- selected_usubjids:

  A character vector of selected unique subject identifiers (USUBJIDs)
  to be included in the plot.

- colorby_var:

  A character string specifying the variable by which to color the lines
  in the plot.

- facet_by:

  A character vector specifying the variables by which to facet the
  plot.

- time_scale:

  A character string specifying the time scale. Options are "By Cycle"
  or other values.

- yaxis_scale:

  A character string specifying the x-axis scale. Options are "Log" or
  other values.

- show_threshold:

  A boolean specifying whether to show a threshold line or not. Default
  is FALSE.

- threshold_value:

  A numeric value to set the y value of the threshold line. Default is
  0.

- show_dose:

  A boolean specifying whether to show dose times as vertical lines.

- cycle:

  A character string or numeric value specifying the cycle to filter by
  when `time_scale` is "By Cycle". Default is NULL.

- palette:

  Specification of the color palette to use for the plot.

## Value

A ggplot object representing the line plot of pharmacokinetic
concentration over time.

## Details

The function performs the following steps:a

- Filters the data based on the selected analytes, matrices, and
  subjects.

- Selects relevant columns and removes rows with missing concentration
  values.

- Converts 'USUBJID', 'ATPTREF', and 'DOSEA' to factors.

- Filters the data by cycle if `time_scale` is "By Cycle" while creating
  duplicates for predose samples if needed.

- Adjusts concentration values for logarithmic scale if `yaxis_scale` is
  "Log".

- Generates a line plot using the `g_ipp` function with the specified
  parameters.

- Adjusts the y-axis to logarithmic scale if `yaxis_scale` is "Log".

- Adds a horizontal line for the threshold value if `show_threshold` is
  TRUE.

- Adds vertical lines for dose times if `show_dose` is TRUE and the
  number of subjects is less than 5.

## Examples

``` r
adnca <- read.csv(system.file("shiny/data/Dummy_data.csv", package = "aNCA"))
# Use actual values from the dummy data for the example
selected_analytes <- head(unique(adnca$ANALYTE), 1)
selected_pcspec <- head(unique(adnca$PCSPEC), 1)
selected_usubjids <- head(unique(adnca$USUBJID), 1)
plot <- general_lineplot(
  data = adnca,
  selected_analytes = selected_analytes,
  selected_pcspec = selected_pcspec,
  selected_usubjids = selected_usubjids,
  colorby_var = "ATPTREF",
  time_scale = "By Cycle",
  yaxis_scale = "Log",
  cycle = "1",
  show_threshold = TRUE,
  threshold_value = 1
)
print(plot)

```
