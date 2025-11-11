# Generate Combined PK Concentration-Time Profile Plot by Cohort

This function generates a list of plotly objects PK concentration-time
profiles by group

## Usage

``` r
pkcg02(
  adpc = data(),
  xvar = "AFRLT",
  yvar = "AVAL",
  xvar_unit = "RRLTU",
  yvar_unit = "AVALU",
  color_var = NULL,
  color_var_label = NULL,
  xbreaks_var = "NFRLT",
  xbreaks_mindist = 0.5,
  xmin = NA,
  xmax = NA,
  ymin = NA,
  ymax = NA,
  xlab = paste0("!", xvar, " [$", xvar_unit, "]"),
  ylab = paste0("!", yvar, " [$", yvar_unit, "]"),
  title = NULL,
  subtitle = NULL,
  footnote = NULL,
  plotgroup_vars = c("ROUTE", "PCSPEC", "PARAM", "TRT01A"),
  plotgroup_names = list(ROUTE = "Route", PCSPEC = "Specimen", PARAM = "Analyte", TRT01A
    = "Treatment"),
  scale = c("LIN", "LOG", "SBS")[1],
  studyid = "STUDYID",
  trt_var = "TRT01A",
  plotly = TRUE
)
```

## Arguments

- adpc:

  A data frame containing the data.

- xvar:

  A character string of the variable name for the x-axis.

- yvar:

  A character string of the variable name for the y-axis.

- xvar_unit:

  A character string of the unit for the x-axis variable.

- yvar_unit:

  A character string of the unit for the y-axis variable.

- color_var:

  A character string of the variable name for the color.

- color_var_label:

  A character string of the color label.

- xbreaks_var:

  A character string of the x-axis breaks.

- xbreaks_mindist:

  A numeric value for `xbreak_mindist`.

- xmin:

  A numeric value for the minimum x-axis limit.

- xmax:

  A numeric value for the maximum x-axis limit.

- ymin:

  A numeric value for the minimum y-axis limit.

- ymax:

  A numeric value for the maximum y-axis limit.

- xlab:

  Character for x-axis label. Defaults: `xvar` label & `xvar_unit`.

- ylab:

  Character for y-axis label. Defaults: `yvar` label & `yvar_unit`.

- title:

  Character for plot title.

- subtitle:

  Character for plot subtitle.

- footnote:

  A character string of a manual footnote for the plot.

- plotgroup_vars:

  A character vector of the variables to group data.

- plotgroup_names:

  A character vector of the grouping variable names.

- scale:

  Scale for the Y axis, either "LIN" or "LOG".

- studyid:

  A character string specifying the study ID variable.

- trt_var:

  A character string specifying the treatment variable.

- plotly:

  Logical indicating whether to return plotly objects. Defaults to TRUE.

## Value

A list of ggplot or plotly objects for each unique group.

## Author

Kezia Kobana magic numbers for footnote position and margin, work in app
up to 4 lines NOTE: might require some fine tuning down the line, looks
fine now

## Examples

``` r
if (FALSE) { # \dontrun{
  adpc <- read.csv(system.file("shiny/data/DummyRO_ADNCA.csv", package = "aNCA"))
  attr(adpc[["AFRLT"]], "label") <- "Actual time from first dose"
  attr(adpc[["AVAL"]], "label") <- "Analysis value"

  plots <- pkcg02(adpc)
  plots_log <- pkcg02(adpc, scale = "LOG")
  plots_custom <- pkcg02(adpc, xmin = 0, xmax = 48, title = "PK Profile", footnote = "Study XYZ")
  plotly::plotly_build(plots[[1]]) # View the first plot
} # }
```
