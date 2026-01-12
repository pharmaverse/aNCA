# Generate PK Concentration-Time Profile Plots

This function generates a list of ggplots for Mean PK concentration-time
profiles.

## Usage

``` r
pkcg03(
  adnca = data(),
  xvar = "NFRLT",
  yvar = "AVAL",
  xvar_unit = "RRLTU",
  yvar_unit = "AVALU",
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
  plotgroup_vars = c("ROUTE", "PCSPEC", "PARAM"),
  plotgroup_names = list(ROUTE = "Route", PCSPEC = "Specimen", PARAM = "Analyte"),
  scale = c("LIN", "LOG", "SBS")[1],
  studyid = "STUDYID",
  mean_group_var = "DOSEA",
  plotly = TRUE,
  summary_method = "Mean_sdi",
  whiskers_lwr_upr = c("Both", "Upper", "Lower")[1]
)
```

## Arguments

- adnca:

  A data frame containing the data.

- xvar:

  A character string of the variable name for the x-axis.

- yvar:

  A character string of the variable name for the y-axis.

- xvar_unit:

  A character string of the unit for the x-axis variable.

- yvar_unit:

  A character string of the unit for the y-axis variable.

- xbreaks_var:

  A character string of the x-axis breaks.

- xbreaks_mindist:

  A numeric value for minimum distance between x-axis breaks in
  centimeters.

- xmin:

  A numeric value specifying the minimum x-axis limit.

- xmax:

  A numeric value specifying the maximum x-axis limit.

- ymin:

  A numeric value for the minimum y-axis limit.

- ymax:

  A numeric value for the maximum y-axis limit.

- xlab:

  Character for x-axis label. Defaults: `xvar` label & `xvar_unit`.

- ylab:

  Character for y-axis label. Defaults: `yvar` label & `yvar_unit`.

- title:

  A character string for plot title.

- subtitle:

  A character string for plot subtitle.

- footnote:

  A character string of a manual footnote for the plot.

- plotgroup_vars:

  A character vector of the variables to group data.

- plotgroup_names:

  A character vector of the grouping variable names.

- scale:

  A character string:"LIN", "LOG", or "SBS", defining the y-axis scale.

- studyid:

  A character string specifying the study ID variable.

- mean_group_var:

  A character string specifying the grouping variable to plot by group.

- plotly:

  Logical indicating whether to return plotly objects. Defaults to TRUE.

- summary_method:

  A character string specifying the stat method to summarize
  observations.

- whiskers_lwr_upr:

  A character string specifying the whisker type (upper, lower or both)

## Value

A list of ggplot objects for each unique group.

## Author

Kezia Kobana magic numbers for footnote position and margin, work in app
up to 4 lines

## Examples

``` r
if (FALSE) { # \dontrun{
  adnca <- read.csv("inst/shiny/data/example-ADNCA.csv")
  attr(adnca[["AFRLT"]], "label") <- "Actual time from first dose"
  attr(adnca[["AVAL"]], "label") <- "Analysis val"

  plots_lin <- pckg03(adnca = adnca, xmax = 1)
  plots_log <- pckg03(adnca = adnca, scale = "LOG")
  plots_sbs <- pckg03(
    adnca = adnca,
    xbreaks_var = "NFRLT",
    xmin = 100,
    xmax = 1000,
    scale = "SBS",
    plotly = TRUE,
    whiskers_lwr_upr = "Both"
  )
} # }
```
