# Generate a Lambda Slope Plot

This function generates a lambda slope plot using pharmacokinetic data.
It calculates relevant lambda parameters and visualizes the data points
used for lambda calculation, along with a linear regression line and
additional plot annotations.

## Usage

``` r
lambda_slope_plot(
  conc_pknca_df,
  row_values,
  myres = myres,
  r2adj_threshold = 0.7,
  time_column = "AFRLT"
)
```

## Arguments

- conc_pknca_df:

  Data frame containing the concentration data (default is
  `mydata$conc$data`).

- row_values:

  A list containing the values for the column_names used for filtering.

- myres:

  A PKNCAresults object containing the results of the NCA analysis

- r2adj_threshold:

  Numeric value representing the R-squared adjusted threshold for
  determining the subtitle color (default is 0.7).

- time_column:

  The name of the time column in the concentration data frame. (default
  is "AFRLT").

## Value

A plotly object representing the lambda slope plot.

## Details

The function performs the following steps:

- Creates duplicates of the pre-dose and last doses of concentration
  data.

- Filters and arranges the input data to obtain relevant lambda
  calculation information.

- Identifies the data points used for lambda calculation.

- Calculates the fitness, intercept, and time span of the half-life
  estimate.

- Determines the subtitle color based on the R-squared adjusted value
  and half-life estimate.

- Generates a ggplot object with the relevant data points, linear
  regression line, and annotations.

- Converts the ggplot object to a plotly object for interactive
  visualization.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example usage:
  plot <- lambda_slope_plot(conc_pknca_df = mydata$conc$data,
                            row_values = list(USUBJID = "001", STUDYID = "A", DOSENO = 1),
                            myres = res_nca,
                            r2adj_threshold = 0.7)
  plot
} # }
```
