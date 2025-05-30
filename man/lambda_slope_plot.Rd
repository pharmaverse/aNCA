% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lambda_slope_plot.R
\name{lambda_slope_plot}
\alias{lambda_slope_plot}
\title{Generate a Lambda Slope Plot}
\usage{
lambda_slope_plot(
  conc_pknca_df,
  row_values,
  myres = myres,
  r2adj_threshold = 0.7
)
}
\arguments{
\item{conc_pknca_df}{Data frame containing the concentration data
(default is \code{mydata$conc$data}).}

\item{row_values}{A list containing the values for the column_names used for filtering.}

\item{myres}{A PKNCAresults object containing the results of the NCA analysis}

\item{r2adj_threshold}{Numeric value representing the R-squared adjusted threshold for
determining the subtitle color (default is 0.7).}
}
\value{
A plotly object representing the lambda slope plot.
}
\description{
This function generates a lambda slope plot using pharmacokinetic data. It calculates relevant
lambda parameters and visualizes the data points used for lambda calculation, along with
a linear regression line and additional plot annotations.
}
\details{
The function performs the following steps:
\itemize{
\item{Creates duplicates of the pre-dose and last doses of concentration data.}
\item{Filters and arranges the input data to obtain relevant lambda calculation information.}
\item{Identifies the data points used for lambda calculation.}
\item{Calculates the fitness, intercept, and time span of the half-life estimate.}
\item{
Determines the subtitle color based on the R-squared adjusted value and half-life estimate.
}
\item{
Generates a ggplot object with the relevant data points,
linear regression line, and annotations.
}
\item{Converts the ggplot object to a plotly object for interactive visualization.}
}
}
\examples{
\dontrun{
  # Example usage:
  plot <- lambda_slope_plot(conc_pknca_df = mydata$conc$data,
                            row_values = list(USUBJID = "001", STUDYID = "A", DOSENO = 1),
                            myres = res_nca,
                            r2adj_threshold = 0.7)
  plot
}

}
