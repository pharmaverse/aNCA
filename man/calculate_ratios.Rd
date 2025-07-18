% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ratio_calculations.R
\name{calculate_ratios}
\alias{calculate_ratios}
\title{Calculate Ratios from PKNCA Results}
\usage{
calculate_ratios(
  data,
  parameter,
  match_cols,
  ref_groups,
  test_groups = NULL,
  adjusting_factor = 1,
  custom_pptestcd = NULL
)
}
\arguments{
\item{data}{A PKNCAresults object or its result data.frame.}

\item{parameter}{Character. The PPTESTCD value to use for the calculation (e.g., "AUCINF").}

\item{match_cols}{Character vector of column names to match between test and reference groups
or a data.frame specifying columns and values.}

\item{ref_groups}{A data.frame specifying reference groups.
At its minimum, contains the contrast variable value(s) for the reference.}

\item{test_groups}{A data.frame specifying test groups. Optional.
By default is NULL, allowing rows not in ref_groups be used as test.}

\item{adjusting_factor}{Numeric to multiply the ratio. Default is 1.}

\item{custom_pptestcd}{Optional character. If provided, will be used as the PPTESTCD value.}
}
\value{
A data.frame result object with the calculated ratios.
}
\description{
Calculate Ratios from PKNCA Results
}
