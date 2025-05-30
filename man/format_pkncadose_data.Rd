% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_data.R
\name{format_pkncadose_data}
\alias{format_pkncadose_data}
\title{Create PK Dose Dataset}
\usage{
format_pkncadose_data(pkncaconc_data, group_columns)
}
\arguments{
\item{pkncaconc_data}{A data frame containing the concentration data.}

\item{group_columns}{A character vector specifying the columns to group by.}
}
\value{
A data frame containing the dose data.
}
\description{
This function creates a pharmacokinetic dose dataset from the provided concentration data.
}
\details{
The function performs the following steps:
\itemize{
\item Arranges and groups the data by group_columns
\item Selects the first row within each group (arranged by DOSNOA- a variable created
in \code{format_pkncaconc_data})
}

Note*: This function is designed to work with the output of \code{format_pkncaconc_data}.
}
