% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reshape_PKNCA_results.R
\name{reshape_pknca_results}
\alias{reshape_pknca_results}
\title{Reshape PKNCA Results}
\usage{
reshape_pknca_results(res_nca)
}
\arguments{
\item{res_nca}{The output of PKNCA::pk.nca}
}
\value{
A data frame which provides an easy overview on the results from the NCA
in each profile/subject and how it was computed lambda (half life) and the results
of the NCA parameters (cmax, AUC, AUClast)
}
\description{
This function reshapes the structure of the results produced by the main function
of the PKNCA package (pk.nca) in a way that each row represents all the main results
summarized for each profile in each individual/patient.
}
