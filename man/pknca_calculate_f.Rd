% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bioavailability.R
\name{pknca_calculate_f}
\alias{pknca_calculate_f}
\title{Calculate bioavailability for intravascular vs extravascular aucs}
\usage{
pknca_calculate_f(res_nca, f_aucs)
}
\arguments{
\item{res_nca}{A list containing non-compartmental analysis (NCA) results,
including concentration and dose data.}

\item{f_aucs}{A character vector of the comparing AUC parameter/s including
the prefix f_ (e.g., \code{c("f_aucinf.obs", "f_auclast")}).}
}
\value{
A data frame with calculated absolute bioavailability values (\code{FABS_})
for individual subjects where IV data is available. If IV data is missing,
it estimates bioavailability using the mean IV AUC for that grouping.
}
\description{
This function calculates bioavailability (F) based on AUC (Area Under Curve) data
extracted from \code{res_nca}. It computes individual bioavailability
where IV and EX data are available
for a subject. If IV data is missing, it estimates bioavailability
using the mean IV values for that grouping.
}
\details{
\itemize{
\item The function extracts AUC data from \code{res_nca$data$conc$data} and
filters for selected AUC types.
\item It separates data into intravascular (IV) and extravascular (EX) groups.
\item Individual bioavailability is calculated for subjects with both
IV and EX data using PKNCA function \code{pk.calc.f}.
\item If IV data is missing for a subject, the function estimates
bioavailability using mean IV values for that grouping.
\item The final output includes bioavailability estimates
for individual subjects and mean-based estimates.
}
}
