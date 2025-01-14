#' This function imputes the start concentration using the log slope method.
#'
#' @param conc Numeric vector of concentrations.
#' @param time Numeric vector of times corresponding to the concentrations.
#' @param start Numeric value indicating the start/dose time.
#' @param end Numeric value indicating the end time.
#' @param ... Additional arguments (currently not used).
#' @param options List of options (currently not used).
#'
#' @return A data frame with imputed start concentration.
#' @details
#' This function adheres to the structure required by the `PKNCA` package to work with its
#' functionalities. For more information, see the
#' [PKNCA Data Imputation Vignette](https://cran.r-project.org/web/packages/PKNCA/vignettes).
#' @export
#'
#' @examples
#' conc <- c(5, 4, 3, 2, 1)
#' time <- c(1, 2, 3, 4, 5)
#' start <- 0
#' end <- 4
#' PKNCA_impute_method_start_logslope(conc, time, start, end)

PKNCA_impute_method_start_logslope <- function(conc, time, start, end, ..., options = list()) { # nolint

  d_conc_time <- data.frame(conc = conc, time = time)
  if (!any(time == start)) {
    all_concs <- conc[time >= start  &  time <= end]
    all_times <- time[time >= start  &  time <= end]
    if (!all(is.na(all_concs))) {
      c0 <- PKNCA::pk.calc.c0(all_concs, all_times, method = "logslope")
      if (!is.na(c0)) {
        d_conc_time <- rbind(d_conc_time, data.frame(time = start, conc = c0))
        d_conc_time <- d_conc_time[order(d_conc_time$time), ]
      }
    }
  }
  d_conc_time
}

#' This function imputes the start concentration using the first concentration after dose
#'
#' @param conc Numeric vector of concentrations.
#' @param time Numeric vector of times corresponding to the concentrations.
#' @param start Numeric value indicating the start/dose time.
#' @param end Numeric value indicating the end time.
#' @param ... Additional arguments (currently not used).
#' @param options List of options (currently not used).
#'
#' @return A data frame with imputed start concentration.
#' @details
#' This function adheres to the structure required by the `PKNCA` package to work with its
#' functionalities.For more information, see the
#' [PKNCA Data Imputation Vignette](https://cran.r-project.org/web/packages/PKNCA/vignettes).
#' @export
#'
#' @examples
#' conc <- c(1, 2, 3, 4, 5)
#' time <- c(1, 2, 3, 4, 5)
#' start <- 0
#' end <- 4
#' PKNCA_impute_method_start_c1(conc, time, start, end)
PKNCA_impute_method_start_c1 <- function(conc, time, start, end, ..., options = list()) { # nolint
  d_conc_time <- data.frame(conc = conc, time = time)
  if (!any(time == start)) {
    all_concs <- conc[time >= start  &  time <= end]
    all_times <- time[time >= start  &  time <= end]
    if (!all(is.na(all_concs))) {
      c1 <- all_concs[which.min(all_times)]
      d_conc_time <- rbind(d_conc_time, data.frame(time = start, conc = c1))
      d_conc_time <- d_conc_time[order(d_conc_time$time), ]
    }
  }
  d_conc_time
}
