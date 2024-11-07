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
#' @export
#'
#' @examples
#' conc <- c(1, 2, 3, 4, 5)
#' time <- c(0, 1, 2, 3, 4)
#' start <- 0
#' end <- 4
#' PKNCA_impute_method_start_log(conc, time, start, end)

PKNCA_impute_method_start_log <- function(conc, time, start, end, ..., options = list()) { # nolint
  ret <- data.frame(conc = conc, time = time)
  mask_start <- time %in% start
  if (!any(mask_start)) {
    all_concs <- conc[time >= start  &  time <= end]
    all_times <- time[time >= start  &  time <= end]
    if (!all(is.na(all_concs))) {
      c0 <- PKNCA::pk.calc.c0(all_concs, all_times, method = "logslope")
      ret <- rbind(ret, data.frame(time = start, conc = c0))
      ret <- ret[order(ret$time), ]
    }
  }
  ret
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
#' @export
#'
#' @examples
#' conc <- c(1, 2, 3, 4, 5)
#' time <- c(0, 1, 2, 3, 4)
#' start <- 0
#' end <- 4
#' PKNCA_impute_method_start_c1(conc, time, start, end)
PKNCA_impute_method_start_c1 <- function(conc, time, start, end, ..., options = list()) { # nolint
  ret <- data.frame(conc = conc, time = time)
  mask_start <- time %in% start
  if (!any(mask_start)) {
    all_concs <- conc[time >= start  &  time <= end]
    all_times <- time[time >= start  &  time <= end]
    if (!all(is.na(all_concs))) {
      c1 <- all_concs[which.min(all_times, na.rm = TRUE)]
      ret <- rbind(ret, data.frame(time = start, conc = c1))
      ret <- ret[order(ret$time), ]
    }
  }
  ret
}
