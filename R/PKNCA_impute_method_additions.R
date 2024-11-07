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
