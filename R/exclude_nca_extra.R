# This file will only be used until PKNCA includes the additional functions in their new version
# ToDo(Gerardo): Remove the file once PKNCA incorporates all our functions (adj.r.squared...)

# Updated exclude_nca_by_param function
exclude_nca_by_param <- function(parameter, min_val = NULL, max_val = NULL) {

  # Determine if thresholds are defined and if so check they are single numeric objects
  thr_def <- validate_thresholds(min_val, max_val)

  function(x, ...) {
    ret <- rep(NA_character_, nrow(x))
    idx_param <- which(x$PPTESTCD %in% parameter)

    if (length(idx_param) == 0) {
      # do nothing
    } else if (length(idx_param) > 1) {
      stop("Should not see more than one ", parameter, " (please report this as a bug)")
    } else if (!is.na(x$PPORRES[idx_param])) {
      current_value <- x$PPORRES[idx_param]
      pretty_name <- PKNCA::get.interval.cols()[[parameter]]$pretty_name

      if (thr_def$is_min_val && current_value < min_val) {
        ret[idx_param] <- sprintf("%s < %g", pretty_name, min_val)
      }
      if (thr_def$is_max_val && current_value > max_val) {
        ret[idx_param] <- sprintf("%s > %g", pretty_name, max_val)
      }
    }
    ret
  }
}

# Helper function to validate if a value is a single numeric and check if it is defined
validate_is_single_numeric <- function(value, name) {
  is_val <- any(!is.null(value) & !is.na(value) & !missing(value))
  if (is_val && (length(value) != 1 || !is.numeric(value))) {
    stop(sprintf("when defined %s must be a single numeric value", name))
  }
  is_val
}

# Updated validate_thresholds function to use validate_is_single_numeric
validate_thresholds <- function(min_val, max_val) {
  is_min_val <- validate_is_single_numeric(min_val, "min_val")
  is_max_val <- validate_is_single_numeric(max_val, "max_val")

  if (is_min_val && is_max_val && min_val > max_val) {
    stop("if both defined min_val must be less than max_val")
  }
  list(
    is_min_val = is_min_val,
    is_max_val = is_max_val
  )
}

