# This file will only be used until PKNCA includes the additional functions in their new version
# ToDo(Gerardo): Remove the file once PKNCA incorporates all our functions (adj.r.squared, aucpeo...)

exclude_nca_by_param <- function(parameter, min_value = NULL, max_value = NULL) {

  function(x, ...) {
    ret <- rep(NA_character_, nrow(x))

    # Find the index of the parameter in the dataset
    idx_param <- which(x$PPTESTCD %in% parameter)
    current_value <- x$PPORRES[idx_param]
    pretty_name <- PKNCA::get.interval.cols()[[parameter]]$pretty_name

    if (length(idx_param) == 0) {
      # Do nothing, the parameter was not calculated
    } else if (length(idx_param) > 1) {
      stop("Should not see more than one ", parameter, " (please report this as a bug)")
    }

    # Check against min_value
    if (!is.null(min_value) && !is.na(current_value) && current_value < min_value) {
      ret[idx_param] <- sprintf("%s < %g", pretty_name, min_value)
    }

    # Check against max_value
    if (!is.null(max_value) && !is.na(current_value) && current_value > max_value) {
      ret[idx_param] <- sprintf("%s > %g", pretty_name, max_value)
    }

    ret
  }
}
