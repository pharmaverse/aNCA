# This file will only be used until PKNCA includes the additional functions in their new version
# ToDo(Gerardo): Remove the file once PKNCA incorporates all our functions (adj.r.squared...)


#' Exclude NCA Results Based on Parameter Thresholds
#'
#' Exclude rows from NCA results based on specified thresholds for a given parameter.
#' This function allows users to define minimum and/or maximum acceptable values
#' for a parameter and excludes rows that fall outside these thresholds.
#'
#' @param parameter The name of the PKNCA parameter to evaluate (e.g., "span.ratio").
#' @param min_thr The minimum acceptable value for the parameter. If not provided, is not applied.
#' @param max_thr The maximum acceptable value for the parameter. If not provided, is not applied.
#' @param affected_parameters Character vector of PKNCA parameters that will be marked as excluded.
#'                            By default is the defined parameter.
#' @returns A function that can be used with `PKNCA::exclude` to mark through the 'exclude'  column
#'          the rows in the PKNCA results based on the specified thresholds for a parameter.
#' @examples
#' # Example dataset
#' my_data <- PKNCA::PKNCAdata(
#'   PKNCA::PKNCAconc(data.frame(conc = c(1, 2, 3, 4),
#'                               time = c(0, 1, 2, 3),
#'                               subject = 1),
#'                    conc ~ time | subject),
#'   PKNCA::PKNCAdose(data.frame(subject = 1, dose = 100, time = 0),
#'                    dose ~ time | subject)
#' )
#' my_result <- PKNCA::pk.nca(my_data)
#'
#' # Exclude rows where span.ratio is less than 100
#' excluded_result <- PKNCA::exclude(
#'   my_result,
#'   FUN = exclude_nca_by_param("span.ratio", min_thr = 100)
#' )
#' as.data.frame(excluded_result)
#'
#' @export

exclude_nca_by_param <- function(
  parameter,
  min_thr = NULL,
  max_thr = NULL,
  affected_parameters = parameter
) {
  # Determine if thresholds are defined and if so check they are single numeric objects
  is_min_thr <- is_single_numeric(min_thr, "min_thr")
  is_max_thr <- is_single_numeric(max_thr, "max_thr")

  if (isTRUE(min_thr > max_thr))
    stop("if both defined min_thr must be less than max_thr")

  function(x, ...) {
    ret <- rep(NA_character_, nrow(x))
    idx_param <- which(x$PPTESTCD == parameter)
    idx_aff_params <- which(x$PPTESTCD %in% affected_parameters)

    if (length(idx_param) > 1)
      stop("Should not see more than one ", parameter, " (please report this as a bug)")

    if (length(idx_param) == 1 && !is.na(x$PPORRES[idx_param]) && length(idx_aff_params) > 0) {
      current_value <- x$PPORRES[idx_param]
      pretty_name <- translate_terms(parameter, "PKNCA", "PPTEST")
      if (is_min_thr && current_value < min_thr) {
        ret[idx_aff_params] <- sprintf("%s < %g", pretty_name, min_thr)
      } else if (is_max_thr && current_value > max_thr) {
        ret[idx_aff_params] <- sprintf("%s > %g", pretty_name, max_thr)
      }
    }
    ret
  }
}

# Helper function to validate if a value is a single numeric and check if it is defined
is_single_numeric <- function(value, name) {
  is_val <- any(!is.null(value) & !is.na(value) & !missing(value))
  if (is_val && (length(value) != 1 || !is.numeric(value))) {
    stop(sprintf("when defined %s must be a single numeric value", name))
  }
  is_val
}

