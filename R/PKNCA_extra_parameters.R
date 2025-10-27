

### Include Excretion Rate Parameters in the App
add.interval.col <- function(name,
                             FUN,
                             values=c(FALSE, TRUE),
                             unit_type,
                             pretty_name,
                             depends=NULL,
                             desc="",
                             sparse=FALSE,
                             formalsmap=list(),
                             datatype=c("interval",
                                        "individual",
                                        "population")) {
  # Check inputs
  if (!is.character(name)) {
    stop("name must be a character string")
  } else if (length(name) != 1) {
    stop("name must have length == 1")
  }
  if (length(FUN) != 1) {
    stop("FUN must have length == 1")
  } else if (!(is.character(FUN) | is.na(FUN))) {
    stop("FUN must be a character string or NA")
  }
  if (!is.null(depends)) {
    if (!is.character(depends)) {
      stop("'depends' must be NULL or a character vector")
    }
  }
  checkmate::assert_logical(sparse, any.missing=FALSE, len=1)
  unit_type <-
    match.arg(
      unit_type,
      choices=c(
        "unitless", "fraction", "%", "count",
        "time", "inverse_time",
        "amount", "amount_dose", "amount_time",
        "conc", "conc_dosenorm",
        "dose", 
        "volume",
        "auc", "aumc",
        "auc_dosenorm", "aumc_dosenorm",
        "clearance", "renal_clearance"
      )
    )
  stopifnot("pretty_name must be a scalar"=length(pretty_name) == 1)
  stopifnot("pretty_name must be a character"=is.character(pretty_name))
  stopifnot("pretty_name must not be an empty string"=nchar(pretty_name) > 0)
  datatype <- match.arg(datatype)
  if (!(datatype %in% "interval")) {
    stop("Only the 'interval' datatype is currently supported.")
  }
  if (length(desc) != 1) {
    stop("desc must have length == 1")
  } else if (!is.character(desc)) {
    stop("desc must be a character string")
  }
  if (!is.list(formalsmap)) {
    stop("formalsmap must be a list")
  } else if (length(formalsmap) > 0 &
             is.null(names(formalsmap))) {
    stop("formalsmap must be a named list")
  } else if (length(formalsmap) > 0 &
             is.na(FUN)) {
    stop("formalsmap may not be given when FUN is NA.")
  } else if (!all(nchar(names(formalsmap)) > 0)) {
    stop("All formalsmap elements must be named")
  }
  # Ensure that the function exists
  if (!is.na(FUN) &&
      length(utils::getAnywhere(FUN)$objs) == 0) {
    stop("The function named '", FUN, "' is not defined.  Please define the function before calling add.interval.col.")
  }
  if (!is.na(FUN) &
      length(formalsmap) > 0) {
    # Ensure that the formalsmap parameters are all in the list of
    # formal arguments to the function.
    if (!all(names(formalsmap) %in% names(formals(utils::getAnywhere(FUN)$objs[[1]])))) {
      stop("All names for the formalsmap list must be arguments to the function.")
    }
  }
  current <- get("interval.cols", envir=PKNCA:::.PKNCAEnv)
  current[[name]] <-
    list(
      FUN=FUN,
      values=values,
      unit_type=unit_type,
      pretty_name=pretty_name,
      desc=desc,
      sparse=sparse,
      formalsmap=formalsmap,
      depends=depends,
      datatype=datatype
    )
  assign("interval.cols", current, envir=PKNCA:::.PKNCAEnv)
}



#' Calculate the midpoint collection time of the last measurable excretion rate
#'
#' @param conc The concentration in the excreta (e.g., urine or feces)
#' @param volume The volume (or mass) of the sample
#' @param time The starting time of the collection interval
#' @param duration.conc The duration of the collection interval
#' @param check Should the concentration and time data be checked?
#' @return The midpoint collection time of the last measurable excretion rate, or NA/0 if not available
#' @export
pk.calc.ertlst <- function(conc, volume, time, duration.conc, check = TRUE) {
  if (check) {
    PKNCA:::assert_conc_time(conc = conc, time = time)
  }
  if (all(is.na(conc))) {
    NA_real_
  } else if (all(conc %in% c(0, NA))) {
    0
  } else {
    er <- conc * volume / duration.conc
    midtime <- time + duration.conc / 2
    max(midtime[!(conc %in% c(NA, 0))])
  }
}

# Add the column to the interval specification
add.interval.col("ertlst",
                 FUN="pk.calc.ertlst",
                 unit_type="time",
                 pretty_name="Tlast excretion rate",
                 desc="The midpoint collection time of the last measurable excretion rate (typically in urine or feces)")

PKNCA::PKNCA.set.summary(
  name="ertlst",
  description="median and range",
  point= PKNCA::business.median,
  spread= PKNCA::business.range
)

#' Calculate the maximum excretion rate
#'
#' @param conc The concentration in the excreta (e.g., urine or feces)
#' @param volume The volume (or mass) of the sample
#' @param time The starting time of the collection interval
#' @param duration.conc The duration of the collection interval
#' @param check Should the concentration data be checked?
#' @return The maximum excretion rate, or NA if not available
#' @export
pk.calc.ermax <- function(conc, volume, time, duration.conc, check = TRUE) {
  if (check) {
    assert_conc(conc = conc)
  }
  if (length(conc) == 0 | all(is.na(conc))) {
    NA
  } else {
    er <- conc * volume / duration.conc
    max(er, na.rm=TRUE)
  }
}

add.interval.col("ermax",
                 FUN="pk.calc.ermax",
                 unit_type="amount_time",
                 pretty_name="Maximum excretion rate",
                 desc="The maximum excretion rate (typically in urine or feces)")

PKNCA::PKNCA.set.summary(
  name="ermax",
  description="geometric mean and geometric coefficient of variation",
  point=PKNCA::business.geomean,
  spread=PKNCA::business.geocv
)

#' Calculate the midpoint collection time of the maximum excretion rate
#'
#' @param conc The concentration in the excreta (e.g., urine or feces)
#' @param volume The volume (or mass) of the sample
#' @param time The starting time of the collection interval
#' @param duration.conc The duration of the collection interval
#' @param check Should the concentration and time data be checked?
#' @param first.tmax If TRUE, return the first time of maximum excretion rate; otherwise, return the last
#' @return The midpoint collection time of the maximum excretion rate, or NA if not available
#' @export
pk.calc.ertmax <- function(conc, volume, time, duration.conc, check = TRUE, first.tmax = NULL) {
  
  if (check) {
    PKNCA:::assert_conc_time(conc = conc, time = time)
  }
  
  if (length(conc) == 0 | all(conc %in% c(NA, 0))) {
    NA
  } else {
    er <- conc * volume / duration.conc
    ermax <- pk.calc.ermax(conc, volume, time, duration.conc, check = FALSE)
    midtime <- time + duration.conc / 2
    ret <- midtime[er %in% ermax]
    
    if (first.tmax) {
      ret[1]
    } else {
      ret[length(ret)]
    }
  }
}

add.interval.col("ertmax",
                 FUN="pk.calc.ertmax",
                 unit_type="time",
                 pretty_name="Tmax excretion rate",
                 desc="The midpoint collection time of the maximum excretion rate (typically in urine or feces)")

PKNCA::PKNCA.set.summary(
  name="ertmax",
  description="median and range",
  point=PKNCA::business.median,
  spread=PKNCA::business.range
)

###########################################################################################################

#' Calculate the total urine volume
#'
#' @param volume The volume (or mass) of the sample
#' @return The sum of urine volumes for the interval
#' @export
pk.calc.volpk <- function(volume) { #nolint
  if (length(volume) == 0) return(NA_real_)
  sum(volume)
}
add.interval.col(
  "volpk",
  FUN = "pk.calc.volpk",
  values = c(FALSE, TRUE),
  unit_type = "volume",
  pretty_name = "Total Urine Volume",
  desc = "The sum of urine volumes for the interval"
)
PKNCA::PKNCA.set.summary(
  name = "volpk",
  description = "geometric mean and geometric coefficient of variation",
  point = PKNCA::business.geomean,
  spread = PKNCA::business.geocv
)

