#' Parse ISO 8601 date-time strings with mixed precision
#'
#' Handles full datetime, datetime without seconds, datetime with hour only,
#' and date-only formats. Returns POSIXct in UTC.
#'
#' @param dtc Character vector of ISO 8601 date-time strings
#' @returns POSIXct vector in UTC
#' @keywords internal
std_dtc_to_rdate <- function(dtc) {
  formats <- c(
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%dT%H:%M",
    "%Y-%m-%dT%H",
    "%Y-%m-%d"
  )
  dtc_to_dt <- list()
  for (fmt in formats) {
    dtc_to_dt[[fmt]] <- as.POSIXct(dtc, format = fmt, tz = "UTC")
  }
  dplyr::coalesce(
    dtc_to_dt[[formats[1]]], dtc_to_dt[[formats[2]]],
    dtc_to_dt[[formats[3]]], dtc_to_dt[[formats[4]]]
  )
}

#' Parse an ISO 8601 duration string to numeric hours
#'
#' Supports the full CDISC-compliant ISO 8601 duration format:
#' \code{PnYnMnDTnHnMnS} and \code{PnW}. Empty strings and \code{NA}
#' values return \code{NA_real_}. Month and year conversions use
#' average lengths (30.4375 days/month, 365.25 days/year).
#'
#' @param x Character vector of ISO 8601 duration strings (e.g. \code{"PT2H"},
#'   \code{"P3D"}, \code{"P1DT12H"}, \code{"P2W"}, \code{"PT0.5H"})
#' @returns Numeric vector of durations in hours
#' @keywords internal
parse_iso8601_duration <- function(x) {
  vapply(x, function(val) {
    if (is.na(val) || !nzchar(trimws(val)) || !grepl("^P", val)) {
      return(NA_real_)
    }
    hours <- 0

    # Week format: PnW (cannot be mixed with other components per ISO 8601)
    w_match <- regmatches(val, regexpr("[0-9.]+(?=W)", val, perl = TRUE))
    if (length(w_match) == 1) return(as.numeric(w_match) * 7 * 24)

    # Date components (before T): years, months, days
    date_part <- sub("T.*", "", sub("^P", "", val))
    y_match <- regmatches(date_part, regexpr("[0-9.]+(?=Y)", date_part,
                                             perl = TRUE))
    if (length(y_match) == 1) hours <- hours + as.numeric(y_match) * 365.25 * 24
    mo_match <- regmatches(date_part, regexpr("[0-9.]+(?=M)", date_part,
                                              perl = TRUE))
    if (length(mo_match) == 1) hours <- hours + as.numeric(mo_match) * 30.4375 * 24
    d_match <- regmatches(date_part, regexpr("[0-9.]+(?=D)", date_part,
                                             perl = TRUE))
    if (length(d_match) == 1) hours <- hours + as.numeric(d_match) * 24

    # Time components (after T): hours, minutes, seconds
    time_part <- if (grepl("T", val)) sub(".*T", "", val) else ""
    h_match <- regmatches(time_part, regexpr("-?[0-9.]+(?=H)", time_part,
                                             perl = TRUE))
    if (length(h_match) == 1) hours <- hours + as.numeric(h_match)
    m_match <- regmatches(time_part, regexpr("-?[0-9.]+(?=M)", time_part,
                                             perl = TRUE))
    if (length(m_match) == 1) hours <- hours + as.numeric(m_match) / 60
    s_match <- regmatches(time_part, regexpr("-?[0-9.]+(?=S)", time_part,
                                             perl = TRUE))
    if (length(s_match) == 1) hours <- hours + as.numeric(s_match) / 3600

    hours
  }, numeric(1), USE.NAMES = FALSE)
}

#' Map CDISC route of administration to PKNCA route
#'
#' @param route Character vector of CDISC route values
#' @returns Character vector of \code{"intravascular"} or
#'   \code{"extravascular"}
#' @keywords internal
route_cdisc_to_pknca <- function(route) {
  intravascular_pattern <- paste0(
    "(INFUS|DRIP|IV|INTRAVEN|IVADMIN|BOLUS|INTRAVASCULAR|INTRA-?ARTERIAL|",
    "INTRACARDIAC|INTRACORONARY)"
  )
  ifelse(
    grepl(intravascular_pattern, gsub("[^[:alnum:]]", "", toupper(route))),
    "intravascular",
    "extravascular"
  )
}


# --- Dose table preparation --------------------------------------------------

#' Prepare a dose lookup table from EX data
#'
#' Parses EX domain data into a standardised dose table with actual and nominal
#' timing, dose numbering, duration, and route. Used internally by both
#' \code{\link{pc_to_PKNCAconc}} and \code{\link{sdtm_to_PKNCAdata}}.
#'
#' @param ex A data.frame containing the EX (Exposure) SDTM domain.
#' @returns A data.frame with one row per dose event and columns:
#'   \code{STUDYID}, \code{USUBJID}, \code{DOSETRT}, \code{DOSEA},
#'   \code{DOSEU}, \code{ROUTE}, \code{std_route}, \code{ADOSEDUR},
#'   \code{AFRLT} (actual dose time from first dose, hours),
#'   \code{NFRLT} (nominal dose time from first dose, hours),
#'   \code{DOSNOA} (sequential dose number per subject),
#'   \code{EXSTDTC_posix} (parsed dose start datetime).
#'
#' @importFrom dplyr mutate group_by ungroup arrange row_number
#' @importFrom rlang .data
#' @keywords internal
.prepare_dose_table <- function(ex) {
  # Parse dates
  exstdtc_posix <- std_dtc_to_rdate(ex$EXSTDTC)

  # Parse duration: from EXDUR (ISO 8601) or EXENDTC - EXSTDTC
  # EXDUR may be present but empty/NA — only use it when it has real values
  has_exdur <- "EXDUR" %in% names(ex) &&
    any(!is.na(ex$EXDUR) & nchar(trimws(ex$EXDUR)) > 0)

  if (has_exdur) {
    adosedur <- if (is.character(ex$EXDUR)) {
      parse_iso8601_duration(ex$EXDUR)
    } else {
      as.numeric(ex$EXDUR)
    }
  } else {
    adosedur <- rep(NA_real_, nrow(ex))
  }

  # Fill remaining NAs from EXENDTC - EXSTDTC
  na_dur <- is.na(adosedur)
  if (any(na_dur) && "EXENDTC" %in% names(ex)) {
    end_posix <- std_dtc_to_rdate(ex$EXENDTC)
    dur_from_dates <- as.numeric(
      difftime(end_posix, exstdtc_posix, units = "hours")
    )
    adosedur[na_dur] <- dur_from_dates[na_dur]
  }

  # Default: 0 for extravascular, NA stays for IV (will need review)
  adosedur[is.na(adosedur)] <- 0

  # Parse nominal elapsed time (EXELTM) if available and non-empty
  exeltm_hours <- NULL
  has_exeltm <- "EXELTM" %in% names(ex) &&
    any(!is.na(ex$EXELTM) & nchar(trimws(ex$EXELTM)) > 0)
  if (has_exeltm) {
    exeltm_hours <- if (is.character(ex$EXELTM)) {
      parse_iso8601_duration(ex$EXELTM)
    } else {
      as.numeric(ex$EXELTM)
    }
  }

  # Build dose table with aNCA-standard column names
  dose_tbl <- data.frame(
    STUDYID       = ex$STUDYID,
    USUBJID       = ex$USUBJID,
    DOSETRT       = ex$EXTRT,
    DOSEA         = as.numeric(ex$EXDOSE),
    DOSEU         = ex$EXDOSU,
    ROUTE         = ex$EXROUTE,
    std_route     = route_cdisc_to_pknca(ex$EXROUTE),
    ADOSEDUR      = adosedur,
    EXSTDTC_posix = exstdtc_posix,
    stringsAsFactors = FALSE
  )

  # AFRLT: actual dose time from first dose per subject
  dose_tbl <- dose_tbl %>%
    group_by(.data$USUBJID) %>%
    mutate(
      AFRLT = as.numeric(difftime(
        .data$EXSTDTC_posix,
        min(.data$EXSTDTC_posix, na.rm = TRUE),
        units = "hours"
      ))
    ) %>%
    ungroup() %>%
    arrange(.data$USUBJID, .data$AFRLT)

  # NFRLT: nominal dose time from first dose
  if (!is.null(exeltm_hours)) {
    dose_tbl$NFRLT <- exeltm_hours
  } else {
    # Fallback: use actual times as nominal
    dose_tbl$NFRLT <- dose_tbl$AFRLT
  }

  # DOSNOA: sequential dose number per subject
  dose_tbl <- dose_tbl %>%
    group_by(.data$USUBJID) %>%
    arrange(.data$AFRLT) %>%
    mutate(DOSNOA = row_number()) %>%
    ungroup()

  dose_tbl
}


# --- PC to PKNCAconc ----------------------------------------------------------

#' Convert PC and EX SDTM domains to a PKNCAconc object
#'
#' Transforms CDISC SDTM PC (concentrations) and EX (exposure) domain data
#' frames into a \code{PKNCAconc} object with all columns required by the
#' aNCA downstream pipeline.
#'
#' @section Timing derivation:
#' \describe{
#'   \item{AFRLT}{Actual time from first dose (hours).
#'     \code{difftime(PCDTC, first_EXSTDTC_per_subject)}.}
#'   \item{ARRLT}{Actual time from most recent dose (hours).
#'     \code{difftime(PCDTC, most_recent_EXSTDTC <= PCDTC)}.
#'     Negative only for samples before the very first dose.}
#'   \item{NRRLT}{Nominal time from most recent dose (hours).
#'     Parsed directly from \code{PCELTM} (ISO 8601 duration).}
#'   \item{NFRLT}{Nominal time from first dose (hours).
#'     \code{NRRLT + EXELTM_of_matched_dose}.}
#' }
#'
#' @section Assumption:
#' \code{PCELTM} / \code{PCRFTDTC} must reference the most recent dose for
#' each sample. If the data provider used a different convention (e.g. the
#' next scheduled dose for pre-dose samples), nominal times will be
#' inconsistent.
#'
#' @param pc A data.frame containing the PC (Pharmacokinetic Concentrations)
#'   SDTM domain. Required columns: STUDYID, USUBJID, PCTEST, PCSTRESN,
#'   PCSTRESU, PCDTC. Optional: PCSPEC, PCELTM, PCRFTDTC, VOLUME, VOLUMEU.
#' @param ex A data.frame containing the EX (Exposure) SDTM domain.
#'   Required columns: STUDYID, USUBJID, EXTRT, EXDOSE, EXDOSU, EXROUTE,
#'   EXSTDTC. Optional: EXENDTC, EXDUR, EXELTM.
#' @param dm Optional data.frame containing the DM (Demographics) SDTM domain.
#'   When provided, demographic columns (AGE, AGEU, SEX, RACE, ARM, ACTARM)
#'   are joined by USUBJID.
#' @param metabolites Optional character vector of PCTEST values to flag as
#'   metabolites (\code{METABFL = "Y"}).
#'
#' @returns A \code{PKNCAconc} object with formula
#'   \code{AVAL ~ AFRLT | STUDYID + PCSPEC + DOSETRT + USUBJID / PARAM}
#'   and all columns required by the aNCA pipeline.
#'
#' @importFrom dplyr mutate group_by ungroup arrange left_join select any_of
#'   filter bind_rows
#' @importFrom rlang .data
#' @importFrom stats as.formula
#' @export
pc_to_PKNCAconc <- function(pc, ex, dm = NULL, metabolites = NULL) { # nolint: object_name_linter


  # --- Validate inputs --------------------------------------------------------
  pc_required <- c("STUDYID", "USUBJID", "PCTEST", "PCSTRESN", "PCSTRESU",
                   "PCDTC")
  ex_required <- c("STUDYID", "USUBJID", "EXTRT", "EXDOSE", "EXDOSU",
                   "EXROUTE", "EXSTDTC")
  .check_required_cols(pc, pc_required, "pc")
  .check_required_cols(ex, ex_required, "ex")

  # --- Build dose lookup table ------------------------------------------------
  dose_tbl <- .prepare_dose_table(ex)

  # --- Parse PC datetimes -----------------------------------------------------
  pc$PCDTC_posix <- std_dtc_to_rdate(pc$PCDTC)

  # First dose time per subject (from EX)
  first_dose <- dose_tbl %>%
    group_by(.data$USUBJID) %>%
    filter(.data$DOSNOA == 1) %>%
    ungroup() %>%
    select("USUBJID", first_dose_time = "EXSTDTC_posix")

  # --- AFRLT: actual time from first dose -------------------------------------
  conc_data <- pc %>%
    left_join(first_dose, by = "USUBJID") %>%
    mutate(
      AFRLT = as.numeric(difftime(
        .data$PCDTC_posix, .data$first_dose_time, units = "hours"
      ))
    )

  # --- Assign each PC record to its most recent prior dose --------------------
  conc_data <- .assign_doses_to_conc(conc_data, dose_tbl)

  # --- ARRLT: actual time from most recent dose -------------------------------
  conc_data <- conc_data %>%
    mutate(
      ARRLT = as.numeric(difftime(
        .data$PCDTC_posix, .data$ref_dose_time, units = "hours"
      ))
    )

  # --- Nominal times ----------------------------------------------------------
  # NRRLT: from PCELTM (nominal time from reference / most recent dose)
  has_pceltm <- "PCELTM" %in% names(pc) &&
    any(!is.na(pc$PCELTM) & nchar(trimws(pc$PCELTM)) > 0)
  if (has_pceltm) {
    nrrlt_parsed <- parse_iso8601_duration(pc$PCELTM)
    # Fill rows where PCELTM was empty/unparseable with ARRLT
    na_nrrlt <- is.na(nrrlt_parsed)
    nrrlt_parsed[na_nrrlt] <- conc_data$ARRLT[na_nrrlt]
    conc_data$NRRLT <- nrrlt_parsed
  } else {
    # Fallback: use ARRLT as nominal
    conc_data$NRRLT <- conc_data$ARRLT
  }

  # NFRLT: NRRLT + nominal dose time of matched dose from first dose
  conc_data$NFRLT <- conc_data$NRRLT + conc_data$ref_dose_NFRLT

  # --- Map remaining columns to aNCA standard names --------------------------
  conc_data$PARAM  <- pc$PCTEST
  conc_data$PCSPEC <- if ("PCSPEC" %in% names(pc)) pc$PCSPEC else "UNKNOWN"
  conc_data$AVAL   <- as.numeric(pc$PCSTRESN)
  conc_data$AVALU  <- pc$PCSTRESU
  conc_data$RRLTU  <- "Hours"
  conc_data$CONCDUR <- 0
  conc_data$ATPTREF <- paste("DOSE", conc_data$DOSNOA)

  # Volume (for excretion analysis)
  if ("VOLUME" %in% names(pc)) {
    conc_data$VOLUME <- as.numeric(pc$VOLUME)
  }
  if ("VOLUMEU" %in% names(pc)) {
    conc_data$VOLUMEU <- pc$VOLUMEU
  }

  # --- Metabolite flag --------------------------------------------------------
  if (!is.null(metabolites) && length(metabolites) > 0) {
    conc_data$METABFL <- ifelse(conc_data$PARAM %in% metabolites, "Y", "")
  } else {
    conc_data$METABFL <- ""
  }

  # --- Join DM demographics ---------------------------------------------------
  if (!is.null(dm)) {
    dm_cols <- c("USUBJID", "AGE", "AGEU", "SEX", "RACE", "ARM", "ACTARM")
    dm_subset <- dm[, intersect(dm_cols, names(dm)), drop = FALSE]
    # Deduplicate DM to one row per subject
    dm_subset <- dm_subset[!duplicated(dm_subset$USUBJID), , drop = FALSE]
    conc_data <- left_join(conc_data, dm_subset, by = "USUBJID")
  }

  # --- ADNCA-compatible aliases ------------------------------------------------
  # Downstream modules (TLG, exploration plots) reference TRT01A.
  # TODO (Gerardo): Reconsider if this is what you actually want to do
  if (!"TRT01A" %in% names(conc_data) && "DOSETRT" %in% names(conc_data)) {
    conc_data$TRT01A <- conc_data$DOSETRT
  }

  # --- Initialise aNCA-specific columns ---------------------------------------
  conc_data$nca_exclude       <- ""
  conc_data$is.excluded.hl    <- FALSE
  conc_data$is.included.hl    <- FALSE
  conc_data$exclude_half.life <- FALSE
  conc_data$REASON            <- ""

  # --- Volume unit conversion (for excretion analysis) -------------------------
  conc_data <- convert_volume_units(conc_data)

  # --- Clean up internal columns ----------------------------------------------
  conc_data <- conc_data %>%
    select(-any_of(c(
      "PCDTC_posix", "first_dose_time", "ref_dose_time", "ref_dose_NFRLT"
    )))

  # --- Build PKNCAconc object -------------------------------------------------
  conc_formula <- AVAL ~ AFRLT | STUDYID + PCSPEC + DOSETRT + USUBJID / PARAM

  args_list <- list(
    data = conc_data,
    formula = conc_formula,
    exclude_half.life = "exclude_half.life",
    include_half.life = "include_half.life",
    time.nominal = "NFRLT",
    duration = "CONCDUR",
    concu = "AVALU",
    timeu = "RRLTU",
    exclude = "nca_exclude"
  )

  if ("VOLUME" %in% names(conc_data)) {
    args_list$volume <- "VOLUME"
  }
  if ("AMOUNTU" %in% names(conc_data)) {
    args_list$amountu <- "AMOUNTU"
  }

  do.call(PKNCA::PKNCAconc, args_list)
}


# --- EX to PKNCAdose ---------------------------------------------------------

#' Convert an EX (Exposure) SDTM domain to a PKNCAdose object
#'
#' Transforms a CDISC SDTM EX domain data frame into a \code{PKNCAdose} object
#' compatible with the aNCA pipeline. The dose formula uses
#' \code{DOSEA ~ AFRLT | STUDYID + DOSETRT + USUBJID}, matching the structure
#' produced by \code{\link{PKNCA_create_data_object}}.
#'
#' @param ex A data.frame containing the EX (Exposure) SDTM domain.
#'   Required columns: STUDYID, USUBJID, EXTRT, EXDOSE, EXDOSU, EXROUTE,
#'   EXSTDTC. Optional: EXENDTC, EXDUR, EXELTM.
#'
#' @returns A \code{PKNCAdose} object with formula
#'   \code{DOSEA ~ AFRLT | STUDYID + DOSETRT + USUBJID}.
#'
#' @importFrom dplyr select any_of
#' @importFrom stats as.formula
#' @export
ex_to_PKNCAdose <- function(ex) { # nolint: object_name_linter

  ex_required <- c("STUDYID", "USUBJID", "EXTRT", "EXDOSE", "EXDOSU",
                   "EXROUTE", "EXSTDTC")
  .check_required_cols(ex, ex_required, "ex")

  dose_tbl <- .prepare_dose_table(ex)

  # Remove internal datetime column before passing to PKNCA
  dose_tbl <- dose_tbl %>%
    select(-any_of("EXSTDTC_posix"))

  dose_formula <- DOSEA ~ AFRLT | STUDYID + DOSETRT + USUBJID

  PKNCA::PKNCAdose(
    data = dose_tbl,
    formula = dose_formula,
    route = "std_route",
    time.nominal = "NFRLT",
    duration = "ADOSEDUR",
    doseu = "DOSEU"
  )
}


# --- Top-level wrapper --------------------------------------------------------

#' Convert SDTM PC, EX, and DM domains to a PKNCAdata object
#'
#' Builds a \code{PKNCAdata} object from SDTM domain data frames, producing
#' output compatible with the aNCA downstream pipeline
#' (\code{\link{PKNCA_update_data_object}}, interval formatting, exploration
#' plots, TLG export, etc.).
#'
#' @section Assumptions:
#' \itemize{
#'   \item \code{PCELTM} / \code{PCRFTDTC} must reference the most recent dose
#'     for each sample. If the data provider used a different convention,
#'     nominal times will be inconsistent.
#'   \item Treatment interval (TRTRINT / tau) is not derived. The app falls
#'     back to dose counting via DOSNOA for single-vs-multiple dose detection.
#' }
#'
#' @param pc A data.frame containing the PC (Pharmacokinetic Concentrations)
#'   SDTM domain.
#' @param ex A data.frame containing the EX (Exposure) SDTM domain.
#' @param dm Optional data.frame containing the DM (Demographics) SDTM domain.
#' @param metabolites Optional character vector of PCTEST values to flag as
#'   metabolites.
#'
#' @returns A \code{PKNCAdata} object with concentration, dose, and units,
#'   ready for use with \code{\link{PKNCA_update_data_object}}.
#'
#' @examples
#' \dontrun{
#' pc <- read.csv("pc_example.csv")
#' ex <- read.csv("ex_example.csv")
#' dm <- read.csv("dm_example.csv")
#' pknca_data <- sdtm_to_PKNCAdata(pc, ex, dm)
#' }
#'
#' @export
sdtm_to_PKNCAdata <- function(pc, ex, dm = NULL, metabolites = NULL) { # nolint: object_name_linter

  pknca_conc <- pc_to_PKNCAconc(pc, ex, dm, metabolites)
  pknca_dose <- ex_to_PKNCAdose(ex)

  # Basic intervals so PKNCAdata can be created
  intervals <- data.frame(
    start = 0, end = Inf,
    cmax = TRUE,
    tmax = TRUE,
    auclast = FALSE,
    aucinf.obs = FALSE
  )

  pknca_data <- PKNCA::PKNCAdata(
    data.conc = pknca_conc,
    data.dose = pknca_dose,
    intervals = intervals,
    units = PKNCA_build_units_table(pknca_conc, pknca_dose)
  )

  pknca_data
}


# --- Internal helpers ---------------------------------------------------------

#' Assign each concentration record to its most recent prior dose
#'
#' For each row in \code{conc_data}, finds the dose from \code{dose_tbl} with
#' the largest \code{EXSTDTC_posix <= PCDTC_posix} for the same subject.
#' Pre-first-dose samples are assigned to dose 1.
#'
#' @param conc_data Data.frame with columns USUBJID, PCDTC_posix.
#' @param dose_tbl Data.frame from \code{.prepare_dose_table}.
#' @returns \code{conc_data} with added columns: DOSETRT, DOSEA, DOSEU, ROUTE,
#'   std_route, ADOSEDUR, DOSNOA, ref_dose_time, ref_dose_NFRLT.
#'
#' @importFrom dplyr left_join filter group_by slice_tail ungroup select
#'   bind_rows arrange
#' @importFrom rlang .data
#' @keywords internal
.assign_doses_to_conc <- function(conc_data, dose_tbl) {
  # Prepare dose lookup with only the columns we need
  dose_lookup <- dose_tbl %>%
    select("USUBJID", "DOSETRT", "DOSEA", "DOSEU", "ROUTE", "std_route",
           "ADOSEDUR", "DOSNOA",
           ref_dose_time = "EXSTDTC_posix",
           ref_dose_NFRLT = "NFRLT")

  # Tag each conc row for tracking

  conc_data$.ROW_ID <- seq_len(nrow(conc_data))

  # Cross-join conc with dose (same subject), keep only doses <= sample time,
  # then pick the most recent dose per concentration record
  matched <- conc_data %>%
    select("USUBJID", "PCDTC_posix", ".ROW_ID") %>%
    left_join(dose_lookup, by = "USUBJID", relationship = "many-to-many") %>%
    filter(.data$ref_dose_time <= .data$PCDTC_posix) %>%
    group_by(.data$.ROW_ID) %>%
    arrange(.data$ref_dose_time) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(-"PCDTC_posix", -"USUBJID")

  # Handle pre-first-dose samples: no dose matched, assign to dose 1
  first_doses <- dose_lookup %>%
    filter(.data$DOSNOA == 1)

  unmatched_ids <- setdiff(conc_data$.ROW_ID, matched$.ROW_ID)
  if (length(unmatched_ids) > 0) {
    unmatched <- conc_data[conc_data$.ROW_ID %in% unmatched_ids, ] %>%
      select("USUBJID", ".ROW_ID") %>%
      left_join(first_doses, by = "USUBJID") %>%
      select(-"USUBJID")
    matched <- bind_rows(matched, unmatched)
  }

  # Join back to conc_data
  conc_data <- conc_data %>%
    left_join(matched, by = ".ROW_ID") %>%
    select(-".ROW_ID")

  conc_data
}


#' Check that required columns exist in a data.frame
#'
#' @param df Data.frame to check.
#' @param required Character vector of required column names.
#' @param df_name Name of the data.frame for error messages.
#' @returns Invisible NULL. Raises an error if columns are missing.
#' @keywords internal
.check_required_cols <- function(df, required, df_name) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      "Missing required columns in ", df_name, ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(NULL)
}
