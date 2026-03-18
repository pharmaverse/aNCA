# Derive ex_example (SDTM EX-like dataset) from adnca_example
#
# This script creates an exposure/dosing dataset with SDTM-style column names
# from the existing ADaM-style adnca_example. It is intended to support and
# test the PC+EX upload workflow (see issue #624).
#
# Datetimes are derived using per-subject treatment start dates from dm_example.

adnca_example <- read.csv("data-raw/adnca_example.csv", na.strings = c("", "NA"))

# Load dm_example to get per-subject reference dates.
# dm_example.R must be run first.
load("data/dm_example.rda")

# Build a lookup: USUBJID -> reference origin (POSIXct)
ref_dates <- stats::setNames(
  as.POSIXct(dm_example$RFXSTDTC, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
  dm_example$USUBJID
)

# Each row in adnca_example carries dosing info repeated across concentration
# observations. Extract one row per dosing event, identified by the combination
# of STUDYID + USUBJID + DOSETRT + ATPTREF.
# Dose time from first dose = AFRLT - ARRLT.
adnca_example$dose_time <- adnca_example$AFRLT - adnca_example$ARRLT

dose_keys <- c("STUDYID", "USUBJID", "DOSETRT", "ATPTREF")
dose_rows <- !duplicated(adnca_example[, dose_keys])
doses <- adnca_example[dose_rows, ]

# Per-row reference origin based on subject
subject_origin <- ref_dates[doses$USUBJID]

ex_example <- data.frame(
  STUDYID  = doses$STUDYID,
  USUBJID  = doses$USUBJID,
  EXTRT    = doses$DOSETRT,
  EXDOSE   = doses$DOSEA,
  EXDOSU   = doses$DOSEU,
  EXROUTE  = doses$ROUTE,
  EXSTDTC  = format(subject_origin + doses$dose_time * 3600,
                    "%Y-%m-%dT%H:%M:%S"),
  EXDUR    = sprintf("PT%gH", doses$ADOSEDUR),
  stringsAsFactors = FALSE
)

usethis::use_data(ex_example, overwrite = TRUE)
