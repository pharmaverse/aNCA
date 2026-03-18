# Derive pc_example (SDTM PC-like dataset) from adnca_example
#
# This script creates a pharmacokinetic concentrations dataset with SDTM-style
# column names from the existing ADaM-style adnca_example. It is intended to
# support and test the PC+EX upload workflow (see issue #624).
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

# Per-row reference origin based on subject
subject_origin <- ref_dates[adnca_example$USUBJID]

# Derive the reference dose datetime per observation:
# dose_time_from_first = AFRLT - ARRLT (time of the reference dose from first dose)
dose_time_from_first <- adnca_example$AFRLT - adnca_example$ARRLT

pc_example <- data.frame(
  STUDYID  = adnca_example$STUDYID,
  USUBJID  = adnca_example$USUBJID,
  PCTEST   = adnca_example$PARAM,
  PCSPEC   = adnca_example$PCSPEC,
  PCSTRESN = adnca_example$AVAL,
  PCSTRESU = adnca_example$AVALU,
  PCDTC    = format(subject_origin + adnca_example$AFRLT * 3600,
                    "%Y-%m-%dT%H:%M:%S"),
  PCRFTDTC = format(subject_origin + dose_time_from_first * 3600,
                    "%Y-%m-%dT%H:%M:%S"),
  PCELTM   = sprintf("PT%gH", adnca_example$NFRLT),
  VOLUME   = adnca_example$VOLUME,
  VOLUMEU  = adnca_example$VOLUMEU,
  stringsAsFactors = FALSE
)

usethis::use_data(pc_example, overwrite = TRUE)
