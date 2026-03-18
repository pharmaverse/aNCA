# Derive dm_example (SDTM DM-like dataset) from adnca_example
#
# This script creates a demographics dataset with SDTM-style column names from
# the existing ADaM-style adnca_example. One row per subject. Each subject gets
# a synthetic treatment start date (RFXSTDTC) that is used as the reference
# origin in pc_example and ex_example.

adnca_example <- read.csv("data-raw/adnca_example.csv", na.strings = c("", "NA"))

# One row per subject
subjects <- adnca_example[!duplicated(adnca_example$USUBJID), ]

# Assign a different treatment start date per subject, staggered by a few days
# to make the example realistic (subjects enrolled on different days).
base_date <- as.POSIXct("2024-01-15 08:00:00", tz = "UTC")
subject_offsets_days <- seq(0, by = 3, length.out = nrow(subjects))
rfxstdtc <- base_date + subject_offsets_days * 86400

dm_example <- data.frame(
  STUDYID  = subjects$STUDYID,
  USUBJID  = subjects$USUBJID,
  AGE      = subjects$AGE,
  AGEU     = subjects$AGEU,
  SEX      = subjects$SEX,
  RACE     = subjects$RACE,
  ARM      = subjects$TRT01A,
  ACTARM   = subjects$TRT01A,
  RFXSTDTC = format(rfxstdtc, "%Y-%m-%dT%H:%M:%S"),
  stringsAsFactors = FALSE
)

usethis::use_data(dm_example, overwrite = TRUE)
