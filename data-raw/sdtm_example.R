# Derive SDTM-like example datasets (DM, PC, EX) from adnca_example
#
# This script creates three datasets with SDTM-style column names from the
# existing ADaM-style adnca_example. Intended to support and test the PC+EX
# upload workflow (see issues #624 and #1086).

adnca_example <- read.csv("data-raw/adnca_example.csv", na.strings = c("", "NA"))

# --- DM (Demographics) -------------------------------------------------------
# One row per subject with demographics and a synthetic treatment start date.

subjects <- adnca_example[!duplicated(adnca_example$USUBJID), ]

# Stagger treatment start dates by 3 days per subject to simulate different
# enrollment dates.
base_date <- as.POSIXct("2024-01-15 08:00:00", tz = "UTC")
subject_offsets_days <- seq(0, by = 3, length.out = nrow(subjects))
rfxstdtc <- base_date + subject_offsets_days * 86400

dm_example <- data.frame(
  DOMAIN = "DM",
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

# --- Reference date lookup ----------------------------------------------------
# Used by both PC and EX below.

ref_dates <- stats::setNames(
  as.POSIXct(dm_example$RFXSTDTC, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
  dm_example$USUBJID
)

# --- PC (Pharmacokinetic Concentrations) --------------------------------------
# One row per concentration observation, same row count as adnca_example.

subject_origin_pc <- ref_dates[adnca_example$USUBJID]
dose_time_from_first <- adnca_example$AFRLT - adnca_example$ARRLT

pc_example <- data.frame(
  DOMAIN = "PC",
  STUDYID  = adnca_example$STUDYID,
  USUBJID  = adnca_example$USUBJID,
  PCTEST   = adnca_example$PARAM,
  PCSPEC   = adnca_example$PCSPEC,
  PCSTRESN = adnca_example$AVAL,
  PCSTRESU = adnca_example$AVALU,
  PCDTC    = format(subject_origin_pc + adnca_example$AFRLT * 3600,
                    "%Y-%m-%dT%H:%M:%S"),
  PCRFTDTC = format(subject_origin_pc + dose_time_from_first * 3600,
                    "%Y-%m-%dT%H:%M:%S"),
  PCELTM   = sprintf("PT%gH", adnca_example$NFRLT),
  VOLUME   = adnca_example$VOLUME,
  VOLUMEU  = adnca_example$VOLUMEU,
  stringsAsFactors = FALSE
)

# --- EX (Exposure / Dosing) ---------------------------------------------------
# One row per dosing event, deduplicated from adnca_example.

ex_example <- adnca_example %>%
  dplyr::mutate(
    dose_time = AFRLT - ARRLT,
    nominal_dose_time = NFRLT - NRRLT
  ) %>%
  dplyr::distinct(STUDYID, USUBJID, DOSETRT, ATPTREF, .keep_all = TRUE) %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::mutate(
    subject_origin = ref_dates[USUBJID],
    exeltm = nominal_dose_time - min(nominal_dose_time)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    DOMAIN   = "EX",
    STUDYID,
    USUBJID,
    EXTRT    = DOSETRT,
    EXDOSE   = DOSEA,
    EXDOSU   = DOSEU,
    EXROUTE  = ROUTE,
    EXSTDTC  = format(subject_origin + dose_time * 3600, "%Y-%m-%dT%H:%M:%S"),
    EXENDTC  = format(subject_origin + dose_time * 3600 + ADOSEDUR * 3600,
                      "%Y-%m-%dT%H:%M:%S"),
    EXDUR    = sprintf("PT%gH", ADOSEDUR),
    EXELTM   = sprintf("PT%gH", exeltm)
  ) %>%
  as.data.frame()

# --- Save all three -----------------------------------------------------------

usethis::use_data(dm_example, overwrite = TRUE)
usethis::use_data(pc_example, overwrite = TRUE)
usethis::use_data(ex_example, overwrite = TRUE)
