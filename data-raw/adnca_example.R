# Generate adnca_example dataset
#
# Self-contained script that creates a simulated ADNCA dataset with:
# - 8 subjects, 8 doses each, 24h dosing interval
# - IV drip (subjects 1-5) and IV bolus (subjects 6-8)
# - Parent analyte (DrugA) and metabolite (Metab-DrugA)
# - Urine samples for subjects 1-5
# - NCA exclusion reasons for subjects 2 and 4
# - Demographic covariates (AGE, SEX, RACE)
#
# Original script history: commits 1a7d488fd -> 941dea4c5 -> 22ac1f959 ->
# f57f8b0d3 -> 789e7e1c0, consolidated here with a fix for the rounding
# artifact where AFRLT and ARRLT were rounded independently, causing
# AFRLT - ARRLT to jitter by +/-0.001h across rows within the same dose period.
#
# Fix: keep actual.dose.time as a first-class column throughout the pipeline,
# compute AFRLT = actual.dose.time + ARRLT from the canonical value, and round
# only once at the end.

library(dplyr)
library(tidyr)

set.seed(1)

# --- PK model ----------------------------------------------------------------

pk_aval <- function(time, Tmax, Cmax, lambda_z) {
  concentration <- ifelse(
    time <= Tmax,
    Cmax * (time / Tmax),
    Cmax * exp(-lambda_z * (time - Tmax))
  )
  ifelse(time < 0, 0, concentration)
}

# --- Helper: add a sample at a given nominal relative time --------------------
# Adds one new sample per (USUBJID, PARAM, PCSPEC, DOSNOP) at the specified
# NRRLT. Uses the stored actual.dose.time (not AFRLT - ARRLT) to avoid
# rounding-induced jitter.

add_sample_per_dose <- function(adnca, nrrlt, sd = 0.05) {
  new_samples <- adnca %>%
    arrange(USUBJID, AFRLT) %>%
    group_by(USUBJID, PARAM, PCSPEC, DOSNOP) %>%
    slice(1) %>%
    group_by(USUBJID, DOSNOP) %>%
    mutate(
      nominal.dose.time = (NFRLT - NRRLT)[1],
      NRRLT = nrrlt,
      ARRLT = NRRLT + rnorm(n = n(), mean = 0, sd = sd),
      NFRLT = nominal.dose.time + NRRLT,
      AFRLT = actual.dose.time + ARRLT
    ) %>%
    ungroup()
  bind_rows(adnca, new_samples) %>%
    arrange(USUBJID, PCSPEC, DOSETRT, PARAM, AFRLT, NFRLT)
}

# --- Build base dataset (one row per subject x dose) -------------------------

adnca <- crossing(
  STUDYID = "S1",
  USUBJID = paste0("S1-", sprintf("%02d", 1:8)),
  DOSNOP = 1:8
) %>%
  mutate(
    SEX = ifelse(
      USUBJID %in% paste0("S1-", sprintf("%02d", c(1, 2, 3, 5, 8))), "M", "F"
    ),
    RACE = ifelse(
      USUBJID %in% paste0("S1-", sprintf("%02d", c(1, 4, 5, 8, 9))),
      "WHITE", "ASIAN"
    ),
    AGE = case_when(
      USUBJID %in% paste0("S1-", sprintf("%02d", c(1, 4))) ~ 25,
      USUBJID %in% paste0("S1-", sprintf("%02d", c(2, 5))) ~ 30,
      USUBJID %in% paste0("S1-", sprintf("%02d", c(6)))    ~ 35,
      USUBJID %in% paste0("S1-", sprintf("%02d", c(3)))    ~ 40,
      USUBJID %in% paste0("S1-", sprintf("%02d", c(7)))    ~ 50,
      USUBJID %in% paste0("S1-", sprintf("%02d", c(8)))    ~ 60
    ),
    AGEU = "Years",
    PARAM = "DrugA",
    PCSPEC = "SERUM",
    ROUTE = "INTRAVENOUS DRIP",
    NDOSEDUR = 3,
    RRLTU = "Hours",
    DOSEU = "mg",
    ATPTREF = paste0("DOSE ", DOSNOP)
  ) %>%
  mutate(
    DOSEA = ifelse(
      USUBJID %in% unique(USUBJID)[c(1, 3, 5, 7, 9)], 5, 10
    ),
    TRT01A = paste0("DrugA ", DOSEA, " mg, Infusion"),
    DOSETRT = "DrugA",
    TRTRINT = 24
  ) %>%
  group_by(USUBJID, DOSNOP) %>%
  mutate(
    # Canonical dose times -- kept as columns, never recovered from subtraction
    nominal.dose.time = TRTRINT * (DOSNOP - 1),
    actual.dose.time = nominal.dose.time + rnorm(n(), mean = 0, sd = 0.1),
    actual.dose.time = ifelse(actual.dose.time < 0, 0, actual.dose.time),
    actual.dose.time = actual.dose.time[1],
    ADOSEDUR = (NDOSEDUR[1] + rnorm(n(), mean = 0, sd = 0.05))[1]
  ) %>%
  ungroup() %>%
  group_by(USUBJID, DOSNOP) %>%
  mutate(rn = row_number()) %>%
  mutate(
    NRRLT = rn * 1.5,
    ARRLT = (NRRLT + rnorm(n(), mean = 0, sd = 0.05))[1],
    NFRLT = nominal.dose.time + NRRLT,
    AFRLT = actual.dose.time + ARRLT
  ) %>%
  ungroup() %>%
  # Add samples at specific nominal relative times
  add_sample_per_dose(-5 / 60, sd = 0.001) %>%
  add_sample_per_dose(0.5) %>%
  add_sample_per_dose(1.5) %>%
  add_sample_per_dose(3) %>%
  add_sample_per_dose(4.5) %>%
  add_sample_per_dose(6) %>%
  add_sample_per_dose(7.5) %>%
  add_sample_per_dose(12) %>%
  # For DOSE 1, actual.dose.time = 0, so AFRLT = ARRLT
  mutate(
    AFRLT = ifelse(ATPTREF == "DOSE 1", ARRLT, AFRLT)
  ) %>%
  # Concentration based on dose and time
  mutate(
    AVAL = pk_aval(ARRLT, Tmax = ADOSEDUR, Cmax = 8 * (DOSEA / 10),
                   lambda_z = 0.5),
    AVALU = "ug/mL"
  ) %>%
  mutate(
    AVAL = AVAL * ifelse(SEX == "M", 1, 1.1),
    AVAL = AVAL * ifelse(RACE == "WHITE", 1, 1.15),
    AVAL = AVAL * ifelse(
      USUBJID %in% paste0("S1-", sprintf("%02d", c(2, 4, 5, 8))), 0.95, 1
    ),
    AVAL = AVAL + rnorm(n = nrow(.), mean = 0, sd = 0.05),
    AVAL = ifelse(AVAL < 0, 0, AVAL)
  ) %>%
  mutate(
    METABFL = "",
    PCSTRESC = as.character(AVAL),
    AEFRLT = AFRLT,
    NEFRLT = NFRLT,
    AERRLT = ARRLT,
    NERRLT = NRRLT
  )

# --- Add metabolite -----------------------------------------------------------

adnca_metab <- adnca %>%
  mutate(
    PARAM = "Metab-DrugA",
    AVAL = AVAL * 0.4,
    METABFL = "Y"
  )

adnca <- bind_rows(adnca, adnca_metab)

# --- IV bolus for subjects 6-8 ------------------------------------------------

adnca <- adnca %>%
  mutate(is.bolus = USUBJID %in% unique(USUBJID)[6:8]) %>%
  mutate(
    ADOSEDUR = ifelse(is.bolus, 0, ADOSEDUR),
    ROUTE = ifelse(is.bolus, "INTRAVENOUS BOLUS", "INTRAVENOUS DRIP"),
    TRT01A = ifelse(
      is.bolus, paste0("DrugA ", DOSEA, " mg, bolus"), TRT01A
    ),
    DOSETRT = "DrugA",
    AVAL = ifelse(
      is.bolus,
      pk_aval(ARRLT, 0, 10 * (DOSEA / max(DOSEA)), 0.5),
      AVAL
    )
  ) %>%
  mutate(
    AVAL = ifelse(is.bolus, AVAL * ifelse(SEX == "M", 1, 1.1), AVAL),
    AVAL = AVAL + rnorm(n = nrow(.), mean = 0, sd = 0.05),
    AVAL = ifelse(AVAL < 0, 0, AVAL),
    PCSTRESC = as.character(AVAL)
  )

# --- Urine samples for subjects 1-5 ------------------------------------------

adnca_urine <- adnca %>%
  filter(USUBJID %in% unique(USUBJID)[1:5]) %>%
  filter(METABFL == "") %>%
  # Keep only non-overlapping collection intervals for urine (4 per dose).
  # Matches the "fix collapsing collection times" correction (6907e901e).
  filter(NRRLT %in% c(0.5, 3, 6, 12)) %>%
  mutate(
    PCSPEC = "URINE",
    VOLUME = round(rnorm(n = nrow(.), mean = 100, sd = 15)),
    VOLUMEU = "mL",
    NEFRLT = NFRLT + 2,
    NERRLT = NRRLT + 2,
    AEFRLT = round(NEFRLT + rnorm(n = nrow(.), mean = 0, sd = 0.1), 1),
    AERRLT = ARRLT + (AEFRLT - AFRLT),
    AVALU = "mg/mL",
    ATPTREF = paste0("DOSE ", DOSNOP)
  )

# --- Assemble final dataset ---------------------------------------------------

adnca_example <- bind_rows(adnca, adnca_urine) %>%
  # Round time variables so that AFRLT - ARRLT is exactly the rounded dose time.
  # Rounding AFRLT and ARRLT independently would introduce IEEE 754 jitter in
  # their difference, so we round the components first and recompute AFRLT/NFRLT.
  mutate(
    actual.dose.time = round(actual.dose.time, 3),
    nominal.dose.time = round(nominal.dose.time, 3),
    ARRLT = round(ARRLT, 3),
    NRRLT = round(NRRLT, 3),
    AFRLT = actual.dose.time + ARRLT,
    NFRLT = nominal.dose.time + NRRLT,
    # For DOSE 1, actual.dose.time = 0, so AFRLT = ARRLT (already handled above
    # but re-enforce after rounding)
    AFRLT = ifelse(ATPTREF == "DOSE 1", ARRLT, AFRLT)
  ) %>%
  # For SERUM rows, excretion time vars mirror the standard time vars.
  # Update them after rounding so they stay in sync.
  mutate(
    AEFRLT = ifelse(PCSPEC == "SERUM", AFRLT, round(AEFRLT, 3)),
    NEFRLT = ifelse(PCSPEC == "SERUM", NFRLT, round(NEFRLT, 3)),
    AERRLT = ifelse(PCSPEC == "SERUM", ARRLT, round(AERRLT, 3)),
    NERRLT = ifelse(PCSPEC == "SERUM", NRRLT, round(NERRLT, 3))
  ) %>%
  # Round remaining numeric columns
  mutate(across(
    where(is.numeric) & !any_of(c(
      "AFRLT", "NFRLT", "ARRLT", "NRRLT",
      "AEFRLT", "NEFRLT", "AERRLT", "NERRLT"
    )),
    ~ round(.x, 3)
  )) %>%
  select(
    any_of(c(
      "STUDYID", "USUBJID", "PCSPEC", "PARAM", "METABFL",
      "AFRLT", "NFRLT", "ARRLT", "NRRLT", "TRTRINT", "RRLTU",
      "AVAL", "AVALU", "VOLUME", "VOLUMEU",
      "DOSETRT", "TRT01A", "DOSEA", "DOSEU", "ROUTE", "ADOSEDUR",
      "ATPTREF", "AGE", "AGEU", "RACE", "SEX",
      "NCA1XRS", "NCA2XRS", "NEFRLT", "NERRLT", "AEFRLT", "AERRLT"
    ))
  ) %>%
  # Remove time duplicates (IV bolus can produce them)
  group_by(STUDYID, USUBJID, PARAM, PCSPEC, AFRLT) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(STUDYID, USUBJID, DOSETRT, PARAM, PCSPEC, AFRLT, NFRLT) %>%
  filter(!is.na(AFRLT))

# --- NCA exclusion reasons ----------------------------------------------------

adnca_example <- adnca_example %>%
  mutate(
    NCA1XRS = ifelse(
      USUBJID %in% unique(USUBJID)[c(2, 4)] & NFRLT <= 48,
      "Invalid administration", ""
    ),
    NCA2XRS = ifelse(is.na(AVAL), "Missing AVAL value", "")
  )

# --- DOSFRM -------------------------------------------------------------------

adnca_example <- adnca_example %>%
  mutate(DOSFRM = ifelse(ROUTE == "ORAL", "TABLET", "INJECTION, SOLUTION"))

# --- Verify: dose times should be consistent within each dose period ----------

dose_time_check <- adnca_example %>%
  mutate(actual.dose.time = AFRLT - ARRLT) %>%
  group_by(USUBJID, ATPTREF) %>%
  summarise(sd_actual = sd(actual.dose.time), .groups = "drop")

max_sd <- max(dose_time_check$sd_actual, na.rm = TRUE)
if (max_sd > 1e-10) {
  warning(sprintf(
    "Dose time jitter detected (max sd = %g). Check rounding logic.", max_sd
  ))
} else {
  message("Dose time consistency check passed (no jitter).")
}

# --- Save ---------------------------------------------------------------------

usethis::use_data(adnca_example, overwrite = TRUE)
write.csv(adnca_example, "data-raw/adnca_example.csv", row.names = FALSE)
