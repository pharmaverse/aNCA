
base_df <- expand.grid(
  USUBJID = c("Subject1", "Subject2", "Subject3", "Subject4"),
  PARAM = c("Analyte1", "Analyte2"),
  PCSPEC = c("Spec1", "Spec2"),
  ATPTREF = c(1, 2), # 2 cycles
  NRRLT = 0:5 # 6 nominal time points per cycle
)

set.seed(123) # for reproducible ARRLT
test_data <- base_df %>%
  dplyr::arrange(USUBJID, PARAM, PCSPEC, ATPTREF, NRRLT) %>%
  dplyr::mutate(
    STUDYID = "Study1",
    EVID = 0,
    RRLTU = "hours",
    AVALU = "ng/mL",
    DOSEA = ifelse(PARAM == "Analyte1", 35, 70),
    SEX = rep(c("M", "F"), length.out = nrow(.)),
    # Create correlated time vars
    # ARRLT: Actual time relative to dose
    ARRLT = ifelse(NRRLT == 0, -0.1, NRRLT + runif(n(), -0.1, 0.1)),
    # AFRLT: Actual time from first dose
    AFRLT = ifelse(ATPTREF == 1, ARRLT, ARRLT + 168), # 168h cycle
    # NFRLT: Nominal time from first dose
    NFRLT = ifelse(ATPTREF == 1, NRRLT, NRRLT + 168),
    # AVAL: Concentration
    AVAL = DOSEA * exp(-0.1 * (NRRLT + 1)) + rnorm(n(), 0, 2)
  )

# --- Add Edge Cases ---

# 1. EVID=1 record (should be filtered out)
evid_rec <- test_data %>%
  dplyr::filter(USUBJID == "Subject1", PARAM == "Analyte1",
                PCSPEC == "Spec1", ATPTREF == 1, NRRLT == 1) %>%
  dplyr::mutate(EVID = 1, AVAL = NA)

# 2. NA AVAL record (should be filtered out)
na_aval_rec <- test_data %>%
  dplyr::filter(USUBJID == "Subject1", PARAM == "Analyte1",
                PCSPEC == "Spec1", ATPTREF == 1, NRRLT == 2) %>%
  dplyr::mutate(AVAL = NA)

# 3. Zero/Negative AVAL records (for log scale test)
zero_aval_recs <- test_data %>%
  dplyr::filter(PARAM == "Analyte2", PCSPEC == "Spec2", ATPTREF == 1, NRRLT == 0) %>%
  dplyr::mutate(AVAL = 0) # All 4 subjects get 0

neg_aval_rec <- test_data %>%
  dplyr::filter(USUBJID == "Subject1", PARAM == "Analyte2",
                PCSPEC == "Spec2", ATPTREF == 1, NRRLT == 1) %>%
  dplyr::mutate(AVAL = -10)

sample_data <- dplyr::bind_rows(
  test_data, evid_rec, na_aval_rec, zero_aval_recs, neg_aval_rec
) %>%
  # Ensure no duplicates from the bind_rows
  dplyr::distinct(USUBJID, PARAM, PCSPEC, ATPTREF, NRRLT, EVID, .keep_all = TRUE)



describe("process_data_individual functions correctly", {

  it("returns a list with default settings", {
    res <- process_data_individual(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1"
    )

    # Default time_scale is "All Time", which uses AFRLT
    expect_true("AFRLT" == res$time_col)
  })


  it("handles missing columns gracefully", {
    incomplete_data <- sample_data %>% select(-AVAL)
    expect_error(
      process_data_individual(
        data = incomplete_data,
        selected_usubjids = "Subject1",
        selected_analytes = "Analyte1",
        selected_pcspec = "Spec1"
      ),
      "object 'AVAL' not found"
    )
  })

  it("filters out EVID != 0 and NA AVAL", {
    p <- process_data_individual(
      data = sample_data, # sample_data contains EVID=1 and NA AVAL
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1"
    )
    # Check that the records were removed by the internal filter
    expect_true(all(p$data$EVID == 0))
    expect_true(all(!is.na(p$data$AVAL)))
  })


  it("changes time scale if selected profiles is not null", {
    p <- process_data_individual(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      profiles_selected = 1
    )
    # 'By Dose Profile' uses ARRLT
    expect_true("ARRLT" == p$time_col)
    # Check that data is filtered to the selected cycle
    expect_true(all(p$processed_data$ATPTREF == 1))
  })

  it("handles predose duplication for 'By Dose Profile'", {

    p <- process_data_individual(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      profiles_selected = 1
    )

    predose_record_in_plot <- p$processed_data %>%
      filter(NFRLT == 168)

    expect_true(nrow(predose_record_in_plot) == 1)
    expect_true(predose_record_in_plot$ATPTREF == 1)
  })

  it("filters non-positive AVAL if log scale selected", {
    p <- process_data_individual(
      data = sample_data, # sample_data has AVAL=0 and AVAL=-10
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte2",
      selected_pcspec = "Spec2",
      profiles_selected = 1,
      ylog_scale = TRUE
    )

    # Check that non-positive values were filtered
    expect_true(all(p$processed_data$AVAL > 0))
  })
})

describe("process_data_mean functions correctly", {


  it("returns a list object with default settings", {
    p <- process_data_mean(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      color_by = "DOSEA"
    )

    # Default time_scale is "All Time", which uses NFRLT for mean plot
    expect_true("NFRLT" == p$time_col)
    # Calculates mean
    expect_true("Mean" %in% names(p$summarised_data))
    # Groups by color by vars
    expect_true("DOSEA" %in% names(p$summarised_data))
  })

  it("handles missing columns gracefully", {
    incomplete_data <- sample_data %>% select(-AVAL)
    expect_error(
      process_data_mean(
        data = incomplete_data,
        selected_analytes = "Analyte1",
        selected_pcspec = "Spec1",
        color_by = "DOSEA"
      ),
      "object 'AVAL' not found"
    )
  })

  it("changes time scale if selected profiles is not null", {
    p <- process_data_mean(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      color_by = "DOSEA",
      profiles_selected = 1
    )

    # 'By Dose Profile' uses NRRLT
    expect_true("NRRLT" == p$time_col)
    # Check that data is filtered to the selected cycle
    expect_true(max(p$summarised_data$NRRLT) <= 5)
  })

  it("supports multiple color_by and facet_by", {
    p <- process_data_mean(
      data = sample_data,
      selected_analytes = c("Analyte1", "Analyte2"),
      selected_pcspec = "Spec1",
      color_by = c("DOSEA", "SEX"),
      facet_by = "PARAM"
    )

    # Check variables exist in summary_data
    expect_true("DOSEA" %in% names(p$summarised_data))
    expect_true("SEX" %in% names(p$summarised_data))
    expect_true("PARAM" %in% names(p$summarised_data))

  })


  it("filters non-positive Mean for log scale", {
    # sample_data (Analyte2, Spec2, Cycle 1, NRRLT=0) has AVAL=0 for all 4 subjects
    # This will result in Mean = 0, which should be filtered.
    p <- process_data_mean(
      data = sample_data,
      selected_analytes = "Analyte2",
      selected_pcspec = "Spec2",
      color_by = "DOSEA",
      profiles_selected = 1,
      ylog_scale = TRUE
    )

    # Check that non-positive Mean values were filtered
    expect_true(all(p$summarised_data$Mean > 0))
  })

})
