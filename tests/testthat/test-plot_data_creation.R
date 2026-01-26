base_df <- expand.grid(
  USUBJID = c("Subject1", "Subject2", "Subject3", "Subject4"),
  PARAM = c("Analyte1", "Analyte2"),
  PCSPEC = c("Spec1", "Spec2"),
  PARAM = c(1, 2), # 2 cycles
  NRRLT = 0:5 # 6 nominal time points per cycle
)

set.seed(123) # for reproducible ARRLT
test_data <- base_df %>%
  dplyr::arrange(USUBJID, PARAM, PCSPEC, PARAM, NRRLT) %>%
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
    AFRLT = ifelse(PARAM == 1, ARRLT, ARRLT + 168), # 168h cycle
    # NFRLT: Nominal time from first dose
    NFRLT = ifelse(PARAM == 1, NRRLT, NRRLT + 168),
    # AVAL: Concentration
    AVAL = DOSEA * exp(-0.1 * (NRRLT + 1)) + rnorm(n(), 0, 2)
  )

# --- Add Edge Cases ---

# 1. EVID=1 record (should be filtered out)
evid_rec <- test_data %>%
  dplyr::filter(USUBJID == "Subject1", PARAM == "Analyte1",
                PCSPEC == "Spec1", PARAM == 1, NRRLT == 1) %>%
  dplyr::mutate(EVID = 1, AVAL = NA)

# 2. NA AVAL record (should be filtered out)
na_aval_rec <- test_data %>%
  dplyr::filter(USUBJID == "Subject1", PARAM == "Analyte1",
                PCSPEC == "Spec1", PARAM == 1, NRRLT == 2) %>%
  dplyr::mutate(AVAL = NA)

# 3. Zero/Negative AVAL records (for log scale test)
zero_aval_recs <- test_data %>%
  dplyr::filter(PARAM == "Analyte2", PCSPEC == "Spec2", PARAM == 1, NRRLT == 0) %>%
  dplyr::mutate(AVAL = 0) # All 4 subjects get 0

neg_aval_rec <- test_data %>%
  dplyr::filter(USUBJID == "Subject1", PARAM == "Analyte2",
                PCSPEC == "Spec2", PARAM == 1, NRRLT == 1) %>%
  dplyr::mutate(AVAL = -10)

sample_data <- dplyr::bind_rows(
  test_data, evid_rec, na_aval_rec, zero_aval_recs, neg_aval_rec
) %>%
  # Ensure no duplicates from the bind_rows
  dplyr::distinct(USUBJID, PARAM, PCSPEC, PARAM, NRRLT, EVID, .keep_all = TRUE)



describe("process_data_individual functions correctly", {

  it("returns a dataframe with default settings", {
    res <- process_data_individual(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1"
    )

    # expect data frame output
    expect_true(is.data.frame(res))
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
    expect_true(all(p$EVID == 0))
    expect_true(all(!is.na(p$AVAL)))
  })


  it("filters data if selected profiles is not null", {
    p <- process_data_individual(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      profiles_selected = 1
    )

    # Check that data is filtered to the selected cycle
    expect_true(all(p$PARAM == 1))
  })

  it("handles predose duplication if selected profiles is not null", {

    p <- process_data_individual(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      profiles_selected = 1
    )

    predose_record_in_plot <- p %>%
      filter(NFRLT == 168)

    expect_true(nrow(predose_record_in_plot) == 1)
    expect_true(predose_record_in_plot$PARAM == 1)
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
    expect_true(all(p$AVAL > 0))
  })
})

describe("process_data_mean functions correctly", {


  it("returns a dataframe with default settings", {
    p <- process_data_mean(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      color_by = "DOSEA"
    )

    # Calculates mean
    expect_true("Mean" %in% names(p))
    # Groups by color by vars
    expect_true("DOSEA" %in% names(p))
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

  it("filters data if selected profiles is not null", {
    p <- process_data_mean(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      color_by = "DOSEA",
      profiles_selected = 1
    )

    # Check that data is filtered to the selected cycle
    expect_true(max(p$NRRLT) <= 5)
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
    expect_true("DOSEA" %in% names(p))
    expect_true("SEX" %in% names(p))
    expect_true("PARAM" %in% names(p))

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
    expect_true(all(p$Mean > 0))
  })

})

sample_data <- FIXTURE_PKNCA_DATA
d_data <- sample_data$conc$data
analytes <- unique(d_data$PARAM)
pcspecs <- unique(d_data$PCSPEC)
subjects <- unique(d_data$USUBJID)

describe("exploration_individualplot: Individual Plot Mode", {
  it("returns a ggplot object with individual labels", {
    p <- exploration_individualplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1],
      selected_usubjids = subjects[1]
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "PK Concentration - Time Profile")
    expect_true(grepl("Concentration", p$labels$y))
    expect_true(grepl("Time", p$labels$x))
    expect_equal(p$labels$colour, "PARAM")
  })

  it("applies faceting", {
    p <- exploration_individualplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      facet_by = "PARAM",
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1],
      selected_usubjids = subjects[1]
    )
    expect_s3_class(p$facet, "FacetWrap")
  })

  it("applies log scale", {
    p <- exploration_individualplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      ylog_scale = TRUE,
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1],
      selected_usubjids = subjects[1]
    )
    is_log_scale <- grepl("log", p$scales$scales[[1]]$trans$name)
    expect_true(is_log_scale)
  })

  it("shows threshold line", {
    p <- exploration_individualplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      threshold_value = 0.1,
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1],
      selected_usubjids = subjects[1]
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomHline" %in% layer_classes)
  })

  it("applies a custom palette", {
    test_palette <- setNames(c("#FF0000", "#0000FF"), analytes[c(1,2)])
    p <- exploration_individualplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      palette = test_palette,
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1],
      selected_usubjids = subjects[1]
    )
    p_build <- ggplot_build(p)
    plot_colors <- unique(p_build$data[[1]]$colour)
    expect_true(all(plot_colors %in% test_palette))
  })

  it("handles empty data.frame with a plot informing of no data", {
    empty_data <- sample_data
    empty_data$conc$data <- empty_data$conc$data[0, ]
    p <- exploration_individualplot(
      pknca_data = empty_data,
      color_by = "PARAM",
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1],
      selected_usubjids = subjects[1]
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Error")
    gg_build <- ggplot_build(p)
    expect_true(any(grepl("No data available", gg_build[[1]][[1]]$label)))
  })
})

describe("exploration_meanplot: Mean Plot Mode", {
  it("returns a ggplot object with mean labels", {
    p <- exploration_meanplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1]
    )
    expect_s3_class(p, "ggplot")
    expect_true(grepl("Mean", p$labels$title))
    expect_true(grepl("Mean", p$labels$y))
    expect_true(grepl("Nominal", p$labels$x))
    expect_equal(p$labels$colour, "PARAM")
  })

  it("applies log scale", {
    p <- exploration_meanplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      ylog_scale = TRUE,
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1]
    )
    is_log_scale <- grepl("log", p$scales$scales[[1]]$trans$name)
    expect_true(is_log_scale)
  })

  it("shows SD error bars (min, max, and both)", {
    p_both <- exploration_meanplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      sd_min = TRUE,
      sd_max = TRUE,
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1]
    )
    p_both_build <- ggplot_build(p_both)
    layer_classes <- sapply(p_both$layers, function(x) class(x$geom)[1])
    expect_true("GeomErrorbar" %in% layer_classes)
  })

  it("shows CI ribbon and updates legend", {
    p <- exploration_meanplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      ci = TRUE,
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1]
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomRibbon" %in% layer_classes)
    expect_true(grepl("(95% CI)", p$labels$colour))
  })

  it("can show both SD bars and CI ribbon", {
    p <- exploration_meanplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      sd_min = TRUE,
      sd_max = TRUE,
      ci = TRUE,
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1]
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomErrorbar" %in% layer_classes)
    expect_true("GeomRibbon" %in% layer_classes)
  })

  it("handles empty data.frame with a plot informing of no data", {
    empty_data <- sample_data
    empty_data$conc$data <- empty_data$conc$data[0, ]
    p <- exploration_meanplot(
      pknca_data = empty_data,
      color_by = "PARAM",
      selected_analytes = analytes[1],
      selected_pcspec = pcspecs[1]
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Error")
    gg_build <- ggplot_build(p)
    expect_true(any(grepl("No data available", gg_build[[1]][[1]]$label)))
  })
})
