base_df <- expand.grid(
  USUBJID = c("Subject1", "Subject2", "Subject3", "Subject4"),
  PARAM = c("Analyte1", "Analyte2"),
  PCSPEC = c("Spec1", "Spec2"),
  ATPTREF = 1,
  NRRLT = 0:5 # 6 nominal time points per cycle
)

set.seed(123) # for reproducible ARRLT
test_data <- base_df %>%
  dplyr::arrange(USUBJID, PARAM, PCSPEC, NRRLT) %>%
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






# Expand the original FIXTURE_PKNCA_DATA to have more subjects for the meanplot tests
sample_data <- FIXTURE_PKNCA_DATA
d_data <- sample_data$conc$data %>%
  filter(PARAM == unique(PARAM)[1], PCSPEC == unique(PCSPEC)[1])
d_data2 <- d_data %>%
  mutate(USUBJID = as.numeric(as.factor(USUBJID)) + 3)
d_data3 <- d_data %>%
  mutate(USUBJID = as.numeric(as.factor(USUBJID)) + 7)
sample_data$conc$data <- bind_rows(d_data, d_data2, d_data3)
pknca_data <- sample_data

conc_data <- pknca_data$conc$data

filtering_list <- list(
  USUBJID = unique(pknca_data$conc$data$USUBJID)[1],
  PARAM = unique(pknca_data$conc$data$PARAM)[1],
  PCSPEC = unique(pknca_data$conc$data$PCSPEC)[1],
  ATPTREF = unique(pknca_data$conc$data$ATPTREF)[1]
)

describe("process_data_individual functions correctly", {

  it("returns a dataframe with default settings", {
    res <- process_data_individual(
      pknca_data = sample_data
    )
    # expect data frame output
    expect_true(is.data.frame(res))
  })

  it("handles missing columns gracefully", {
    conc_col <- sample_data$conc$columns$concentration
    data_no_conc_col <- sample_data
    data_no_conc_col$conc$data <- select(conc_data, -all_of(conc_col))
    expect_error(
      process_data_individual(
        pknca_data = data_no_conc_col
      ),
      "object 'AVAL' not found"
    )
  })

  it("filters out EVID != 0 and NA concentration", {
    conc_col <- sample_data$conc$columns$concentration
    conc_data_with_na_evid <- conc_data
    conc_data_with_na_evid$EVID <- c(0, rep(1, nrow(conc_data) - 1))
    conc_data_with_na_evid[[conc_col]][c(2,3)] <- NA
    sample_data_with_na_evid <- sample_data
    sample_data_with_na_evid$conc$data <- conc_data_with_na_evid

    p <- process_data_individual(
      pknca_data = sample_data_with_na_evid,
      filtering_list = list(
        USUBJID = sample_data$USUBJID[1],
        PARAM = sample_data$PARAM[1],
        PCSPEC = sample_data$PCSPEC[1]
      )
    )
    expect_true(all(p$EVID == 0))
    expect_true(all(!is.na(p[[conc_col]])))
  })

  it("filters data according to filtering_list", {
    p <- process_data_individual(
      pknca_data = sample_data,
      filtering_list = list(
        ATPTREF = sample_data$ATPTREF[1]
      )
    )
    expect_true(all(p$ATPTREF == 1))
  })

  it("handles predose duplication if filtering_list is used", {
    p <- process_data_individual(
      data = sample_data,
      filtering_list = list(
        USUBJID = sample_data$USUBJID[1],
        PARAM = sample_data$PARAM[1],
        PCSPEC = sample_data$PCSPEC[1],
        ATPTREF = sample_data$ATPTREF[1]
      )
    )
    predose_record_in_plot <- p %>%
      filter(NFRLT == 168)
    expect_true(nrow(predose_record_in_plot) == 1)
    expect_true(predose_record_in_plot$ATPTREF == sample_data$ATPTREF[1])
  })

  it("filters non-positive AVAL if log scale selected", {
    aval_zeros <- conc_data
    aval_zeros$AVAL[c(1,2)] <- c(0, 0)
    pknca_aval_zeros <- sample_data
    pknca_aval_zeros$conc$data <- aval_zeros

    p <- process_data_individual(
      pknca_data = pknca_aval_zeros,
      ylog_scale = TRUE
    )
    expect_true(all(p[[conc_col]] > 0))
  })
})

describe("process_data_mean functions correctly", {

  it("returns a dataframe with default settings", {
    p <- process_data_mean(
      pknca_data = sample_data,
      filtering_list = list(
        PARAM = sample_data$PARAM[1],
        PCSPEC = sample_data$PCSPEC[1]
      ),
      extra_grouping_vars = "ATPTREF"
    )
    expect_true("Mean" %in% names(p))
    expect_true("ATPTREF" %in% names(p))
  })

  it("handles missing columns gracefully", {
    incomplete_data <- conc_data %>% select(-AVAL)
    pknca_incomplete <- sample_data
    pknca_incomplete$conc$data <- incomplete_data
    expect_error(
      process_data_mean(
        pknca_data = pknca_incomplete,
        filtering_list = list(
          PARAM = sample_data$PARAM[1],
          PCSPEC = sample_data$PCSPEC[1]
        ),
        extra_grouping_vars = "ATPTREF"
      ),
      "object 'AVAL' not found"
    )
  })

  it("filters data if filtering_list is used", {
    p <- process_data_mean(
      pknca_data = pknca_data,
      filtering_list = list(
        PARAM = conc_data$PARAM[1],
        PCSPEC = conc_data$PCSPEC[1],
        ATPTREF = conc_data$ATPTREF[1]
      ),
      extra_grouping_vars = c("ATPTREF", "PARAM", "PCSPEC")
    )
    expect_true(all(p$ATPTREF == conc_data$ATPTREF[1]))
    expect_true(all(p$PARAM == conc_data$PARAM[1]))
    expect_true(all(p$PCSPEC == conc_data$PCSPEC[1]))
  })

  it("supports using extra grouping variables to keep after summarization", {
    p <- process_data_mean(
      pknca_data = pknca_data,
      filtering_list = list(
        PARAM = c(conc_data$PARAM[1], conc_data$PARAM[2]),
        PCSPEC = conc_data$PCSPEC[1]
      ),
      extra_grouping_vars = c("DOSETRT", "PCSPEC", "PARAM"),
    )
    expect_true("DOSETRT" %in% names(p))
    expect_true("PCSPEC" %in% names(p))
    expect_true("PARAM" %in% names(p))
  })

  it("filters non-positive Mean for log scale", {
    zero_data <- conc_data %>%
      mutate(AVAL = ifelse(USUBJID %in% unique(USUBJID)[1:2], 0, AVAL))
    pknca_zero_data <- pknca_data
    pknca_zero_data$conc$data <- zero_data
    p <- process_data_mean(
      pknca_data = pknca_zero_data,
      ylog_scale = TRUE
    )
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
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1],
        USUBJID = subjects[1]
      )
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
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1],
        USUBJID = subjects[1]
      )
    )
    expect_s3_class(p$facet, "FacetWrap")
  })

  it("applies log10 scale to y-axis when ylog_scale is TRUE", {
    p <- exploration_individualplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      ylog_scale = TRUE
    )
    plot_build <- ggplot_build(p)
    expect_equal(plot_build$layout$panel_scales_y[[1]]$trans$name, "log-10")
  })

  it("shows threshold line", {
    p <- exploration_individualplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      threshold_value = 0.1,
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1],
        USUBJID = subjects[1]
      )
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomHline" %in% layer_classes)
  })

  it("applies a custom palette", {

    p <- exploration_individualplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      palette = "viridis",
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1],
        USUBJID = subjects[1]
      )
    )
    p_build <- ggplot_build(p)
    plot_colors <- unique(p_build$data[[1]]$colour)
    expect_true(all(plot_colors %in% viridis::viridis(1)))
  })

  it("handles empty data.frame with a plot informing of no data", {
    empty_data <- sample_data
    empty_data$conc$data <- empty_data$conc$data[0, ]
    p <- exploration_individualplot(
      pknca_data = empty_data,
      color_by = "PARAM",
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1],
        USUBJID = subjects[1]
      )
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Error")
    gg_build <- ggplot_build(p)
    expect_true(any(grepl("No data available", gg_build[[1]][[1]]$label)))
  })

  it("shows dose lines when show_dose is TRUE", {
    p <- exploration_individualplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      show_dose = TRUE,
      facet_by = "PARAM",
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1],
        USUBJID = subjects[1]
      )
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomVline" %in% layer_classes)

    vline_layer <- p$layers[[which(layer_classes == "GeomVline")]]
    expect_true(all("PARAM" %in% names(vline_layer$data)))
  })
})

describe("exploration_meanplot: Mean Plot Mode", {
  it("returns a ggplot object with mean labels", {
    p <- exploration_meanplot(
      pknca_data = sample_data,
      color_by = "PARAM"
    )
    expect_s3_class(p, "ggplot")
    expect_true(grepl("Mean", p$labels$title))
    expect_true(grepl("Mean", p$labels$y))
    expect_true(grepl("Nominal", p$labels$x))
    expect_equal(p$labels$colour, "PARAM")
  })

  it("applies log10 scale to y-axis when ylog_scale is TRUE", {
    p <- exploration_meanplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      ylog_scale = TRUE
    )
    plot_build <- ggplot_build(p)
    expect_equal(plot_build$layout$panel_scales_y[[1]]$trans$name, "log-10")
  })

  it("returns a ggplot object with mean labels", {
    p <- exploration_meanplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1]
      )
    )
    expect_s3_class(p, "ggplot")
    expect_true(grepl("Mean", p$labels$title))
    expect_true(grepl("Mean", p$labels$y))
    expect_true(grepl("Nominal", p$labels$x))
    expect_equal(p$labels$colour, "PARAM")
  })

  it("shows SD error bars (min, max, and both)", {
    p_both <- exploration_meanplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      sd_min = TRUE,
      sd_max = TRUE,
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1]
      )
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
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1]
      )
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
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1]
      )
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomErrorbar" %in% layer_classes)
    expect_true("GeomRibbon" %in% layer_classes)
  })

  it("shows dose lines when show_dose is TRUE", {
    p <- exploration_meanplot(
      pknca_data = sample_data,
      color_by = "PARAM",
      show_dose = TRUE,
      facet_by = "PARAM",
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1]
      )
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomVline" %in% layer_classes)

    vline_layer <- p$layers[[which(layer_classes == "GeomVline")]]
    expect_true(all("PARAM" %in% names(vline_layer$data)))
  })

  it("handles empty data.frame with a plot informing of no data", {
    empty_data <- sample_data
    empty_data$conc$data <- empty_data$conc$data[0, ]
    p <- exploration_meanplot(
      pknca_data = empty_data,
      color_by = "PARAM",
      filtering_list = list(
        PARAM = analytes[1],
        PCSPEC = pcspecs[1]
      )
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Error")
    gg_build <- ggplot_build(p)
    expect_true(any(grepl("No data available", gg_build[[1]][[1]]$label)))
  })
})
