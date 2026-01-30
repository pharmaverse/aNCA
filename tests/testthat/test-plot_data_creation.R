# Expand the original FIXTURE_PKNCA_DATA to have more subjects for the meanplot tests
pknca_data <- FIXTURE_PKNCA_DATA
d_data <- pknca_data$conc$data %>%
  filter(PARAM == unique(PARAM)[1], PCSPEC == unique(PCSPEC)[1])
d_data2 <- d_data %>%
  mutate(USUBJID = as.numeric(as.factor(USUBJID)) + 3)
d_data3 <- d_data %>%
  mutate(USUBJID = as.numeric(as.factor(USUBJID)) + 7)
pknca_data$conc$data <- bind_rows(d_data, d_data2, d_data3)
conc_data <- pknca_data$conc$data

analytes <- unique(d_data$PARAM)
pcspecs <- unique(d_data$PCSPEC)
subjects <- unique(d_data$USUBJID)

describe("process_data_individual functions correctly", {
  conc_col <- pknca_data$conc$columns$concentration
  default_filter <- list(
    USUBJID = pknca_data$USUBJID[1],
    PARAM = pknca_data$PARAM[1],
    PCSPEC = pknca_data$PCSPEC[1],
    ATPTREF = pknca_data$ATPTREF[1]
  )

  it("returns a dataframe with default settings", {
    res <- process_data_individual(pknca_data = pknca_data)
    expect_true(is.data.frame(res))
  })

  it("handles missing columns gracefully", {
    data_no_conc_col <- pknca_data
    data_no_conc_col$conc$data <- select(conc_data, -all_of(conc_col))
    expect_error(
      process_data_individual(pknca_data = data_no_conc_col),
      "object 'AVAL' not found"
    )
  })

  it("filters out NA concentration", {
    conc_data_with_na <- conc_data
    conc_data_with_na$EVID <- c(0, rep(1, nrow(conc_data) - 1))
    conc_data_with_na[[conc_col]][c(2, 3)] <- NA
    pknca_data_with_na_evid <- pknca_data
    pknca_data_with_na_evid$conc$data <- conc_data_with_na
    p <- process_data_individual(pknca_data = pknca_data_with_na_evid)
    expect_true(all(!is.na(p[[conc_col]])))
  })

  it("filters data according to filtering_list", {
    p <- process_data_individual(
      pknca_data = pknca_data,
      filtering_list = list(ATPTREF = pknca_data$ATPTREF[1])
    )
    expect_true(all(p$ATPTREF == 1))
  })

  # TODO (Gerardo): Relax this assumption in the future. If possible
  # by not using dose_profile_duplicates
  it("handles predose duplication if filtering_list (ATPTREF) is used", {
    conc_data_subj2 <- filter(conc_data, USUBJID == subjects[2], ATPTREF %in% c(1, 2))
    # Create a predose record with a custom time and assign it to ATPTREF = 2
    predose <- conc_data[1, ]
    predose$AFRLT <- 4.99
    predose$ARRLT <- -0.01
    predose$ATPTREF <- 2
    # Combine with the rest of the data
    test_data <- bind_rows(conc_data, predose)
    test_pknca <- pknca_data
    test_pknca$conc$data <- test_data
    # Filter for ATPTREF = 1 (should only get the predose with ATPTREF = 1)
    p1 <- process_data_individual(test_pknca, filtering_list = list(ATPTREF = 1))
    expect_equal(filter(p1, AFRLT == 4.99)$ATPTREF, 1)
    # Filter for ATPTREF = 2 (should only get the predose with ATPTREF = 2)
    p2 <- process_data_individual(test_pknca, filtering_list = list(ATPTREF = 2))
    expect_equal(filter(p2, AFRLT == 4.99)$ATPTREF, 2)
  })

  it("filters non-positive AVAL if log scale selected", {
    aval_zeros <- conc_data
    aval_zeros$AVAL[c(1, 2)] <- c(0, 0)
    pknca_aval_zeros <- pknca_data
    pknca_aval_zeros$conc$data <- aval_zeros
    p <- process_data_individual(
      pknca_data = pknca_aval_zeros,
      ylog_scale = TRUE
    )
    expect_true(all(p[[conc_col]] > 0))
  })
})

describe("process_data_mean functions correctly", {
  filter_param_pcspec <- list(
    PARAM = pknca_data$PARAM[1], PCSPEC = pknca_data$PCSPEC[1]
  )
  filter_all <- list(
    PARAM = conc_data$PARAM[1], PCSPEC = conc_data$PCSPEC[1], ATPTREF = conc_data$ATPTREF[1]
  )

  it("returns a dataframe with default settings", {
    p <- process_data_mean(
      pknca_data = pknca_data,
      filtering_list = filter_param_pcspec,
      extra_grouping_vars = "ATPTREF"
    )
    expect_true("Mean" %in% names(p))
    expect_true("ATPTREF" %in% names(p))
  })

  it("handles missing columns gracefully", {
    incomplete_data <- conc_data %>% select(-AVAL)
    pknca_incomplete <- pknca_data
    pknca_incomplete$conc$data <- incomplete_data
    expect_error(
      process_data_mean(
        pknca_data = pknca_incomplete,
        filtering_list = filter_param_pcspec,
        extra_grouping_vars = "ATPTREF"
      ),
      "object 'AVAL' not found"
    )
  })

  it("filters data if filtering_list is used", {
    p <- process_data_mean(
      pknca_data = pknca_data,
      filtering_list = filter_all,
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
      extra_grouping_vars = c("DOSETRT", "PCSPEC", "PARAM")
    )
    expect_true("DOSETRT" %in% names(p))
    expect_true("PCSPEC" %in% names(p))
    expect_true("PARAM" %in% names(p))
  })

  it("filters non-positive Mean for log scale", {
    zero_data <- conc_data %>% mutate(AVAL = ifelse(USUBJID %in% unique(USUBJID)[1:2], 0, AVAL))
    pknca_zero_data <- pknca_data
    pknca_zero_data$conc$data <- zero_data
    p <- process_data_mean(
      pknca_data = pknca_zero_data,
      ylog_scale = TRUE
    )
    expect_true(all(p$Mean > 0))
  })
})

describe("exploration_individualplot: Individual Plot Mode", {
  it("returns a ggplot object with individual labels", {
    p <- exploration_individualplot(
      pknca_data = pknca_data,
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
      pknca_data = pknca_data,
      color_by = "PARAM",
      facet_by = "PARAM"
    )
    expect_s3_class(p$facet, "FacetWrap")
  })

  it("applies log10 scale to y-axis when ylog_scale is TRUE", {
    p <- exploration_individualplot(
      pknca_data = pknca_data,
      color_by = "PARAM",
      ylog_scale = TRUE
    )
    plot_build <- ggplot_build(p)
    expect_equal(plot_build$layout$panel_scales_y[[1]]$trans$name, "log-10")
  })

  it("shows threshold line", {
    p <- exploration_individualplot(
      pknca_data = pknca_data,
      color_by = "PARAM",
      threshold_value = 0.1
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomHline" %in% layer_classes)
  })

  it("applies a custom palette", {
    p <- exploration_individualplot(
      pknca_data = pknca_data,
      color_by = "PARAM",
      palette = "viridis"
    )
    p_build <- ggplot_build(p)
    plot_colors <- unique(p_build$data[[1]]$colour)
    expect_true(all(plot_colors %in% viridisLite::viridis(1)))
  })

  it("handles empty data.frame with a plot informing of no data", {
    empty_data <- pknca_data
    empty_data$conc$data <- empty_data$conc$data[0, ]
    p <- exploration_individualplot(
      pknca_data = empty_data,
      color_by = "PARAM"
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Error")
    gg_build <- ggplot_build(p)
    expect_true(any(grepl("No data available", gg_build[[1]][[1]]$label)))
  })

  it("shows dose lines when show_dose is TRUE", {
    p <- exploration_individualplot(
      pknca_data = pknca_data,
      color_by = "PARAM",
      show_dose = TRUE
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
      pknca_data = pknca_data,
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
      pknca_data = pknca_data,
      color_by = "PARAM",
      ylog_scale = TRUE
    )
    plot_build <- ggplot_build(p)
    expect_equal(plot_build$layout$panel_scales_y[[1]]$trans$name, "log-10")
  })

  it("returns a ggplot object with mean labels", {
    p <- exploration_meanplot(
      pknca_data = pknca_data,
      color_by = "PARAM"
    )
    expect_s3_class(p, "ggplot")
    expect_true(grepl("Mean", p$labels$title))
    expect_true(grepl("Mean", p$labels$y))
    expect_true(grepl("Nominal", p$labels$x))
    expect_equal(p$labels$colour, "PARAM")
  })

  it("shows SD error bars (min, max, and both)", {
    p_both <- exploration_meanplot(
      pknca_data = pknca_data,
      color_by = "PARAM",
      sd_min = TRUE,
      sd_max = TRUE
    )
    p_both_build <- ggplot_build(p_both)
    layer_classes <- sapply(p_both$layers, function(x) class(x$geom)[1])
    expect_true("GeomErrorbar" %in% layer_classes)
  })

  it("shows CI ribbon and updates legend", {
    p <- exploration_meanplot(
      pknca_data = pknca_data,
      color_by = "PARAM",
      ci = TRUE
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomRibbon" %in% layer_classes)
    expect_true(grepl("(95% CI)", p$labels$colour))
  })

  it("can show both SD bars and CI ribbon", {
    p <- exploration_meanplot(
      pknca_data = pknca_data,
      color_by = "PARAM",
      sd_min = TRUE,
      sd_max = TRUE,
      ci = TRUE
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomErrorbar" %in% layer_classes)
    expect_true("GeomRibbon" %in% layer_classes)
  })

  it("shows dose lines when show_dose is TRUE", {
    p <- exploration_meanplot(
      pknca_data = pknca_data,
      color_by = "PARAM",
      show_dose = TRUE,
      facet_by = "PARAM"
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomVline" %in% layer_classes)

    vline_layer <- p$layers[[which(layer_classes == "GeomVline")]]
    expect_true(all("PARAM" %in% names(vline_layer$data)))
  })

  it("uses NRRLT as x axis when use_time_since_last_dose is TRUE", {
    p_nfrlt <- exploration_meanplot(
      pknca_data = pknca_data,
      color_by = "PARAM",
      use_time_since_last_dose = FALSE
    )
    pb_nfrlt <- ggplot_build(p_nfrlt)
    nfrlt_vals <- unique(pknca_data$conc$data$NFRLT)
    expect_true(all(nfrlt_vals %in% pb_nfrlt$data[[1]]$x))

    p_nrrlt <- exploration_meanplot(
      pknca_data = pknca_data,
      color_by = "PARAM",
      use_time_since_last_dose = TRUE
    )
    pb_nrrlt <- ggplot_build(p_nrrlt)
    nrrlt_vals <- unique(pknca_data$conc$data$NRRLT)
    expect_true(all(nrrlt_vals %in% pb_nrrlt$data[[1]]$x))
    expect_false(all(nfrlt_vals %in% pb_nrrlt$data[[1]]$x))
  })

  it("handles empty data.frame with a plot informing of no data", {
    empty_data <- pknca_data
    empty_data$conc$data <- empty_data$conc$data[0, ]
    p <- exploration_meanplot(
      pknca_data = empty_data,
      color_by = "PARAM"
    )
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Error")
    gg_build <- ggplot_build(p)
    expect_true(any(grepl("No data available", gg_build[[1]][[1]]$label)))
  })
})
