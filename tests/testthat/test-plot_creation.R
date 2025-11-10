
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
  dplyr::filter(USUBJID == "Subject1", PARAM == "Analyte1", PCSPEC == "Spec1", ATPTREF == 1, NRRLT == 1) %>%
  dplyr::mutate(EVID = 1, AVAL = NA)

# 2. NA AVAL record (should be filtered out)
na_aval_rec <- test_data %>%
  dplyr::filter(USUBJID == "Subject1", PARAM == "Analyte1", PCSPEC == "Spec1", ATPTREF == 1, NRRLT == 2) %>%
  dplyr::mutate(AVAL = NA)

# 3. Zero/Negative AVAL records (for log scale test)
zero_aval_recs <- test_data %>%
  dplyr::filter(PARAM == "Analyte2", PCSPEC == "Spec2", ATPTREF == 1, NRRLT == 0) %>%
  dplyr::mutate(AVAL = 0) # All 4 subjects get 0

neg_aval_rec <- test_data %>%
  dplyr::filter(USUBJID == "Subject1", PARAM == "Analyte2", PCSPEC == "Spec2", ATPTREF == 1, NRRLT == 1) %>%
  dplyr::mutate(AVAL = -10)

sample_data <- dplyr::bind_rows(
  test_data, predose_rec, evid_rec, na_aval_rec, zero_aval_recs, neg_aval_rec
) %>%
  # Ensure no duplicates from the bind_rows
  dplyr::distinct(USUBJID, PARAM, PCSPEC, ATPTREF, NRRLT, EVID, .keep_all = TRUE)



describe("create_indplot functions correctly", {
  
  # Mock Shiny's validate/need to throw a standard error
  mockery::stub(create_indplot, 'validate', function(x) { if (!x$condition) stop(x$message) })
  mockery::stub(create_indplot, 'need', function(expr, message) { list(condition = expr, message = message) })
  
  it("returns a ggplot object with default settings", {
    p <- create_indplot(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1"
    )
    expect_s3_class(p, "ggplot")
    # Default time_scale is "All Time", which uses AFRLT
    expect_true("AFRLT" %in% names(p$data))
    expect_true(all(p$data$time_var == p$data$AFRLT))
    # Default y_var is AVAL
    expect_true(all(p$data[[p$labels$y]] == p$data$AVAL))
    # Default group_var is USUBJID
    expect_true("USUBJID" %in% names(p$data))
  })
  
  it("handles empty/filtered data gracefully", {
    expect_error(
      create_indplot(
        data = sample_data,
        selected_usubjids = "NonExistentSubject", # This filter results in 0 rows
        selected_analytes = "Analyte1",
        selected_pcspec = "Spec1"
      ),
      "No data available for the selected filters."
    )
  })
  
  it("handles missing columns gracefully", {
    incomplete_data <- sample_data %>% select(-AVAL)
    expect_error(
      create_indplot(
        data = incomplete_data,
        selected_usubjids = "Subject1",
        selected_analytes = "Analyte1",
        selected_pcspec = "Spec1"
      ),
      "object 'AVAL' not found"
    )
  })
  
  it("filters out EVID != 0 and NA AVAL", {
    p <- create_indplot(
      data = sample_data, # sample_data contains EVID=1 and NA AVAL
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1"
    )
    # Check that the records were removed by the internal filter
    expect_true(all(p$data$EVID == 0))
    expect_true(all(!is.na(p$data$AVAL)))
  })
  
  it("supports 'By Dose Profile' time_scale", {
    p <- create_indplot(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      time_scale = "By Dose Profile",
      cycle = 1
    )
    expect_s3_class(p, "ggplot")
    # 'By Dose Profile' uses ARRLT
    expect_true("ARRLT" %in% names(p$data))
    expect_true(all(p$data$time_var == p$data$ARRLT))
    # Check that data is filtered to the selected cycle
    expect_true(all(p$data$ATPTREF == 1))
  })
  
  it("handles predose duplication for 'By Dose Profile'", {

    p <- create_indplot(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      time_scale = "By Dose Profile",
      cycle = 1 
    )
    
    predose_record_in_plot <- p$data %>%
      filter(NFRLT == 168)
    
    expect_true(nrow(predose_record_in_plot) == 1)
    expect_true(predose_record_in_plot$ATPTREF == 1)
  })
  
  it("supports multiple colorby_var", {
    p <- create_indplot(
      data = sample_data,
      selected_usubjids = c("Subject1", "Subject2"),
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      colorby_var = c("DOSEA", "SEX")
    )
    expect_s3_class(p, "ggplot")
    # Check that the color variable was created by interacting two columns
    expect_true(all(p$data$color_var %in% c("35, M", "35, F")))
    # Check that the legend label is correct
    expect_equal(p$labels$colour, c('DOSEA, SEX'))
  })
  
  it("supports log scale and filters non-positive AVAL", {
    p <- create_indplot(
      data = sample_data, # sample_data has AVAL=0 and AVAL=-10
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte2",
      selected_pcspec = "Spec2",
      time_scale = "By Dose Profile",
      cycle = 1,
      yaxis_scale = "log"
    )
    expect_s3_class(p, "ggplot")
    # Check that non-positive values were filtered
    expect_true(all(p$data$AVAL > 0))
    # Check that log-transform was added
    is_log_scale <- grepl("log", p$scales$scales[[1]]$trans$name)
    expect_true(is_log_scale)
  })
  
  it("correctly applies faceting", {
    p <- create_indplot(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      facet_by = c("PARAM", "PCSPEC")
    )
    expect_s3_class(p$facet, "FacetWrap")
    expect_equal(length(p$facet$params$facets), 2)
  })
  
  it("shows threshold line when requested", {
    p <- create_indplot(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      show_threshold = TRUE,
      threshold_value = 10
    )
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomHline" %in% layer_classes)
    hline_layer <- p$layers[[which(layer_classes == "GeomHline")]]
    expect_equal(hline_layer$data$yintercept, 10)
  })
  
  it("shows dose lines and respects facets", {
    p <- create_indplot(
      data = sample_data,
      selected_usubjids = "Subject1",
      selected_analytes = c("Analyte1", "Analyte2"),
      selected_pcspec = "Spec1",
      show_dose = TRUE,
      facet_by = "PARAM"
    )
    
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomVline" %in% layer_classes)
    
    vline_layer <- p$layers[[which(layer_classes == "GeomVline")]]
    
    # Check that TIME_DOSE was calculated correctly (AFRLT - ARRLT)
    expect_true("TIME_DOSE" %in% names(vline_layer$data))
    expected_dose_times <- unique(round(sample_data$AFRLT - sample_data$ARRLT, 6))
    expect_true(all(vline_layer$data$TIME_DOSE %in% expected_dose_times))
    
    # Check that the faceting variable is in the vline data
    expect_true("PARAM" %in% names(vline_layer$data))
  })
  
  it("correctly applies a persistent color palette", {
    test_palette <- c("Subject1" = "#FF0000", "Subject2" = "#0000FF")
    
    p <- create_indplot(
      data = sample_data,
      selected_usubjids = c("Subject1", "Subject2"),
      selected_analytes = "Analyte1",
      selected_pcspec = "Spec1",
      colorby_var = "USUBJID",
      palette = test_palette
    )
    
    p_build <- ggplot_build(p)
    plot_colors <- unique(p_build$data[[1]]$colour)
    
    # Check that the colors actually used in the plot match our palette
    expect_true(all(plot_colors %in% test_palette))
  })
})