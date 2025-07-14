# Sample data for testing
sample_data <- data.frame(
  STUDYID = rep("Study1", 24),
  USUBJID = rep(c("Subject1", "Subject2", "Subject1", "Subject2"), each = 6),
  PARAM = rep(c("Analyte1", "Analyte 2"), each = 12),
  PCSPEC = rep(c("Spec1", "Spec2", "Spec1", "Spec2"), each = 6),
  NCA_PROFILE = rep(c(1, 2), each = 12),
  EVID = rep(0, 24),
  NRRLT = rep(0:5, 4),
  ARRLT = c(0:4, -1, 0:4, -1, 0:4, -1, 0:4, -1),
  AFRLT = c(0:4, 7, 0:4, 7, 0:4, 7, 0:4, 7),
  AVAL = c(
    10, 20, 30, 40, 50, 60,
    15, 25, 35, 45, 55, 65,
    12, 22, 32, 42, 52, 62,
    18, 28, 38, 48, 58, 68
  ),
  RRLTU = rep("hours", 24),
  AVALU = rep("ng/mL", 24),
  DOSEA = rep(35, 24)
)

describe("general_lineplot functions correctly", {
  it("returns a ggplot object", {
    p <- general_lineplot(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      selected_pcspec = "Spec1",
      colorby_var = "NCA_PROFILE",
      time_scale = "By Cycle",
      yaxis_scale = "Linear",
      cycle = 1,
      show_threshold = TRUE,
      threshold_value = 0
    )
    expect_s3_class(p, "ggplot")
  })

  it("handles empty data gracefully", {
    empty_data <- sample_data[0, ]
    p <- general_lineplot(
      data = empty_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      selected_pcspec = "Spec1",
      colorby_var = "NCA_PROFILE",
      time_scale = "By Cycle",
      yaxis_scale = "Linear",
      cycle = 1,
      show_threshold = TRUE,
      threshold_value = 0
    )
    expect_s3_class(p, "ggplot")
    expect_true(length(p$layers) == 0)
  })

  it("handles missing columns gracefully", {
    incomplete_data <- sample_data %>% select(-AVAL)
    expect_error(
      general_lineplot(
        data = incomplete_data,
        selected_analytes = "Analyte1",
        selected_usubjids = c("Subject1", "Subject2"),
        selected_pcspec = "Spec1",
        colorby_var = "NCA_PROFILE",
        time_scale = "By Cycle",
        yaxis_scale = "Linear",
        cycle = 1,
        show_threshold = TRUE,
        threshold_value = 0
      ),
      "object 'AVAL' not found"
    )
  })

  it("supports multiple variables for colorby_var", {
    extended_data <- sample_data %>% mutate(ExtraVar = rep(c("A", "B"), 12))
    p <- general_lineplot(
      data = extended_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      selected_pcspec = "Spec1",
      colorby_var = c("NCA_PROFILE", "ExtraVar"),
      time_scale = "Overall",
      yaxis_scale = "Linear"
    )
    expect_s3_class(p, "ggplot")
  })

  it("can plot with logarithmic scale", {
    p <- general_lineplot(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      selected_pcspec = "Spec1",
      colorby_var = "NCA_PROFILE",
      time_scale = "By Cycle",
      yaxis_scale = "Log",
      cycle = 1,
      show_threshold = TRUE,
      threshold_value = 0
    )
    expect_s3_class(p, "ggplot")

    # Check for logarithmic scale in the plot
    is_log_scale <- grepl("log", p$scales$scales[[1]]$trans$name)
    expect_true(is_log_scale)
  })

  it("returns a ggplot object with no data available when preprocessed_data is empty", {
    filtered_data <- sample_data %>% filter(USUBJID == "NonExistentSubject")
    p <- general_lineplot(
      data = filtered_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("NonExistentSubject"),
      selected_pcspec = "Spec1",
      colorby_var = "NCA_PROFILE",
      time_scale = "By Cycle",
      yaxis_scale = "Linear",
      cycle = 1,
      show_threshold = TRUE,
      threshold_value = 0
    )
    expect_s3_class(p, "ggplot")
    expect_true(p$labels$title == "No data available for selected parameters")
  })

  it("handles predose records when ARRLT < 0 and AFRLT > 0", {
    modified_data <- sample_data
    modified_data$ARRLT <- c(-1, 0, 1, 2, 3, 4, -2, 0, 1, 2, 3, 4,
                             -1, 0, 1, 2, 3, 4, -2, 0, 1, 2, 3, 4)
    modified_data$AFRLT <- c(0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0,
                             1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5)
    p <- general_lineplot(
      data = modified_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      selected_pcspec = "Spec1",
      colorby_var = "NCA_PROFILE",
      time_scale = "By Cycle",
      yaxis_scale = "Linear",
      cycle = 1,
      show_threshold = TRUE,
      threshold_value = 0
    )
    expect_s3_class(p, "ggplot")
    expect_true(nrow(p$data) > 0) # Ensure predose records are handled
  })

  it("handles By Dose Profile time_scale with predose duplication", {

    p <- general_lineplot(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_usubjids = "Subject1",
      selected_pcspec = "Spec1",
      colorby_var = "NCA_PROFILE",
      time_scale = "By Dose Profile",
      yaxis_scale = "Linear",
      cycle = 1
    )

    # Extract AFRLT values used in the plot
    plotted_afrlts <- unique(p$data$AFRLT)

    # Check if AFRLT = 7 was duplicated into cycle 1
    expect_true(7 %in% plotted_afrlts)
  })

  it("removes non-positive AVAL values for log scale", {
    bad_data <- sample_data
    bad_data$AVAL[1:3] <- c(0, -5, -10)
    p <- general_lineplot(
      data = bad_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      selected_pcspec = "Spec1",
      colorby_var = "NCA_PROFILE",
      time_scale = "By Cycle",
      yaxis_scale = "Log",
      cycle = 1
    )
    expect_s3_class(p, "ggplot")
    expect_true(all(p$data$AVAL > 0))
  })

  it("handles time_scale not equal to 'By Cycle'", {
    p <- general_lineplot(
      data = sample_data,
      selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"),
      selected_pcspec = "Spec1",
      colorby_var = "NCA_PROFILE",
      time_scale = "Overall",
      yaxis_scale = "Linear",
      cycle = 1,
      show_threshold = TRUE,
      threshold_value = 0
    )
    expect_s3_class(p, "ggplot")
    expect_true("AFRLT" %in% names(p$data)) # Ensure time variable is AFRLT
  })
  
  it("correctly filters out EVID != 0", {
    evid_data <- sample_data
    evid_data$EVID[1:5] <- 1 
    
    p <- general_lineplot(
      data = evid_data, selected_analytes = "Analyte1",
      selected_usubjids = "Subject1", selected_pcspec = "Spec1",
      colorby_var = "USUBJID", time_scale = "Overall", yaxis_scale = "Linear"
    )
    
    expect_true(all(p$data$EVID == 0))
  })
  
  it("correctly applies faceting", {
    p <- general_lineplot(
      data = sample_data, selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"), selected_pcspec = "Spec1",
      colorby_var = "USUBJID", facet_by = "PARAM", # Facet by Analyte
      time_scale = "Overall", yaxis_scale = "Linear"
    )
    expect_s3_class(p$facet, "FacetWrap")
  })
  
  it("shows dose lines when requested", {
    test_data <- sample_data %>% mutate(TIME_DOSE = round(AFRLT - ARRLT, 6))
    
    p <- general_lineplot(
      data = test_data, selected_analytes = "Analyte1",
      selected_usubjids = "Subject1", selected_pcspec = "Spec1",
      colorby_var = "USUBJID", time_scale = "Overall", yaxis_scale = "Linear",
      show_dose = TRUE
    )
    
    # Check that a GeomVline layer was added to the plot
    layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
    expect_true("GeomVline" %in% layer_classes)
  })
  
  it("dose lines are facet-specific", {
    test_data <- sample_data %>% mutate(TIME_DOSE = round(AFRLT - ARRLT, 6))
    
    p <- general_lineplot(
      data = test_data, selected_analytes = c("Analyte1", "Analyte2"),
      selected_usubjids = "Subject1", selected_pcspec = "Spec1",
      colorby_var = "USUBJID", time_scale = "Overall", yaxis_scale = "Linear",
      show_dose = TRUE, facet_by = "PARAM" # Facet by Analyte
    )
    
    # Find the GeomVline layer
    vline_layer_index <- which(sapply(p$layers, function(x) "GeomVline" %in% class(x$geom)))
    vline_layer <- p$layers[[vline_layer_index]]
    
    # Check that the vline data includes the faceting variable
    expect_true("PARAM" %in% names(vline_layer$data))
  })
  
  it("correctly applies a persistent color palette", {
    # Create a sample palette for the USUBJID variable
    test_palette <- c("Subject1" = "#FF0000", "Subject2" = "#0000FF") # Red and Blue
    
    p <- general_lineplot(
      data = sample_data, selected_analytes = "Analyte1",
      selected_usubjids = c("Subject1", "Subject2"), selected_pcspec = "Spec1",
      colorby_var = "USUBJID", time_scale = "Overall", yaxis_scale = "Linear",
      palette = test_palette # Pass the custom palette
    )
    
    # Build the plot to inspect its components
    p_build <- ggplot_build(p)
    plot_colors <- unique(p_build$data[[1]]$colour)
    
    # Check that the colors actually used in the plot match our palette
    expect_true(all(plot_colors %in% test_palette))
  })
})
