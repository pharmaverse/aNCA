DATA_FIXTURE <- list(
  conc = list(
    data = data.frame(
      STUDYID = 1,
      PCSPEC = 1,
      USUBJID = rep(1:4, each = 4),
      NCA_PROFILE = 1,
      IX = rep(1:4, times = 4),
      PARAM = rep("A", 16),
      is.included.hl = FALSE,
      is.excluded.hl = FALSE,
      exclude_half.life = FALSE,
      REASON = ""
    )
  )
)

DOSNOS_FIXTURE <- data.frame(
  USUBJID = rep(1:4, each = 1),
  PARAM = rep("A", 4),
  PCSPEC = rep(1, 4),
  NCA_PROFILE = rep(1, 4)
)

slope_groups <- c("USUBJID", "PARAM", "PCSPEC", "NCA_PROFILE")



EXISTING_FIXTURE <- data.frame(
  TYPE = "Exclusion",
  USUBJID = 1,
  NCA_PROFILE = 1,
  PARAM = "A",
  PCSPEC = 1,
  RANGE = "3:6"
)

describe("check_slope_rule_overlap", {
  it("should add new row if no overlap is detected", {
    # different type #
    NEW <- data.frame(
      TYPE = "Selection",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "1:3"
    )

    expect_equal(nrow(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)), 2)

    # different USUBJID #
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 2,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "1:3"
    )

    expect_equal(nrow(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)), 2)

    # different NCA_PROFILE #
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 2,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "1:3"
    )

    expect_equal(nrow(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)), 2)

  })

  it("should remove overlapping points if no new points are detected", {
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "4:5"
    )

    expect_equal(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)$RANGE, "3,6")

    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "3:4"
    )

    expect_equal(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)$RANGE, "5:6")

  })

  it("should add new points of partial overlap is detected", {
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "4:9"
    )

    expect_equal(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)$RANGE, "3:9")

  })

  it("should remove full row if full range of rule is removed", {
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "3:6"
    )

    expect_equal(nrow(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)), 0)

  })

  it("should warn if more than one range for single subject, profile and rule type is detected", {
    EXISTING <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      NCA_PROFILE = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "3:6"
    )

    DUPLICATE <- EXISTING %>%
      mutate(
        RANGE = "4:7"
      )

    expect_warning(
      check_slope_rule_overlap(rbind(EXISTING, DUPLICATE), DUPLICATE),
      "More than one range for single subject, profile and rule type detected."
    )
  })
})

describe("detect_pknca_data_changes", {
  it("detects changes in data, hl_adj, and intervals", {
    # Setup minimal old and new objects

    old <- FIXTURE_PKNCA_DATA

    # No change
    new <- old
    res <- detect_pknca_data_changes(old, new, "exclude_half.life", "include_half.life")
    expect_false(res$in_data)
    expect_false(res$in_hl_adj)
    expect_false(res$in_selected_intervals)
    # Change in data
    new2 <- new
    new2$conc$data$VAL[1] <- 99
    res2 <- detect_pknca_data_changes(old, new2, "exclude_half.life", "include_half.life")
    expect_true(res2$in_data)
    # Change in hl_adj
    new3 <- new
    new3$conc$data$exclude_half.life[2] <- TRUE
    res3 <- detect_pknca_data_changes(old, new3, "exclude_half.life", "include_half.life")
    expect_true(res3$in_hl_adj)
    # Change in intervals
    new4 <- new
    new4$intervals <- data.frame(ID = 1:4)
    res4 <- detect_pknca_data_changes(old, new4, "exclude_half.life", "include_half.life")
    expect_true(res4$in_selected_intervals)
  })
})

describe("handle_hl_adj_change", {
  it("updates only affected groups in plot_outputs", {
    # Setup dummy PKNCA data objects and plot_outputs
    old_pknca_data <- FIXTURE_PKNCA_DATA
    new_pknca_data <- old_pknca_data
    new_pknca_data$conc$data <- new_pknca_data$conc$data %>%
      mutate(
        exclude_half.life = ifelse(
          USUBJID == unique(USUBJID)[3] & AFRLT == 4.5,
          TRUE,
          exclude_half.life
        )
      )

    old_plots <- get_halflife_plots(old_pknca_data)$plots

    # Patch .update_plots_with_pknca to just return a marker
    new_plots <- handle_hl_adj_change(new_pknca_data, old_pknca_data, old_plots)
    
    ix_unchanged_plots <- setdiff(seq_len(length(new_plots)), 4)
    expect_equal(new_plots[ix_unchanged_plots], old_plots[ix_unchanged_plots])
    
    old_plots_exp_details <- list(
      color = c("red", "red", "green", "green", "green"),
      size = 15,
      symbol = c("circle", "circle", "circle", "circle", "circle"),
      line = list(color = "rgba(255,127,14,1)")
    )

    new_plots_exp_details <- list(
      color = c("red", "green", "green", "red", "green"),
      size = 15,
      symbol = c("circle", "circle", "circle", "x", "circle"),
      line = list(color = "rgba(255,127,14,1)")
    )
    
    expect_equal(old_plots[[4]]$x$data[[2]]$marker, old_plots_exp_details, ignore_attr = TRUE)
    expect_equal(new_plots[[4]]$x$data[[2]]$marker, new_plots_exp_details, ignore_attr = TRUE)
  })
})

describe("handle_interval_change", {
  it("updates and removes plots based on interval changes", {
    new_pknca_data <- FIXTURE_PKNCA_DATA
    new_pknca_data$intervals <- new_pknca_data$intervals[2:3, ]
    old_pknca_data <- FIXTURE_PKNCA_DATA

    old_plots <- get_halflife_plots(old_pknca_data)$plots
    new_plots <- handle_interval_change(new_pknca_data, old_pknca_data, old_plots)

    expect_equal(length(old_plots), 13)
    expect_equal(length(new_plots), 2)
    expect_equal(old_plots[2:3], new_plots)
  })
})
