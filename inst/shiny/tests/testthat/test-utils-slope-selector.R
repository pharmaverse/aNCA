EXISTING_FIXTURE <- data.frame(
  TYPE = "Exclusion",
  USUBJID = 1,
  ATPTREF = 1,
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
      ATPTREF = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "1:3"
    )

    expect_equal(nrow(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)), 2)

    # different USUBJID #
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 2,
      ATPTREF = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "1:3"
    )

    expect_equal(nrow(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)), 2)

    # different ATPTREF #
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      ATPTREF = 2,
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
      ATPTREF = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "4:5"
    )

    expect_equal(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)$RANGE, "3,6")

    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      ATPTREF = 1,
      PARAM = "A",
      PCSPEC = 1,
      RANGE = "3:4"
    )

    expect_equal(check_slope_rule_overlap(EXISTING_FIXTURE, NEW)$RANGE, "5:6")
  })

  it("should add new points if partial overlap is detected", {
    NEW <- data.frame(
      TYPE = "Exclusion",
      USUBJID = 1,
      ATPTREF = 1,
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
      ATPTREF = 1,
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
      ATPTREF = 1,
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
  old_data <- FIXTURE_PKNCA_DATA

  it("detects change in data when there is no previous object", {
    new_data <- old_data
    res <- detect_pknca_data_changes(NULL, new_data)
    expect_true(res$in_data)
  })

  it("detects no change when old and new are identical", {
    res <- detect_pknca_data_changes(old_data, old_data)
    expect_false(res$in_data)
    expect_false(res$in_hl_adj)
    expect_false(res$in_selected_intervals)
  })

  it("detects change in data", {
    changed_data <- old_data
    changed_data$conc$data$VAL[1] <- 99
    res <- detect_pknca_data_changes(old_data, changed_data)
    expect_true(res$in_data)
  })

  it("detects change in hl_adj", {
    changed_hl <- old_data
    changed_hl$conc$data$exclude_half.life[2] <- TRUE
    res <- detect_pknca_data_changes(old_data, changed_hl)
    expect_true(res$in_hl_adj)
  })

  it("detects change in intervals", {
    changed_int <- old_data
    changed_int$intervals <- data.frame(ID = 1:4)
    res <- detect_pknca_data_changes(old_data, changed_int)
    expect_true(res$in_selected_intervals)
  })
})

describe("handle_hl_adj_change", {
  it("updates only affected groups in plot_outputs", {
    # Setup dummy PKNCA data objects and plot_outputs
    old_data <- FIXTURE_PKNCA_DATA
    new_data <- old_data
    new_data$conc$data <- new_data$conc$data %>%
      mutate(
        exclude_half.life = ifelse(
          USUBJID == unique(USUBJID)[3] & AFRLT == 4.5,
          TRUE,
          exclude_half.life
        )
      )

    # Create plots using the original and updated PKNCA data
    old_plots <- withCallingHandlers(
      get_halflife_plots(old_data)$plots,
      # Because of the NA record there will be an expected warning
      warning = function(w) {
        if (grepl("Ignoring 1 observations", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    )
    new_plots <- handle_hl_adj_change(new_data, old_data, old_plots)

    # Check that the plots for other groups remain unchanged
    ix_unchanged_plots <- setdiff(seq_along(new_plots), 4)
    expect_equal(new_plots[ix_unchanged_plots], old_plots[ix_unchanged_plots])

    # Define the expected differences in the original and updated plots
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
    # Check that the affected plot has been updated correctly
    expect_equal(old_plots[[4]]$x$data[[2]]$marker, old_plots_exp_details, ignore_attr = TRUE)
    expect_equal(new_plots[[4]]$x$data[[2]]$marker, new_plots_exp_details, ignore_attr = TRUE)
  })
})


describe("handle_interval_change", {
  old_data <- FIXTURE_PKNCA_DATA
  old_data$intervals <- old_data$intervals[1:3, ]
  old_plots <- get_halflife_plots(old_data)$plots

  it("removes plots when intervals are removed", {
    new_data <- FIXTURE_PKNCA_DATA
    new_data$intervals <- new_data$intervals[1, ]
    new_plots <- handle_interval_change(new_data, old_data, old_plots)

    expect_equal(length(old_plots), 3)
    expect_equal(length(new_plots), 1)
    expect_equal(old_plots[names(new_plots)], new_plots)
  })

  it("adds plots when intervals are added", {
    new_data <- FIXTURE_PKNCA_DATA
    new_data$intervals <- new_data$intervals[1:4, ]
    new_plots <- handle_interval_change(new_data, old_data, old_plots)

    expect_equal(length(old_plots), 3)
    expect_equal(length(new_plots), 4)
    expect_equal(new_plots[names(old_plots)], old_plots)
  })
})

describe("update_plots_with_pknca", {
  old_data <- FIXTURE_PKNCA_DATA
  old_data$intervals <- old_data$intervals[1:2, ]
  old_plots <- get_halflife_plots(old_data)$plots

  new_data <- old_data
  new_data$conc$data$exclude_half.life <- TRUE

  it("updates all plots when no intervals are specified", {
    updated_plots <- update_plots_with_pknca(new_data, old_plots, NULL)
    expect_equal(updated_plots[[1]]$x$data[[2]]$marker$symbol, rep("x", 5), ignore_attr = TRUE)
    expect_equal(old_plots[[2]]$x$data[[2]]$marker$symbol, rep("circle", 5), ignore_attr = TRUE)
  })
  it("does not update any plot when the specified intervals are an empty dataframe", {
    updated_plots <- update_plots_with_pknca(new_data, old_plots, data.frame())
    expect_equal(updated_plots, old_plots)
  })
})
