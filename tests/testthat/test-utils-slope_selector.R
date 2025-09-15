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

    # No previous object
    new <- old
    res <- detect_pknca_data_changes(NULL, new, "exclude_half.life", "include_half.life")
    expect_true(res$in_data)
    
    # No change
    res <- detect_pknca_data_changes(old, old, "exclude_half.life", "include_half.life")
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
  it("removes plots when intervals are removed", {
    new_pknca_data <- FIXTURE_PKNCA_DATA
    new_pknca_data$intervals <- new_pknca_data$intervals[2:3, ]
    old_pknca_data <- FIXTURE_PKNCA_DATA

    old_plots <- get_halflife_plots(old_pknca_data)$plots
    new_plots <- handle_interval_change(new_pknca_data, old_pknca_data, old_plots)

    expect_equal(length(old_plots), 13)
    expect_equal(length(new_plots), 2)
    expect_equal(old_plots[names(new_plots)], new_plots)
  })

  it("adds plots when intervals are added", {
    old_pknca_data <- FIXTURE_PKNCA_DATA
    old_pknca_data$intervals <- old_pknca_data$intervals[2:3, ]
    new_pknca_data <- FIXTURE_PKNCA_DATA

    old_plots <- get_halflife_plots(old_pknca_data)$plots
    new_plots <- handle_interval_change(new_pknca_data, old_pknca_data, old_plots)

    expect_equal(length(old_plots), 2)
    expect_equal(length(new_plots), 13)
    expect_equal(new_plots[names(old_plots)], old_plots)
  })

})

describe("arrange_plots_by_groups", {
  it("orders plots by specified group columns", {
    named_list <- list(
      "B: 2, A: 1" = "plot1",
      "B: 1, A: 2" = "plot2",
      "B: 1, A: 1" = "plot3"
    )
    # Should order by B then A
    ordered <- arrange_plots_by_groups(named_list, c("B", "A"))
    expect_equal(names(ordered), c("B: 1, A: 1", "B: 1, A: 2", "B: 2, A: 1"))
    # Should order by A then B
    ordered2 <- arrange_plots_by_groups(named_list, c("A", "B"))
    expect_equal(names(ordered2), c("B: 2, A: 1", "B: 1, A: 1", "B: 1, A: 2"))
  })
})

describe(".update_pknca_with_rules", {
  it("applies selection and exclusion rules to data", {
    # Minimal PKNCA data object
    data <- list(
      conc = list(
        data = data.frame(ID = 1:5, TIME = 1:5, exclude_half.life = FALSE, include_half.life = FALSE, REASON = ""),
        columns = list(time = "TIME", exclude_half.life = "exclude_half.life", include_half.life = "include_half.life")
      )
    )
    attr(data, "groups") <- c("ID")
    group_vars <- function(x) "ID"
    assignInNamespace("group_vars", group_vars, ns = "globalenv")
    # Selection rule
    slopes <- data.frame(TYPE = "Selection", ID = 1, RANGE = "2:4", REASON = "sel")
    data2 <- .update_pknca_with_rules(data, slopes)
    expect_true(all(data2$conc$data$include_half.life[2:4]))
    # Exclusion rule
    slopes2 <- data.frame(TYPE = "Exclusion", ID = 1, RANGE = "3:5", REASON = "exc")
    data3 <- .update_pknca_with_rules(data2, slopes2)
    expect_true(all(data3$conc$data$exclude_half.life[3:5]))
    # REASON is appended
    expect_true(grepl("sel", data3$conc$data$REASON[2]))
    expect_true(grepl("exc", data3$conc$data$REASON[3]))
  })
})

describe(".update_plots_with_pknca", {
  it("updates plots for affected intervals", {
    # Minimal PKNCA data object
    data <- list(
      conc = list(
        data = data.frame(ID = 1:3, TIME = 1:3),
        columns = list()
      ),
      intervals = data.frame(ID = 1:3, start = 1:3, end = 2:4)
    )
    group_vars <- function(x) "ID"
    assignInNamespace("group_vars", group_vars, ns = "globalenv")
    get_halflife_plots <- function(p) list(plots = list("ID: 1, start: 1, end: 2" = "plot1", "ID: 2, start: 2, end: 3" = "plot2", "ID: 3, start: 3, end: 4" = "plot3"))
    assignInNamespace("get_halflife_plots", get_halflife_plots, ns = "globalenv")
    plot_outputs <- list("ID: 1, start: 1, end: 2" = "oldplot1", "ID: 2, start: 2, end: 3" = "oldplot2")
    intervals_to_update <- data.frame(ID = 2:3, start = 2:3, end = 3:4)
    updated <- .update_plots_with_pknca(data, plot_outputs, intervals_to_update)
    expect_equal(updated[["ID: 2, start: 2, end: 3"]], "plot2")
    expect_equal(updated[["ID: 3, start: 3, end: 4"]], "plot3")
    expect_equal(updated[["ID: 1, start: 1, end: 2"]], "oldplot1")
  })
})

describe("arrange_plots_by_groups", {
  it("orders plots by specified group columns", {
    named_list <- list(
      "B: 2, A: 1" = "plot1",
      "B: 1, A: 2" = "plot2",
      "B: 1, A: 1" = "plot3"
    )
    # Should order by B then A
    ordered <- arrange_plots_by_groups(named_list, c("B", "A"))
    expect_equal(names(ordered), c("B: 1, A: 1", "B: 1, A: 2", "B: 2, A: 1"))
    # Should order by A then B
    ordered2 <- arrange_plots_by_groups(named_list, c("A", "B"))
    expect_equal(names(ordered2), c("B: 1, A: 1", "B: 2, A: 1", "B: 1, A: 2"))
  })
})

describe(".update_pknca_with_rules", {
  it("applies selection and exclusion rules to data", {
    # Minimal PKNCA data object
    old <- FIXTURE_PKNCA_DATA
    group1 <- data$intervals %>% select(any_of(c(group_vars(old), "start", "end"))) %>% .[1,]

    # Selection rule
    slopes_incl <- cbind(
      data.frame(TYPE = "Selection", ID = 1, RANGE = "2:4", REASON = "because I want to test it :)"),
      group1
    )
    slopes_excl <- cbind(
      data.frame(TYPE = "Exclusion", ID = 1, RANGE = "2:4", REASON = "there are always good reasons"),
      group1
    )
    new_with_incl <- .update_pknca_with_rules(old, slopes_incl)
    new_with_excl <- .update_pknca_with_rules(old, slopes_excl)

    old_have_points_na <- all(is.na(old$conc$data %>%
                                      filter(USUBJID == group1$USUBJID, AFRLT >= 2, AFRLT <= 4) %>%
                                      pull(include_half.life))
                             )
    new_have_points_incl <- all(new_with_incl$conc$data %>%
                                filter(USUBJID == group1$USUBJID, AFRLT >= 2, AFRLT <= 4) %>%
                                pull(include_half.life))

    new_have_points_excl <- all(new_with_excl$conc$data %>%
                                filter(USUBJID == group1$USUBJID, AFRLT >= 2, AFRLT <= 4) %>%
                                pull(exclude_half.life))

    expect_true(all(old_have_points_na, new_have_points_incl, new_have_points_excl))
  })
})

describe(".update_plots_with_pknca", {
  it("updates plots for affected intervals", {
    data <- FIXTURE_PKNCA_DATA
    # group_vars <- function(x) "ID"
    # assignInNamespace("group_vars", group_vars, ns = "globalenv")
    # get_halflife_plots <- function(p) list(plots = list("ID: 1, start: 1, end: 2" = "plot1", "ID: 2, start: 2, end: 3" = "plot2", "ID: 3, start: 3, end: 4" = "plot3"))
    # assignInNamespace("get_halflife_plots", get_halflife_plots, ns = "globalenv")
    # plot_outputs <- list("ID: 1, start: 1, end: 2" = "oldplot1", "ID: 2, start: 2, end: 3" = "oldplot2")
    # intervals_to_update <- data.frame(ID = 2:3, start = 2:3, end = 3:4)
    # updated <- .update_plots_with_pknca(data, plot_outputs, intervals_to_update)
    # expect_equal(updated[["ID: 2, start: 2, end: 3"]], "plot2")
    # expect_equal(updated[["ID: 3, start: 3, end: 4"]], "plot3")
    # expect_equal(updated[["ID: 1, start: 1, end: 2"]], "oldplot1")
  })
})
