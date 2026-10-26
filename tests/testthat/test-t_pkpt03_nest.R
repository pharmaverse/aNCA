describe("t_pkpt03_nest", {
  it("preserves independently computed values in both the ARD and displayed cells", {
    skip_if_not_installed("gtsummary", "2.6.1")
    d <- nest_adpp_fixture()
    table <- t_pkpt03_nest(d)[[1]]
    frame <- attr(table, "tlg_data")
    expect_equal(as.vector(frame$n), c(3, 3))
    expect_equal(as.vector(frame$Mean), c(6, 2.333))
    expect_equal(as.vector(frame$SD), c(3, 1.528))
    expect_equal(as.vector(frame$CV_pct), c(50, 65.5))
    expect_equal(as.vector(frame$GeoMean), round(c(exp(mean(log(c(3, 6, 9)))), 2), 3))
    expect_equal(frame$GeoCV_pct[2], round(sqrt(exp(sd(log(c(1, 2, 4)))^2) - 1) * 100, 1))
    expect_equal(as.vector(frame$Median), c(6, 2))
    expect_equal(as.vector(frame$Min), c(3, 1))
    expect_equal(as.vector(frame$Max), c(9, 4))
    expect_identical(table$table_body$stat_2, c("6", "2.333"))
    ard <- attr(table, "tlg_ard")
    expect_equal(unlist(ard$stat[ard$stat_name == "s2"]), as.numeric(frame$Mean))
    expect_equal(frame, t_pkpt03_col(d, strat_var = c("TRT01A", "PARAM", "AVALU"))[[1]])
    expect_equal(as.vector(frame$AVALU), c("h*mg/L", "mg/L"))
  })

  it("respects exclusions, missing values, parameter selection and equal repeated records", {
    skip_if_not_installed("gtsummary", "2.6.1")
    d <- nest_adpp_fixture()
    d$PPSUMXF[5] <- "Y"
    d$AVAL[3] <- NA_real_
    d <- rbind(d, d[1, ])
    frame <- attr(t_pkpt03_nest(d, param_filter = "Cmax")[[1]], "tlg_data")
    expect_equal(as.vector(frame$n), 1)
    expect_equal(as.vector(frame$Mean), 1)
    expect_true(is.na(frame$SD))
    d$AVAL <- NA_real_
    table <- t_pkpt03_nest(d, param_filter = "Cmax")[[1]]
    expect_equal(as.numeric(attr(table, "tlg_data")$n), 0)
    expect_identical(table$table_body$stat_2, "")
    expect_match(t_pkpt03_nest(d[0, ])[[1]], "No data")
    expect_match(t_pkpt03_nest(d, param_filter = "absent")[[1]], "No data")
  })

  it("preserves decimal precision regardless of magnitude and session digits", {
    skip_if_not_installed("gtsummary", "2.6.1")
    withr::local_options(list(digits = 3))
    d <- nest_adpp_fixture()
    d$AVAL[d$PARAM == "AUC"] <- c(123456.789, 123456.790, 123456.791)
    table <- t_pkpt03_nest(d, param_filter = "AUC", stats = c("Mean", "Min", "Max"))[[1]]
    expect_identical(unname(unlist(table$table_body[c("stat_1", "stat_2", "stat_3")])),
                     c("123456.79", "123456.789", "123456.791"))
    expect_equal(as.numeric(attr(table, "tlg_data")$Mean), 123456.790)

    d$AVAL[d$PARAM == "AUC"] <- 1e12 + c(0.125, 0.250, 0.375)
    table <- t_pkpt03_nest(d, param_filter = "AUC", stats = c("Mean", "Min", "Max"))[[1]]
    expect_identical(unname(unlist(table$table_body[c("stat_1", "stat_2", "stat_3")])),
                     c("1000000000000.25", "1000000000000.125", "1000000000000.375"))

    d$DOSEA <- c(2.5, 2.5, 10, 10, 10, 10)
    table <- t_pkpt03_nest(d, strat_var = c("DOSEA", "PARAM"), stats = "n")[[1]]
    expect_identical(gtsummary::as_tibble(table, col_labels = FALSE)$key_1,
                     c("2.5", "2.5", "10", "10"))
  })

  it("summarizes each interval of an interval parameter separately", {
    skip_if_not_installed("gtsummary", "2.6.1")
    frame <- attr(t_pkpt03_nest(nest_interval_fixture(), param_filter = c("AUC", "Cmax"),
                                stats = c("n", "Mean"))[[1]], "tlg_data")
    expect_equal(as.vector(frame$PARAM), c("AUC (PT0H-PT2H)", "AUC (PT0H-PT6H)", "Cmax"))
    expect_equal(as.numeric(frame$n), c(3, 3, 3))
    expect_equal(as.numeric(frame$Mean), c(6, 12, 2.333))
  })

  it("uses split keys literally as default subtitles", {
    skip_if_not_installed("gtsummary", "2.6.1")
    d <- nest_adpp_fixture()
    d$PPCAT <- "Drug {x} $USUBJID !PARAM"
    table <- t_pkpt03_nest(d)[[1]]
    expect_equal(attr(table, "tlg_annotations")$subtitle,
                 "PPCAT: Drug {x} $USUBJID !PARAM / PPSPEC: SERUM")
    expect_false(attr(table, "tlg_group_header"))
    custom <- t_pkpt03_nest(d, subtitle = "Analyte: $PPSPEC")[[1]]
    expect_equal(attr(custom, "tlg_annotations")$subtitle, "Analyte: SERUM")
    expect_null(attr(custom, "tlg_group_header"))
  })

  it("retains comparison groups and only selected statistics", {
    skip_if_not_installed("gtsummary", "2.6.1")
    d <- nest_adpp_fixture()
    d$SEX <- rep(c("F", "M", "M"), each = 2)
    table <- t_pkpt03_nest(d, col_group_var = "SEX", stats = c("n", "Mean"))[[1]]
    frame <- attr(table, "tlg_data")
    expect_equal(as.numeric(frame[[paste0("F", .GROUP_SEP, "Mean")]]), c(3, 1))
    expect_equal(as.numeric(frame[[paste0("M", .GROUP_SEP, "Mean")]]), c(7.5, 3))
    expect_equal(length(attr(table, "tlg_ard_columns")), 4)
    expect_equal(unique(table$table_styling$spanning_header$spanning_header), c("F", "M"))
    expect_warning(t_pkpt03_nest(d, strat_var = c("absent", "PARAM")), "not found")
    expect_error(t_pkpt03_nest(d, stats = "absent"), "valid summary statistic")
  })

  it("rejects conflicting duplicates and resolves annotations per split", {
    skip_if_not_installed("gtsummary", "2.6.1")
    d <- nest_adpp_fixture()
    other <- d[1, ]
    other$AVAL <- NA_real_
    expect_error(t_pkpt03_nest(rbind(other, d)), "conflicting values")
    other <- d
    other$PPCAT <- "Other"
    tables <- t_pkpt03_nest(rbind(d, other), title = "$PPCAT", footnote = "Units: $AVALU")
    expect_equal(attr(tables[[2]], "tlg_annotations")$title, "Other")
    expect_match(attr(tables[[1]], "tlg_annotations")$footnote, "mg/L", fixed = TRUE)
  })
})

describe("NEST summary labels", {
  it("preserves custom labels in annotation references after splitting", {
    skip_if_not_installed("gtsummary", "2.6.1")
    d <- nest_adpp_fixture()
    attr(d$PPCAT, "label") <- "Analyte name"
    table <- t_pkpt03_nest(d, param_filter = "Cmax", title = "!PPCAT: $PPCAT")[[1]]
    expect_equal(attr(table, "tlg_annotations")$title, "Analyte name: Drug")
  })
})
