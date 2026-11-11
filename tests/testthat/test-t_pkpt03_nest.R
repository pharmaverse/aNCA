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
    expect_identical(table$table_body$key_1, c("2.5", NA_character_, "10", NA_character_))
    expect_equal(as.numeric(attr(table, "tlg_data")$DOSEA), c(2.5, 2.5, 10, 10))
  })

  it("groups parent labels only in the display and keeps every parameter and unit visible", {
    skip_if_not_installed("gtsummary", "2.6.1")
    d <- nest_adpp_fixture()
    other <- d
    other$TRT01A <- "B"
    table <- t_pkpt03_nest(rbind(d, other), stats = c("n", "Mean"))[[1]]
    expect_identical(table$table_body$key_1, c("A", NA_character_, "B", NA_character_))
    expect_equal(as.vector(table$table_body$key_2), rep(c("AUC", "Cmax"), 2))
    expect_equal(as.vector(table$table_body$key_3), rep(c("h*mg/L", "mg/L"), 2))
    expect_equal(as.vector(attr(table, "tlg_data")$TRT01A), c("A", "A", "B", "B"))
    expect_equal(as.numeric(attr(table, "tlg_data")$Mean), rep(c(6, 2.333), 2))
    headers <- table$table_styling$header
    keys <- headers[headers$column %in% c("key_1", "key_2", "key_3"), ]
    expect_equal(keys$label, c("Treatment Group", "PK Parameter", "Unit"))
    expect_true(all(keys$align == "left"))

    # Parameter-first custom layouts keep their treatment values on every row.
    custom <- t_pkpt03_nest(rbind(d, other), strat_var = c("PARAM", "TRT01A"))[[1]]
    expect_equal(as.vector(custom$table_body$key_2), c("A", "B", "A", "B"))
  })

  it("summarizes each interval of an interval parameter separately", {
    skip_if_not_installed("gtsummary", "2.6.1")
    frame <- attr(t_pkpt03_nest(nest_interval_fixture(), param_filter = c("AUC", "Cmax"),
                                stats = c("n", "Mean"))[[1]], "tlg_data")
    expect_equal(as.vector(frame$PARAM), c("AUC (PT0H-PT2H)", "AUC (PT0H-PT6H)", "Cmax"))
    expect_equal(as.numeric(frame$n), c(3, 3, 3))
    expect_equal(as.numeric(frame$Mean), c(6, 12, 2.333))
  })

  it("keeps complementary partial-AUC windows separate after missing records or exclusions", {
    skip_if_not_installed("gtsummary", "2.6.1")
    d <- nest_interval_fixture()
    d <- d[d$PARAM == "AUC", ]
    retained <- (d$USUBJID == "1" & d$PPENINT == "PT2H") |
      (d$USUBJID != "1" & d$PPENINT == "PT6H")
    d$PPSUMXF[!retained] <- "Y"
    for (input in list(d, d[retained, ])) {
      table <- t_pkpt03_nest(input, param_filter = "AUC", stats = c("n", "Mean", "SD"))[[1]]
      frame <- attr(table, "tlg_data")
      expect_equal(as.vector(frame$PARAM), c("AUC (PT0H-PT2H)", "AUC (PT0H-PT6H)"))
      expect_equal(as.numeric(frame$n), c(1, 2))
      expect_equal(as.numeric(frame$Mean), c(3, 15))
      expect_equal(as.numeric(frame$SD), c(NA_real_, round(sd(c(12, 18)), 3)))
      expect_equal(table$table_body$stat_2, c("3", "15"))
    }
    # All subjects retain the same window: its identity must still be explicit.
    d$PPSUMXF <- ifelse(d$PPENINT == "PT6H", "Y", "")
    frame <- attr(t_pkpt03_nest(d, stats = c("n", "Mean"))[[1]], "tlg_data")
    expect_equal(as.vector(frame$PARAM), "AUC (PT0H-PT2H)")
    expect_equal(as.numeric(frame$n), 3)
    expect_equal(as.numeric(frame$Mean), 6)
  })

  it("pools parameters with one interval per subject, matching the legacy table", {
    skip_if_not_installed("gtsummary", "2.6.1")
    urine <- nest_urine_fixture()
    frame <- attr(t_pkpt03_nest(urine)[[1]], "tlg_data")
    expect_equal(as.vector(frame$PARAM), "Amt Rec")
    expect_equal(as.numeric(frame$n), 3)
    expect_equal(frame, t_pkpt03_col(urine, list_vars = c("PPCAT", "PPSPEC", "ATPTREF"),
                                     strat_var = c("TRT01A", "PARAM", "AVALU"))[[1]])
  })

  it("does not let excluded urine windows change eligible grouping or statistics", {
    skip_if_not_installed("gtsummary", "2.6.1")
    urine <- nest_urine_fixture()
    extra <- urine[1, ]
    extra$PPENINT <- "PT6H"
    extra$AVAL <- 1
    extra$PPSUMXF <- "Y"
    expected <- attr(t_pkpt03_nest(urine, stats = c("n", "Mean", "SD"))[[1]], "tlg_data")
    for (input in list(rbind(urine, extra), rbind(extra, urine))) {
      frame <- attr(t_pkpt03_nest(input, stats = c("n", "Mean", "SD"))[[1]], "tlg_data")
      expect_equal(frame, expected)
      expect_equal(as.numeric(frame$n), 3)
      expect_equal(as.numeric(frame$Mean), 6)
      expect_equal(as.numeric(frame$SD), 3)
    }
  })

  it("uses nonblank parameter codes per row to retain urine pooling", {
    skip_if_not_installed("gtsummary", "2.6.1")
    urine <- nest_urine_fixture()
    urine$PARAM <- "Custom amount"
    urine$PPTESTCD <- urine$PARAMCD
    urine$PARAMCD <- c(NA_character_, "", " ")
    frame <- attr(t_pkpt03_nest(urine, stats = c("n", "Mean", "SD"))[[1]], "tlg_data")
    expect_equal(as.vector(frame$PARAM), "Custom amount")
    expect_equal(as.numeric(frame$n), 3)
    expect_equal(as.numeric(frame$Mean), 6)
    expect_equal(as.numeric(frame$SD), 3)
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
    expect_equal(attr(custom, "tlg_annotations")$subtitle,
                 "Analyte: SERUM\nPPCAT: Drug {x} $USUBJID !PARAM / PPSPEC: SERUM")
    expect_false(attr(custom, "tlg_group_header"))
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

  it("keeps custom row headers instead of replacing them with catalog defaults", {
    skip_if_not_installed("gtsummary", "2.6.1")
    d <- nest_adpp_fixture()
    attr(d$TRT01A, "label") <- "Assigned regimen"
    attr(d$PARAM, "label") <- "Endpoint"
    table <- t_pkpt03_nest(d)[[1]]
    expect_true(all(c("Assigned regimen", "Endpoint") %in% table$table_styling$header$label))
    expect_equal(attr(attr(table, "tlg_data")$TRT01A, "label"), "Assigned regimen")
  })
})
