describe("l_pkpl01_nest", {
  it("retains listing records, units and excluded values alongside legacy output", {
    skip_if_not_installed("crane", "0.3.2")
    skip_if_not_installed("rlistings")
    d <- nest_adpp_fixture()
    d$PPSUMXF[1] <- "Y"
    table <- l_pkpl01_nest(d)[[1]]
    frame <- attr(table, "tlg_data")
    legacy <- l_pkpl01(d)[[1]]
    expect_equal(as.vector(frame$Cmax), c(1, 2, 4))
    expect_equal(as.vector(frame$AUC), c(3, 6, 9))
    expect_equal(as.vector(frame$Cmax), as.numeric(legacy$Cmax))
    expect_equal(as.vector(frame$USUBJID), c("1", "2", "3"))
    expect_true("Cmax (mg/L)" %in% table$table_styling$header$label)
    expect_equal(as.vector(table$table_body$col_1), c("A", NA, NA))
    expect_equal(as.vector(table$table_body$col_2), c("1", "2", "3"))
    headers <- table$table_styling$header
    keys <- headers[headers$column %in% c("col_1", "col_2"), ]
    expect_equal(keys$label, c("Treatment Group", "Subject"))
    expect_true(all(keys$align == "left"))
    selected <- l_pkpl01_nest(d, param_filter = "Cmax")[[1]]
    expect_named(attr(selected, "tlg_data"), c("TRT01A", "USUBJID", "Cmax"))
    expect_match(l_pkpl01_nest(d, param_filter = "absent")[[1]], "No data")
  })

  it("restores child keys at parent boundaries and collapses only equal records", {
    skip_if_not_installed("crane", "0.3.2")
    d <- nest_adpp_fixture()[1:2, ]
    other <- d
    other$TRT01A <- "B"
    table <- l_pkpl01_nest(rbind(other, d, d))[[1]]
    expect_equal(as.vector(table$table_body$col_1), c("A", "B"))
    expect_equal(as.vector(table$table_body$col_2), c("1", "1"))
    other <- d
    other$AVAL[1] <- 99
    expect_error(l_pkpl01_nest(rbind(other, d)), "conflicting values")
    other$AVISIT <- "Day 2"
    d$AVISIT <- "Day 1"
    table <- l_pkpl01_nest(rbind(d, other),
                           grouping_vars = c("TRT01A", "USUBJID", "AVISIT"))[[1]]
    expect_equal(as.numeric(attr(table, "tlg_data")$Cmax), c(1, 99))
    expect_equal(as.vector(table$table_body$col_2), c("1", NA))
  })

  it("rejects mixed units and missing keys instead of labeling incompatible values", {
    skip_if_not_installed("crane", "0.3.2")
    d <- nest_adpp_fixture()
    d$AVALU[3] <- "ug/L"
    expect_error(l_pkpl01_nest(d), "conflicting values or units")
    d$USUBJID <- NULL
    expect_error(l_pkpl01_nest(d), "Missing required columns")
  })

  it("displays the existing three-decimal values regardless of session digits", {
    skip_if_not_installed("crane", "0.3.2")
    skip_if_not_installed("rlistings")
    withr::local_options(list(digits = 3))
    d <- nest_adpp_fixture()
    d$AVAL[d$PARAM == "AUC"] <- c(123456.7894, 12808.7371, 0.5)
    table <- l_pkpl01_nest(d)[[1]]
    expected <- c(123456.789, 12808.737, 0.5)
    expect_equal(as.numeric(attr(table, "tlg_data")$AUC), expected)
    expect_equal(as.numeric(l_pkpl01(d)[[1]]$AUC), expected)
    expect_identical(gtsummary::as_tibble(table, col_labels = FALSE)$col_3,
                     c("123456.789", "12808.737", "0.5"))
    html <- gt::as_raw_html(.nest_gt(table))
    for (value in c(">123456.789<", ">12808.737<", ">0.5<")) expect_match(html, value, fixed = TRUE)
    # The default subtitle has one line per split variable; gt must not run them together.
    expect_match(html, "Parameter Category: Drug<br>Specimen Material Type: SERUM", fixed = TRUE)
    expect_false(attr(table, "tlg_group_header"))
  })

  it("lists each interval of an interval parameter in its own column", {
    skip_if_not_installed("crane", "0.3.2")
    table <- l_pkpl01_nest(nest_interval_fixture(), param_filter = c("AUC", "Cmax"))[[1]]
    frame <- attr(table, "tlg_data")
    expect_named(frame, c("TRT01A", "USUBJID", "AUC (PT0H-PT2H)", "AUC (PT0H-PT6H)", "Cmax"))
    expect_equal(as.numeric(frame[["AUC (PT0H-PT6H)"]]), c(6, 12, 18))
    expect_true("AUC (PT0H-PT2H) (h*mg/L)" %in% table$table_styling$header$label)
    urine <- attr(l_pkpl01_nest(nest_urine_fixture())[[1]], "tlg_data")
    expect_named(urine, c("TRT01A", "USUBJID", "Amt Rec"))
    expect_equal(as.numeric(urine[["Amt Rec"]]), c(3, 6, 9))
  })

  it("retains partial-AUC window columns when subjects have complementary records", {
    skip_if_not_installed("crane", "0.3.2")
    d <- nest_interval_fixture()
    d <- d[d$PARAM == "AUC", ]
    retained <- (d$USUBJID == "1" & d$PPENINT == "PT2H") |
      (d$USUBJID != "1" & d$PPENINT == "PT6H")
    d$PPSUMXF[!retained] <- "Y"
    table <- l_pkpl01_nest(d[retained, ], param_filter = "AUC")[[1]]
    frame <- attr(table, "tlg_data")
    expect_named(frame, c("TRT01A", "USUBJID", "AUC (PT0H-PT2H)", "AUC (PT0H-PT6H)"))
    expect_equal(as.numeric(frame[["AUC (PT0H-PT2H)"]]), c(3, NA, NA))
    expect_equal(as.numeric(frame[["AUC (PT0H-PT6H)"]]), c(NA, 12, 18))
    expect_true("AUC (PT0H-PT2H) (h*mg/L)" %in% table$table_styling$header$label)
    expect_true("AUC (PT0H-PT6H) (h*mg/L)" %in% table$table_styling$header$label)
    # Listings retain excluded records; window labels agree with the summary.
    complete <- attr(l_pkpl01_nest(d, param_filter = "AUC")[[1]], "tlg_data")
    expect_equal(as.numeric(complete[["AUC (PT0H-PT2H)"]]), c(3, 6, 9))
    expect_equal(as.numeric(complete[["AUC (PT0H-PT6H)"]]), c(6, 12, 18))
  })

  it("uses nonblank parameter codes per row to keep one urine listing column", {
    skip_if_not_installed("crane", "0.3.2")
    urine <- nest_urine_fixture()
    urine$PARAM <- "Custom amount"
    urine$PPTESTCD <- urine$PARAMCD
    urine$PARAMCD <- factor(c(NA_character_, "", " "))
    table <- l_pkpl01_nest(urine)[[1]]
    frame <- attr(table, "tlg_data")
    expect_named(frame, c("TRT01A", "USUBJID", "Custom amount"))
    expect_equal(as.numeric(frame[["Custom amount"]]), c(3, 6, 9))
    expect_true("Custom amount (h*mg/L)" %in% table$table_styling$header$label)
  })

  it("follows a custom subtitle with the split key", {
    skip_if_not_installed("crane", "0.3.2")
    table <- l_pkpl01_nest(nest_adpp_fixture(), subtitle = "Population: $TRT01A")[[1]]
    expect_equal(attr(table, "tlg_annotations")$subtitle,
                 "Population: A\nPPCAT: Drug / PPSPEC: SERUM")
    expect_false(attr(table, "tlg_group_header"))
  })

  it("explains when missing split keys remove every record", {
    skip_if_not_installed("crane", "0.3.2")
    d <- nest_adpp_fixture()
    d$PPCAT <- NA_character_
    expect_warning(result <- l_pkpl01_nest(d), "6 row\\(s\\).*excluded")
    expect_identical(result, list(all = "No data available for this NEST listing."))
    expect_warning(result <- l_pkpl01_nest(d[1:2, ], grouping_vars = character()),
                   "2 row\\(s\\).*excluded")
    expect_identical(result, list(all = "No data available for this NEST listing."))
  })
})

describe("NEST listing unit metadata", {
  it("retains available units in the shared legacy listing preparation", {
    skip_if_not_installed("rlistings")
    d <- nest_adpp_fixture()
    d$AVAL[1] <- NA_real_
    for (unit in c(NA_character_, "", " ")) {
      d$AVALU[1] <- unit
      legacy <- l_pkpl01(d)[[1]]
      expect_equal(attr(legacy$Cmax, "label"), "Cmax (mg/L)")
      expect_equal(as.numeric(legacy$Cmax), c(NA, 2, 4))
    }
  })

  it("allows missing results without units and keeps known units on the header", {
    skip_if_not_installed("crane", "0.3.2")
    d <- nest_adpp_fixture()
    d$AVAL[1] <- NA_real_
    for (unit in c(NA_character_, "", " ")) {
      d$AVALU[1] <- unit
      table <- l_pkpl01_nest(d)[[1]]
      expect_true(is.na(attr(table, "tlg_data")$Cmax[1]))
      expect_true("Cmax (mg/L)" %in% table$table_styling$header$label)
      d$AVAL[1] <- 5
      expect_error(l_pkpl01_nest(d), "nonmissing value has no unit")
      d$AVAL[1] <- NA_real_
    }
  })
})

describe("NEST listing labels", {
  it("renders parameter names and custom headers literally in HTML", {
    skip_if_not_installed("crane", "0.3.2")
    d <- nest_adpp_fixture()
    d$PARAM[d$PARAM == "Cmax"] <- "Cmax <terminal> & peak"
    attr(d$TRT01A, "label") <- "Dose <nominal> & regimen"
    table <- l_pkpl01_nest(d)[[1]]
    html <- gt::as_raw_html(.nest_gt(table))
    expect_match(html, "Cmax &lt;terminal&gt; &amp; peak (mg/L)", fixed = TRUE)
    expect_match(html, "Dose &lt;nominal&gt; &amp; regimen", fixed = TRUE)
    expect_equal(as.numeric(attr(table, "tlg_data")[["Cmax <terminal> & peak"]]), c(1, 2, 4))
  })

  it("preserves literal parameter names and custom headers in Word", {
    skip_if_not_installed("crane", "0.3.2")
    skip_if_not_installed("flextable")
    skip_if_not_installed("officer")
    d <- nest_adpp_fixture()
    d$PARAM[d$PARAM == "Cmax"] <- "Cmax <terminal> & peak"
    attr(d$TRT01A, "label") <- "Dose <nominal> & regimen"
    table <- l_pkpl01_nest(d)[[1]]
    dest <- withr::local_tempfile(fileext = ".docx")
    flextable::save_as_docx(gtsummary::as_flex_table(table), path = dest)
    text <- officer::docx_summary(officer::read_docx(dest))$text
    expect_true("Cmax <terminal> & peak (mg/L)" %in% text)
    expect_true("Dose <nominal> & regimen" %in% text)
  })

  it("preserves custom labels through filtering, splitting and sorting plain data frames", {
    skip_if_not_installed("crane", "0.3.2")
    d <- nest_adpp_fixture()
    attr(d$PPCAT, "label") <- "Analyte name"
    attr(d$TRT01A, "label") <- "Treatment group"
    table <- l_pkpl01_nest(d, param_filter = "Cmax", title = "!PPCAT: $PPCAT")[[1]]
    expect_equal(attr(table, "tlg_annotations")$title, "Analyte name: Drug")
    expect_false(grepl("ERR", attr(table, "tlg_annotations")$subtitle))
    expect_true("Treatment group" %in% table$table_styling$header$label)
  })
})
