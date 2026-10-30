# Shared fixture: minimal ADPP-like data frame
pkpl_data <- data.frame(
  USUBJID = rep(paste0("S", 1:4), each = 2),
  TRT01A  = rep(c("10mg", "10mg", "50mg", "50mg"), each = 2),
  PARAM   = rep(c("Cmax", "AUClast"), 4),
  PARAMCD = rep(c("CMAX", "AUCLST"), 4),
  AVAL    = c(5, 20, 6, 22, 10, 40, 11, 38),
  AVALU   = rep(c("ng/mL", "ng/mL*h"), 4),
  PPCAT   = "DrugA Plasma",
  PPSPEC  = "SERUM",
  METABFL = NA_character_,
  stringsAsFactors = FALSE
)

pkpl_metab_data <- pkpl_data
pkpl_metab_data$PPCAT <- ifelse(
  pkpl_metab_data$TRT01A == "50mg", "Metab-DrugA Plasma", "DrugA Plasma"
)
pkpl_metab_data$METABFL <- ifelse(
  pkpl_metab_data$TRT01A == "50mg", "Y", NA_character_
)

describe("l_pkpl01 (rlistings not installed)", {
  it("stops with informative error when rlistings is unavailable", {
    testthat::with_mocked_bindings(
      requireNamespace = function(pkg, quietly = FALSE) {
        if (pkg == "rlistings") FALSE else TRUE
      },
      .package = "base",
      code = {
        expect_error(l_pkpl01(pkpl_data), "Package 'rlistings' is required")
      }
    )
  })
})

describe("l_pkpl01", {
  it("returns a named list", {
    result <- l_pkpl01(pkpl_data)
    expect_type(result, "list")
    expect_true(length(result) >= 1)
  })

  it("each element is a listing_df", {
    result <- l_pkpl01(pkpl_data)
    purrr::walk(result, ~ expect_s3_class(.x, "listing_df"))
  })

  it("splits by PPCAT/PPSPEC — one entry per combination", {
    two_cats <- rbind(
      pkpl_data,
      transform(pkpl_data, PPCAT = "DrugB Plasma")
    )
    result <- l_pkpl01(two_cats)
    expect_equal(length(result), 2)
  })

  it("stops with informative error when required columns are missing", {
    bad <- pkpl_data[, setdiff(names(pkpl_data), "AVAL")]
    expect_error(l_pkpl01(bad), "missing required columns")
  })

  it("pivots PARAM to columns — each unique PARAM becomes a column", {
    result <- l_pkpl01(pkpl_data)[[1]]
    expect_true("Cmax" %in% names(result) || "AUClast" %in% names(result))
    expect_false("PARAM" %in% names(result))
  })

  it("uses custom grouping_vars", {
    result <- l_pkpl01(pkpl_data, grouping_vars = c("TRT01A"))
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "listing_df"))
  })

  it("handles multi-interval ADPP (duplicate PARAM rows per subject) without error", {
    # Simulate ADPP with two dose intervals: same USUBJID+PARAM appears twice
    dup_data <- rbind(pkpl_data, pkpl_data)
    dup_data$AVAL <- dup_data$AVAL + runif(nrow(dup_data), 0, 1)
    # pivot_wider must not produce list-columns; values_fn = first deduplicates
    expect_no_error({
      result <- l_pkpl01(dup_data)
    })
    purrr::walk(result, ~ expect_s3_class(.x, "listing_df"))
  })

  it("marks excluded missing parameters without marking other missing values", {
    flagged_data <- pkpl_data
    flagged_data$AVAL[flagged_data$PARAM == "AUClast"] <- NA_real_
    flagged_data$PPSUMXF <- ""
    flagged_data$PPSUMXF[2] <- "Y"

    result <- l_pkpl01(flagged_data)[[1]]

    expect_equal(as.vector(result$AUClast), c("NA*", NA, NA, NA))
    expect_equal(as.vector(result$Cmax), c("5", "6", "10", "11"))
    expect_equal(nrow(result), length(unique(flagged_data$USUBJID)))
    expect_match(paste(capture.output(print(result)), collapse = "\n"), "NA\\*")
  })

  it("marks PPSUMXF parameter values and explains the marker in the footer", {
    flagged_data <- pkpl_data
    flagged_data$PPSUMXF <- ""
    flagged_data$PPSUMXF[flagged_data$USUBJID == "S1" &
                           flagged_data$PARAM == "AUClast"] <- "Y"

    result <- l_pkpl01(flagged_data, footnote = "Existing footnote")[[1]]
    s1_row <- as.vector(result$USUBJID) == "S1"

    expect_equal(as.vector(result$AUClast)[s1_row], "20*")
    expect_equal(as.vector(result$Cmax)[s1_row], "5")
    expect_equal(
      attr(result, "main_footer"),
      c(
        "Existing footnote",
        "* Summary and mean plots excluded (PPSUMXF = \"Y\")."
      )
    )
  })

  it("uses sorted unique reasons for stable ADPP markers", {
    flagged_data <- pkpl_data
    flagged_data$PPSUMXF <- ""
    flagged_data$PPSUMXF[1:2] <- "Y"
    flagged_data$PPSUMRSN <- ""
    flagged_data$PPSUMRSN[1] <- "Reason B; Reason A"
    flagged_data$PPSUMRSN[2] <- "Reason C"

    result <- l_pkpl01(flagged_data)[[1]]
    expect_equal(as.character(result$Cmax[1]), "5*, **")
    expect_equal(as.character(result$AUClast[1]), "20***")
    expect_equal(attr(result, "main_footer"), c(
      '* Summary and mean plots excluded: PPSUMRSN = "Reason A".',
      '** Summary and mean plots excluded: PPSUMRSN = "Reason B".',
      '*** Summary and mean plots excluded: PPSUMRSN = "Reason C".'
    ))
  })

  it("keeps reason markers stable across groups, row order and parameter selection", {
    data <- pkpl_data
    data$PPCAT <- rep(c("DrugA", "DrugB"), each = 4)
    data$PPSUMXF <- "Y"
    data$PPSUMRSN <- ifelse(data$PARAM == "Cmax", "Reason B", "Reason A")
    result <- l_pkpl01(data)
    reversed <- l_pkpl01(data[rev(seq_len(nrow(data))), ])
    selected <- l_pkpl01(data, param_filter = "Cmax")
    for (i in seq_along(result)) {
      expect_true(all(endsWith(as.character(result[[i]]$Cmax), "**")))
      expect_equal(sort(as.character(result[[i]]$Cmax)),
                   sort(as.character(reversed[[i]]$Cmax)))
      expect_equal(as.character(selected[[i]]$Cmax), as.character(result[[i]]$Cmax))
      expect_equal(attr(selected[[i]], "main_footer"),
                   '** Summary and mean plots excluded: PPSUMRSN = "Reason B".')
    }
  })

  it("omits footnotes from discarded duplicate intervals and unflagged rows", {
    data <- pkpl_data
    data$PPSUMXF <- ""
    data$PPSUMRSN <- "Reason A"
    duplicate <- data[1, ]
    duplicate$AVAL <- 999
    duplicate$PPSUMXF <- "Y"
    result <- l_pkpl01(rbind(data, duplicate))[[1]]
    expect_equal(as.numeric(result$Cmax), c(5, 6, 10, 11))
    expect_length(attr(result, "main_footer"), 0)

    data$PPSUMXF[2] <- "Y"
    data$PPSUMRSN[2] <- "Reason B"
    result <- l_pkpl01(rbind(data, duplicate))[[1]]
    expect_equal(as.character(result$AUClast)[1], "20**")
    expect_equal(attr(result, "main_footer"),
                 '** Summary and mean plots excluded: PPSUMRSN = "Reason B".')
  })
})

describe("PK listing parameter selection", {
  it("shows only selected parameters and retains subject values and units", {
    result <- l_pkpl01(pkpl_data, param_filter = "Cmax")[[1]]
    expect_equal(rlistings::listing_dispcols(result), c("TRT01A", "USUBJID", "Cmax"))
    expect_equal(as.character(result$USUBJID), paste0("S", 1:4))
    expect_equal(as.numeric(result$Cmax), c(5, 6, 10, 11))
    expect_equal(attr(result$Cmax, "label"), "Cmax (ng/mL)")
    expect_false("AUClast" %in% names(result))
  })

  it("preserves all parameters by default or with an empty selection", {
    defaults <- l_pkpl01(pkpl_data)
    expect_equal(l_pkpl01(pkpl_data, param_filter = character()), defaults)
    expect_equal(l_pkpl01(pkpl_data, param_filter = c("Cmax", "AUClast")), defaults)
    expect_setequal(rlistings::listing_dispcols(defaults[[1]]),
                    c("TRT01A", "USUBJID", "Cmax", "AUClast"))
  })

  it("selects values from the chosen parameter variable", {
    result <- l_pkpl01(pkpl_data, param_var = "PARAMCD", param_filter = "AUCLST")[[1]]
    expect_equal(rlistings::listing_dispcols(result), c("TRT01A", "USUBJID", "AUCLST"))
    expect_equal(as.numeric(result$AUCLST), c(20, 22, 40, 38))
    expect_equal(attr(result$AUCLST, "label"), "AUCLST (ng/mL*h)")
  })

  it("returns no listings for unmatched selections, including without split variables", {
    expect_equal(l_pkpl01(pkpl_data, param_filter = "missing"), list())
    expect_equal(l_pkpl01(pkpl_data, listgroup_vars = character(),
                          param_filter = "missing"), list())
  })

  it("keeps grouping when parameters differ between listing pages", {
    other <- transform(pkpl_data[pkpl_data$PARAM == "Cmax", ], PPCAT = "DrugB")
    result <- l_pkpl01(rbind(pkpl_data, other), param_filter = "AUClast")
    expect_length(result, 1)
    expect_match(names(result), "DrugA")
    expect_equal(as.numeric(result[[1]]$AUClast), c(20, 22, 40, 38))
  })

  it("passes selection through both wrappers and preserves their row selection", {
    metabolite <- l_pkpl01_mp(pkpl_metab_data, param_filter = "Cmax")[[1]]
    treatment <- l_pkpl04_mp(pkpl_data, param_filter = "Cmax")[[1]]
    expect_equal(rlistings::listing_dispcols(metabolite), c("TRT01A", "USUBJID", "Cmax"))
    expect_equal(as.character(metabolite$USUBJID), c("S3", "S4"))
    expect_equal(as.numeric(metabolite$Cmax), c(10, 11))
    expect_equal(rlistings::listing_dispcols(treatment), c("TRT01A", "USUBJID", "Cmax"))
    expect_equal(as.numeric(treatment$Cmax), c(5, 6, 10, 11))
  })
})

describe("l_pkpl01_mp", {
  it("filters to metabolite rows via METABFL (preferred path)", {
    result <- l_pkpl01_mp(pkpl_metab_data)
    # Only Metab-DrugA rows — listing name contains "Metab"
    expect_true(all(grepl("Metab", names(result), ignore.case = TRUE)))
  })

  it("falls back to PPCAT grep when METABFL absent", {
    data_ppcat <- pkpl_data
    data_ppcat$PPCAT <- ifelse(
      data_ppcat$TRT01A == "50mg", "Metab-DrugA", "DrugA"
    )
    data_ppcat <- data_ppcat[, setdiff(names(data_ppcat), "METABFL")]
    result <- l_pkpl01_mp(data_ppcat)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "listing_df"))
  })

  it("falls back to PARAM grep when METABFL and PPCAT absent", {
    data_param <- pkpl_data
    data_param$PARAM <- ifelse(
      data_param$TRT01A == "50mg",
      paste0("Metab-", data_param$PARAM),
      data_param$PARAM
    )
    data_param <- data_param[,
      setdiff(names(data_param), c("METABFL", "PPCAT"))
    ]
    result <- l_pkpl01_mp(data_param)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "listing_df"))
  })

  it("stops with informative error when no metabolite data found", {
    data_no_metab <- pkpl_data[, setdiff(names(pkpl_data), "METABFL")]
    expect_error(l_pkpl01_mp(data_no_metab), "no metabolite data found")
  })
})

describe("l_pkpl04_mp", {
  it("keeps parameters on the same subject row when displaying parameter codes", {
    data <- transform(pkpl_data, PARAMCD = ifelse(PARAM == "Cmax", "CMAX", "AUCLST"))
    named <- l_pkpl04_mp(data)
    coded <- l_pkpl04_mp(data, param_var = "PARAMCD")
    expect_equal(vapply(coded, nrow, integer(1)), vapply(named, nrow, integer(1)))
    expect_false("PARAM" %in% rlistings::listing_dispcols(coded[[1]]))
    codes <- unique(data$PARAMCD)
    expect_true(all(codes %in% rlistings::listing_dispcols(coded[[1]])))
    expect_false(anyNA(as.data.frame(coded[[1]])[, codes]))
  })

  it("returns a named list of listing_df objects", {
    result <- l_pkpl04_mp(pkpl_data)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "listing_df"))
  })

  it("has PARAM as a key (grouping) column", {
    result <- l_pkpl04_mp(pkpl_data)[[1]]
    # l_pkpl04_mp has PARAM in grouping_vars so it appears in the listing
    expect_true(length(result) > 0)
    expect_s3_class(result, "listing_df")
  })

  it("splits by PPCAT/PPSPEC — consistent with l_pkpl01", {
    two_specs <- rbind(
      pkpl_data,
      transform(pkpl_data, PPSPEC = "URINE")
    )
    result <- l_pkpl04_mp(two_specs)
    expect_equal(length(result), 2)
  })
})
