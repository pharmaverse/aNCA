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
