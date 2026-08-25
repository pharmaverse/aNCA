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

# The parent rows plus the ratio rows that Parameter Selection > Ratios appends
# during the NCA run: one metabolite/parent family (reference on the analyte) and
# one treatment family (reference on TRT01A).
.pkpl_ratio_rows <- function(ppcat, bracket, prefix, rows = TRUE) {
  # `rows` keeps the fixture honest: a ratio row exists only for the test side of
  # the comparison, never for the reference group itself.
  src <- pkpl_data[rows, , drop = FALSE]
  transform(
    src,
    PPCAT    = ppcat,
    PARAM    = paste(prefix, src$PARAM),
    PARAMCD  = paste0(substr(prefix, 1, 2), src$PARAMCD),
    AVAL     = src$AVAL / 2,
    AVALU    = "fraction",
    PPANMETH = paste0(src$PARAMCD, " TO ", src$PARAMCD, " [", bracket, "]")
  )
}

pkpl_ratio_data <- rbind(
  transform(pkpl_data, PPANMETH = NA_character_),
  .pkpl_ratio_rows("Metab-DrugA Plasma", "PARAM: DrugA Plasma", "MRatio"),
  .pkpl_ratio_rows(
    "DrugA Plasma", "TRT01A: 10mg", "TRatio",
    rows = pkpl_data$TRT01A == "50mg"
  )
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

  it("shows a later dose profile's value rather than a leading NA", {
    # A ratio is missing for any dose profile where either side could not be
    # computed, and the first profile is often one of them.  Taking the first row
    # regardless left the listing blank for a subject the summary table counted
    # and averaged -- the two outputs disagreed on the same data.
    multi <- do.call(rbind, lapply(c("S1", "S2"), function(s) {
      transform(
        pkpl_data[pkpl_data$USUBJID == s, ][rep(1, 4), ],
        AVISIT = paste("Cycle", 1:4),
        AVAL   = c(NA, NA, 7, 9)
      )
    }))
    out <- l_pkpl01(multi)[[1]]
    expect_equal(as.numeric(out[["Cmax"]]), c(7, 7))
  })

  it("still reports NA when every row for a subject is missing", {
    all_na <- transform(
      pkpl_data[rep(1, 3), ], AVISIT = paste("Cycle", 1:3), AVAL = NA_real_
    )
    expect_true(is.na(l_pkpl01(all_na)[[1]][["Cmax"]]))
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
})

describe("l_pkpl01_mp", {
  it("lists the M/P ratio rows and names the parent in the listing key", {
    result <- l_pkpl01_mp(pkpl_ratio_data)
    expect_equal(
      names(result),
      "RATIO: Metab-DrugA Plasma / DrugA Plasma / PPSPEC: SERUM"
    )
    purrr::walk(result, ~ expect_s3_class(.x, "listing_df"))
  })

  it("excludes treatment ratios and raw parameter rows", {
    # Only the analyte-referenced family belongs here; TRatio rows reference
    # TRT01A and the un-prefixed rows are not ratios at all.
    cols <- names(l_pkpl01_mp(pkpl_ratio_data)[[1]])
    expect_true(any(grepl("MRatio", cols)))
    expect_false(any(grepl("TRatio", cols)))
    expect_false(any(grepl("^Cmax|^AUClast", cols)))
  })

  it("errors instead of listing raw rows when no ratios were configured", {
    expect_error(
      l_pkpl01_mp(transform(pkpl_data, PPANMETH = NA_character_)),
      "l_pkpl01_mp: no ratio parameters found"
    )
  })

  it("does not treat mean-residence-time parameters as ratios", {
    mrt <- transform(
      pkpl_data,
      PARAMCD = rep(c("MRTLST", "MRTIFO"), 4),
      PPANMETH = NA_character_
    )
    expect_error(l_pkpl01_mp(mrt), "no ratio parameters found")
  })
})

describe("l_pkpl04_mp", {
  it("lists treatment ratios, the complement of the M/P ones", {
    result <- l_pkpl04_mp(pkpl_ratio_data)
    expect_equal(names(result), "RATIO: 50mg / 10mg / PPSPEC: SERUM")
    purrr::walk(result, ~ expect_s3_class(.x, "listing_df"))
  })

  it("excludes metabolite/parent ratios and raw parameter rows", {
    cols <- names(l_pkpl04_mp(pkpl_ratio_data)[[1]])
    expect_true(any(grepl("TRatio", cols)))
    expect_false(any(grepl("MRatio", cols)))
    expect_false(any(grepl("^Cmax|^AUClast", cols)))
  })

  it("errors when the data holds only metabolite/parent ratios", {
    mp_only <- rbind(
      transform(pkpl_data, PPANMETH = NA_character_),
      .pkpl_ratio_rows("Metab-DrugA Plasma", "PARAM: DrugA Plasma", "MRatio")
    )
    expect_error(
      l_pkpl04_mp(mp_only),
      "none are treatment ratios.*only metabolite/parent ratios were found"
    )
  })

  it("errors instead of listing raw rows when no ratios were configured", {
    expect_error(
      l_pkpl04_mp(transform(pkpl_data, PPANMETH = NA_character_)),
      "l_pkpl04_mp: no ratio parameters found"
    )
  })
})
