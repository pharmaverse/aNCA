# Tests for shared TLG helpers in R/utils-tlg.R.

describe(".select_stats", {
  # A minimal flat summary table: two key columns + a full stat block.
  flat <- data.frame(
    TRT01A = c("A", "B"),
    PARAM  = c("AUC", "AUC"),
    n      = c(3L, 4L),
    Mean   = c(10, 20),
    SD     = c(1, 2),
    CV_pct = c(10, 10),
    stringsAsFactors = FALSE
  )

  it("returns the frame unchanged when stats is NULL or empty", {
    expect_identical(.select_stats(flat, NULL), flat)
    expect_identical(.select_stats(flat, character(0)), flat)
  })

  it("keeps key columns plus the selected stats, preserving order", {
    out <- .select_stats(flat, c("n", "CV_pct"))
    expect_equal(names(out), c("TRT01A", "PARAM", "n", "CV_pct"))
    # Key columns and kept values are untouched.
    expect_equal(out$TRT01A, c("A", "B"))
    expect_equal(out$CV_pct, c(10, 10))
  })

  it("silently ignores requested stats the table does not produce", {
    # n_blq / GeoMean are not columns of `flat`.
    out <- .select_stats(flat, c("Mean", "n_blq", "GeoMean"))
    expect_equal(names(out), c("TRT01A", "PARAM", "Mean"))
  })

  it("rebuilds col_groups and drops groups left without columns", {
    # Group-comparison shape: prefixed leaf names + a col_groups attribute.
    sep <- .GROUP_SEP
    g <- data.frame(TRT01A = "A", PARAM = "AUC", stringsAsFactors = FALSE)
    for (lvl in c("F", "M")) {
      for (st in c("n", "Mean", "SD")) {
        g[[paste0(lvl, sep, st)]] <- 1
      }
    }
    attr(g, "col_groups") <- list(
      F = paste0("F", sep, c("n", "Mean", "SD")),
      M = paste0("M", sep, c("n", "Mean", "SD"))
    )

    out <- .select_stats(g, c("n", "Mean"))
    # Both key columns survive; each group keeps only n + Mean leaves.
    expect_equal(
      names(out),
      c("TRT01A", "PARAM",
        paste0("F", sep, c("n", "Mean")),
        paste0("M", sep, c("n", "Mean")))
    )
    cg <- attr(out, "col_groups")
    expect_equal(names(cg), c("F", "M"))
    expect_true(all(vapply(cg, length, integer(1)) == 2L))
    expect_true(all(unlist(cg) %in% names(out)))
  })

  it("drops a group entirely when none of its stats are selected", {
    sep <- .GROUP_SEP
    g <- data.frame(TRT01A = "A", stringsAsFactors = FALSE)
    g[[paste0("F", sep, "n")]] <- 1
    g[[paste0("F", sep, "Mean")]] <- 2
    attr(g, "col_groups") <- list(F = paste0("F", sep, c("n", "Mean")))

    # Select a stat the group never had -> group vanishes, only key column left.
    out <- .select_stats(g, "SD")
    expect_equal(names(out), "TRT01A")
    expect_equal(length(attr(out, "col_groups")), 0L)
  })
})

describe(".parse_ratio_reference", {
  it("extracts a single key/value pair from the bracket", {
    out <- .parse_ratio_reference("CMAX TO CMAX [PARAM: DrugA]")[[1]]
    expect_equal(out, c(PARAM = "DrugA"))
  })

  it("extracts every key when the reference spans several grouping variables", {
    out <- .parse_ratio_reference("CMAX TO CMAX [PARAM: DrugA, PCSPEC: Plasma]")[[1]]
    expect_equal(out, c(PARAM = "DrugA", PCSPEC = "Plasma"))
  })

  it("ignores an analysis method prepended by .apply_metadata_ppanmeth()", {
    # export_cdisc() prepends the parameter's own method with "; ", so the ratio
    # string is not always the whole field.
    out <- .parse_ratio_reference(
      "Interpolation truncated at next dose time; AUCINT TO AUCINT [PARAM: DrugA]"
    )[[1]]
    expect_equal(out, c(PARAM = "DrugA"))
  })

  it("returns nothing when the groups are identical and no bracket was written", {
    expect_length(.parse_ratio_reference("CMAX TO CMAX")[[1]], 0)
  })

  it("returns nothing for NA or an unparseable bracket", {
    expect_length(.parse_ratio_reference(NA_character_)[[1]], 0)
    expect_length(.parse_ratio_reference("CMAX TO CMAX [DrugA]")[[1]], 0)
  })

  it("keeps a reference value that itself contains the ', ' separator", {
    # ", " separates pairs and can also occur inside an analyte name.  Dropping
    # the unprefixed fragment truncated the denominator, and truncation merges:
    # two distinct references collapse to one split key and get pooled.
    er <- .parse_ratio_reference("CMAX TO CMAX [PARAM: Drug A, Extended Release]")[[1]]
    ir <- .parse_ratio_reference("CMAX TO CMAX [PARAM: Drug A, Immediate Release]")[[1]]
    expect_equal(er, c(PARAM = "Drug A, Extended Release"))
    expect_false(identical(unname(er), unname(ir)))
  })

  it("still separates the pairs when a value contains ', '", {
    out <- .parse_ratio_reference("CMAX TO CMAX [PARAM: Drug A, ER, PCSPEC: Plasma]")[[1]]
    expect_equal(out, c(PARAM = "Drug A, ER", PCSPEC = "Plasma"))
  })

  it("is vectorized over the column", {
    out <- .parse_ratio_reference(c("A TO B [PARAM: X]", "A TO B"))
    expect_length(out, 2)
    expect_equal(out[[1]], c(PARAM = "X"))
    expect_length(out[[2]], 0)
  })
})

describe(".parse_ratio_parameters", {
  it("reads the pair with and without a reference bracket", {
    expect_equal(
      .parse_ratio_parameters(c("AUCLST TO CMAX", "AUCLST TO CMAX [PARAM: DrugA]")),
      matrix(
        c("AUCLST", "AUCLST", "CMAX", "CMAX"),
        ncol = 2, dimnames = list(NULL, c("test", "ref"))
      )
    )
  })

  it("ignores an analysis method prepended by .apply_metadata_ppanmeth()", {
    out <- .parse_ratio_parameters("Linear up log down; AUCLST TO CMAX")
    expect_equal(unname(out[1, ]), c("AUCLST", "CMAX"))
  })

  it("returns NA for NA and for free text that merely contains ' TO '", {
    out <- .parse_ratio_parameters(c(NA_character_, "measured from dose TO last conc"))
    expect_true(all(is.na(out)))
  })
})

describe("filter_ratio_rows", {
  # Minimal ADPP shape: one parent row, one analyte-referenced (M/P) ratio row,
  # one treatment-referenced ratio row.
  adpp <- data.frame(
    PARAMCD = c("CMAX", "MRCMAX", "RACMAX"),
    PPCAT   = c("DrugA", "Metab-DrugA", "DrugA"),
    TRT01A  = c("50mg", "50mg", "50mg"),
    AVAL    = c(10, 0.5, 2),
    PPANMETH = c(
      NA_character_,
      "CMAX TO CMAX [PARAM: DrugA]",
      "CMAX TO CMAX [TRT01A: 10mg]"
    ),
    stringsAsFactors = FALSE
  )

  it("selects ratio rows on PPANMETH and names the parent alongside the metabolite", {
    out <- filter_ratio_rows(adpp, "caller", "analyte")
    expect_equal(nrow(out), 1)
    expect_equal(out$PARAMCD, "MRCMAX")
    expect_equal(as.character(out$RATIOREF), "DrugA")
    expect_equal(as.character(out$RATIO), "Metab-DrugA / DrugA")
  })

  it("reads the numerator from PPCAT, where export_cdisc() puts the analyte", {
    # The bracket key is the pre-export column name (PARAM); in ADPP that value
    # lives in PPCAT, so the numerator must be resolved through the rename.
    out <- filter_ratio_rows(adpp, "caller", "analyte")
    expect_equal(
      as.character(out$RATIO),
      paste0(out$PPCAT, " / ", as.character(out$RATIOREF))
    )
  })

  it("labels the derived columns so !RATIO annotations resolve", {
    out <- filter_ratio_rows(adpp, "caller", "analyte")
    expect_equal(attr(out$RATIO, "label"), "Metabolite / Parent")
    expect_equal(attr(out$RATIOREF, "label"), "Parent (reference analyte)")
  })

  it("selects treatment ratios as the complement of the analyte ones", {
    out <- filter_ratio_rows(adpp, "caller", "other")
    expect_equal(nrow(out), 1)
    expect_equal(out$PARAMCD, "RACMAX")
    expect_equal(as.character(out$RATIO), "50mg / 10mg")
    expect_equal(attr(out$RATIO, "label"), "Test / Reference")
  })

  it("does not read free-text analysis method as a ratio", {
    # PPANMETH is a permitted ADPP variable carrying free text, and
    # .apply_metadata_ppanmeth() writes a parameter's own method into it.  A bare
    # grepl(" TO ") swept those rows into the ratio outputs.
    prose <- adpp[adpp$PARAMCD == "CMAX", ]
    prose$PPANMETH <- "Concentrations interpolated from dose TO last measurable conc"
    expect_error(filter_ratio_rows(prose, "caller", "any"), "no ratio parameters found")
  })

  it("does not read a bracketed annotation with no comparison as a ratio", {
    # The bracket alone used to be enough, so an analysis method that happens to
    # carry a "key: value" note was summarized under a ratio heading and labelled
    # with the note's value -- without containing " TO " anywhere.
    noted <- adpp[adpp$PARAMCD == "CMAX", ]
    noted$PPANMETH <- "Interpolated [source: nominal]"
    expect_error(filter_ratio_rows(noted, "caller", "any"), "no ratio parameters found")
  })

  it("still selects a ratio whose parameter code is not a bare token", {
    # A ratio of a ratio carries "RAAUCLST (mean)" as its parameter, so the pair
    # is not a two-token string; the reference bracket identifies it instead.
    chained <- adpp[adpp$PARAMCD == "RACMAX", ]
    chained$PPANMETH <- "RACMAX (mean) TO RACMAX (mean) [TRT01A: 10mg]"
    expect_equal(nrow(filter_ratio_rows(chained, "caller", "other")), 1)
  })

  it("names both sides of a reference that spans analyte and specimen", {
    # Keeping only the analyte key read "Metab / DrugA" for a metabolite in urine
    # referenced against the parent in serum, hiding the change of matrix.
    cross <- adpp[adpp$PARAMCD == "MRCMAX", ]
    cross$PPSPEC <- "URINE"
    cross$PPANMETH <- "CMAX TO CMAX [PARAM: DrugA, PCSPEC: SERUM]"
    out <- filter_ratio_rows(cross, "caller", "analyte")
    expect_equal(as.character(out$RATIOREF), "DrugA, SERUM")
    expect_equal(as.character(out$RATIO), "Metab-DrugA, URINE / DrugA, SERUM")
  })

  it("warns rather than silently replacing a RATIO column already in the data", {
    clash <- adpp
    clash$RATIO <- "pre-existing"
    expect_warning(
      filter_ratio_rows(clash, "caller", "analyte"),
      "already has a column named RATIO"
    )
  })

  it("keeps both families under ref_type = 'any'", {
    expect_equal(nrow(filter_ratio_rows(adpp, "caller", "any")), 2)
  })

  it("does not mistake mean-residence-time parameters for ratios", {
    # Every MRT parameter starts with "MR", which is why selection keys on
    # PPANMETH rather than on a PARAMCD prefix.
    mrt <- data.frame(
      PARAMCD = c("MRTLST", "MRTIFO", "MRTIBLST", "MRTICIFO", "MRTEVIFP"),
      PPCAT   = "DrugA",
      AVAL    = 1:5,
      PPANMETH = NA_character_,
      stringsAsFactors = FALSE
    )
    expect_error(
      filter_ratio_rows(mrt, "caller", "analyte"),
      "no ratio parameters found"
    )
  })

  it("errors with re-run instructions when no ratios were configured", {
    no_ratios <- adpp[adpp$PARAMCD == "CMAX", ]
    expect_error(filter_ratio_rows(no_ratios, "caller", "analyte"), "^caller: ")
    expect_error(
      filter_ratio_rows(no_ratios, "caller", "analyte"),
      "Parameter Selection > Ratios and re-run the NCA"
    )
  })

  it("errors when PPANMETH was dropped entirely for being all-missing", {
    expect_error(
      filter_ratio_rows(adpp[, setdiff(names(adpp), "PPANMETH")], "caller", "analyte"),
      "no ratio parameters found"
    )
  })

  it("says which family was found when the wrong one is requested", {
    only_mp <- adpp[adpp$PARAMCD != "RACMAX", ]
    expect_error(
      filter_ratio_rows(only_mp, "caller", "other"),
      "none are treatment ratios.*only metabolite/parent ratios were found"
    )
  })

  it("does not claim the complement is all treatment ratios", {
    # The non-analyte family also holds route, specimen and same-group ratios, so
    # the message must not name treatment in that direction.
    only_same_group <- adpp[adpp$PARAMCD == "MRCMAX", ]
    only_same_group$PPANMETH <- "AUCLST TO CMAX"
    expect_error(
      filter_ratio_rows(only_same_group, "caller", "analyte"),
      "only ratios referenced against something other than the analyte were found"
    )
  })

  it("names the parameter pair for a same-group ratio, which has no bracket", {
    bare <- adpp[adpp$PARAMCD == "MRCMAX", ]
    bare$PPANMETH <- "AUCLST TO CMAX"
    # No bracket means no reference key, so nothing identifies it as M/P.
    expect_error(filter_ratio_rows(bare, "caller", "analyte"), "none are metabolite")

    # Left as NA these rows were dropped by split_and_apply() and l_pkpl04_mp
    # rendered an empty listing, even though this is a real ratio.
    out <- filter_ratio_rows(bare, "caller", "other")
    expect_true(is.na(out$RATIOREF))
    expect_equal(as.character(out$RATIO), "AUCLST / CMAX")
  })

  it("prefers the reference group over the parameter pair when both are readable", {
    out <- filter_ratio_rows(adpp, "caller", "analyte")
    expect_equal(as.character(out$RATIO), "Metab-DrugA / DrugA")
  })

  it("gives every selected row a label, so none is dropped as a stray NA", {
    # RATIO is a split key and split_and_apply() drops rows with an NA there, so a
    # missing label makes the row vanish behind a warning.  Selection admits a row
    # only when one of the two parsers reads it, and either one supplies a label:
    # the reference groups, or the parameter pair for a bracket-less same-group ratio.
    mixed <- adpp[rep(which(adpp$PARAMCD != "CMAX"), 2), ]
    mixed$PPANMETH[3:4] <- "AUCLST TO CMAX"
    out <- filter_ratio_rows(mixed, "caller", "any")
    expect_equal(nrow(out), 4)
    expect_false(anyNA(out$RATIO))
    expect_true("AUCLST / CMAX" %in% as.character(out$RATIO))
  })

  it("shows the reference alone when only some reference keys reach ADPP", {
    # ROUTE has no ADPP column here, so reading TRT01A alone would render
    # "50mg / 10mg, iv" -- two sides of a "/" describing different things.
    partial <- adpp[adpp$PARAMCD == "RACMAX", ]
    partial$PPANMETH <- "CMAX TO CMAX [TRT01A: 10mg, ROUTE: iv]"
    out <- filter_ratio_rows(partial, "caller", "other")
    expect_equal(as.character(out$RATIOREF), "10mg, iv")
    expect_equal(as.character(out$RATIO), "10mg, iv")
  })

  it("names both sides when every reference key does reach ADPP", {
    both <- adpp[adpp$PARAMCD == "RACMAX", ]
    both$TRT01A <- "50mg"
    both$PPANMETH <- "CMAX TO CMAX [TRT01A: 10mg]"
    out <- filter_ratio_rows(both, "caller", "other")
    expect_equal(as.character(out$RATIO), "50mg / 10mg")
  })

  it("keeps a reference key whose ADPP column is absent from the data", {
    # PCSPEC is renamed to PPSPEC on export; if neither is present the reference
    # is still reported, only the numerator side is unknown.
    spec <- adpp[adpp$PARAMCD == "RACMAX", ]
    spec$PPANMETH <- "CMAX TO CMAX [PCSPEC: Plasma]"
    out <- filter_ratio_rows(spec, "caller", "other")
    expect_equal(as.character(out$RATIOREF), "Plasma")
    expect_equal(as.character(out$RATIO), "Plasma")
  })
})

describe("filter_ratio_rows: ratios that could not be computed", {
  # One M/P ratio with a value, one the NCA run left empty.
  adpp <- data.frame(
    PARAMCD  = c("MRCMAX", "MRAUCLST"),
    PARAM    = c("Metabolite Ratio for Max Conc", "Metabolite Ratio for AUClast"),
    PPCAT    = "Metab-DrugA",
    AVAL     = c(0.5, NA_real_),
    PPANMETH = "CMAX TO CMAX [PARAM: DrugA]",
    stringsAsFactors = FALSE
  )

  it("warns and names the parameter that came back empty", {
    expect_warning(
      filter_ratio_rows(adpp, "caller", "analyte"),
      "no value was computed for 1 of 2 ratio parameter\\(s\\).*AUClast"
    )
  })

  it("raises it as a user-facing tlg_warning, not console noise", {
    # Only `tlg_warning` conditions are surfaced as notifications by the Shiny layer.
    expect_warning(
      filter_ratio_rows(adpp, "caller", "analyte"),
      class = "tlg_warning"
    )
  })

  it("keeps the empty rows rather than silently dropping them", {
    out <- suppressWarnings(filter_ratio_rows(adpp, "caller", "analyte"))
    expect_equal(nrow(out), 2)
    expect_true(any(is.na(out$AVAL)))
  })

  it("stays quiet when every ratio has a value", {
    ok <- adpp
    ok$AVAL <- c(0.5, 0.8)
    expect_no_warning(filter_ratio_rows(ok, "caller", "analyte"))
  })

  it("stays quiet when a parameter is only partly missing", {
    # A single subject with no value is normal; the warning is for a parameter
    # that produced nothing at all.
    partial <- rbind(adpp, transform(adpp[2, ], AVAL = 0.9))
    expect_no_warning(filter_ratio_rows(partial, "caller", "analyte"))
  })
})
