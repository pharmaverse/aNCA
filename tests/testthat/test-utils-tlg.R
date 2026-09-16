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

# Issue #1430: title, subtitle and footnote resolution shared by the table TLGs.

describe(".tlg_label", {
  df <- data.frame(
    PARAM  = c("DrugA", "DrugA"),
    TRT01A = c("5 mg", "10 mg"),
    stringsAsFactors = FALSE
  )

  it("returns NULL for unset text so the caller can fall back to its own default", {
    expect_null(.tlg_label(df, NULL))
    expect_null(.tlg_label(df, ""))
  })

  it("resolves a $COL token that is constant within the split", {
    expect_equal(.tlg_label(df, "Analyte: $PARAM"), "Analyte: DrugA")
  })

  it("collapses a token that varies within the split instead of returning a vector", {
    # A vector reaching labs()/as_listing() blanks the label rather than erroring, so the
    # helper must hand back exactly one string.
    out <- .tlg_label(df, "Treatment: $TRT01A")
    expect_length(out, 1L)
    expect_equal(out, "Treatment: 5 mg, Treatment: 10 mg")
  })

  it("passes plain text through unchanged", {
    expect_equal(.tlg_label(df, "NE: Not Estimable"), "NE: Not Estimable")
  })
})

describe(".split_subtitle", {
  it("returns NULL when there are no split variables", {
    expect_null(.split_subtitle(data.frame(A = 1), character(0)))
  })

  it("uses the column label when one is present", {
    df <- data.frame(PARAM = "DrugA", stringsAsFactors = FALSE)
    attr(df$PARAM, "label") <- "Parameter"
    expect_equal(.split_subtitle(df, "PARAM"), "Parameter: $PARAM")
  })

  it("falls back to the column name rather than emitting ERR", {
    # `!PARAM` would resolve to the literal string "ERR" when the column carries no label,
    # which is what ADNCA does for PARAM/PCSPEC.
    df <- data.frame(PARAM = "DrugA", stringsAsFactors = FALSE)
    expect_equal(.split_subtitle(df, "PARAM"), "PARAM: $PARAM")
  })

  it("joins several split variables with newlines", {
    df <- data.frame(PARAM = "DrugA", PCSPEC = "SERUM", stringsAsFactors = FALSE)
    expect_equal(.split_subtitle(df, c("PARAM", "PCSPEC")), "PARAM: $PARAM\nPCSPEC: $PCSPEC")
  })
})

describe(".attach_table_labs", {
  df   <- data.frame(n = 1)
  data <- data.frame(PARAM = "DrugA", stringsAsFactors = FALSE)

  it("attaches all three labels as attributes", {
    out <- .attach_table_labs(df, data, "T", "Analyte: $PARAM", "F")
    expect_equal(attr(out, "tlg_title"), "T")
    expect_equal(attr(out, "tlg_subtitle"), "Analyte: DrugA")
    expect_equal(attr(out, "tlg_footnote"), "F")
  })

  it("leaves the table contents untouched and omits labels that are unset", {
    out <- .attach_table_labs(df, data, "T", NULL, NULL)
    expect_equal(names(out), names(df))
    expect_equal(out$n, df$n)
    expect_null(attr(out, "tlg_subtitle"))
    expect_null(attr(out, "tlg_footnote"))
  })
})
