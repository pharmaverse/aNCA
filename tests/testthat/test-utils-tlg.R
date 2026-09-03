# Tests for shared TLG helpers in R/utils-tlg.R.

describe("filter_summary_excluded", {
  it("drops PKSUMXF == 'Y' rows (ADNCA summary exclusion)", {
    df <- data.frame(
      x = 1:5,
      PKSUMXF = c("", "Y", "", "Y", NA_character_),
      stringsAsFactors = FALSE
    )
    result <- filter_summary_excluded(df)
    expect_equal(result$x, c(1L, 3L, 5L))
  })

  it("drops PPSUMXF == 'Y' rows (ADPP summary exclusion)", {
    df <- data.frame(
      x = 1:4,
      PPSUMXF = c("", "Y", "", "Y"),
      stringsAsFactors = FALSE
    )
    result <- filter_summary_excluded(df)
    expect_equal(result$x, c(1L, 3L))
  })

  it("is a no-op when no exclusion-flag column is present", {
    df <- data.frame(x = 1:3)
    expect_equal(filter_summary_excluded(df)$x, 1:3)
  })

  it("applies both flags when both columns are present", {
    df <- data.frame(
      x = 1:4,
      PKSUMXF = c("Y", "",  "",  ""),
      PPSUMXF = c("",  "Y", "",  ""),
      stringsAsFactors = FALSE
    )
    # A record excluded by either dataset's flag is dropped from summaries.
    expect_equal(filter_summary_excluded(df)$x, c(3L, 4L))
  })

  it("preserves column label attributes across the row filter", {
    df <- data.frame(
      AVAL = c(1, 2, 3),
      PKSUMXF = c("", "Y", ""),
      stringsAsFactors = FALSE
    )
    attr(df$AVAL, "label") <- "Analysis Value"
    result <- filter_summary_excluded(df)
    expect_equal(attr(result$AVAL, "label"), "Analysis Value")
    expect_equal(nrow(result), 2)
  })
})

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
