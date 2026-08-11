# Tests for the bulk TLG export helpers (issue #1344).
local({
  library(shiny)
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(file.path(shiny_dir, "functions", "zip-utils.R"), local = TRUE)
  source(file.path(shiny_dir, "functions", "tlg_export.R"), local = TRUE)
},
envir = parent.env(environment()))

# A plotly object carrying its source ggplot, the way the g_pkcg* functions return them.
stashed_plotly <- function() {
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  aNCA:::.with_ggplot(plotly::ggplotly(gg), gg)
}

entry <- function(type, items, label = "lbl", dataset = "ADNCA") {
  list(type = type, items = items,
       def = list(label = label, type = tools::toTitleCase(type), dataset = dataset))
}

describe(".tlg_export_slug", {
  it("collapses runs of non-alphanumeric characters to single underscores", {
    expect_equal(.tlg_export_slug("PARAM: DrugA / PCSPEC: SERUM"), "PARAM_DrugA_PCSPEC_SERUM")
  })

  it("trims leading and trailing underscores", {
    expect_equal(.tlg_export_slug(" / DrugA / "), "DrugA")
  })

  it("returns an empty string for empty or missing input", {
    expect_equal(.tlg_export_slug(NA), "")
    expect_equal(.tlg_export_slug(""), "")
    expect_equal(.tlg_export_slug(character()), "")
  })

  it("caps the length so paths stay portable", {
    expect_lte(nchar(.tlg_export_slug(strrep("a", 200))), 60)
  })
})

describe(".tlg_export_basename", {
  it("drops the catalog type prefix so the name starts with the pkid", {
    expect_equal(.tlg_export_basename("g_pkcg01_lin", NULL), "pkcg01_lin")
    expect_equal(.tlg_export_basename("t_pkct01_dose", NULL), "pkct01_dose")
    expect_equal(.tlg_export_basename("l_pkcl02_uri", NULL), "pkcl02_uri")
  })

  it("appends the slugged split key", {
    expect_equal(.tlg_export_basename("g_pkcg01_lin", "DrugA"), "pkcg01_lin_DrugA")
  })

  it("treats the split_and_apply 'all' sentinel as no split", {
    expect_equal(.tlg_export_basename("t_pkct01", "all"), "pkct01")
  })

  it("omits the suffix when the split key slugs down to nothing", {
    # A key of only separators leaves no usable text; appending a bare "_" would be worse
    # than no suffix at all.
    expect_equal(.tlg_export_basename("t_pkct01", " / / "), "pkct01")
    expect_equal(.tlg_export_basename("t_pkct01", ""), "pkct01")
  })
})

describe(".prepare_export_frame", {
  it("flattens the two-level 'Compare in columns' header into readable names", {
    # Grouped summary tables prefix each statistic with "<level><.GROUP_SEP>"; a flat file
    # has one header row, so the raw name would carry a \037 control character.
    sep <- aNCA:::.GROUP_SEP
    df <- data.frame(1, 2, 3)
    names(df) <- c("TRT01A", paste0("F", sep, "Mean"), paste0("M", sep, "Mean"))
    out <- .prepare_export_frame(df)
    expect_equal(names(out), c("TRT01A", "F - Mean", "M - Mean"))
    expect_false(any(grepl(sep, names(out), fixed = TRUE)))
  })

  it("prefers the column's display label over the terse statistic name", {
    sep <- aNCA:::.GROUP_SEP
    df <- data.frame(a = 1)
    names(df) <- paste0("F", sep, "GeoMean")
    attr(df[[1]], "label") <- "Geometric Mean"
    expect_equal(names(.prepare_export_frame(df)), "F - Geometric Mean")
  })

  it("leaves an ordinary table untouched", {
    df <- data.frame(TRT01A = "A", Mean = 1)
    expect_equal(names(.prepare_export_frame(df)), c("TRT01A", "Mean"))
  })

  it("reduces a listing to the columns actually displayed", {
    skip_if_not_installed("rlistings")
    # listing_df keeps every source column (working ones like id_list included) but only
    # displays listing_dispcols(); the export should match the screen.
    raw <- data.frame(USUBJID = c("1", "2"), AVAL = c(1, 2), id_list = c("x", "x"))
    lst <- rlistings::as_listing(raw, key_cols = "USUBJID", disp_cols = c("USUBJID", "AVAL"))
    out <- .prepare_export_frame(lst)
    expect_equal(names(out), c("USUBJID", "AVAL"))
    expect_false("id_list" %in% names(out))
  })
})

describe("write_tlg_exports", {
  it("writes each type into its own folder", {
    d <- withr::local_tempdir()
    write_tlg_exports(
      list(
        g_pkcg01_lin = entry("graph", list(all = stashed_plotly())),
        t_pkct01     = entry("table", list(all = head(mtcars))),
        l_pkcl01     = entry("listing", list(all = head(iris)))
      ),
      d
    )
    expect_true(file.exists(file.path(d, "Graphs", "pkcg01_lin.png")))
    expect_true(file.exists(file.path(d, "Tables", "pkct01.csv")))
    expect_true(file.exists(file.path(d, "Tables", "pkct01.xlsx")))
    expect_true(file.exists(file.path(d, "Listings", "pkcl01.csv")))
  })

  it("defaults graphs to PNG only -- HTML would add a _files/ dependency tree per graph", {
    d <- withr::local_tempdir()
    write_tlg_exports(list(g_pkcg01_lin = entry("graph", list(all = stashed_plotly()))), d)
    expect_false(file.exists(file.path(d, "Graphs", "pkcg01_lin.html")))
    expect_length(list.dirs(file.path(d, "Graphs"), recursive = FALSE), 0)
  })

  it("names split outputs after the split key", {
    d <- withr::local_tempdir()
    items <- setNames(list(head(mtcars), head(mtcars)), c("PARAM: DrugA", "PARAM: DrugB"))
    write_tlg_exports(list(t_pkct01 = entry("table", items)), d, table_formats = "csv")
    expect_setequal(
      list.files(file.path(d, "Tables")),
      c("pkct01_PARAM_DrugA.csv", "pkct01_PARAM_DrugB.csv")
    )
  })

  it("numbers every file when the builder returns an unnamed list", {
    # pkcg01 returns one plot per subject with no names; leaving the first bare and
    # suffixing the rest would read as if the bare file were a combined output.
    d <- withr::local_tempdir()
    write_tlg_exports(
      list(t_pkct01 = entry("table", list(head(mtcars), head(mtcars), head(mtcars)))),
      d, table_formats = "csv"
    )
    expect_setequal(
      list.files(file.path(d, "Tables")),
      c("pkct01_1.csv", "pkct01_2.csv", "pkct01_3.csv")
    )
  })

  it("does not number a single unnamed output", {
    d <- withr::local_tempdir()
    write_tlg_exports(list(t_pkct01 = entry("table", list(head(mtcars)))), d,
                      table_formats = "csv")
    expect_equal(list.files(file.path(d, "Tables")), "pkct01.csv")
  })

  it("does not let two split keys that slug alike overwrite each other", {
    d <- withr::local_tempdir()
    items <- setNames(list(head(mtcars), head(mtcars)), c("DrugA / SERUM", "DrugA - SERUM"))
    write_tlg_exports(list(t_pkct01 = entry("table", items)), d, table_formats = "csv")
    expect_length(list.files(file.path(d, "Tables")), 2)
  })

  it("skips a failed TLG and records it rather than aborting the export", {
    d <- withr::local_tempdir()
    m <- write_tlg_exports(
      list(
        t_broken = entry("table", list("Error: something went wrong")),
        t_pkct01 = entry("table", list(all = head(mtcars)))
      ),
      d, table_formats = "csv"
    )
    expect_equal(m$status[m$id == "t_broken"], "skipped")
    expect_match(m$note[m$id == "t_broken"], "something went wrong")
    # The healthy TLG in the same order still made it out.
    expect_true(file.exists(file.path(d, "Tables", "pkct01.csv")))
  })

  it("records a write failure as an error and keeps going", {
    # save_dispatch() rejects anything that is not a ggplot / data.frame / plotly.  That
    # must not take the download down with it.
    d <- withr::local_tempdir()
    m <- write_tlg_exports(
      list(
        t_odd    = entry("table", list(all = function() NULL)),
        t_pkct01 = entry("table", list(all = head(mtcars)))
      ),
      d, table_formats = "csv"
    )
    expect_equal(m$status[m$id == "t_odd"], "error")
    expect_match(m$note[m$id == "t_odd"], "Unsupported output type")
    expect_true(file.exists(file.path(d, "Tables", "pkct01.csv")))
  })

  it("records a TLG that produced nothing as empty", {
    d <- withr::local_tempdir()
    m <- write_tlg_exports(list(t_empty = entry("table", list())), d)
    expect_equal(m$status, "empty")
  })

  it("writes a manifest describing every output", {
    d <- withr::local_tempdir()
    write_tlg_exports(
      list(t_pkct01 = entry("table", list(all = head(mtcars)), label = "pkct01 - Summary")),
      d, table_formats = "csv"
    )
    man <- read.csv(file.path(d, "manifest.csv"))
    expect_named(man, c("id", "label", "type", "dataset", "file", "status", "note"))
    expect_equal(man$label, "pkct01 - Summary")
    expect_equal(man$file, "Tables/pkct01")
  })

  it("routes an unrecognised type to Other/ instead of erroring", {
    d <- withr::local_tempdir()
    expect_no_error(
      write_tlg_exports(list(x = entry("mystery", list(all = head(mtcars)))), d,
                        table_formats = "csv")
    )
    expect_true(file.exists(file.path(d, "Other", "x.csv")))
  })

  it("returns an empty manifest when there is nothing to export", {
    d <- withr::local_tempdir()
    m <- write_tlg_exports(list(), d)
    expect_equal(nrow(m), 0)
    expect_true(file.exists(file.path(d, "manifest.csv")))
  })
})

describe("write_tlg_exports: tidiness", {
  it("does not leave an empty folder behind for a type whose outputs all failed", {
    d <- withr::local_tempdir()
    write_tlg_exports(
      list(
        l_broken = entry("listing", list("Error: no urine data")),
        t_pkct01 = entry("table", list(all = head(mtcars)))
      ),
      d, table_formats = "csv"
    )
    expect_false(dir.exists(file.path(d, "Listings")))
    expect_true(dir.exists(file.path(d, "Tables")))
  })
})
