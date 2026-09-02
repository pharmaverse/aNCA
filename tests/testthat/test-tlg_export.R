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
    expect_true(file.exists(file.path(d, "Graphs", "png", "pkcg01_lin", "pkcg01_lin.png")))
    expect_true(file.exists(file.path(d, "Tables", "csv", "pkct01", "pkct01.csv")))
    expect_true(file.exists(file.path(d, "Tables", "xlsx", "pkct01.xlsx")))
    expect_true(file.exists(file.path(d, "Listings", "csv", "pkcl01", "pkcl01.csv")))
  })

  it("separates csv and xlsx so neither folder listing is cluttered by the other", {
    d <- withr::local_tempdir()
    write_tlg_exports(list(t_pkct01 = entry("table", list(all = head(mtcars)))), d)
    expect_setequal(basename(list.dirs(file.path(d, "Tables"), recursive = FALSE)),
                    c("csv", "xlsx"))
  })

  it("defaults graphs to PNG only -- HTML is an order of magnitude larger per graph", {
    d <- withr::local_tempdir()
    write_tlg_exports(list(g_pkcg01_lin = entry("graph", list(all = stashed_plotly()))), d)
    expect_equal(basename(list.dirs(file.path(d, "Graphs"), recursive = FALSE)), "png")
    expect_length(list.files(file.path(d, "Graphs"), pattern = "\\.html$",
                             recursive = TRUE), 0)
  })

  it("gives a split graph its own folder so Graphs/ stays navigable", {
    # pkcg01 emits one plot per subject; a hundred-odd of them flat in Graphs/ is unusable.
    d <- withr::local_tempdir()
    items <- setNames(list(stashed_plotly(), stashed_plotly()), c("S1", "S2"))
    write_tlg_exports(list(g_pkcg01_lin = entry("graph", items)), d)
    expect_setequal(list.files(file.path(d, "Graphs", "png", "pkcg01_lin")),
                    c("S1.png", "S2.png"))
  })

  it("gives an unsplit graph a folder too, so the layout stays predictable", {
    # Loose files sitting beside directories at the top of Graphs/ read as untidy (#1344).
    d <- withr::local_tempdir()
    write_tlg_exports(list(g_pkcg02_lin = entry("graph", list(all = stashed_plotly()))), d)
    expect_equal(list.files(file.path(d, "Graphs", "png")), "pkcg02_lin")
    expect_equal(list.files(file.path(d, "Graphs", "png", "pkcg02_lin")), "pkcg02_lin.png")
  })

  it("keeps each graph format in its own directory rather than interleaving them", {
    d <- withr::local_tempdir()
    items <- setNames(list(stashed_plotly(), stashed_plotly()), c("S1", "S2"))
    write_tlg_exports(list(g_pkcg01_lin = entry("graph", items)), d,
                      ggplot_formats = c("png", "html"))
    expect_setequal(list.files(file.path(d, "Graphs")), c("png", "html"))
    expect_setequal(list.files(file.path(d, "Graphs", "png", "pkcg01_lin")),
                    c("S1.png", "S2.png"))
    expect_equal(list.files(file.path(d, "Graphs", "html")), "pkcg01_lin.html")
  })

  it("names split csv outputs after the split key, grouped by TLG", {
    d <- withr::local_tempdir()
    items <- setNames(list(head(mtcars), head(mtcars)), c("PARAM: DrugA", "PARAM: DrugB"))
    write_tlg_exports(list(t_pkct01 = entry("table", items)), d, table_formats = "csv")
    expect_setequal(
      list.files(file.path(d, "Tables", "csv", "pkct01")),
      c("PARAM_DrugA.csv", "PARAM_DrugB.csv")
    )
  })

  it("collapses a split table into one workbook with a sheet per split", {
    d <- withr::local_tempdir()
    items <- setNames(list(head(mtcars), head(iris)), c("PARAM: DrugA", "PARAM: DrugB"))
    m <- write_tlg_exports(list(t_pkct01 = entry("table", items)), d, table_formats = "xlsx")
    expect_equal(list.files(file.path(d, "Tables", "xlsx")), "pkct01.xlsx")
    expect_match(m$note[m$status == "ok"], "2 sheets")
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
      list.files(file.path(d, "Tables", "csv", "pkct01")),
      c("1.csv", "2.csv", "3.csv")
    )
  })

  it("does not number a single unnamed output", {
    d <- withr::local_tempdir()
    write_tlg_exports(list(t_pkct01 = entry("table", list(head(mtcars)))), d,
                      table_formats = "csv")
    expect_equal(list.files(file.path(d, "Tables", "csv", "pkct01")), "pkct01.csv")
  })

  it("does not let two split keys that slug alike overwrite each other", {
    d <- withr::local_tempdir()
    items <- setNames(list(head(mtcars), head(mtcars)), c("DrugA / SERUM", "DrugA - SERUM"))
    write_tlg_exports(list(t_pkct01 = entry("table", items)), d, table_formats = "csv")
    expect_length(list.files(file.path(d, "Tables", "csv", "pkct01")), 2)
  })

  it("records the written format in the manifest file column", {
    d <- withr::local_tempdir()
    items <- setNames(list(stashed_plotly()), "S1")
    m <- write_tlg_exports(list(g_pkcg01_lin = entry("graph", items)), d,
                           ggplot_formats = c("png", "html"))
    expect_setequal(
      m$file[m$status == "ok"],
      c("Graphs/png/pkcg01_lin/S1.png", "Graphs/html/pkcg01_lin.html")
    )
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
    expect_true(file.exists(file.path(d, "Tables", "csv", "pkct01", "pkct01.csv")))
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
    expect_true(file.exists(file.path(d, "Tables", "csv", "pkct01", "pkct01.csv")))
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
    expect_equal(man$file[man$file != ""][1], "Tables/csv/pkct01/pkct01.csv")
  })

  it("routes an unrecognised type to Other/ instead of erroring", {
    d <- withr::local_tempdir()
    expect_no_error(
      write_tlg_exports(list(x = entry("mystery", list(all = head(mtcars)))), d,
                        table_formats = "csv")
    )
    expect_true(file.exists(file.path(d, "Other", "csv", "x", "x.csv")))
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

describe("write_tlg_exports: PDF output", {
  # Read as bytes: readLines() on a PDF produces invalid multibyte strings and a wall of
  # encoding warnings.  "/Type /Pages" is the single page-tree node, so subtract it.
  pdf_pages <- function(path) {
    raw <- readBin(path, "raw", file.size(path))
    n_page  <- length(grepRaw("/Type /Page", raw, all = TRUE, fixed = TRUE))
    n_pages <- length(grepRaw("/Type /Pages", raw, all = TRUE, fixed = TRUE))
    n_page - n_pages
  }

  it("writes one multi-page document per TLG rather than a file per plot", {
    # Paging through pkcg01_lin.pdf beats opening twenty-odd separate files.
    d <- withr::local_tempdir()
    items <- setNames(list(stashed_plotly(), stashed_plotly(), stashed_plotly()),
                      c("S1", "S2", "S3"))
    m <- write_tlg_exports(list(g_pkcg01_lin = entry("graph", items)), d,
                           ggplot_formats = "pdf")
    expect_equal(list.files(file.path(d, "Graphs", "pdf")), "pkcg01_lin.pdf")
    expect_equal(pdf_pages(file.path(d, "Graphs", "pdf", "pkcg01_lin.pdf")), 3)
    expect_match(m$note[m$status == "ok"], "3 pages")
  })

  it("still writes the per-plot formats alongside it", {
    d <- withr::local_tempdir()
    items <- setNames(list(stashed_plotly(), stashed_plotly()), c("S1", "S2"))
    write_tlg_exports(list(g_pkcg01_lin = entry("graph", items)), d,
                      ggplot_formats = c("png", "pdf"))
    expect_length(list.files(file.path(d, "Graphs", "png", "pkcg01_lin")), 2)
    expect_equal(list.files(file.path(d, "Graphs", "pdf")), "pkcg01_lin.pdf")
  })

  it("reports a failed output once, not once per requested format", {
    d <- withr::local_tempdir()
    m <- write_tlg_exports(list(g_broken = entry("graph", list(all = "Error: nope"))), d,
                           ggplot_formats = c("png", "pdf"))
    expect_equal(nrow(m), 1)
    expect_equal(m$status, "skipped")
  })

  it("says so rather than emitting a blank page when nothing can be drawn", {
    # A plotly with no stashed ggplot cannot be rendered to a vector device.
    d <- withr::local_tempdir()
    bare <- plotly::ggplotly(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
                               ggplot2::geom_point())
    m <- write_tlg_exports(list(g_x = entry("graph", list(all = bare))), d,
                           ggplot_formats = "pdf")
    expect_equal(m$status, "skipped")
    expect_match(m$note, "none of the plots could be rendered")
  })
})

describe("write_tlg_exports: combined HTML output", {
  it("writes one document per TLG rather than a file per plot", {
    # Each widget written separately inlines its own copy of the plotly bundle, so a
    # twenty-plot output costs ~74 MB against ~3.7 MB combined (#1344).
    d <- withr::local_tempdir()
    items <- setNames(list(stashed_plotly(), stashed_plotly(), stashed_plotly()),
                      c("S1", "S2", "S3"))
    m <- write_tlg_exports(list(g_pkcg01_lin = entry("graph", items)), d,
                           ggplot_formats = "html")
    expect_equal(list.files(file.path(d, "Graphs", "html")), "pkcg01_lin.html")
    expect_match(m$note[m$status == "ok"], "3 plots")
  })

  it("heads each plot with its split key so a long document stays navigable", {
    d <- withr::local_tempdir()
    items <- setNames(list(stashed_plotly(), stashed_plotly()), c("S1", "S2"))
    write_tlg_exports(list(g_pkcg01_lin = entry("graph", items, label = "Lin conc")), d,
                      ggplot_formats = "html")
    html <- paste(
      readLines(file.path(d, "Graphs", "html", "pkcg01_lin.html"), warn = FALSE),
      collapse = "\n"
    )
    expect_match(html, "<h1>Lin conc</h1>", fixed = TRUE)
    expect_match(html, "<h2>S1</h2>", fixed = TRUE)
    expect_match(html, "<h2>S2</h2>", fixed = TRUE)
  })

  it("leaves no dependency folder beside the document", {
    # save_html() stages a lib/ directory; it belongs in tempdir(), not in the archive.
    skip_if_not(rmarkdown::pandoc_available(), "pandoc needed to self-contain the HTML")
    d <- withr::local_tempdir()
    write_tlg_exports(list(g_pkcg02_lin = entry("graph", list(all = stashed_plotly()))), d,
                      ggplot_formats = "html")
    expect_equal(list.files(file.path(d, "Graphs", "html")), "pkcg02_lin.html")
  })

  it("converts a plain ggplot, which is what the pkpg builders return", {
    d <- withr::local_tempdir()
    gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
    m <- write_tlg_exports(list(p_pkpg03_boxp = entry("graph", list(all = gg))), d,
                           ggplot_formats = "html")
    expect_equal(m$status, "ok")
    expect_true(file.exists(file.path(d, "Graphs", "html", "pkpg03_boxp.html")))
  })

  it("still writes the per-plot formats alongside it", {
    d <- withr::local_tempdir()
    items <- setNames(list(stashed_plotly(), stashed_plotly()), c("S1", "S2"))
    write_tlg_exports(list(g_pkcg01_lin = entry("graph", items)), d,
                      ggplot_formats = c("png", "html"))
    expect_length(list.files(file.path(d, "Graphs", "png", "pkcg01_lin")), 2)
    expect_equal(list.files(file.path(d, "Graphs", "html")), "pkcg01_lin.html")
  })

  it("reports a failed output once, not once per requested format", {
    d <- withr::local_tempdir()
    m <- write_tlg_exports(list(g_broken = entry("graph", list(all = "Error: nope"))), d,
                           ggplot_formats = c("png", "html"))
    expect_equal(nrow(m), 1)
    expect_equal(m$status, "skipped")
  })

  it("skips the document entirely when every plot failed", {
    d <- withr::local_tempdir()
    items <- setNames(list("Error: a", "Error: b"), c("S1", "S2"))
    m <- write_tlg_exports(list(g_broken = entry("graph", items)), d,
                           ggplot_formats = "html")
    expect_equal(unique(m$status), "skipped")
    expect_false(dir.exists(file.path(d, "Graphs")))
  })
})

describe("write_tlg_exports: table and listing PDF output", {
  it("writes one document per TLG with the splits as sections", {
    d <- withr::local_tempdir()
    items <- setNames(list(head(mtcars), head(mtcars)), c("PARAM: DrugA", "PARAM: DrugB"))
    m <- write_tlg_exports(list(t_pkct01 = entry("table", items)), d,
                           table_formats = "pdf")
    expect_equal(list.files(file.path(d, "Tables", "pdf")), "pkct01.pdf")
    expect_equal(m$status, "ok")
    expect_match(m$note, "pages?$")
  })

  it("puts a listing under Listings/pdf rather than beside the tables", {
    d <- withr::local_tempdir()
    write_tlg_exports(list(l_pkcl01 = entry("listing", list(all = head(mtcars)))), d,
                      table_formats = "pdf")
    expect_equal(list.files(file.path(d, "Listings", "pdf")), "pkcl01.pdf")
  })

  it("keeps pdf out of the csv and xlsx folders", {
    d <- withr::local_tempdir()
    write_tlg_exports(list(t_pkct01 = entry("table", list(all = head(mtcars)))), d,
                      table_formats = c("csv", "xlsx", "pdf"))
    expect_setequal(basename(list.dirs(file.path(d, "Tables"), recursive = FALSE)),
                    c("csv", "xlsx", "pdf"))
  })

  it("is not written unless it is asked for", {
    d <- withr::local_tempdir()
    write_tlg_exports(list(t_pkct01 = entry("table", list(all = head(mtcars)))), d,
                      table_formats = "xlsx")
    expect_false(dir.exists(file.path(d, "Tables", "pdf")))
  })

  it("records a write failure as an error and keeps the other formats", {
    d <- withr::local_tempdir()
    m <- with_mocked_bindings(
      write_tlg_exports(list(t_pkct01 = entry("table", list(all = head(mtcars)))), d,
                        table_formats = c("csv", "pdf")),
      export_as_pdf = function(...) stop("device is busy"),
      .package = "formatters"
    )
    pdf_row <- m[grepl("pdf", m$file), ]
    expect_equal(pdf_row$status, "error")
    expect_match(pdf_row$note, "device is busy")
    expect_true(file.exists(file.path(d, "Tables", "csv", "pkct01", "pkct01.csv")))
  })

  it("paginates a wide table by column instead of clipping it at the margin", {
    # A summary table with a column per statistic per group runs well past one page width;
    # formatters splits it, which is the reason for using it over a plain table grob.
    d <- withr::local_tempdir()
    wide <- as.data.frame(matrix(1, nrow = 3, ncol = 40))
    names(wide) <- paste0("LongStatisticName_", seq_len(40))
    m <- write_tlg_exports(list(t_pkct01 = entry("table", list(all = wide))), d,
                           table_formats = "pdf")
    expect_equal(m$status, "ok")
    expect_false(grepl("exceeds the page", m$note))
    expect_gt(as.integer(sub(" .*", "", m$note)), 1)
  })
})

describe(".tlg_pdf_captions", {
  it("uses the split key verbatim, unlike the Excel-safe sheet names", {
    expect_equal(
      .tlg_pdf_captions(c("PARAM: DrugA / PCSPEC: SERUM", "PARAM: DrugB"),
                        c("a", "b"), "pkct01"),
      c("PARAM: DrugA / PCSPEC: SERUM", "PARAM: DrugB")
    )
  })

  it("leaves an unsplit output uncaptioned rather than repeating the title", {
    expect_equal(.tlg_pdf_captions("all", "pkct01", "pkct01"), "")
    expect_equal(.tlg_pdf_captions(NULL, "pkct01", "pkct01"), "")
  })

  it("falls back to the base name when a split key is missing", {
    expect_equal(.tlg_pdf_captions(c("", NA), c("pkct01_1", "pkct01_2"), "pkct01"),
                 c("pkct01_1", "pkct01_2"))
  })
})
