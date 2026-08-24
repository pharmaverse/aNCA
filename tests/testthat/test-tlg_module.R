# Source the TLG module to test pure utility functions
local({
  library(shiny)
  # The module's error handler calls log_error(); the app attaches logger in app.R.
  library(logger)
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(
    file.path(shiny_dir, "modules", "tab_tlg", "tlg_module.R"),
    local = TRUE
  )
  source(
    file.path(shiny_dir, "modules", "tab_tlg", "tlg_option_numeric.R"),
    local = TRUE
  )
  source(
    file.path(shiny_dir, "modules", "tab_tlg", "tlg_option_select.R"),
    local = TRUE
  )
},
envir = parent.env(environment()))

describe("filter_tlg_excluded", {
  it("removes rows where the named flag (PKSUMXF) is 'Y'", {
    df <- data.frame(
      x = 1:5,
      PKSUMXF = c("", "Y", "", "Y", ""),
      stringsAsFactors = FALSE
    )
    result <- filter_tlg_excluded(df, "PKSUMXF")
    expect_equal(nrow(result), 3)
    expect_equal(result$x, c(1, 3, 5))
  })

  it("returns all rows when the named flag column is absent", {
    df <- data.frame(x = 1:3)
    result <- filter_tlg_excluded(df, "PKSUMXF")
    expect_equal(nrow(result), 3)
    expect_equal(result$x, 1:3)
  })

  it("returns all rows when the named flag is all empty", {
    df <- data.frame(
      x = 1:3,
      PKSUMXF = rep("", 3),
      stringsAsFactors = FALSE
    )
    result <- filter_tlg_excluded(df, "PKSUMXF")
    expect_equal(nrow(result), 3)
  })

  it("returns empty data frame when all rows are excluded", {
    df <- data.frame(
      x = 1:2,
      PKSUMXF = c("Y", "Y"),
      stringsAsFactors = FALSE
    )
    result <- filter_tlg_excluded(df, "PKSUMXF")
    expect_equal(nrow(result), 0)
  })

  it("removes rows where the named flag (PPSUMXF) is 'Y' (ADPP exclusion flag)", {
    df <- data.frame(
      x       = 1:4,
      PPSUMXF = c("", "Y", "", "Y"),
      stringsAsFactors = FALSE
    )
    result <- filter_tlg_excluded(df, "PPSUMXF")
    expect_equal(nrow(result), 2)
    expect_equal(result$x, c(1L, 3L))
  })

  it("applies only the named flag and ignores the other dataset's flag", {
    # A record excluded from the ADPP summary (PPSUMXF == "Y") but not the ADNCA
    # summary must still survive ADNCA (PKSUMXF) filtering, and vice-versa.
    df <- data.frame(
      x       = 1:4,
      PKSUMXF = c("Y", "",  "",  ""),
      PPSUMXF = c("",  "Y", "",  ""),
      stringsAsFactors = FALSE
    )
    # Filtering as ADNCA drops only the PKSUMXF == "Y" row; the PPSUMXF row stays.
    adnca <- filter_tlg_excluded(df, "PKSUMXF")
    expect_equal(adnca$x, c(2L, 3L, 4L))
    # Filtering as ADPP drops only the PPSUMXF == "Y" row; the PKSUMXF row stays.
    adpp <- filter_tlg_excluded(df, "PPSUMXF")
    expect_equal(adpp$x, c(1L, 3L, 4L))
  })
})

# ---------------------------------------------------------------------------
# tlg_data_key
# ---------------------------------------------------------------------------

describe("tlg_data_key", {
  it("routes listings to the unfiltered '<dataset>_all' source", {
    expect_equal(tlg_data_key("listing", "ADNCA"), "ADNCA_all")
    expect_equal(tlg_data_key("listing", "ADPP"), "ADPP_all")
  })

  it("routes tables and graphs to the summary-filtered source", {
    expect_equal(tlg_data_key("table", "ADNCA"), "ADNCA")
    expect_equal(tlg_data_key("graph", "ADNCA"), "ADNCA")
    expect_equal(tlg_data_key("table", "ADPP"), "ADPP")
    expect_equal(tlg_data_key("graph", "ADPP"), "ADPP")
  })
})

# ---------------------------------------------------------------------------
# .tlg_module_edit_widget
# ---------------------------------------------------------------------------

describe(".tlg_module_edit_widget", {
  it("returns an h1 group-label tag when opt_id contains '.group_label'", {
    result <- .tlg_module_edit_widget(
      "section.group_label", "My Section", data = NULL
    )
    html <- as.character(result)
    expect_true(grepl("tlg-group-label", html))
    expect_true(grepl("My Section",      html))
  })

  it("dispatches to the numeric UI widget for type 'numeric'", {
    opt_def <- list(type = "numeric", label = "A Number", default = 1)
    result  <- .tlg_module_edit_widget("mod-myopt", opt_def, data = NULL)
    html    <- as.character(result)
    # tlg_option_numeric_ui returns a numericInput
    expect_true(grepl("number", html, ignore.case = TRUE))
  })

  it("dispatches to the select UI widget for type 'select'", {
    opt_def <- list(
      type     = "select",
      label    = "A Choice",
      choices  = c("X", "Y"),
      default  = NULL,
      multiple = FALSE
    )
    result <- .tlg_module_edit_widget("mod-myopt", opt_def, data = NULL)
    html   <- as.character(result)
    # tlg_option_select_ui returns a selectInput
    expect_true(grepl("X", html))
    expect_true(grepl("Y", html))
  })
})

# ---------------------------------------------------------------------------
# tlg_module_server
# ---------------------------------------------------------------------------

describe("tlg_module_server", {
  test_data <- shiny::reactive(
    list(conc = list(data = data.frame(
      NFRLT = 1:3, AVAL = c(5, 4, 3), stringsAsFactors = FALSE
    )))
  )
  render_list_ok  <- function(data, ...) list("plot_a", "plot_b", "plot_c")

  it("skips character-valued options (group label markers)", {
    # options[[opt]] is a plain string → is.character() branch returns NULL,
    # so it is excluded from options_values (line 206 in tlg_module.R).
    # The resulting reactiveValues object should have no entries.
    expect_no_error(
      shiny::testServer(
        tlg_module_server,
        args = list(
          data        = test_data,
          type        = "graph",
          render_list = render_list_ok,
          options     = list(section_title = "My Section")
        ),
        {
          expect_equal(length(reactiveValuesToList(options_values)), 0)
        }
      )
    )
  })

  it("page navigation: next_page increments current_page", {
    shiny::testServer(
      tlg_module_server,
      args = list(
        data        = test_data,
        type        = "graph",
        render_list = render_list_ok,
        options     = list()
      ),
      {
        session$setInputs(next_page = 1)
        session$flushReact()
        expect_equal(current_page(), 2)
      }
    )
  })

  it("page navigation: previous_page decrements current_page", {
    shiny::testServer(
      tlg_module_server,
      args = list(
        data        = test_data,
        type        = "graph",
        render_list = render_list_ok,
        options     = list()
      ),
      {
        session$setInputs(next_page = 1)
        session$flushReact()
        session$setInputs(previous_page = 1)
        session$flushReact()
        expect_equal(current_page(), 1)
      }
    )
  })

  it("select_page returns NULL early when value is empty string", {
    shiny::testServer(
      tlg_module_server,
      args = list(
        data        = test_data,
        type        = "graph",
        render_list = render_list_ok,
        options     = list()
      ),
      {
        # Setting select_page to "" should hit the early-return guard
        # and leave current_page unchanged at its initial value of 1
        session$setInputs(select_page = "")
        session$flushReact()
        expect_equal(current_page(), 1)
      }
    )
  })

  it("still renders when a multi-select option is left empty (no default)", {
    # Regression: a `multiple` selectInput with nothing selected reports NULL.
    # tlg_module_server's is-null guard would then return NULL and blank the
    # whole table.  With the option server coercing NULL -> "", the empty option
    # is simply dropped and the table renders with the function default.
    adnca <- shiny::reactive(data.frame(
      TRT01A = rep(c("A", "B"), each = 4),
      ATPTREF = "D1",
      NFRLT  = rep(c(1, 2), 4),
      AVAL   = as.numeric(1:8),
      PARAM  = "Drug",
      PCSPEC = "PLASMA",
      stringsAsFactors = FALSE
    ))
    shiny::testServer(
      tlg_module_server,
      args = list(
        data        = adnca,
        type        = "table",
        render_list = t_pkct01,
        options     = list(stats = list(
          type = "select", choices = ".stats", multiple = TRUE
        ))
      ),
      {
        # `stats-select` deliberately left unset -> NULL (empty multi-select).
        session$setInputs(entries_per_page = "All")
        session$elapse(800)  # clear the debounce(750)
        res <- tlg_list()
        expect_false(is.null(res))
        expect_gte(length(res), 1)
        expect_s3_class(res[[1]], "data.frame")
      }
    )
  })

  # Note: the tryCatch error-handler path inside tlg_list (lines 181-190 of
  # tlg_module.R) is not unit-tested here.  The handler calls log_error()
  # which requires a running Shiny session; the debounce(750) reactive also
  # caches and re-throws the error before the tryCatch return value can be
  # observed.  This path is covered by end-to-end / integration tests.
})

# ---------------------------------------------------------------------------
# .tlg_module_edit_widget
# ---------------------------------------------------------------------------

describe(".tlg_module_edit_widget", {
  it("returns an h1 group-label tag when opt_id contains '.group_label'", {
    result <- .tlg_module_edit_widget(
      "section.group_label", "My Section", data = NULL
    )
    html <- as.character(result)
    expect_true(grepl("tlg-group-label", html))
    expect_true(grepl("My Section",      html))
  })

  it("dispatches to the numeric UI widget for type 'numeric'", {
    opt_def <- list(type = "numeric", label = "A Number", default = 1)
    result  <- .tlg_module_edit_widget("mod-myopt", opt_def, data = NULL)
    html    <- as.character(result)
    # tlg_option_numeric_ui returns a numericInput
    expect_true(grepl("number", html, ignore.case = TRUE))
  })

  it("dispatches to the select UI widget for type 'select'", {
    opt_def <- list(
      type     = "select",
      label    = "A Choice",
      choices  = c("X", "Y"),
      default  = NULL,
      multiple = FALSE
    )
    result <- .tlg_module_edit_widget("mod-myopt", opt_def, data = NULL)
    html   <- as.character(result)
    # tlg_option_select_ui returns a selectInput
    expect_true(grepl("X", html))
    expect_true(grepl("Y", html))
  })
})

# ---------------------------------------------------------------------------
# tlg_module_server
# ---------------------------------------------------------------------------

describe("tlg_module_server", {
  test_data <- shiny::reactive(
    list(conc = list(data = data.frame(
      NFRLT = 1:3, AVAL = c(5, 4, 3), stringsAsFactors = FALSE
    )))
  )
  render_list_ok  <- function(data, ...) list("plot_a", "plot_b", "plot_c")

  it("skips character-valued options (group label markers)", {
    # options[[opt]] is a plain string → is.character() branch returns NULL,
    # so it is excluded from options_values (line 206 in tlg_module.R).
    # The resulting reactiveValues object should have no entries.
    expect_no_error(
      shiny::testServer(
        tlg_module_server,
        args = list(
          data        = test_data,
          type        = "graph",
          render_list = render_list_ok,
          options     = list(section_title = "My Section")
        ),
        {
          expect_equal(length(reactiveValuesToList(options_values)), 0)
        }
      )
    )
  })

  it("page navigation: next_page increments current_page", {
    shiny::testServer(
      tlg_module_server,
      args = list(
        data        = test_data,
        type        = "graph",
        render_list = render_list_ok,
        options     = list()
      ),
      {
        session$setInputs(next_page = 1)
        session$flushReact()
        expect_equal(current_page(), 2)
      }
    )
  })

  it("page navigation: previous_page decrements current_page", {
    shiny::testServer(
      tlg_module_server,
      args = list(
        data        = test_data,
        type        = "graph",
        render_list = render_list_ok,
        options     = list()
      ),
      {
        session$setInputs(next_page = 1)
        session$flushReact()
        session$setInputs(previous_page = 1)
        session$flushReact()
        expect_equal(current_page(), 1)
      }
    )
  })

  it("select_page returns NULL early when value is empty string", {
    shiny::testServer(
      tlg_module_server,
      args = list(
        data        = test_data,
        type        = "graph",
        render_list = render_list_ok,
        options     = list()
      ),
      {
        # Setting select_page to "" should hit the early-return guard
        # and leave current_page unchanged at its initial value of 1
        session$setInputs(select_page = "")
        session$flushReact()
        expect_equal(current_page(), 1)
      }
    )
  })

  # Note: the tryCatch error-handler path inside tlg_list (lines 181-190 of
  # tlg_module.R) is not unit-tested here.  The handler calls log_error()
  # which requires a running Shiny session; the debounce(750) reactive also
  # caches and re-throws the error before the tryCatch return value can be
  # observed.  This path is covered by end-to-end / integration tests.

  # Column-label restoration for the exclamation-mark label-reference syntax
  # (issue 1336) now happens at the tab_tlg boundary, where labels are applied to
  # each dataset before it reaches this module -- the module receives an
  # already-labelled, exclusion-filtered plain data frame.  That behaviour is
  # covered by the tab_tlg boundary tests.

  it("current_page_items() returns the slice for the current page", {
    render_5 <- function(data, ...) as.list(paste0("p", 1:5))
    shiny::testServer(
      tlg_module_server,
      args = list(
        data        = test_data,
        type        = "graph",
        render_list = render_5,
        options     = list()
      ),
      {
        session$setInputs(entries_per_page = 2)
        session$elapse(800)
        session$flushReact()
        expect_equal(current_page_items(), list("p1", "p2"))

        session$setInputs(next_page = 1)
        session$elapse(800)
        session$flushReact()
        expect_equal(current_page_items(), list("p3", "p4"))
      }
    )
  })
})

# Graph panels are bound by output ID, and `render_graph_outputs()` registers those bindings as
# `plot_<index>`.  The UI side must use the same index: several graph builders (g_pkcg03, the
# p_pkpg* family) return NAMED lists, and deriving the ID from the name produced IDs that never
# matched a binding — rendering a blank panel — or, once a split key contained a colon, broke
# Shiny's client-data handler and took the session down.

describe("render_graph_outputs: output IDs", {
  # Minimal wrapper so the real render_graph_outputs() is exercised.
  graph_mod <- function(id, items) {
    shiny::moduleServer(id, function(input, output, session) {
      render_graph_outputs(output, session, shiny::reactive(items))
    })
  }

  rendered_ids <- function(items) {
    ids <- NULL
    shiny::testServer(graph_mod, args = list(items = items), {
      html <- as.character(output$tlg_output$html)
      ids <<- unlist(regmatches(html, gregexpr('plot_[^"]+', html)))
    })
    ids
  }

  fake_plot <- function() list(height = 400)

  it("uses sequential indices for an unnamed plot list", {
    expect_equal(rendered_ids(list(fake_plot(), fake_plot())), c("plot_1", "plot_2"))
  })

  it("uses sequential indices for a NAMED plot list, not the names", {
    items <- setNames(list(fake_plot(), fake_plot()), c("PPCAT: DrugA", "PPCAT: DrugB"))
    ids <- rendered_ids(items)
    expect_equal(ids, c("plot_1", "plot_2"))
  })

  it("never emits an output ID containing a character invalid in a Shiny id", {
    items <- setNames(list(fake_plot()), "ROUTE: IV / PARAM: DrugA")
    expect_false(any(grepl("[: /]", rendered_ids(items))))
  })

  it("shows the split key as a group header when the list is named", {
    items <- setNames(list(fake_plot()), "PPCAT: DrugA")
    shiny::testServer(graph_mod, args = list(items = items), {
      html <- as.character(output$tlg_output$html)
      expect_true(grepl("PPCAT: DrugA", html, fixed = TRUE))
    })
  })
})

# req()/validate() raise conditions that inherit from `error`.  The render tryCatch must let
# those propagate so Shiny gates the output, rather than painting "Error: ADPP data is not
# available..." (or a bare "Error:") into the panel before NCA has run.

describe("tlg_module_server: Shiny control-flow conditions", {
  base_args <- function(render_list) {
    list(type = "graph", data = shiny::reactive(data.frame(AVAL = 1)),
         render_list = render_list, options = list(),
         grouping_vars = shiny::reactive(character()))
  }

  it("lets req() propagate instead of rendering it as an error string", {
    shiny::testServer(tlg_module_server, args = base_args(function(data, ...) shiny::req(FALSE)), {
      expect_error(tlg_list())
    })
  })

  it("still renders a genuine error as text", {
    args <- base_args(function(data, ...) stop("a genuine failure"))
    shiny::testServer(tlg_module_server, args = args, {
      expect_match(tlg_list(), "^Error: a genuine failure")
    })
  })
})
