# Source the TLG module to test pure utility functions
local({
  library(shiny)
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
  it("removes rows where PKSUM1F is 'Y'", {
    df <- data.frame(
      x = 1:5,
      PKSUM1F = c("", "Y", "", "Y", ""),
      stringsAsFactors = FALSE
    )
    result <- filter_tlg_excluded(df)
    expect_equal(nrow(result), 3)
    expect_equal(result$x, c(1, 3, 5))
  })

  it("returns all rows when PKSUM1F is absent", {
    df <- data.frame(x = 1:3)
    result <- filter_tlg_excluded(df)
    expect_equal(nrow(result), 3)
    expect_equal(result$x, 1:3)
  })

  it("returns all rows when PKSUM1F is all empty", {
    df <- data.frame(
      x = 1:3,
      PKSUM1F = rep("", 3),
      stringsAsFactors = FALSE
    )
    result <- filter_tlg_excluded(df)
    expect_equal(nrow(result), 3)
  })

  it("returns empty data frame when all rows are excluded", {
    df <- data.frame(
      x = 1:2,
      PKSUM1F = c("Y", "Y"),
      stringsAsFactors = FALSE
    )
    result <- filter_tlg_excluded(df)
    expect_equal(nrow(result), 0)
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

  # Note: the tryCatch error-handler path inside tlg_list (lines 181-190 of
  # tlg_module.R) is not unit-tested here.  The handler calls log_error()
  # which requires a running Shiny session; the debounce(750) reactive also
  # caches and re-throws the error before the tryCatch return value can be
  # observed.  This path is covered by end-to-end / integration tests.
})
