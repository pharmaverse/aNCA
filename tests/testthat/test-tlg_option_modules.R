# Tests for TLG option Shiny modules: numeric, text, select
# Uses shiny::testServer() for server-side reactive logic.

# Source all required module files
local({
  library(shiny)
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(
    file.path(shiny_dir, "modules", "tab_tlg", "tlg_option_numeric.R"),
    local = TRUE
  )
  source(
    file.path(shiny_dir, "modules", "tab_tlg", "tlg_option_text.R"),
    local = TRUE
  )
  source(
    file.path(shiny_dir, "modules", "tab_tlg", "tlg_option_select.R"),
    local = TRUE
  )
},
envir = parent.env(environment()))

# ---------------------------------------------------------------------------
# tlg_option_numeric
# ---------------------------------------------------------------------------

describe("tlg_option_numeric_ui", {
  it("returns a numericInput tag with the given default value", {
    opt_def <- list(label = "My Num", default = 42)
    ui   <- tlg_option_numeric_ui("test-numeric", opt_def, data = NULL)
    html <- as.character(ui)
    expect_true(grepl("numeric", html, ignore.case = TRUE))
    expect_true(grepl("42",      html))
  })

  it("falls back to extracting label from id when label is NULL", {
    opt_def <- list(label = NULL, default = 0)
    ui   <- tlg_option_numeric_ui("module-myvar", opt_def, data = NULL)
    html <- as.character(ui)
    expect_true(grepl("myvar", html))
  })
})

describe("tlg_option_numeric_server", {
  it("returns the current numeric input value as a reactive", {
    opt_def       <- list(label = "Num", default = 5)
    reset_trigger <- shiny::reactive(0)

    shiny::testServer(
      tlg_option_numeric_server,
      args = list(
        opt_def       = opt_def,
        data          = shiny::reactive(NULL),
        reset_trigger = reset_trigger
      ),
      {
        session$setInputs(numeric = 10)
        expect_equal(session$getReturned()(), 10)
      }
    )
  })

  it("resets without error when reset_trigger fires", {
    opt_def       <- list(label = "Num", default = 7)
    trigger_count <- shiny::reactiveVal(0)
    reset_trigger <- shiny::reactive(trigger_count())

    expect_no_error(
      shiny::testServer(
        tlg_option_numeric_server,
        args = list(
          opt_def       = opt_def,
          data          = shiny::reactive(NULL),
          reset_trigger = reset_trigger
        ),
        {
          session$setInputs(numeric = 99)
          trigger_count(1)
          session$flushReact()
        }
      )
    )
  })
})

# ---------------------------------------------------------------------------
# tlg_option_text
# ---------------------------------------------------------------------------

describe("tlg_option_text_ui", {
  it("returns a textInput tag with the given default value", {
    opt_def <- list(label = "My Text", default = "hello")
    ui   <- tlg_option_text_ui("test-text", opt_def, data = NULL)
    html <- as.character(ui)
    expect_true(grepl("text",  html, ignore.case = TRUE))
    expect_true(grepl("hello", html))
  })

  it("falls back to extracting label from id when label is NULL", {
    opt_def <- list(label = NULL, default = "")
    ui   <- tlg_option_text_ui("module-myvar", opt_def, data = NULL)
    html <- as.character(ui)
    expect_true(grepl("myvar", html))
  })
})

describe("tlg_option_text_server", {
  it("returns the current text input value as a reactive", {
    opt_def       <- list(label = "Text", default = "default_val")
    reset_trigger <- shiny::reactive(0)

    shiny::testServer(
      tlg_option_text_server,
      args = list(
        opt_def       = opt_def,
        data          = shiny::reactive(NULL),
        reset_trigger = reset_trigger
      ),
      {
        session$setInputs(text = "custom_value")
        expect_equal(session$getReturned()(), "custom_value")
      }
    )
  })

  it("resets without error when reset_trigger fires", {
    opt_def       <- list(label = "Text", default = "orig")
    trigger_count <- shiny::reactiveVal(0)
    reset_trigger <- shiny::reactive(trigger_count())

    expect_no_error(
      shiny::testServer(
        tlg_option_text_server,
        args = list(
          opt_def       = opt_def,
          data          = shiny::reactive(NULL),
          reset_trigger = reset_trigger
        ),
        {
          session$setInputs(text = "changed")
          trigger_count(1)
          session$flushReact()
        }
      )
    )
  })
})

# ---------------------------------------------------------------------------
# tlg_option_select
# ---------------------------------------------------------------------------

describe("tlg_option_select_ui", {
  it("returns a selectInput with explicit choices", {
    opt_def <- list(
      label    = "Select",
      choices  = c("A", "B", "C"),
      default  = NULL,
      multiple = FALSE
    )
    ui   <- tlg_option_select_ui("test-sel", opt_def, data = NULL)
    html <- as.character(ui)
    expect_true(grepl("A", html))
    expect_true(grepl("B", html))
    expect_true(grepl("C", html))
  })

  it("pre-selects the default value when provided", {
    opt_def <- list(
      label    = "Select",
      choices  = c("A", "B"),
      default  = "B",
      multiple = FALSE
    )
    ui   <- tlg_option_select_ui("test-sel", opt_def, data = NULL)
    html <- as.character(ui)
    expect_true(grepl("selected", html, ignore.case = TRUE))
  })

  it("selects all choices when default is '.all'", {
    opt_def <- list(
      label    = "Select",
      choices  = c("X", "Y"),
      default  = ".all",
      multiple = TRUE
    )
    ui   <- tlg_option_select_ui("test-sel", opt_def, data = NULL)
    html <- as.character(ui)
    expect_true(grepl("X", html))
    expect_true(grepl("Y", html))
  })

  it("selects the PKNCA grouping vars present in the data when default is '.pknca_groups'", {
    sample_data <- shiny::reactive(
      list(conc = list(data = data.frame(TRT01A = "a", PARAM = "b", AVAL = 1)))
    )
    opt_def <- list(
      label    = "Stratify",
      choices  = ".colnames",
      default  = ".pknca_groups",
      multiple = TRUE
    )
    ui <- shiny::isolate(
      tlg_option_select_ui(
        "m-strat_var", opt_def, data = sample_data,
        grouping_vars = shiny::reactive(c("TRT01A", "PARAM", "NOTINDATA"))
      )
    )
    html <- as.character(ui)
    # Grouping vars present in the data are pre-selected...
    expect_true(grepl('value="TRT01A" selected', html))
    expect_true(grepl('value="PARAM" selected', html))
    # ...a non-grouping data column is offered but NOT selected...
    expect_true(grepl('value="AVAL"', html))
    expect_false(grepl('value="AVAL" selected', html))
    # ...and a grouping var absent from the data is not offered at all.
    expect_false(grepl("NOTINDATA", html))
  })

  it("falls back to nothing selected for '.pknca_groups' when no groups are available", {
    sample_data <- shiny::reactive(
      list(conc = list(data = data.frame(TRT01A = "a", AVAL = 1)))
    )
    opt_def <- list(
      label = "Stratify", choices = ".colnames",
      default = ".pknca_groups", multiple = TRUE
    )
    ui <- shiny::isolate(
      tlg_option_select_ui("m-strat_var", opt_def, data = sample_data)
    )
    html <- as.character(ui)
    expect_false(grepl("selected", html))
  })

  it("derives choices from column names when choices is '.colnames'", {
    sample_data <- shiny::reactive(
      list(conc = list(data = data.frame(COL1 = 1, COL2 = 2)))
    )
    opt_def <- list(
      label    = "Select",
      choices  = ".colnames",
      default  = NULL,
      multiple = FALSE
    )
    # data() is called in UI; must run inside an isolate context
    ui   <- shiny::isolate(
      tlg_option_select_ui("test-sel", opt_def, data = sample_data)
    )
    html <- as.character(ui)
    expect_true(grepl("COL1", html))
    expect_true(grepl("COL2", html))
  })

  it("derives choices from a data column when choices starts with '$'", {
    sample_data <- shiny::reactive(
      list(conc = list(data = data.frame(GRP = c("G1", "G2", "G1"))))
    )
    opt_def <- list(
      label    = "Select",
      choices  = "$GRP",
      default  = NULL,
      multiple = FALSE
    )
    ui   <- shiny::isolate(
      tlg_option_select_ui("test-sel", opt_def, data = sample_data)
    )
    html <- as.character(ui)
    expect_true(grepl("G1", html))
    expect_true(grepl("G2", html))
  })

  it("falls back to extracting label from id when label is NULL", {
    opt_def <- list(label = NULL, choices = c("A"), default = NULL,
                    multiple = FALSE)
    ui   <- tlg_option_select_ui("module-myvar", opt_def, data = NULL)
    html <- as.character(ui)
    expect_true(grepl("myvar", html))
  })
})

describe("tlg_option_select_server", {
  it("returns the selected value as a reactive", {
    opt_def       <- list(
      label    = "Sel",
      choices  = c("A", "B"),
      default  = NULL,
      multiple = FALSE
    )
    reset_trigger <- shiny::reactive(0)

    shiny::testServer(
      tlg_option_select_server,
      args = list(
        opt_def       = opt_def,
        data          = shiny::reactive(NULL),
        reset_trigger = reset_trigger
      ),
      {
        session$setInputs(select = "B")
        expect_equal(session$getReturned()(), "B")
      }
    )
  })

  it("returns empty string when nothing is selected", {
    opt_def       <- list(
      label    = "Sel",
      choices  = c("A", "B"),
      default  = NULL,
      multiple = FALSE
    )
    reset_trigger <- shiny::reactive(0)

    shiny::testServer(
      tlg_option_select_server,
      args = list(
        opt_def       = opt_def,
        data          = shiny::reactive(NULL),
        reset_trigger = reset_trigger
      ),
      {
        session$setInputs(select = "")
        expect_equal(session$getReturned()(), "")
      }
    )
  })

  it("coerces an empty multi-select (NULL input) to \"\" so rendering is not blocked", {
    # A `multiple` selectInput with nothing selected reports input$select = NULL.
    # If the server passed that NULL through, tlg_module_server's is-null guard
    # would return NULL and blank the whole table.  It must return "" instead.
    opt_def       <- list(
      label    = "Stats",
      choices  = c("n", "Mean"),
      default  = NULL,
      multiple = TRUE
    )
    reset_trigger <- shiny::reactive(0)

    shiny::testServer(
      tlg_option_select_server,
      args = list(
        opt_def       = opt_def,
        data          = shiny::reactive(NULL),
        reset_trigger = reset_trigger
      ),
      {
        session$setInputs(select = NULL)
        expect_equal(session$getReturned()(), "")
      }
    )
  })

  it("resets without error when reset_trigger fires", {
    opt_def       <- list(
      label    = "Sel",
      choices  = c("A", "B"),
      default  = "A",
      multiple = FALSE
    )
    trigger_count <- shiny::reactiveVal(0)
    reset_trigger <- shiny::reactive(trigger_count())

    expect_no_error(
      shiny::testServer(
        tlg_option_select_server,
        args = list(
          opt_def       = opt_def,
          data          = shiny::reactive(NULL),
          reset_trigger = reset_trigger
        ),
        {
          session$setInputs(select = "B")
          trigger_count(1)
          session$flushReact()
        }
      )
    )
  })
})

describe(".sensible_group_cols", {
  df <- data.frame(
    USUBJID = paste0("S", 1:20),                        # high cardinality
    SEX     = rep(c("M", "F"), 10),                     # categorical, 2 levels
    RACE    = rep(c("W", "B", "A", "O", "W"), 4),       # categorical, 4 levels
    AVAL    = seq_len(20) + 0.5,                         # continuous numeric
    DOSEA   = rep(c(5, 10, 20), length.out = 20),        # low-cardinality numeric
    ROUTE   = "IV",                                      # single level
    AMOUNTU = c("mg", rep(NA, 19)),                      # only one real level
    BLANK   = "",                                        # all blank
    stringsAsFactors = FALSE
  )

  it("keeps categorical and low-cardinality columns", {
    cols <- .sensible_group_cols(df)
    expect_true(all(c("SEX", "RACE", "DOSEA") %in% cols))
  })

  it("excludes identifiers, continuous, single-level and blank columns", {
    cols <- .sensible_group_cols(df)
    expect_false(any(c("USUBJID", "AVAL", "ROUTE", "AMOUNTU", "BLANK") %in% cols))
  })

  it("respects the max_levels cap", {
    expect_false("RACE" %in% .sensible_group_cols(df, max_levels = 3))
    expect_true("SEX" %in% .sensible_group_cols(df, max_levels = 3))
  })
})
