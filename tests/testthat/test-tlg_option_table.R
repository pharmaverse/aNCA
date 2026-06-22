# Tests for tlg_option_table Shiny module
# Uses shiny::testServer() for server-side reactive logic.

# Source required files
local({
  library(shiny)
  library(reactable)
  library(reactable.extras)
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(
    file.path(shiny_dir, "functions", "utils.R"),
    local = TRUE
  )
  source(
    file.path(shiny_dir, "modules", "tab_tlg", "tlg_option_table.R"),
    local = TRUE
  )
},
envir = parent.env(environment()))

# ---------------------------------------------------------------------------
# Shared test fixtures
# ---------------------------------------------------------------------------

# Minimal opt_def for a two-column table (text + select)
make_opt_def <- function(
  default_rows = list(
    list(col1 = "A", col2 = "X"),
    list(col1 = "B", col2 = "Y")
  )
) {
  list(
    label = "Edit Table",
    cols  = list(
      col1 = list(type = "text",   label = "Column 1"),
      col2 = list(
        type    = "select",
        label   = "Column 2",
        choices = c("X", "Y", "Z")
      )
    ),
    default_rows = default_rows
  )
}

make_data <- function() {
  shiny::reactive(list(conc = list(data = data.frame(A = 1:2, B = 3:4))))
}

# ---------------------------------------------------------------------------
# tlg_option_table_ui
# ---------------------------------------------------------------------------

describe("tlg_option_table_ui", {
  it("returns an actionButton with the opt_def label", {
    opt_def <- list(label = "My Table Button")
    ui   <- tlg_option_table_ui("test-tbl", opt_def, data = NULL)
    html <- as.character(ui)
    expect_true(grepl("My Table Button", html))
    expect_true(grepl("open_table",      html))
  })

  it("falls back to id as label when label is NULL", {
    opt_def <- list(label = NULL)
    ui   <- tlg_option_table_ui("test-tbl", opt_def, data = NULL)
    html <- as.character(ui)
    expect_true(grepl("test-tbl", html))
  })
})

# ---------------------------------------------------------------------------
# tlg_option_table_server
# ---------------------------------------------------------------------------

describe("tlg_option_table_server: default table", {
  it("returns a reactive whose initial value equals the default_rows tibble", {
    opt_def       <- make_opt_def()
    reset_trigger <- shiny::reactive(0)

    shiny::testServer(
      tlg_option_table_server,
      args = list(
        opt_def       = opt_def,
        data          = make_data(),
        reset_trigger = reset_trigger
      ),
      {
        result <- session$getReturned()()
        expect_s3_class(result, "tbl_df")
        expect_equal(nrow(result), 2)
        expect_equal(result$col1, c("A", "B"))
        expect_equal(result$col2, c("X", "Y"))
      }
    )
  })

  it("converts '$NA' placeholder in default_rows to real NA", {
    opt_def <- make_opt_def(
      default_rows = list(list(col1 = "$NA", col2 = "X"))
    )
    reset_trigger <- shiny::reactive(0)

    shiny::testServer(
      tlg_option_table_server,
      args = list(
        opt_def       = opt_def,
        data          = make_data(),
        reset_trigger = reset_trigger
      ),
      {
        result <- session$getReturned()()
        expect_true(is.na(result$col1[1]))
      }
    )
  })
})

describe("tlg_option_table_server: confirm_changes applies edits", {
  it("output_table updates after confirm_changes is clicked", {
    opt_def       <- make_opt_def()
    reset_trigger <- shiny::reactive(0)

    shiny::testServer(
      tlg_option_table_server,
      args = list(
        opt_def       = opt_def,
        data          = make_data(),
        reset_trigger = reset_trigger
      ),
      {
        session$setInputs(
          col1 = list(row = 1, column = "col1", value = "EDITED")
        )
        session$setInputs(confirm_changes = 1)
        session$flushReact()
        expect_equal(session$getReturned()()$col1[1], "EDITED")
      }
    )
  })
})

describe("tlg_option_table_server: cancel discards edits", {
  it("output_table stays unchanged after cancel is clicked", {
    opt_def       <- make_opt_def()
    reset_trigger <- shiny::reactive(0)

    shiny::testServer(
      tlg_option_table_server,
      args = list(
        opt_def       = opt_def,
        data          = make_data(),
        reset_trigger = reset_trigger
      ),
      {
        session$setInputs(
          col1 = list(row = 1, column = "col1", value = "WILL_DISCARD")
        )
        session$setInputs(cancel = 1)
        session$flushReact()
        expect_equal(session$getReturned()()$col1[1], "A")
      }
    )
  })
})

describe("tlg_option_table_server: add_row", {
  it("adds a new empty row to the edits table after confirm", {
    opt_def       <- make_opt_def()
    reset_trigger <- shiny::reactive(0)

    shiny::testServer(
      tlg_option_table_server,
      args = list(
        opt_def       = opt_def,
        data          = make_data(),
        reset_trigger = reset_trigger
      ),
      {
        session$setInputs(confirm_changes = 1)
        session$flushReact()
        before <- nrow(session$getReturned()())

        session$setInputs(add_row = 1)
        session$flushReact()
        session$setInputs(confirm_changes = 2)
        session$flushReact()

        after <- nrow(session$getReturned()())
        expect_equal(after, before + 1)
      }
    )
  })
})

describe("tlg_option_table_server: reset_trigger", {
  it("restores the default table when reset_trigger fires", {
    opt_def       <- make_opt_def()
    trigger_count <- shiny::reactiveVal(0)
    reset_trigger <- shiny::reactive(trigger_count())

    shiny::testServer(
      tlg_option_table_server,
      args = list(
        opt_def       = opt_def,
        data          = make_data(),
        reset_trigger = reset_trigger
      ),
      {
        session$setInputs(
          col1 = list(row = 1, column = "col1", value = "CHANGED")
        )
        session$setInputs(confirm_changes = 1)
        session$flushReact()
        expect_equal(session$getReturned()()$col1[1], "CHANGED")

        trigger_count(1)
        session$flushReact()
        expect_equal(session$getReturned()()$col1[1], "A")
      }
    )
  })
})
