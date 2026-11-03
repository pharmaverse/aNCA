local({
  library(shiny)
  library(reactable)
  library(reactable.extras)
  source(system.file("shiny/functions/utils.R", package = "aNCA"), local = TRUE)
  source(system.file("shiny/functions/utils-tlg-settings.R", package = "aNCA"), local = TRUE)
  for (type in c("numeric", "table")) {
    source(system.file(paste0("shiny/modules/tab_tlg/tlg_option_", type, ".R"),
                       package = "aNCA"), local = TRUE)
  }
}, envir = parent.env(environment()))

describe("TLG options settings contract", {
  definitions <- list(example = list(options = list(
    groups = list(type = "select", multiple = TRUE),
    column = list(type = "select", multiple = FALSE),
    minimum = list(type = "numeric"),
    formatting = list(type = "table", cols = list(variable = list(), label = list())),
    title = list(type = "text")
  )))

  it("round trips empty selections, empty tables and zero through YAML", {
    saved <- list(example = list(
      groups = character(), column = "", minimum = 0,
      formatting = data.frame(variable = character(), label = character())
    ))
    yaml_saved <- yaml::yaml.load(yaml::as.yaml(saved))
    expect_identical(.normalize_tlg_options(yaml_saved, definitions), saved)
    saved$example$formatting <- data.frame(variable = "AVAL", label = "!AVAL\n$AVALU")
    loaded <- yaml::yaml.load(yaml::as.yaml(saved))
    expect_identical(.normalize_tlg_options(loaded, definitions), saved)
  })

  it("retains valid options while warning on unknown or malformed values", {
    saved <- list(example = list(groups = c("SEX", "TRT01A"), column = c("A", "B"),
                                 minimum = c(1, 2), title = "wrong namespace", removed = TRUE))
    warnings <- character()
    actual <- withCallingHandlers(
      .normalize_tlg_options(saved, definitions),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(warnings, 4)
    expect_identical(actual, list(example = list(groups = c("SEX", "TRT01A"))))
    expect_warning(.normalize_tlg_options(list(retired = list(groups = "SEX")), definitions),
                   "no longer in the catalog")
    expect_identical(.normalize_tlg_options(NULL, definitions), list())
  })

  it("matches stable keys after catalog reordering and leaves new options absent", {
    saved <- list(example = list(groups = "SEX"))
    expanded <- c(list(new_output = list(options = list())), definitions)
    expanded$example$options$new_option <- list(type = "select", default = "new")
    expect_identical(.normalize_tlg_options(saved, expanded), saved)
    expect_warning(.normalize_tlg_options(list(example = list(formatting = list(variable = "X"))),
                                          definitions), "example/formatting")
  })
})


describe("Numeric option settings lifecycle", {
  it("restores zero and clears, then resets to the catalog rather than creation value", {
    restored <- reactiveVal(0)
    reset <- reactiveVal(0)
    testServer(tlg_option_numeric_server, args = list(
      opt_def = list(default = 5), data = reactive(NULL),
      reset_trigger = reset, value_override = restored
    ), {
      session$flushReact()
      expect_identical(session$getReturned()(), 0)
      session$setInputs(numeric = 9)
      expect_identical(session$getReturned()(), 9)
      restored(NA_real_)
      session$flushReact()
      expect_true(is.na(session$getReturned()()))
      reset(1)
      session$flushReact()
      expect_identical(session$getReturned()(), 5)
      restored(NULL)
      session$flushReact()
      expect_identical(session$getReturned()(), 5)
    })
  })
})

describe("Table option settings lifecycle", {
  it("exports only confirmed edits and resets both staged and applied values", {
    defaults <- list(cols = list(variable = list(type = "text")),
                     default_rows = list(list(variable = "AVAL")))
    restored <- reactiveVal(data.frame(variable = "NFRLT"))
    reset <- reactiveVal(0)
    testServer(tlg_option_table_server, args = list(
      opt_def = defaults, data = reactive(data.frame(AVAL = 1)),
      reset_trigger = reset, value_override = restored
    ), {
      session$flushReact()
      expect_identical(session$getReturned()()$variable, "NFRLT")
      session$setInputs(variable = list(row = 1, column = "variable", value = "DRAFT"))
      expect_identical(session$getReturned()()$variable, "NFRLT")
      session$setInputs(confirm_changes = 1)
      expect_identical(session$getReturned()()$variable, "DRAFT")
      reset(1)
      session$flushReact()
      expect_identical(session$getReturned()()$variable, "AVAL")
      session$setInputs(confirm_changes = 2)
      expect_identical(session$getReturned()()$variable, "AVAL")
      restored(data.frame(variable = character()))
      session$flushReact()
      expect_identical(nrow(session$getReturned()()), 0L)
      session$setInputs(confirm_changes = 3)
      expect_identical(nrow(session$getReturned()()), 0L)
    })
  })
})


describe("TLG table settings collection", {
  it("replaces complete formatting tables when their row count changes", {
    initial <- data.frame(variable = "AVAL", label = "Concentration")
    definitions <- list(example = list(options = list(
      formatting = list(type = "table", cols = list(variable = list(), label = list()))
    )))
    registry <- new.env(parent = emptyenv())
    for (current in list(initial[0, ], rbind(initial, initial))) {
      assign("example", list(options = function() list(formatting = current)), registry)
      saved <- .collect_tlg_options(registry, list(example = list(formatting = initial)))
      expect_identical(saved$example$formatting, current)
      restored <- .normalize_tlg_options(yaml::yaml.load(yaml::as.yaml(saved)), definitions)
      expect_equal(restored$example$formatting, current, ignore_attr = TRUE)
    }
  })
})
