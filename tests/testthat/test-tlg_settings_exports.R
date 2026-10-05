# Exercise both settings writers with the same live TLG order. Only unrelated
# NCA child modules are stubbed; the download, versioning and reader are real.
local({
  library(shiny)
  export_env <- new.env(parent = environment())
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(file.path(shiny_dir, "modules", "tab_nca", "nca_setup.R"), local = export_env)
  source(file.path(shiny_dir, "functions", "zip-utils.R"), local = export_env)

  export_env$settings_server <- function(...) list(all = reactive(list(method = "linear")))
  export_env$parameter_selection_server <- function(...) {
    list(
      selections = reactive(list(cmax = TRUE)),
      types_df = reactive(data.frame(type = "single"))
    )
  }
  export_env$general_exclusions_server <- function(...) reactive(data.frame(exclude = FALSE))
  export_env$ratios_table_server <- function(...) reactive(NULL)
  export_env$units_table_server <- function(...) NULL
  export_env$slope_selector_server <- function(...) reactive(NULL)

  describe("TLG order in settings exports", {
    it("saves current order details in standalone and ZIP settings, including version history", {
      saved_order <- list(
        t_pkct01 = list(
          Selection = TRUE, Footnote = "First line\nSecond line: !AVAL",
          Stratification = "TRT01A", Comment = "Check dose groups"
        ),
        g_pkcg01_lin = list(
          Selection = FALSE, Footnote = NA_character_, Stratification = "", Comment = ""
        )
      )
      testServer(export_env$nca_setup_server, args = list(
        data = reactive(NULL), adnca_data = reactive(NULL),
        extra_group_vars = reactive(character()), settings_override = reactive(NULL)
      ), {
        session$userData$units_table <- reactiveVal(NULL)
        session$userData$tlg_order <- reactiveVal(saved_order)
        session$userData$settings_versions <- reactiveVal(list())
        session$userData$settings <- final_settings
        session$userData$ratio_table <- ratio_table
        session$userData$slope_rules <- slope_rules
        session$userData$project_prefix <- function(sep) paste0("Example", sep)
        session$setInputs(settings_save_comment = "TLG order")

        standalone <- read_settings(output$settings_download)
        expect_identical(standalone$tlg_order, saved_order)
        expect_identical(standalone$settings$method, "linear")

        # Editing after the first export must be read at download time.
        updated_order <- saved_order
        updated_order$t_pkct01$Comment <- "Reviewed"
        updated_order$g_pkcg01_lin$Selection <- TRUE
        session$userData$tlg_order(updated_order)
        zip_dir <- withr::local_tempdir()
        export_env$.export_settings(zip_dir, session)
        zip_file <- file.path(zip_dir, "settings.yaml")
        expect_identical(read_settings(zip_file)$tlg_order, updated_order)
        expect_identical(read_settings(zip_file, version = 2)$tlg_order, saved_order)
        expect_identical(read_settings(output$settings_download)$tlg_order, updated_order)
      })
    })

    it("still exports settings when the TLG module has not published an order", {
      testServer(export_env$nca_setup_server, args = list(
        data = reactive(NULL), adnca_data = reactive(NULL),
        extra_group_vars = reactive(character()), settings_override = reactive(NULL)
      ), {
        session$userData$units_table <- reactiveVal(NULL)
        session$userData$settings_versions <- reactiveVal(list())
        session$userData$settings <- final_settings
        session$userData$ratio_table <- ratio_table
        session$userData$slope_rules <- slope_rules
        session$userData$project_prefix <- function(sep) paste0("Example", sep)
        standalone <- read_settings(output$settings_download)
        zip_dir <- withr::local_tempdir()
        export_env$.export_settings(zip_dir, session)
        zipped <- read_settings(file.path(zip_dir, "settings.yaml"))
        expect_null(standalone$tlg_order)
        expect_null(zipped$tlg_order)
        expect_identical(zipped$settings, standalone$settings)
      })
    })
  })
})
