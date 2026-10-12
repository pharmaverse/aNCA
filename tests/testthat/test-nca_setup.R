# Verify the actual standalone download and ZIP writer share the saved labels.
local({
  library(shiny)
  export_env <- new.env(parent = environment())
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(file.path(shiny_dir, "modules", "tab_nca", "nca_setup.R"), local = export_env)
  source(file.path(shiny_dir, "functions", "zip-utils.R"), local = export_env)
  export_env$settings_server <- function(...) list(all = reactive(list(method = "linear")))
  export_env$parameter_selection_server <- function(...) {
    list(selections = reactive(list(cmax = TRUE)),
         types_df = reactive(data.frame(type = "single")))
  }
  export_env$general_exclusions_server <- function(...) reactive(data.frame(exclude = FALSE))
  export_env$ratios_table_server <- function(...) reactive(NULL)
  export_env$units_table_server <- function(...) NULL
  export_env$slope_selector_server <- function(...) reactive(NULL)

  describe("sidebar labels in settings downloads", {
    it("writes current labels in both formats and preserves older version labels", {
      labels <- list(t_pkct01 = list(title = "$STUDYID", subtitle = "!AVAL", footnote = ""))
      testServer(export_env$nca_setup_server, args = list(
        data = reactive(NULL), adnca_data = reactive(NULL),
        extra_group_vars = reactive(character()), settings_override = reactive(NULL)
      ), {
        session$userData$units_table <- reactiveVal(NULL)
        session$userData$tlg_labels <- reactiveVal(labels)
        session$userData$settings_versions <- reactiveVal(list())
        session$userData$settings <- final_settings
        session$userData$ratio_table <- ratio_table
        session$userData$slope_rules <- slope_rules
        session$userData$project_prefix <- function(sep) paste0("Example", sep)
        expect_identical(read_settings(output$settings_download)$tlg_labels, labels)

        updated <- labels
        updated$t_pkct01$footnote <- "Reviewed\nNew version"
        session$userData$tlg_labels(updated)
        target <- withr::local_tempdir()
        export_env$.export_settings(target, session)
        path <- file.path(target, "settings.yaml")
        expect_identical(read_settings(path)$tlg_labels, updated)
        expect_identical(read_settings(path, version = 2)$tlg_labels, labels)
        expect_identical(read_settings(output$settings_download)$tlg_labels, updated)
        expect_identical(export_env$.settings_export_payload(session)$tlg_labels, updated)

        session$userData$tlg_labels <- NULL
        export_env$.export_settings(target, session)
        expect_null(read_settings(path)$tlg_labels)
        expect_null(read_settings(output$settings_download)$tlg_labels)
      })
    })
  })
})
