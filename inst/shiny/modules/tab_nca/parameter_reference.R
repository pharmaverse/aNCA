#' Parameter Reference Table Module
#'
#' Displays a searchable reference table of all PK parameters the app can
#' compute, with metadata about each one (category, type, app location,
#' PKNCA function, and formula).
#'
#' @param id Module namespace ID.

parameter_reference_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; gap: 0.5em; align-items: center; margin-bottom: 1em;",
      tags$h2(
        "Parameter Reference",
        style = "font-size: 1.2em; margin-bottom: 0;"
      )
    ),
    p(
      "Searchable reference of all PK parameters available in the app.",
      "Use the search box or column filters to find specific parameters.",
      style = "color: #666; margin-bottom: 1em;"
    ),
    reactable_ui(ns("param_table"))
  )
}

parameter_reference_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ref_data <- reactive({
      params <- metadata_nca_parameters

      # Derive App Location from TYPE, CAT, and can_excretion
      params$app_location <- vapply(seq_len(nrow(params)), function(i) {
        type <- params$TYPE[i]
        cat <- params$CAT[i]
        can_exc <- params$can_excretion[i]

        locations <- character(0)

        if (type %in% c("Standard", "IV")) {
          locations <- c(locations, "NCA > Setup > Parameter Selection")
        }
        if (type == "Urine" || identical(can_exc, "T")) {
          locations <- c(locations, "NCA > Additional Analysis > Excretion")
        }
        if (type == "PKNCA-not-covered" && cat == "Ratio") {
          locations <- c(locations, "NCA > Additional Analysis > Ratios")
        }
        if (type == "Sparse") {
          locations <- c(
            locations,
            "NCA > Setup > Parameter Selection (sparse)"
          )
        }

        if (length(locations) == 0) {
          "NCA > Setup > Parameter Selection"
        } else {
          paste(locations, collapse = "; ")
        }
      }, character(1))

      # Build PKNCA function display with link
      params$pknca_fun <- vapply(seq_len(nrow(params)), function(i) {
        fun <- params$FUN[i]
        if (is.na(fun) || fun == "") {
          "\u2014"
        } else {
          fun
        }
      }, character(1))

      # Select and rename columns for display
      data.frame(
        PPTESTCD = params$PPTESTCD,
        PPTEST = params$PPTEST,
        Description = params$description,
        Category = params$CAT,
        Type = params$TYPE,
        App_Location = params$app_location,
        PKNCA_Function = params$pknca_fun,
        Depends = params$Depends,
        stringsAsFactors = FALSE
      )
    })

    param_cols <- function(data) {
      list(
        PPTESTCD = colDef(
          name = "PPTESTCD",
          minWidth = 100,
          filterable = TRUE
        ),
        PPTEST = colDef(
          name = "Parameter Name",
          minWidth = 180,
          filterable = TRUE
        ),
        Description = colDef(
          name = "Description",
          minWidth = 250,
          style = list(whiteSpace = "normal"),
          filterable = TRUE
        ),
        Category = colDef(
          name = "Category",
          minWidth = 120,
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf(
                "Reactable.setFilter('param_table', '%s', event.target.value || undefined)",
                name
              ),
              tags$option(value = "", "All"),
              lapply(sort(unique(values)), function(v) tags$option(value = v, v))
            )
          },
          filterable = TRUE
        ),
        Type = colDef(
          name = "Type",
          minWidth = 120,
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf(
                "Reactable.setFilter('param_table', '%s', event.target.value || undefined)",
                name
              ),
              tags$option(value = "", "All"),
              lapply(sort(unique(values)), function(v) tags$option(value = v, v))
            )
          },
          filterable = TRUE
        ),
        App_Location = colDef(
          name = "App Location",
          minWidth = 220,
          style = list(whiteSpace = "normal"),
          filterable = TRUE
        ),
        PKNCA_Function = colDef(
          name = "PKNCA Function",
          minWidth = 140,
          cell = function(value) {
            if (value == "\u2014" || is.na(value) || value == "") {
              "\u2014"
            } else {
              tags$a(
                href = paste0(
                  "https://cran.r-project.org/web/packages/PKNCA/PKNCA.pdf"
                ),
                target = "_blank",
                value
              )
            }
          },
          filterable = TRUE
        ),
        Depends = colDef(
          name = "Depends On",
          minWidth = 150,
          style = list(whiteSpace = "normal"),
          filterable = TRUE
        )
      )
    }

    reactable_server(
      "param_table",
      ref_data,
      columns = param_cols,
      download_buttons = c("csv", "xlsx"),
      file_name = "PK_Parameter_Reference",
      defaultPageSize = 25,
      pageSizeOptions = reactive(c(25, 50, 100, nrow(ref_data())))
    )
  })
}
