#' Parameter Selection module
#'
#' Renders a matrix UI where rows are study types and columns are NCA
#' parameters (PPTESTCD), grouped by TYPE. Each cell is a checkbox
#' indicating whether a parameter should be calculated for that study type.
#' Hovering a column header shows the full parameter name (PPTEST).
#'
#' @param id A unique namespace ID for the module.
#' @param processed_pknca_data A `reactive` expression returning a
#'   `PKNCAdata` object that has been processed.
#' @param parameter_override A `reactive` expression returning a named list
#'   used to override selections.
#'
#' @returns A `list` containing two reactives:
#'   \item{selections}{A `reactive` list where names are study types and
#'     values are vectors of selected PKNCA parameters, e.g.,
#'     `list("Study Type A" = c("p1", "p2"))`.}
#'   \item{types_df}{A `reactive` data frame containing the study type detection results.}

parameter_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Header row with help button
    fluidRow(
      column(
        width = 2,
        tags$h2(
          "Parameter Selection",
          style = "font-size:1.2em; margin-bottom:0.6em; margin-right:1em;"
        )
      ),
      column(
        width = 8,
        actionButton(ns("show_param_ref"),
          label = "PK parameter details",
          icon = icon("book"),
          class = "btn-sm btn-outline-primary"
        )
      ),
      column(
        width = 2,
        dropdown(
          div(
            tags$h2("Parameter Selection Help"),
            p(
              "Selections are independent for each study type ",
              "and can be customized as needed. ",
              "From top-to-bottom, this page shows:"
            ),
            tags$ul(
              tags$li(
                tags$b("Study types table"),
                ": Detected study types and the number of subjects."
              ),
              tags$li(
                tags$b("Parameter matrix"),
                ": Rows are study types, columns are PK parameters. ",
                "Click a cell to toggle whether a parameter is calculated ",
                "for that study type. Hover a column header to see the ",
                "full parameter name."
              )
            )
          ),
          style = "unite",
          right = TRUE,
          icon = icon("question"),
          status = "primary",
          width = "500px"
        ),
      ),
    ),
    p("The following study types were detected in the data:"),
    card(reactable_ui(ns("study_types")), class = "border-0 shadow-none"),

    br(),
    fluidRow(
      column(
        width = 10,
        p(
          "Select the parameters to calculate for each study type. ",
          "Hover column headers for full parameter names. ",
          "Selections can be overridden by uploading a settings file."
        )
      ),
      column(
        width = 2,
        actionButton(
          ns("clear_all"),
          label = "Clear all",
          icon = icon("eraser"),
          class = "btn-sm btn-outline-danger",
          style = "margin-top: 0.5em;"
        )
      )
    ),

    div(
      class = "param-matrix-wrapper",
      uiOutput(ns("param_matrix_ui"))
    )
  )
}

parameter_selection_server <- function(id, processed_pknca_data, parameter_override) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Default parameters selected on first load
    DEFAULT_PARAMS <- c(
      "aucinf.obs", "aucinf.obs.dn",
      "auclast", "auclast.dn",
      "cmax", "cmax.dn",
      "clast.obs", "clast.obs.dn",
      "tlast", "tmax",
      "half.life", "cl.obs", "vss.obs", "vz.obs",
      "mrt.last", "mrt.obs",
      "lambda.z", "lambda.z.n.points",
      "r.squared", "span.ratio",
      "adj.r.squared", "lambda.z.time.first",
      "aucpext.obs", "aucpext.pred",
      "ae", "fe"
    )

    # List of parameter data frames by type
    all_params <- metadata_nca_parameters %>%
      filter(!TYPE %in% c("PKNCA-not-covered", "IV")) %>%
      select(
        TYPE, PKNCA, PPTESTCD, PPTEST,
        can_excretion, can_non_excretion, can_single_dose,
        can_multiple_dose, can_extravascular, can_metabolite
      ) %>%
      mutate(sort_order = row_number())

    # Retrieve study types
    study_types_df <- reactive({
      req(processed_pknca_data())

      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)
      group_columns <- unique(c(conc_group_columns, dose_group_columns))

      groups <- group_columns %>%
        purrr::keep(\(col) {
          !is.null(col) &&
            length(unique(processed_pknca_data()$conc$data[[col]])) > 1
        })

      detect_study_types(
        processed_pknca_data()$conc$data,
        groups,
        metabfl_column = "METABFL",
        route_column = processed_pknca_data()$dose$columns$route,
        volume_column = processed_pknca_data()$conc$columns$volume
      )
    })

    # Create summary of study types
    study_types_summary <- reactive({
      req(study_types_df())

      conc_group_columns <- group_vars(processed_pknca_data()$conc)
      dose_group_columns <- group_vars(processed_pknca_data()$dose)
      group_columns <- unique(c(conc_group_columns, dose_group_columns))
      subj_column <- processed_pknca_data()$conc$columns$subject

      groups <- group_columns %>%
        purrr::keep(\(col) {
          !is.null(col) && col != subj_column &&
            length(unique(processed_pknca_data()$conc$data[[col]])) > 1
        })

      study_types_df() %>%
        group_by(type, !!!syms(groups)) %>%
        summarise(`Subjects Count` = n_distinct(USUBJID), .groups = "drop") %>%
        rename("Study Type" = type)
    })

    # ReactiveVal for parameter selection state (wide df with boolean columns)
    selections_state <- reactiveVal()

    # Build the base state from data or overrides
    base_selections <- reactive({
      req(study_types_df())
      study_type_names <- unique(study_types_df()$type)

      selections_override <- tryCatch({
        parameter_override()
      }, error = function(e) {
        NULL
      })

      .apply_parameter_selections(
        selection_df = all_params,
        study_type_names = study_type_names,
        default_params = DEFAULT_PARAMS,
        selections_override = selections_override
      ) %>%
        select(-starts_with("can_"))
    })

    # Sync the base state to the live state and update checkboxes via JS
    # so the table does not re-render on override/data changes.
    observeEvent(base_selections(), {
      new_state <- base_selections()
      selections_state(new_state)
      .sync_checkboxes_js(session, new_state, unique(study_types_df()$type))
    })

    # Get a simple reactive list of study type names
    study_types_list <- reactive(unique(study_types_df()$type))

    # --- Matrix UI rendering ---
    # Only re-renders when the table structure changes (study types list),
    # not on individual checkbox clicks.
    output$param_matrix_ui <- renderUI({
      req(study_types_list())
      # Read selections once for initial render; further clicks are
      # handled without re-rendering.
      state <- isolate(selections_state())
      req(state)
      study_types <- study_types_list()

      .build_matrix_html(state, study_types, ns)
    })

    # --- Handle checkbox clicks ---
    # Updates internal state only; the checkbox is already toggled in the
    # browser so no re-render is needed.
    observeEvent(input$matrix_click, {
      click <- input$matrix_click
      req(click$study_type, click$param)

      state <- selections_state()
      st <- click$study_type
      pknca_code <- click$param

      if (st %in% names(state) && pknca_code %in% state$PKNCA) {
        row_idx <- which(state$PKNCA == pknca_code)
        state[[st]][row_idx] <- isTRUE(click$checked)
        selections_state(state)
      }
    })

    # --- Clear all selections ---
    observeEvent(input$clear_all, {
      state <- selections_state()
      req(state)
      study_type_names <- study_types_list()
      for (st in study_type_names) {
        state[[st]] <- FALSE
      }
      selections_state(state)
      .sync_checkboxes_js(session, state, study_type_names)
    })

    # Reactable for summary of study types
    reactable_server(
      "study_types",
      study_types_summary,
      height = "28vh"
    )

    # Transform the TRUE/FALSE data frame into a named list of parameter vectors
    parameter_lists_by_type <- reactive({
      req(selections_state())
      df <- selections_state()
      study_type_names <- unique(study_types_df()$type)

      if (length(study_type_names) == 0) return(list())
      req(all(study_type_names %in% names(df)))

      df %>%
        tidyr::pivot_longer(
          cols = any_of(study_type_names),
          names_to = "study_type",
          values_to = "selected"
        ) %>%
        filter(selected == TRUE) %>%
        select(study_type, PKNCA) %>%
        split(.$study_type) %>%
        purrr::map(~ .x$PKNCA)
    })

    # On all changes, disable NCA button briefly to prevent running NCA
    # before settings are applied
    observeEvent(parameter_lists_by_type(), {
      runjs(glue::glue(
        "buttonTimeout(
          '#nca-run_nca',
          {1000},
          'Applying settings...',
          'Run NCA'
        );"
      ))
    })

    # PK parameter reference modal
    observeEvent(input$show_param_ref, .show_param_ref_modal())

    # Return list — same interface as before
    list(
      selections = parameter_lists_by_type,
      types_df = study_types_df
    )
  })
}


#' Show the PK parameter reference modal with a searchable reactable.
#' @noRd
.show_param_ref_modal <- function() {
  ref_data <- .build_param_ref_data()
  pknca_ref_base <- "https://humanpred.github.io/pknca/reference/"
  showModal(modalDialog(
    title = "PK Parameter Details",
    size = "l",
    easyClose = TRUE,
    reactable(
      ref_data,
      searchable = TRUE,
      sortable = TRUE,
      filterable = TRUE,
      highlight = TRUE,
      striped = TRUE,
      compact = TRUE,
      defaultPageSize = 10,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, nrow(ref_data)),
      width = "100%",
      style = list(fontSize = "0.75em"),
      columns = list(
        PPTESTCD = colDef(name = "Short Name"),
        PPTEST = colDef(name = "Parameter Name"),
        Description = colDef(
          name = "Description",
          minWidth = 200,
          style = list(whiteSpace = "normal")
        ),
        App_Location = colDef(
          name = "App Location",
          style = list(whiteSpace = "normal")
        ),
        PKNCA_Function = colDef(
          name = "PKNCA Function",
          html = TRUE,
          cell = function(value) {
            if (value == "\u2014") {
              "\u2014"
            } else if (startsWith(value, "pk.calc.")) {
              func_url <- paste0(pknca_ref_base, value, ".html")
              as.character(htmltools::tags$a(
                href = func_url,
                target = "_blank",
                style = paste0(
                  "color: #0d6efd;",
                  "text-decoration: underline;"
                ),
                value
              ))
            } else {
              value
            }
          }
        )
      )
    ),
    footer = modalButton("Close")
  ))
}

#' Build the parameter reference data frame for the modal.
#' Derives App Location from TYPE, CAT, and can_excretion.
#' @return A data frame with 5 columns for display.
.build_param_ref_data <- function() {
  params <- metadata_nca_parameters

  app_location <- vapply(
    seq_len(nrow(params)),
    function(i) {
      type <- params$TYPE[i]
      cat <- params$CAT[i]
      can_exc <- params$can_excretion[i]
      locs <- character(0)
      if (type %in% c("Standard", "IV")) {
        locs <- c(locs, "Setup > Parameter Selection")
      }
      if (type == "Urine" || identical(can_exc, "T")) {
        locs <- c(
          locs, "Additional Analysis > Excretion"
        )
      }
      if (type == "PKNCA-not-covered" && cat == "Ratio") {
        locs <- c(
          locs, "Additional Analysis > Ratios"
        )
      }
      if (length(locs) == 0) "Setup > Parameter Selection"
      else paste(locs, collapse = "; ")
    },
    character(1)
  )

  pknca_fun <- ifelse(
    is.na(params$FUN) | params$FUN == "" |
      params$TYPE == "PKNCA-not-covered",
    "\u2014",
    params$FUN
  )

  data.frame(
    PPTESTCD = params$PPTESTCD,
    PPTEST = params$PPTEST,
    Description = params$description,
    App_Location = app_location,
    PKNCA_Function = pknca_fun,
    stringsAsFactors = FALSE
  )
}

#' Helper to Apply Default or Override Parameter Selections
#'
#' Populates a selection data frame with boolean columns for each study type,
#' indicating which parameters are selected based on either default rules or a
#' provided override list.
#'
#' @param selection_df A data frame containing PK parameters and their metadata.
#'   Must include a 'PKNCA' column and logical columns for various attributes
#'   (e.g., 'can_excretion', 'can_single_dose').
#' @param study_type_names A character vector of study type names to generate
#'   selection columns for.
#' @param default_params A character vector of default PKNCA parameters to select.
#' @param selections_override An optional named list where names correspond to
#'   study types and values are character vectors of PKNCA parameters to select.
#'   If NULL, default logic is applied.
#'
#' @returns The 'selection_df' data frame with added boolean columns for each
#'   study type.
#'
.apply_parameter_selections <- function(selection_df,
                                        study_type_names,
                                        default_params,
                                        selections_override = NULL) {

  # Use override if available, otherwise use defaults
  if (is.null(selections_override) || length(selections_override) == 0) {
    # Default behavior
    for (st_name in study_type_names) {
      is_selected <- selection_df$PKNCA %in% default_params

      # Apply metadata rules
      is_selected <- is_selected & if (st_name == "Excretion Data") {
        selection_df$can_excretion
      } else {
        selection_df$can_non_excretion
      }

      if (grepl("Single", st_name)) {
        is_selected <- is_selected & selection_df$can_single_dose
      }
      if (grepl("Multiple", st_name)) {
        is_selected <- is_selected & selection_df$can_multiple_dose
      }
      if (grepl("Extravascular", st_name)) {
        is_selected <- is_selected & selection_df$can_extravascular
      }
      if (grepl("Metabolite", st_name)) {
        is_selected <- is_selected & selection_df$can_metabolite
      }

      selection_df[[st_name]] <- is_selected
    }
  } else {
    # Override behavior
    for (st_name in study_type_names) {
      override_params <- selections_override[[st_name]]
      if (!is.null(override_params)) {
        selection_df[[st_name]] <- selection_df$PKNCA %in% override_params
      } else {
        selection_df[[st_name]] <- FALSE
      }
    }
  }
  selection_df
}

#' Build the parameter matrix HTML table.
#'
#' Pure function that generates the HTML `<table>` with checkboxes.
#' Extracted so `renderUI` stays concise.
#'
#' @param state The current selections_state data frame.
#' @param study_types Character vector of study type names.
#' @param ns The module namespace function.
#' @return A `tags$div` containing the matrix table.
#' @noRd
.build_matrix_html <- function(state, study_types, ns) {
  params_meta <- state %>%
    select(PKNCA, PPTESTCD, PPTEST, TYPE, sort_order) %>%
    distinct(PKNCA, .keep_all = TRUE) %>%
    arrange(sort_order)

  type_groups <- split(
    params_meta,
    factor(params_meta$TYPE, levels = unique(params_meta$TYPE))
  )

  # Two header rows: TYPE group spans + individual param columns

  group_header_cells <- list(tags$th(
    class = "param-matrix-corner", ""
  ))
  param_header_cells <- list(tags$th(
    class = "param-matrix-row-header", "Study Type"
  ))

  for (type_name in names(type_groups)) {
    grp <- type_groups[[type_name]]
    group_header_cells <- c(group_header_cells, list(
      tags$th(
        class = "param-matrix-group-header",
        colspan = nrow(grp),
        type_name
      )
    ))
    for (i in seq_len(nrow(grp))) {
      param_header_cells <- c(param_header_cells, list(
        tags$th(
          class = "param-matrix-col-header",
          title = grp$PPTEST[i],
          `data-pknca` = grp$PKNCA[i],
          grp$PPTESTCD[i]
        )
      ))
    }
  }

  # Body rows — one per study type

  body_rows <- lapply(study_types, function(st) {
    cells <- list(tags$td(
      class = "param-matrix-row-header", st
    ))
    for (i in seq_len(nrow(params_meta))) {
      pknca_code <- params_meta$PKNCA[i]
      is_checked <- isTRUE(state[[st]][state$PKNCA == pknca_code])
      cb_id <- paste0(
        "cb__",
        gsub("[^A-Za-z0-9]", "_", st),
        "__",
        gsub("[^A-Za-z0-9]", "_", pknca_code)
      )
      cells <- c(cells, list(
        tags$td(
          class = paste0(
            "param-matrix-cell",
            if (is_checked) " checked" else ""
          ),
          tags$input(
            type = "checkbox",
            class = "param-matrix-checkbox",
            id = ns(cb_id),
            checked = if (is_checked) NA else NULL,
            `data-study-type` = st,
            `data-pknca` = pknca_code,
            onclick = paste0(
              "Shiny.setInputValue('", ns("matrix_click"), "',",
              "{study_type: this.dataset.studyType,",
              " param: this.dataset.pknca,",
              " checked: this.checked,",
              " ts: Date.now()});"
            )
          )
        )
      ))
    }
    tags$tr(cells)
  })

  tags$div(
    class = "param-matrix-container",
    tags$table(
      class = "param-matrix-table",
      tags$thead(
        tags$tr(group_header_cells),
        tags$tr(param_header_cells)
      ),
      tags$tbody(body_rows)
    )
  )
}

#' Sync all checkboxes in the matrix to match the current state via JS.
#'
#' Used when the state changes externally (overrides, clear-all) to
#' update the DOM without re-rendering the table.
#'
#' @param session The Shiny session object.
#' @param state The current selections_state data frame.
#' @param study_type_names Character vector of study type names.
#' @noRd
.sync_checkboxes_js <- function(session, state, study_type_names) {
  js_lines <- character(0)
  for (st in study_type_names) {
    for (i in seq_len(nrow(state))) {
      is_checked <- isTRUE(state[[st]][i])
      cb_id <- paste0(
        "cb__",
        gsub("[^A-Za-z0-9]", "_", st),
        "__",
        gsub("[^A-Za-z0-9]", "_", state$PKNCA[i])
      )
      full_id <- session$ns(cb_id)
      checked_str <- if (is_checked) "true" else "false"
      checked_class <- if (is_checked) {
        "el.parentElement.classList.add('checked');"
      } else {
        "el.parentElement.classList.remove('checked');"
      }
      js_lines <- c(js_lines, paste0(
        "(function(){var el=document.getElementById('",
        full_id, "');if(el){el.checked=", checked_str, ";",
        checked_class, "}})();"
      ))
    }
  }
  if (length(js_lines) > 0) {
    runjs(paste(js_lines, collapse = "\n"))
  }
}
