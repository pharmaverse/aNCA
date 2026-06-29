#' Tab handling Tables, Listings and Graphs.
#'
#' @details
#' Tab provides the user with a selection of TLGs and allows the display and customization of
#' various tables, lists and graphs. The definitions table as well as all customization options
#' are based on `tlg.yaml` file in the root directory of the application. This module calls on
#' `tlg_module`s for each TLG in submitted order, creating a coprehensive UI for all visualizations.
#'
#' Read more in the contributing guide.
#'
#' @param id ID of the module
#' @param data ADNCA data object, processed and mapped.

#' Parses TLG definitions from the yaml file, holds all definitions.
.TLG_DEFINITIONS <- {
  defs <- yaml::read_yaml(system.file("shiny/tlg.yaml", package = "aNCA"))

  defs <- purrr::imap(defs, function(opt_def, opt_id) {
    if ("template" %in% names(opt_def)) {
      template_def <- defs[[opt_def$template]]

      for (d in names(opt_def)) {
        if (d == "template") next

        if (d == "options") {
          for (o in names(opt_def$options)) {
            template_def$options[[o]] <- opt_def$options[[o]]
          }
        } else {
          template_def[[d]] <- opt_def[[d]]
        }
      }

      opt_def <- template_def
    }

    opt_def
  }) %>%
    setNames(names(defs))
}

js_close_button <- tags$button(
  type = "button",
  onclick = "$(this).closest('.modal').modal('hide');",
  `aria-label` = "Close",
  # Style updated for color and size
  style = "color: white; border: none; background: transparent; font-size: 1.2em; padding: 0;",
  icon("times")
)

tab_tlg_ui <- function(id) {
  ns <- NS(id)

  navset_pill(
    id = ns("tlg_tabs"),
    nav_panel(
      "Order details",
      card(
        style = "margin-top: 1em;",
        div(
          actionButton(ns("add_tlg"), "Add TLG"),
          actionButton(ns("remove_tlg"), "Remove TLG"),
          actionButton(ns("submit_tlg_order"), "Submit Order Details", class = "btn-primary")
        )
      ),
      card(reactable_ui(ns("selected_tlg_table"))),
    ),
    nav_panel("Tables", uiOutput(ns("tables"), class = "tlg-module"), value = "Tables"),
    nav_panel("Listings", uiOutput(ns("listings"), class = "tlg-module"), value = "Listings"),
    nav_panel("Graphs", uiOutput(ns("graphs"), class = "tlg-module"), value = "Graphs"),
    # disable loader for initial empty UI render #
    footer = tags$style(
      id = "tlg-load-hide", HTML(paste0(".tlg-module .load-container {opacity: 0;}"))
    )
  )
}

tab_tlg_server <- function(id, data, adpp = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    log_trace("{session$ns(id)}: Attaching server.")

    #' Load TLG orders definitions
    tlg_order <- reactiveVal({
      purrr::map_dfr(.TLG_DEFINITIONS, ~ dplyr::tibble(
        Selection = .x$is_default,
        Type = .x$type,
        Dataset = case_when(
          .x$dataset == "ADNCA" ~ "PK Concentrations",
          .x$dataset == "ADPP" ~ "PK Parameters",
          TRUE ~ .x$dataset
        ),
        PKid = .x$pkid,
        Output = paste0("<a href='", .x$link, "' target='_blank'>", .x$description, "</a>"),
        Label = .x$label,
        Description = .x$description,
        Condition = .x$condition,
        Footnote = NA_character_,
        Stratification = NA_character_,
        Comment = NA_character_
      )) %>%
        dplyr::mutate(id = dplyr::row_number(), .before = dplyr::everything())
    })

    # Based on the TLG list conditions for data() define the preselected rows in $Selection
    observeEvent(list(tlg_order(), data()), {
      req(data())

      # Unparsable conditions will be ignored
      new_tlg_order <- tryCatch({
        tlg_order() %>%
          mutate(
            Selection = case_when(
              Condition == "" | is.na(Condition) | is.null(Condition) ~ Selection,
              any(unique(toupper(data()$conc$data$PCSPEC)) %in% Condition) ~ TRUE,
              TRUE ~ Selection
            )
          )
      }, error = function(e) {
        tlg_order()
      })

      tlg_order(new_tlg_order)
    })

    displayed_order <- reactive({
      dplyr::filter(tlg_order(), Selection) %>%
        dplyr::select(-id, -Selection)
    }) %>%
      bindEvent(data(), input$confirm_add_tlg, input$remove_tlg)

    selected_tlg_state <- reactable_server(
      "selected_tlg_table",
      displayed_order,
      download_buttons = c("csv", "xlsx"),
      groupBy = c("Type", "Dataset"),
      defaultExpanded = TRUE,
      wrap = TRUE,
      selection = "multiple",
      editable = c("Footnote", "Stratification", "Condition", "Comment"),
      columns = function(df) {
        define_cols(df, overrides = list(Output = colDef(html = TRUE)))
      }
    )

    observeEvent(selected_tlg_state()$edit(), {
      info <- selected_tlg_state()$edit()

      new_tlg_order <- tlg_order()
      new_tlg_order[new_tlg_order$Selection, ][info$row, info$column] <- info$value
      tlg_order(new_tlg_order)
    })

    # Show modal when the add_tlg button is pressed
    observeEvent(input$add_tlg, {
      showModal(modalDialog(
        title = div(
          "Add TLGs to Order",
          js_close_button,
          style = "position: relative;"
        ),
        reactable_ui(session$ns("modal_tlg_table")),
        footer = tagList(
          modalButton("Close"),
          actionButton(session$ns("confirm_add_tlg"), "Add TLGs to Order")
        ),
        size = "l"
      ))
    })

    modal_tlg_state <- reactable_server(
      "modal_tlg_table",
      reactive({
        dplyr::filter(tlg_order(), !Selection) %>%
          dplyr::select(-id, -Selection, -Footnote, -Stratification, -Condition, -Comment)
      }),
      download_buttons = c("csv", "xlsx"),
      groupBy = c("Type", "Dataset"),
      wrap = TRUE,
      selection = "multiple",
      defaultExpanded = TRUE,
      width = "775px", # fit to the modal width
      columns = function(df) {
        define_cols(df, overrides = list(Output = colDef(html = TRUE)))
      }
    )

    # Update the Selection column when the confirm_add_tlg button is pressed
    observeEvent(input$confirm_add_tlg, {
      selected_rows <- modal_tlg_state()$selected
      if (length(selected_rows) > 0) {
        tlg_order_data <- tlg_order()
        tlg_order_data$Selection[!tlg_order_data$Selection][selected_rows] <- TRUE
        tlg_order(tlg_order_data)
      }
      removeModal()
    })

    # Update the Selection column when the remove_tlg button is pressed
    observeEvent(input$remove_tlg, {
      selected_rows <- selected_tlg_state()$selected
      if (length(selected_rows) > 0) {
        tlg_order_data <- tlg_order()
        tlg_order_data$Selection[tlg_order_data$Selection][selected_rows] <- FALSE
        tlg_order(tlg_order_data)
      }
    })

    # Toggle submit button depending on whether the data is available #
    observeEvent(data(), ignoreInit = FALSE, ignoreNULL = FALSE, {
      shinyjs::toggleState("submit_tlg_order", !is.null(data()$conc$data))
      shinyjs::toggleState("submit_tlg_order_alt", !is.null(data()$conc$data))
    })

    #' change tab to first populated tab
    #' for mysterious reasons nav_select() and updateTabsetPanel() were not working,
    #' so solved this using JavaScript
    observeEvent(list(input$submit_tlg_order), ignoreInit = TRUE, {
      tab_to_switch <- pull(tlg_order_filtered()[1, "Type"]) %>% paste0("s")
      shinyjs::runjs(
        paste0("
          // change the tab to graphs //
          $(`#", session$ns("tlg_tabs"), " a[data-value='", tab_to_switch, "']`)[0].click();

          // enable spinner, as it was disabled for initial empty UI render //
          setTimeout(function() {
            $('#tlg-load-hide').remove();
          }, 500);  
        ")
      )
    })

    # Submit the TLG order, filter selected TLGs
    tlg_order_filtered <- reactive({
      req(data())
      tlg_order_filt <- tlg_order()[tlg_order()$Selection, ]
      log_debug("Submitted TLGs:\n", paste0("* ", tlg_order_filt$Description, collapse = "\n"))

      tlg_order_filt
    }) %>%
      bindEvent(c(input$submit_tlg_order))

    # Normalized ADNCA concentration data for graph/listing modules
    conc_data <- reactive({
      req(data())
      filter_tlg_excluded(data()$conc$data)
    })

    # ADPP with PPSUMFL-excluded rows removed (mirrors conc_data for ADNCA)
    adpp_data <- reactive({
      validate(need(
        !is.null(adpp()),
        "ADPP data is not available. Run NCA first to view PK parameter outputs."
      ))
      filter_tlg_excluded(adpp())
    })

    # Track which module IDs have already been registered for this session.
    # tlg_module_server() calls Shiny's moduleServer(), which registers reactive
    # observers (pagination buttons, entries-per-page, etc.) every time it is
    # called.  Because renderUI re-executes on re-submit, calling
    # tlg_module_server() with the same ID a second time would accumulate
    # duplicate observers that fire multiple times per user action.
    # output$tlg_output is safely deduplicated by Shiny (second assignment
    # destroys the first), but observers are not — only this environment prevents
    # the duplication.  The environment lives inside moduleServer(), so it is
    # fresh per Shiny session and does not leak across sessions.
    .registered_modules <- new.env(parent = emptyenv())

    # Shared helper: build navset_pill_list panels for one TLG type.
    # Factored out to eliminate the copy-paste across table / graph / listing
    # renderUI blocks.  `id_suffix` must be unique per type to produce
    # deterministic, stable module IDs.
    .build_tlg_panels <- function(g_ids, type, id_suffix) {
      lapply(g_ids, function(g_id) {
        g_def     <- .TLG_DEFINITIONS[[g_id]]
        module_id <- paste0(g_id, id_suffix)
        tlg_data  <- if (g_def$dataset == "ADPP") adpp_data else conc_data

        panel_ui <- if (exists(g_def$fun)) {
          # Only register the Shiny module once per session to avoid accumulating
          # duplicate pagination observers on re-submit.
          if (!exists(module_id, envir = .registered_modules, inherits = FALSE)) {
            tlg_module_server(module_id, tlg_data, type, get(g_def$fun), g_def$options)
            assign(module_id, TRUE, envir = .registered_modules)
          }
          tlg_module_ui(session$ns(module_id), type, g_def$options)
        } else {
          tags$div(paste(tools::toTitleCase(type), "not implemented yet"))
        }

        nav_panel(g_def$label, panel_ui)
      })
    }

    # Create and render Table interface and modules
    output$tables <- renderUI({
      req(tlg_order_filtered())
      ids    <- filter(tlg_order_filtered(), Type == "Table") %>% pull("id")
      panels <- .build_tlg_panels(ids, "table", "_tbl")
      panels$"widths" <- c(2, 10)
      do.call(navset_pill_list, panels)
    })

    # Create and render Graph interface and modules
    output$graphs <- renderUI({
      req(tlg_order_filtered())
      ids    <- filter(tlg_order_filtered(), Type == "Graph") %>% pull("id")
      panels <- .build_tlg_panels(ids, "graph", "_grp")
      panels$"widths" <- c(2, 10)
      do.call(navset_pill_list, panels)
    })

    output$listings <- renderUI({
      req(tlg_order_filtered())
      ids <- filter(tlg_order_filtered(), Type == "Listing") %>% pull("id")

      if (!requireNamespace("rlistings", quietly = TRUE)) {
        panels <- list(nav_panel(
          "Listings",
          tags$div(
            class = "alert alert-warning",
            "Package 'rlistings' is not installed. Install it to view listings:",
            tags$code("install.packages('rlistings')")
          )
        ))
      } else {
        panels <- .build_tlg_panels(ids, "listing", "_lst")
      }

      panels$"widths" <- c(2, 10)
      do.call(navset_pill_list, panels)
    })
  })
}
