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

  defs <- purrr::imap(defs, \(opt_def, opt_id) {
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
  }) |>
    setNames(names(defs))
}

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
      card(
        DTOutput(ns("selected_tlg_table"))
      ),
      card(
        div(
          actionButton(ns("submit_tlg_order_alt"), "Submit Order Details", class = "btn-primary")
        )
      )
    ),
    nav_panel("Tables", "To be added"),
    nav_panel("Listings", uiOutput(ns("listings"), class = "tlg-module"), value = "Listings"),
    nav_panel("Graphs", uiOutput(ns("graphs"), class = "tlg-module"), value = "Graphs"),
    # disable loader for initial empty UI render #
    footer = tags$style(
      id = "tlg-load-hide", HTML(paste0(".tlg-module .load-container {opacity: 0;}"))
    )
  )
}

tab_tlg_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    log_trace("{session$ns(id)}: Attaching server.")

    #' Load TLG orders definitions
    tlg_order <- reactiveVal({
      purrr::map_dfr(.TLG_DEFINITIONS, ~ dplyr::tibble(
        Selection = .x$is_default,
        Type = .x$type,
        Dataset = .x$dataset,
        PKid = paste0("<a href='", .x$link, "' target='_blank'>", .x$pkid, "</a>"),
        Description = .x$description,
        Footnote = NA_character_,
        Stratification = NA_character_,
        Condition = NA_character_,
        Comment = NA_character_
      )) %>%
        dplyr::mutate(id = dplyr::row_number(), .before = dplyr::everything())
    })

    # Based on the TLG list conditions for data() define the preselected rows in $Selection
    observeEvent(list(tlg_order(), data()), {
      req(data())
      column_of_conditions <- gsub("([=<>!].*)", "", tlg_order()$Condition)

      new_tlg_order <- tlg_order() %>%
        mutate(
          Selection = case_when(
            Condition == "" | is.na(Condition) ~ Selection,
            !column_of_conditions %in% names(data()$conc$data) ~ FALSE,
            sum(eval(parse(text = Condition), envir = data()$conc$data)) > 0 ~ TRUE,
            TRUE ~ FALSE
          )
        )

      tlg_order(new_tlg_order)
    })

    # Render the TLG list for the user's inspection
    output$selected_tlg_table <- DT::renderDT({
      log_trace("Rendering TLG table.")
      datatable(
        data = dplyr::filter(tlg_order(), Selection),
        editable = list(
          target = "cell",
          disable = list(
            columns = which(!names(tlg_order()) %in% c("Footnote", "Stratification", "Comment"))
          )
        ),
        rownames = TRUE,
        escape = FALSE,
        selection = list(
          mode = "multiple"
        ),
        extensions = c("RowGroup", "Buttons"),
        options = list(
          scrollX = TRUE,
          fixedHeader = TRUE,
          dom = "Blfrtip",
          buttons = list(
            list(extend = "copy", title = paste0("TLG_table_", Sys.Date())),
            list(extend = "csv", filename = paste0("TLG_table_", Sys.Date()))
          ),
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '0.75em');",
            "  $(thead).find('th').css('text-align', 'center');",
            "}"
          ),
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = "150px", targets = "_all"),
            list(
              visible = FALSE,
              targets = c(
                0,
                which(!names(tlg_order()) %in% c(
                  "Type", "Dataset", "PKid", "Label", "Footnote", "Stratification", "Comment"
                ))
              )
            )
          ),
          rowGroup = list(dataSrc = which(names(tlg_order()) %in% c("Type", "Dataset"))),
          lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
          paging = TRUE
        ),
        class = "row-border compact"
      ) %>%
        formatStyle(
          columns = colnames(tlg_order()),
          fontSize = "75%",
          fontFamily = "Arial"
        )
    }, server = FALSE)

    # Save table changes from the UI into the server
    observeEvent(input$selected_tlg_table_cell_edit, {
      info <- input$selected_tlg_table_cell_edit

      new_tlg_order <- tlg_order()
      new_tlg_order[new_tlg_order$Selection, ][info$row, info$col] <- info$value
      tlg_order(new_tlg_order)
    })

    # Show modal when the add_tlg button is pressed
    observeEvent(input$add_tlg, {
      showModal(modalDialog(
        title = tagList(
          span("Add TLGs to Order"),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            span(`aria-hidden` = "true", HTML("&times;"))
          )
        ),
        DTOutput(session$ns("modal_tlg_table")),
        footer = tagList(
          modalButton("Close"),
          actionButton(session$ns("confirm_add_tlg"), "Add TLGs to Order")
        ),
        size = "l"
      ))
    })

    # Render the DT table in the modal
    output$modal_tlg_table <- DT::renderDT({
      datatable(
        data = dplyr::filter(tlg_order(), !Selection),
        selection = list(mode = "multiple"),
        escape = FALSE,
        editable = FALSE,
        extensions = c("RowGroup", "Select", "Buttons"),
        options = list(
          scrollX = TRUE,
          fixedHeader = TRUE,
          dom = "Blfrtip",
          buttons = list(
            list(extend = "copy", title = paste0("TLG_modal_table_", Sys.Date())),
            list(extend = "csv", filename = paste0("TLG_modal_table_", Sys.Date()))
          ),
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '0.75em');",
            "  $(thead).find('th').css('text-align', 'center');",
            "}"
          ),
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = "150px", targets = "_all")
          ),
          rowGroup = list(dataSrc = which(names(tlg_order()) %in% c("Type", "Dataset"))),
          lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
          paging = TRUE
        ),
        class = "row-border compact"
      ) %>%
        formatStyle(
          columns = colnames(tlg_order()),
          fontSize = "75%",
          fontFamily = "Arial"
        )
    })

    # Update the Selection column when the confirm_add_tlg button is pressed
    observeEvent(input$confirm_add_tlg, {
      selected_rows <- input$modal_tlg_table_rows_selected
      if (length(selected_rows) > 0) {
        tlg_order_data <- tlg_order()
        tlg_order_data$Selection[!tlg_order_data$Selection][selected_rows] <- TRUE
        tlg_order(tlg_order_data)
      }
      removeModal()
    })

    # Update the Selection column when the remove_tlg button is pressed
    observeEvent(input$remove_tlg, {
      selected_rows <- input$selected_tlg_table_rows_selected
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
    observeEvent(list(input$submit_tlg_order, input$submit_tlg_order_alt), ignoreInit = TRUE, {
      tab_to_switch <- pull(tlg_order_filtered()[1, "Type"]) |> paste0("s")
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
    }) |>
      bindEvent(c(input$submit_tlg_order, input$submit_tlg_order_alt))

    # Create and render Graph interface and modules
    output$graphs <- renderUI({
      req(tlg_order_filtered())
      tlg_order_graphs <- filter(tlg_order_filtered(), Type == "Graph") %>%
        select("id") %>%
        pull()

      panels <- lapply(tlg_order_graphs, function(g_id) {
        graph_ui <- {
          g_def <- .TLG_DEFINITIONS[[g_id]]
          module_id <- paste0(g_id, stringi::stri_rand_strings(1, 5))

          if (exists(g_def$fun)) {
            tlg_module_server(module_id, data, "graph", get(g_def$fun), g_def$options)
            tlg_module_ui(session$ns(module_id), "graph", g_def$options)
          } else {
            tags$div("Graph not implemented yet")
          }
        }

        nav_panel(g_def$label, graph_ui)
      })

      panels$"widths" <- c(2, 10)

      do.call(navset_pill_list, panels)
    })

    output$listings <- renderUI({
      req(tlg_order_filtered())

      tlg_order_listings <- filter(tlg_order_filtered(), Type == "Listing") %>%
        select("id") %>%
        pull()

      panels <- lapply(tlg_order_listings, function(g_id) {
        list_ui <- {
          g_def <- .TLG_DEFINITIONS[[g_id]]
          module_id <- paste0(g_id, stringi::stri_rand_strings(1, 5))

          if (exists(g_def$fun)) {
            tlg_module_server(module_id, data, "listing", get(g_def$fun), g_def$options)
            tlg_module_ui(session$ns(module_id), "listing", g_def$options)
          } else {
            tags$div("Listing not implemented yet")
          }
        }

        nav_panel(g_def$label, list_ui)
      })

      panels$"widths" <- c(2, 10)

      do.call(navset_pill_list, panels)
    })
  })
}
