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
    setNames(defs)
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
    nav_panel("Listings", "To be added"),
    nav_panel("Graphs", uiOutput(ns("graphs"), class = "tlg-plot-module"), value = "Graphs")
  )
}

tab_tlg_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    log_trace("{session$ns(id)}: Attaching server.")

    data <- session$userData$data

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
            !column_of_conditions %in% names(data()) ~ FALSE,
            sum(eval(parse(text = Condition), envir = data())) > 0 ~ TRUE,
            TRUE ~ FALSE
          )
        )

      tlg_order(new_tlg_order)
    })

    # Render the TLG list for the user's inspection
    output$selected_tlg_table <- DT::renderDT({
      log_trace("Rendering TLG table.")
      datatable(
        class = "table table-striped table-bordered",
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
        extensions = c("RowGroup"),
        options = list(
          paging = FALSE,
          searching = TRUE,
          autoWidth = TRUE,
          dom = "ft",
          columnDefs = list(
            list(width = "150px", targets = "_all"),
            list(className = "dt-center", targets = "_all"),
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
          rowGroup = list(dataSrc = which(names(tlg_order()) %in% c("Type", "Dataset")))
        )
      ) %>%
        formatStyle(
          columns = colnames(tlg_order()),
          fontSize = "14px",
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
        class = "table table-striped table-bordered",
        extensions = c("RowGroup", "Select"),
        options = list(
          paging = FALSE,
          searching = TRUE,
          autoWidth = TRUE,
          dom = "ft",
          columnDefs = list(
            list(
              visible = FALSE,
              targets = which(!names(tlg_order()) %in% c("Type", "Dataset", "PKid", "Label"))
            ),
            list(targets = 0, orderable = FALSE, className = "select-checkbox")
          ),
          select = list(
            style = "multiple",
            selector = "td:first-child",
            server = FALSE
          ),
          rowGroup = list(dataSrc = which(names(tlg_order()) %in% c("Type", "Dataset")))
        )
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
    observeEvent(session$userData$data(), ignoreInit = FALSE, ignoreNULL = FALSE, {
      shinyjs::toggleState("submit_tlg_order", !is.null(session$userData$data()))
      shinyjs::toggleState("submit_tlg_order_alt", !is.null(session$userData$data()))
    })

    # When the user submits the TLG order...
    observeEvent(list(input$submit_tlg_order, input$submit_tlg_order_alt), ignoreInit = TRUE, {
      req(session$userData$data())
      log_trace("Submitting TLG order...")

      tlg_order_filt <- tlg_order()[tlg_order()$Selection, ]
      log_debug("Submitted TLGs:\n", paste0("* ", tlg_order_filt$Description, collapse = "\n"))

      if (sum(tlg_order_filt$Type == "Table") > 0) {
        updateRadioButtons(
          session = session,
          inputId = "buttons_Tables",
          label = "Table to display",
          choices = tlg_order_filt$Label[tlg_order_filt$Type == "Table"]
        )
      } else {
        updateRadioButtons(session = session, inputId = "buttons_Tables", label = "", choices = "")
      }

      if (sum(tlg_order_filt$Type == "Listing") > 0) {
        updateRadioButtons(
          session = session,
          inputId = "buttons_Listings",
          label = "Listing to display",
          choices = tlg_order_filt$Label[tlg_order_filt$Type == "Listing"]
        )
      } else {
        updateRadioButtons(session = session,
                           inputId = "buttons_Listings",
                           label = "",
                           choices = "")
      }

      tlg_order_graphs <- filter(tlg_order_filt, Type == "Graph") %>%
        select("id") %>%
        pull()

      panels <- lapply(tlg_order_graphs, function(g_id) {
        plot_ui <- {
          g_def <- .TLG_DEFINITIONS[[g_id]]

          if (exists(g_def$fun)) {
            tlg_plot_server(g_id, get(g_def$fun), g_def$options)
            tlg_plot_ui(session$ns(g_id))
          } else {
            tags$div("Plot not implemented yet")
          }
        }

        nav_panel(g_def$label, plot_ui)
      })

      panels$"widths" <- c(2, 10)
      output$graphs <- renderUI({
        do.call(navset_pill_list, panels)
      })

      #' change tab to first populated tab
      #' for mysterious reasons nav_select() and updateTabsetPanel() were not working,
      #' so solved this using JavaScript
      shinyjs::runjs(paste0("$(`#", session$ns("tlg_tabs"), " a[data-value='Graphs']`)[0].click()"))
    })
  })
}
