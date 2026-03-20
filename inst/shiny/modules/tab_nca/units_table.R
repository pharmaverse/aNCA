#' TODO: add proper documentation
#'
#' @details
#' Module requires session-wide object for storing units table: `session$userData$units_table`.
#' All copies of the module will modify this single source of truth. This is so that the units
#' selection is respected across the whole application, regardless of where the user decides
#' to set the units.
#'
units_table_ui <- function(id) {
  ns <- NS(id)
  # Button to open a module message with the parameter units table #
  tagList(
    actionButton(
      ns("open_units_table"),
      icon = icon("scale-balanced"),
      label = "Parameter Units",
      disabled = FALSE
    )
  )
}

units_table_server <- function(id, mydata, ratio_table = reactive(NULL)) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ratio_units <- reactive({
      req(ratio_table())
      derive_ratio_units(ratio_table(), mydata()$units)
    })

    modal_units_table <- reactiveVal(NULL)
    observeEvent(input$open_units_table, {
      default_units <- mydata()$units %>%
        dplyr::mutate(default = TRUE)

      # Append ratio parameter rows
      ratio_rows <- ratio_units()
      if (!is.null(ratio_rows) && nrow(ratio_rows) > 0) {
        ratio_rows$default <- TRUE
        # Ensure same columns, fill missing with NA
        for (col in setdiff(names(default_units), names(ratio_rows))) {
          ratio_rows[[col]] <- NA
        }
        default_units <- dplyr::bind_rows(
          default_units, ratio_rows[, names(default_units)]
        )
      }

      if (!is.null(session$userData$units_table())) {
        custom_units <- dplyr::mutate(session$userData$units_table(), default = FALSE)
        by_cols <- intersect(names(default_units), names(custom_units))
        by_cols <- setdiff(by_cols, c("PPSTRESU", "conversion_factor", "default"))
        dplyr::rows_update(
          default_units,
          custom_units,
          by = by_cols,
          unmatched = "ignore"
        ) %>%
          modal_units_table()
      } else {
        modal_units_table(default_units)
      }

      showModal(modalDialog(
        title = tagList(
          span("Units of NCA parameter results")
        ),
        reactable_ui(ns("modal_units_table")),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("save_units_table"), "Save Units Table")
        ),
        size = "l"
      ))
    })

    # Define rows from units table not of interest for the user.
    # Ratio parameter rows are always visible.
    rows_to_hide_units_table <- reactive({
      req(modal_units_table())
      tbl <- modal_units_table()

      group_cols <- intersect(
        names(PKNCA::getGroups(mydata()$conc)), names(mydata()$units)
      )
      groups_to_keep <- select(mydata()$intervals, any_of(group_cols))
      params_to_keep <- names(purrr::keep(mydata()$intervals, ~ is.logical(.x) && any(.x)))

      # Identify ratio rows (group columns are NA)
      ratio_pptestcds <- if (!is.null(ratio_units())) ratio_units()$PPTESTCD else character(0)

      rows_to_keep <- tbl %>%
        mutate(nrow = row_number()) %>%
        filter(PPTESTCD %in% params_to_keep | PPTESTCD %in% ratio_pptestcds)
      if (ncol(groups_to_keep) > 0 && length(ratio_pptestcds) > 0) {
        # Keep ratio rows unconditionally, apply group filter only to PKNCA rows
        pknca_rows <- rows_to_keep %>% filter(!PPTESTCD %in% ratio_pptestcds)
        ratio_rows <- rows_to_keep %>% filter(PPTESTCD %in% ratio_pptestcds)
        pknca_rows <- inner_join(
          pknca_rows, unique(groups_to_keep),
          by = intersect(names(pknca_rows), names(groups_to_keep))
        )
        rows_to_keep <- dplyr::bind_rows(pknca_rows, ratio_rows)
      } else if (ncol(groups_to_keep) > 0) {
        rows_to_keep <- inner_join(
          rows_to_keep, unique(groups_to_keep),
          by = intersect(names(rows_to_keep), names(groups_to_keep))
        )
      }

      setdiff(seq_len(nrow(tbl)), rows_to_keep$nrow)
    })

    #' Rendering the modal units table
    unit_edits <- reactable_server(
      "modal_units_table",
      reactive({
        # Add a hidden flag to the data based on the indices to hide
        data <- modal_units_table() %>%
          mutate(
            PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTEST")
          )
        data$is_hidden <- FALSE
        rows_to_hide <- rows_to_hide_units_table()
        if (length(rows_to_hide) > 0) {
          data$is_hidden[rows_to_hide] <- TRUE
        }
        data
      }),
      wrap = TRUE,
      width = "775px", # fit to the modal width
      editable = c("PPSTRESU", "conversion_factor"),
      columns = list(
        PPTESTCD = colDef(name = "Parameter"),
        PPORRESU = colDef(name = "Default Unit"),
        PPSTRESU = colDef(name = "Custom Unit"),
        conversion_factor = colDef(name = "Conversion Factor"),
        is_hidden = colDef(show = FALSE),
        default = colDef(show = FALSE)
      ),
      pagination = FALSE,
      filterable = TRUE,
      rowStyle = reactable::JS("function(rowInfo) {
        if (rowInfo && rowInfo.values && rowInfo.values['is_hidden']) {
          return { display: 'none' }
        }
      }")
    )

    # Accept user modifications in the modal units table
    observeEvent(unit_edits()$edit(), {
      req(unit_edits()$edit())
      info <- unit_edits()$edit()
      modal_units_table <- modal_units_table()

      # If the edited cell is in the 'Conversion Factor' only accept numeric values
      if (info$column == "conversion_factor") {
        info$value <- suppressWarnings(as.numeric(info$value))

        if (is.na(info$value)) {
          showNotification(
            "Please enter a valid numeric value for the Conversion Factor.",
            type = "error",
            duration = 5
          )
          return()
        }
      }

      # Make the edition in the units table
      modal_units_table[info$row, info$column] <- info$value

      # If the custom unit was changed recalculate the conversion factor
      if (info$column == "PPSTRESU") {
        def_unit <- modal_units_table[info$row, "PPORRESU"]
        cust_unit <- modal_units_table[info$row, "PPSTRESU"]
        conversion_factor_value <- get_conversion_factor(def_unit, cust_unit)

        # If the modification lead to an unexpected conversion factor notify the user
        if (any(is.na(conversion_factor_value))) {
          showNotification(
            paste0(
              "Unrecognised conversion: ", def_unit, " > ", cust_unit,
              ". Either make sure both units are defined in the UNIDATA
              library or impute a conversion factor yourself"
            ),
            type = "error",
            duration = 12.5
          )
        }

        modal_units_table[info$row, "default"] <- FALSE
        modal_units_table[info$row, "conversion_factor"] <- conversion_factor_value
      }

      # Update the server table
      modal_units_table(modal_units_table)
    })

    # When save button is pressed substitute the original units table based on the modal one
    observeEvent(input$save_units_table, {
      # Make sure there are no missing entries (no NA in conversion factor)
      if (any(is.na(modal_units_table()$conversion_factor))) {
        id_cols <- setdiff(names(mydata()$units),
                           c("PPTESTCD", "PPORRESU", "PPSTRESU", "conversion_factor"))
        invalid_entries <- modal_units_table() %>%
          filter(is.na(`conversion_factor`)) %>%
          mutate(
            entry = paste0(PPTESTCD, " (", paste(!!!syms(id_cols), sep = ", "), ")")
          ) %>%
          pull(entry)

        showNotification(
          paste0(
            "Please, make sure to use only recognised convertible units in `Custom Unit` ",
            "(i.e, day, h, min, sec, g/L).",
            " If not, introduce yourself the corresponding `Conversion Factor` value in: ",
            paste(invalid_entries, collapse = ", ")
          ),
          duration = NULL,
          closeButton = TRUE,
          type = "warning"
        )
        return()
      }

      log_trace("Applying custom units specification.")
      modal_units_table() %>%
        dplyr::filter(!default) %>%
        session$userData$units_table()

      # Close the modal message window for the user
      removeModal()
    })

    #' Update local `modal_units_table()` if the global value changes.
    observeEvent(session$userData$units_table(), {
      default_units <- mydata()$units %>%
        dplyr::mutate(default = TRUE)

      # Append ratio rows
      ratio_rows <- ratio_units()
      if (!is.null(ratio_rows) && nrow(ratio_rows) > 0) {
        ratio_rows$default <- TRUE
        for (col in setdiff(names(default_units), names(ratio_rows))) {
          ratio_rows[[col]] <- NA
        }
        default_units <- dplyr::bind_rows(
          default_units, ratio_rows[, names(default_units)]
        )
      }

      custom_units <- dplyr::mutate(session$userData$units_table(), default = FALSE)
      by_cols <- intersect(names(default_units), names(custom_units))
      by_cols <- setdiff(by_cols, c("PPSTRESU", "conversion_factor", "default"))
      dplyr::rows_update(
        default_units,
        custom_units,
        by = by_cols,
        unmatched = "ignore"
      ) %>%
        modal_units_table()
    })
  })
}
