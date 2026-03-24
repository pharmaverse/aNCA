#' Save Settings Module
#'
#' Provides a save button (for the app header) and a modal dialog where
#' the user can enter an optional comment before saving the current app
#' state as a new versioned settings entry. The settings YAML is
#' downloaded to the user's machine.

save_settings_ui <- function(id) {
  ns <- NS(id)
  actionButton(
    ns("open_save_modal"),
    label = NULL,
    icon = icon("floppy-disk"),
    class = "btn btn-outline-primary btn-sm",
    title = "Save settings"
  )
}

save_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Open the save modal
    observeEvent(input$open_save_modal, {
      showModal(modalDialog(
        title = "Save Settings",
        textInput(
          ns("save_comment"),
          label = "Comment (optional)",
          placeholder = "e.g. final NCA, first draft"
        ),
        footer = tagList(
          downloadButton(ns("download_settings"), "Save", class = "btn-primary"),
          modalButton("Cancel")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    })

    output$download_settings <- downloadHandler(
      filename = function() {
        pn <- session$userData$project_name()
        prefix <- if (nzchar(pn)) pn else "settings"
        paste0(prefix, ".yaml")
      },
      content = function(file) {
        # Collect current settings payload
        settings_list <- session$userData$settings()

        if (!is.null(settings_list$units)) {
          settings_list$units <- settings_list$units %>%
            dplyr::filter(!default) %>%
            dplyr::select(-default)
        }

        payload <- list(
          settings = settings_list,
          slope_rules = session$userData$slope_rules(),
          filters = session$userData$applied_filters
        )

        # Determine dataset name
        dataset_name <- tryCatch(
          session$userData$study_ids_label(),
          error = function(e) ""
        )

        # Determine active tab
        active_tab <- tryCatch(
          session$userData$active_tab(),
          error = function(e) ""
        )

        # Create new version entry
        new_version <- create_settings_version(
          settings_data = payload,
          comment = input$save_comment %||% "",
          dataset = dataset_name,
          tab = active_tab
        )

        # Merge with existing versions if available
        existing <- session$userData$settings_versions()
        versions <- add_settings_version(existing, new_version)

        # Update stored versions
        session$userData$settings_versions(versions)

        # Write to file
        write_versioned_settings(versions, file)

        removeModal()
        showNotification("Settings saved.", type = "message")
      }
    )
  })
}
