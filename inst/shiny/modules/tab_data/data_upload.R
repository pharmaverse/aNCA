#' Module responsible for loading and validating raw ADNCA data.
#'
#' @details
#' Upon startup, when no data is provided by the user, the module will return dummy data
#' available with the application. Upon upload, user data will be loaded from .csv or .rds files.
#'
#' @param id ID of the module.
#'
#' @returns A reactive with raw adnca data as provided by the user (or dummy dataset).

data_upload_ui <- function(id) {
  ns <- NS(id)

  div(
    div(
      class = "upload-container",
      id = ns("upload_container"),
      p("Upload your PK dataset and Settings file (optional)."),
      fileInput(
        ns("data_upload"),
        width = "50%",
        label = NULL,
        multiple = TRUE,
        placeholder = paste(names(aNCA:::readers), collapse = ", "),
        buttonLabel = list(icon("folder"), "Upload File...")
      ),
      uiOutput(ns("file_loading_message"))
    ),
    card(reactable_ui(ns("data_display")), class = "border-0 shadow-none")
  )
}

data_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    #' Dummy data is automatically loaded on startup if no data path is provided
    DUMMY_DATA <- adnca_example

    #' Display file loading error if any issues arise
    file_loading_error <- reactiveVal(NULL)
    settings_override <- reactiveVal(NULL) # Store loaded settings
    pending_versioned <- reactiveVal(NULL) # Versioned settings awaiting selection

    output$file_loading_message <- renderUI({
      if (is.null(file_loading_error())) {
        p("")
      } else {
        p(span(HTML(file_loading_error())), class = "error-string")
      }
    })

    datapath <- getOption("aNCA.datapath", NULL)

    raw_data <- (
      reactive({
        # Reset errors at the start of the reactive
        file_loading_error(NULL)

        #' If no data is provided by the user, load dummy data
        if (is.null(input$data_upload$datapath) & is.null(datapath)) {
          return(DUMMY_DATA)
        }

        if (is.null(input$data_upload$datapath)) {
          log_info("Data upload module initialized with datapath: ", datapath)
          paths <- datapath
          filenames <- basename(datapath)
        } else {
          paths <- input$data_upload$datapath
          filenames <- input$data_upload$name
        }

        # Iterate over files: Identify file extension and read either settings or data
        read_results <- purrr::map2(paths, filenames, function(path, name) {
          if (tools::file_ext(path) %in% c("yml", "yaml")) {
            tryCatch({
              obj <- read_settings(path)
              list(status = "success", type = "settings", content = obj, name = name)
            }, error = function(e) {
              list(status = "error", msg = conditionMessage(e), name = name)
            })
          } else {
            tryCatch({
              obj <- read_pk(path)
              list(status = "success", type = "data", content = obj, name = name)
            }, error = function(e) {
              list(status = "error", msg = conditionMessage(e), name = name)
            })
          }
        })

        # Process results
        successful_loads <- purrr::keep(read_results, \(x) x$status == "success")
        errors <- purrr::keep(read_results, \(x) x$status == "error") %>%
          purrr::map(\(x) paste0(x$name, ": ", x$msg))

        # Extract and apply settings if any found
        found_settings <- purrr::keep(successful_loads, \(x) x$type == "settings")

        if (length(found_settings) > 1) {
          # Error: Too many settings files
          errors <- append(errors, "Error: Multiple settings files detected.
                           Please upload only one settings file.")
          # Do not apply any settings if ambiguous
          settings_override(NULL)

        } else if (length(found_settings) == 1) {
          latest <- found_settings[[1]]
          versioned_attr <- attr(latest$content, "versioned")

          if (!is.null(versioned_attr) && length(versioned_attr$versions) > 1) {
            # Versioned file with multiple versions — show selection modal
            pending_versioned(versioned_attr)
            .show_version_modal(session, ns, versioned_attr$versions)
          } else {
            # Single version or legacy — apply directly
            content <- latest$content
            attr(content, "versioned") <- NULL
            settings_override(content)

            # Store versions for future saves
            if (!is.null(versioned_attr)) {
              session$userData$settings_versions(versioned_attr$versions)
            }

            log_success("Settings successfully loaded from ", latest$name)
            showNotification("Settings successfully loaded.", type = "message")
          }
        }

        loaded_data <- DUMMY_DATA

        found_data <- purrr::keep(successful_loads, \(x) x$type == "data")
        # Handle Errors
        if (length(found_data) > 0) {
          tryCatch({
            loaded_data <- found_data %>%
              purrr::map("content") %>%
              dplyr::bind_rows() %>%
              #mutate all to character to prevent errors
              dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

            log_success("All user data loaded successfully.")
          }, error = function(e) {
            # combine errors
            errors <<- append(errors, paste0("Error combining files: ", e$message))
            log_error("Error binding user data: ", e$message)
          })
        }

        if (length(errors) > 0) {
          file_loading_error(paste(errors, collapse = "<br>"))
          log_error("Errors loading files: ", paste(errors, collapse = "; "))
        }

        loaded_data

      }) %>%
        bindEvent(input$data_upload, ignoreNULL = FALSE)
    )

    observeEvent(raw_data(), {
      session$userData$raw_data <- raw_data()
    })

    reactable_server(
      "data_display",
      raw_data,
      height = "50vh",
      pageSizeOptions = reactive(c(10, 25, 50, 100, nrow(raw_data())))
    )

    # Handle version selection from modal
    observeEvent(input$version_select_btn, {
      versioned <- pending_versioned()
      req(versioned)

      selected_idx <- as.integer(input$version_choice)
      if (is.na(selected_idx) || selected_idx < 1 ||
            selected_idx > length(versioned$versions)) {
        showNotification("Invalid version selection.", type = "error")
        return()
      }

      chosen <- versioned$versions[[selected_idx]]
      content <- extract_version_settings(chosen)
      settings_override(content)

      # Store all versions for future saves
      session$userData$settings_versions(versioned$versions)

      removeModal()
      pending_versioned(NULL)

      comment_label <- if (nzchar(chosen$comment)) chosen$comment else chosen$datetime
      log_success("Settings restored from version: ", comment_label)
      showNotification(
        paste0("Settings restored (", comment_label, ")."),
        type = "message"
      )
    })

    # Handle version deletion from modal
    observeEvent(input$version_delete_btn, {
      versioned <- pending_versioned()
      req(versioned)

      selected_idx <- as.integer(input$version_choice)
      if (is.na(selected_idx) || selected_idx < 1 ||
            selected_idx > length(versioned$versions)) {
        showNotification("Invalid version selection.", type = "error")
        return()
      }

      if (length(versioned$versions) <= 1) {
        showNotification("Cannot delete the last remaining version.", type = "warning")
        return()
      }

      updated_versions <- delete_settings_version(versioned$versions, selected_idx)
      updated <- list(versions = updated_versions, format = "versioned")
      pending_versioned(updated)

      # Re-show modal with updated list
      .show_version_modal(session, ns, updated_versions)
      showNotification("Version deleted.", type = "message")
    })

    list(
      adnca_raw = raw_data,
      settings_override = settings_override
    )
  })
}

#' Show a modal for selecting a settings version.
#' @param session Shiny session.
#' @param ns Namespace function.
#' @param versions List of version entries.
#' @noRd
.show_version_modal <- function(session, ns, versions) {
  summary_df <- settings_version_summary(versions)

  choices <- setNames(summary_df$index, paste0(
    ifelse(nzchar(summary_df$comment), summary_df$comment, "(no comment)"),
    " — ", summary_df$datetime,
    ifelse(nzchar(summary_df$dataset), paste0(" [", summary_df$dataset, "]"), "")
  ))

  showModal(modalDialog(
    title = "Select Settings Version",
    p("This settings file contains multiple versions. Select which version to restore."),
    radioButtons(
      ns("version_choice"),
      label = NULL,
      choices = choices,
      selected = 1
    ),
    tags$div(
      style = "margin-top: 8px;",
      reactable::reactable(
        summary_df[, c("comment", "datetime", "dataset", "anca_version", "tab")],
        compact = TRUE,
        bordered = TRUE,
        highlight = TRUE,
        defaultPageSize = 5,
        columns = list(
          comment = reactable::colDef(name = "Comment"),
          datetime = reactable::colDef(name = "Date/Time"),
          dataset = reactable::colDef(name = "Dataset"),
          anca_version = reactable::colDef(name = "aNCA Version"),
          tab = reactable::colDef(name = "Tab")
        )
      )
    ),
    footer = tagList(
      actionButton(ns("version_select_btn"), "Restore", class = "btn-primary"),
      actionButton(ns("version_delete_btn"), "Delete Selected", class = "btn-danger"),
      modalButton("Cancel")
    ),
    easyClose = TRUE,
    size = "l"
  ))
}
