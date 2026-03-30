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
    ns <- session$ns

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
        file_loading_error(NULL)

        upload_paths <- .resolve_upload_paths(
          input$data_upload, datapath, session
        )
        if (is.null(upload_paths)) {
          session$userData$dataset_filename <- "adnca_example"
          return(DUMMY_DATA)
        }

        .process_uploaded_files(
          upload_paths$paths, upload_paths$filenames,
          DUMMY_DATA, settings_override, pending_versioned,
          file_loading_error, session, ns
        )
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

    output$version_table <- renderReactable({
      versioned <- pending_versioned()
      req(versioned)
      .build_version_reactable(versioned$versions)
    })

    .get_selected_version <- function() {
      getReactableState("version_table", "selected")
    }

    observeEvent(input$version_select_btn, {
      .handle_version_restore(
        pending_versioned, .get_selected_version,
        settings_override, session
      )
    })

    observeEvent(input$version_delete_btn, {
      .handle_version_delete(pending_versioned, .get_selected_version)
    })

    list(
      adnca_raw = raw_data,
      settings_override = settings_override
    )
  })
}

#' Resolve file paths and names from upload input or datapath option.
#' @param data_upload The file input value (input$data_upload).
#' @param datapath Option-based data path.
#' @param session Shiny session.
#' @returns A list with `paths` and `filenames`, or NULL if no data.
#' @noRd
.resolve_upload_paths <- function(data_upload, datapath, session) {
  if (is.null(data_upload$datapath) && is.null(datapath)) {
    return(NULL)
  }

  if (is.null(data_upload$datapath)) {
    log_info("Data upload module initialized with datapath: ", datapath)
    filenames <- basename(datapath)
    session$userData$dataset_filename <- paste(filenames, collapse = ", ")
    list(paths = datapath, filenames = filenames)
  } else {
    list(paths = data_upload$datapath, filenames = data_upload$name)
  }
}

#' Read, classify, and combine uploaded files.
#' @param paths Character vector of file paths.
#' @param filenames Character vector of original file names.
#' @param fallback Default data.frame when no data files are found.
#' @param settings_override reactiveVal for settings.
#' @param pending_versioned reactiveVal for versioned settings.
#' @param file_loading_error reactiveVal for error display.
#' @param session Shiny session.
#' @param ns Namespace function.
#' @returns A data.frame of loaded data.
#' @noRd
.process_uploaded_files <- function(paths, filenames, fallback,
                                    settings_override, pending_versioned,
                                    file_loading_error, session, ns) {
  read_results <- purrr::map2(paths, filenames, .read_uploaded_file)

  successful_loads <- purrr::keep(read_results, \(x) x$status == "success")
  errors <- purrr::keep(read_results, \(x) x$status == "error") %>%
    purrr::map(\(x) paste0(x$name, ": ", x$msg))

  errors <- .apply_uploaded_settings(
    successful_loads, errors, settings_override,
    pending_versioned, session, ns
  )

  loaded_data <- .combine_uploaded_data(successful_loads, fallback, session)
  if (!is.null(loaded_data$error)) {
    errors <- append(errors, loaded_data$error)
  }

  if (length(errors) > 0) {
    file_loading_error(paste(errors, collapse = "<br>"))
    log_error("Errors loading files: ", paste(errors, collapse = "; "))
  }

  loaded_data$data
}

#' Build a reactable for version selection.
#' @param versions List of version entries.
#' @returns A reactable widget.
#' @noRd
.build_version_reactable <- function(versions) {
  summary_df <- settings_version_summary(versions)
  display_df <- summary_df[, c(
    "datetime", "anca_version", "tab", "dataset", "comment"
  )]

  reactable(
    display_df,
    selection = "single",
    defaultSelected = 1,
    onClick = "select",
    compact = TRUE,
    bordered = TRUE,
    highlight = TRUE,
    defaultPageSize = 10,
    theme = reactableTheme(
      rowSelectedStyle = list(
        backgroundColor = "#CCE5FF",
        boxShadow = "inset 2px 0 0 0 #0d6efd"
      )
    ),
    columns = list(
      datetime = colDef(name = "Date/Time", minWidth = 240),
      anca_version = colDef(name = "aNCA Version", minWidth = 165),
      tab = colDef(name = "Tab", minWidth = 105),
      dataset = colDef(name = "Dataset", minWidth = 180),
      comment = colDef(name = "Comment", minWidth = 210)
    )
  )
}

#' Handle restoring a selected version from the modal.
#' @param pending_versioned reactiveVal with versioned data.
#' @param get_selected Function returning selected row index.
#' @param settings_override reactiveVal for settings.
#' @param session Shiny session.
#' @noRd
.handle_version_restore <- function(pending_versioned, get_selected,
                                    settings_override, session) {
  versioned <- pending_versioned()
  req(versioned)

  selected_idx <- get_selected()
  if (is.null(selected_idx) || length(selected_idx) == 0) {
    showNotification("Please select a version first.", type = "warning")
    return()
  }

  chosen <- versioned$versions[[selected_idx]]
  content <- tryCatch(
    extract_version_settings(chosen),
    error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    }
  )
  if (is.null(content)) return()

  settings_override(content)

  session$userData$settings_versions(versioned$versions)

  removeModal()
  pending_versioned(NULL)

  comment_label <- if (nzchar(chosen$comment)) {
    chosen$comment
  } else {
    chosen$datetime
  }
  log_success("Settings restored from version: ", comment_label)
  showNotification(
    paste0("Settings restored (", comment_label, ")."),
    type = "message"
  )
}

#' Handle deleting a selected version from the modal.
#' @param pending_versioned reactiveVal with versioned data.
#' @param get_selected Function returning selected row index.
#' @noRd
.handle_version_delete <- function(pending_versioned, get_selected) {
  versioned <- pending_versioned()
  req(versioned)

  selected_idx <- get_selected()
  if (is.null(selected_idx) || length(selected_idx) == 0) {
    showNotification("Please select a version to delete.", type = "warning")
    return()
  }

  if (length(versioned$versions) <= 1) {
    showNotification(
      "Cannot delete the last remaining version.",
      type = "warning"
    )
    return()
  }

  updated_versions <- delete_settings_version(
    versioned$versions, selected_idx
  )
  updated <- list(versions = updated_versions, format = "versioned")
  pending_versioned(updated)
  showNotification("Version deleted.", type = "message")
}

#' Read a single uploaded file and return a status list.
#' @param path File path.
#' @param name Original filename.
#' @returns A list with status, type, content/msg, and name.
#' @noRd
.read_uploaded_file <- function(path, name) {
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
}

#' Process uploaded settings files: validate count, handle versioned vs legacy.
#' @param successful_loads List of successful load results.
#' @param errors Current error list.
#' @param settings_override reactiveVal to store settings.
#' @param pending_versioned reactiveVal for versioned settings.
#' @param session Shiny session.
#' @param ns Namespace function.
#' @returns Updated error list.
#' @noRd
.apply_uploaded_settings <- function(successful_loads, errors,
                                     settings_override, pending_versioned,
                                     session, ns) {
  found_settings <- purrr::keep(successful_loads, \(x) x$type == "settings")

  if (length(found_settings) > 1) {
    errors <- append(errors, "Error: Multiple settings files detected.
                     Please upload only one settings file.")
    settings_override(NULL)
  } else if (length(found_settings) == 1) {
    latest <- found_settings[[1]]
    versioned_attr <- attr(latest$content, "versioned")

    if (!is.null(versioned_attr) && length(versioned_attr$versions) > 1) {
      pending_versioned(versioned_attr)
      .show_version_modal(session, ns, versioned_attr$versions)
    } else if (!is.null(versioned_attr) && length(versioned_attr$versions) == 1) {
      content <- tryCatch(
        extract_version_settings(versioned_attr$versions[[1]]),
        error = function(e) {
          errors <<- append(errors, conditionMessage(e))
          NULL
        }
      )
      if (!is.null(content)) {
        settings_override(content)
        session$userData$settings_versions(versioned_attr$versions)
        log_success("Settings successfully loaded from ", latest$name)
        showNotification("Settings successfully loaded.", type = "message")
      }
    } else {
      settings_override(latest$content)
      log_success("Settings successfully loaded from ", latest$name)
      showNotification("Settings successfully loaded.", type = "message")
    }
  }

  errors
}

#' Combine uploaded data files into a single data.frame.
#' @param successful_loads List of successful load results.
#' @param fallback Default data to return if no data files found.
#' @param session Shiny session (for storing dataset_filename).
#' @returns A list with `data` and optional `error`.
#' @noRd
.combine_uploaded_data <- function(successful_loads, fallback, session) {
  found_data <- purrr::keep(successful_loads, \(x) x$type == "data")
  loaded_data <- fallback
  error <- NULL

  if (length(found_data) > 0) {
    tryCatch({
      loaded_data <- found_data %>%
        purrr::map("content") %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

      data_names <- purrr::map_chr(found_data, "name")
      session$userData$dataset_filename <- paste(
        data_names, collapse = ", "
      )

      log_success("All user data loaded successfully.")
    }, error = function(e) {
      error <<- paste0("Error combining files: ", e$message)
      log_error("Error binding user data: ", e$message)
    })
  }

  list(data = loaded_data, error = error)
}

#' Show a modal for selecting a settings version.
#' @param session Shiny session.
#' @param ns Namespace function.
#' @param versions List of version entries.
#' @noRd
.show_version_modal <- function(session, ns, versions) {
  showModal(modalDialog(
    title = "Select Settings Version",
    p("Select a row to choose which version to restore."),
    reactableOutput(ns("version_table")),
    footer = tagList(
      actionButton(ns("version_select_btn"), "Restore", class = "btn-primary"),
      actionButton(
        ns("version_delete_btn"), "Delete Selected",
        class = "btn-danger"
      ),
      modalButton("Cancel")
    ),
    easyClose = TRUE,
    size = "l"
  ))
}
