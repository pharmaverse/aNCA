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
    reactable_ui(ns("data_display"))
  )
}

data_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    #' Dummy data is automatically loaded on startup if no data path is provided
    DUMMY_DATA <- read.csv(
      system.file("shiny/data/example-ADNCA.csv", package = "aNCA"),
      na.strings = c("", "NA")
    )

    #' Display file loading error if any issues arise
    file_loading_error <- reactiveVal(NULL)
    settings_override <- reactiveVal(NULL) # Store loaded settings

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

        # Iterate over files: Try reading as Data, then as Settings
        read_results <- purrr::map2(paths, filenames, .read_uploaded_file)

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
          # Success: Single settings file
          latest <- found_settings[[1]]
          settings_override(latest$content)
          log_success("Settings successfully loaded from ", latest$name)
          showNotification(paste("Settings successfully loaded."), type = "message")
        }

        loaded_data <- DUMMY_DATA

        found_data <- purrr::keep(successful_loads, \(x) x$type == "data")
        # Handle Errors
        if (length(found_data) > 0) {
          tryCatch({
            loaded_data <- successful_loads %>%
              purrr::map("data") %>%
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
      pageSizeOptions = reactive(c(10, 25, 50, 100, nrow(raw_data()))),
      height = "50vh",
      class = "reactable-table",
      style = list(fontSize = "0.75em")
    )

    list(
      adnca_raw = raw_data,
      settings_override = settings_override
    )
  })
}

# Helper Main file reader: attempts Data read, falls back to Settings read
.read_uploaded_file <- function(path, name) {
  tryCatch({
    # Attempt 1: Read as Data
    data <- read_pk(path)
    list(status = "success", data = data, name = name, type = "data")
  }, error = function(e_pk) {
    # Attempt 2: Read as Settings
    tryCatch({
      settings <- .parse_settings_yaml(path, name)
      if (!is.null(settings)) {
        list(status = "success", content = settings, name = name, type = "settings")
      } else {
        stop(e_pk$message) # Throw original PK error if not settings
      }
    }, error = function(e_yaml) {
      list(status = "error", msg = e_pk$message, name = name)
    })
  })
}

# Helper Logic to parse and structure settings YAML
.parse_settings_yaml <- function(path, name) {
  # Check extension
  is_yaml <- tolower(tools::file_ext(name)) %in% c("yaml", "yml")

  if (!is_yaml) return(NULL)

  obj <- yaml::read_yaml(path)

  if (!is.list(obj) || !"settings" %in% names(obj)) return(NULL)

  if (!is.null(obj$slope_rules)) {
    obj$slope_rules$manual_slopes <- .bind_settings_list(obj$slope_rules$manual_slopes)
    obj$slope_rules$profiles_per_subject <- .bind_settings_list(obj$slope_rules$manual_slopes)
  }

  if (!is.null(obj$settings)) {
    obj$settings$partial_aucs <- .bind_settings_list(obj$settings$partial_aucs)
    obj$settings$units <- .bind_settings_list(obj$settings$units)
  }

  obj
}

# Helper to standardize binding lists within settings
.bind_settings_list <- function(obj_list) {
  if (!is.null(obj_list) && is.list(obj_list)) dplyr::bind_rows(obj_list) else obj_list
}
