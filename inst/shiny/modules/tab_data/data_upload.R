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
      p("Upload your PK dataset."),
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

    output$file_loading_message <- renderUI({
      if (is.null(file_loading_error())) {
        p("")
      } else {
        p(file_loading_error(), class = "error-string")
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

        # Iterate over files: Read and classify
        read_results <- purrr::map2(paths, filenames, function(path, name) {
          tryCatch({
            # Attempt to read
            data <- read_pk(path)
            list(status = "success", data = data, name = name, type = "data")
          }, error = function(e) {
            # If read_pk fails
            # TODO: @Jana, check if settings file is loaded and then create settings override (719)
            list(status = "error", message = e$message, name = name)
          })
        })

        # Process results
        successful_loads <- purrr::keep(read_results, ~ .x$status == "success")
        errors <- purrr::keep(read_results, \(x) x$status == "error")

        # Handle Errors
        if (length(successful_loads) == 0) {
          error_msgs <- purrr::map_chr(errors, \(x) paste0(.x$name, ": ", .x$message))
          file_loading_error(paste(error_msgs, collapse = "<br>"))
          log_error("Errors loading files: ", paste(error_msgs, collapse = "; "))
          return(DUMMY_DATA)
        }
        # Case: At least some files were read, attempt to bind them
        tryCatch({
          combined_df <- successful_loads %>%
            purrr::map("data") %>%
            dplyr::bind_rows() %>%
            #mutate all to character to prevent errors
            dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

          # If there were partial failures (some read, some didn't), warn the user
          if (length(errors) > 0) {
            error_msgs <- purrr::map_chr(errors, \(x) paste0(.x$name, ": ", .x$message))
            file_loading_error(paste(error_msgs, collapse = "<br>"))
            log_warn("Some files failed to load: ", paste(error_msgs, collapse = "; "))
          } else {
            log_success("All user data loaded successfully.")
          }

          combined_df
        }, error = function(e) {
          # Case: Binding failed (e.g. column mismatch)
          # Combine read errors with the bind error
          bind_error <- paste0("Error combining files: ", e$message)

          if (length(errors) > 0) {
            read_errors <- purrr::map_chr(errors, \(x) paste0(.x$name, ": ", .x$message))
            file_loading_error(paste(c(read_errors, bind_error), collapse = "<br>"))
          } else {
            file_loading_error(bind_error)
          }

          log_error("Error binding user data: ", e$message)
          DUMMY_DATA
        })

      }) %>%
        bindEvent(input$data_upload, ignoreNULL = FALSE)
    )

    reactable_server(
      "data_display",
      raw_data,
      pageSizeOptions = reactive(c(10, 25, 50, 100, nrow(raw_data()))),
      height = "50vh",
      class = "reactable-table",
      style = list(fontSize = "0.75em")
    )

    raw_data
  })
}
