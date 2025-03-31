download_settings_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    pickerInput(
      ns("settings_save_fmt"),
      "Format",
      choices = c("xlsx", "rds"),
      width = "30%"
    ),
    downloadButton(ns("settings_save"), class = "custom-download-button",
                   label = "Save Project Settings")
  )
}

#' Save Settings Server Module
#'
#' The module handles the server logic for saving project settings from NCA setup & slope adj.
#' The file can be downloaded in two formats: RDS or XLSX.
#'
#' - id The module's ID.
#' - processed_pknca_data A reactive expression containing the project data to be saved.
#' - parent_session The parent Shiny session.
#'
download_settings_server <- function(id, processed_pknca_data, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$settings_save <- downloadHandler(
      filename = function() {
        fname <- paste0(processed_pknca_data()$conc$data$STUDYID[1], "_aNCAsetts_", Sys.Date(),
                        ".", input$settings_save_fmt)
      },
      content = function(file) {

        processed_pknca_data <- processed_pknca_data()
        conc_cols <- c(unname(unlist(processed_pknca_data$conc$columns)),
                       "is.included.hl", "is.excluded.hl", "REASON")
        conc_logical_cols <- sapply(processed_pknca_data$conc$data[conc_cols], is.logical) |>
          which() |>
          names()

        # Save only from concentration records the ones pointing the customizations
        processed_pknca_data$conc$data <- processed_pknca_data$conc$data %>%
          filter(is.excluded.hl + is.included.hl > 0) %>%
          select(any_of(c(conc_cols, "DOSNO")))

        processed_pknca_data$dose$data <- processed_pknca_data$dose$data %>%
          select(any_of(c(unname(unlist(processed_pknca_data$dose$columns)), "DOSNO")))

        # Save into processed_pknca_data the information associated to the flag rules
        rule_inputs_logical <- names(parent_session$input) %>%
          keep(~startsWith(.x, "nca_setup-rule")) %>%
          sapply(., \(x) parent_session$input[[x]])

        threshold_inputs <- names(parent_session$input) %>%
          keep(~startsWith(.x, "nca_setup-") & endsWith(.x, "_threshold")) %>%
          sapply(., \(x) parent_session$input[[x]])

        processed_pknca_data$flag_rules <- ifelse(rule_inputs_logical, threshold_inputs, NA)
        names(processed_pknca_data$flag_rules) <- gsub("nca_setup-", "",
                                                       names(processed_pknca_data$flag_rules))
        processed_pknca_data$flag_rules <- stack(processed_pknca_data$flag_rules)

        # Save the file in the format requested by the user
        switch(
          input$settings_save_fmt,
          "rds" = saveRDS(processed_pknca_data, file),
          "xlsx" = {
            # Excel files have some limitations  that need to be accounted for to prevent issues
            processed_pknca_data$options <- as.data.frame(
              c(
                as.list(processed_pknca_data$options$single.dose.aucs),
                processed_pknca_data$options[
                  which(names(processed_pknca_data$options) != "single.dose.aucs")
                ]
              )
            )
            processed_pknca_data$intervals <- replace(processed_pknca_data$intervals,
                                                      processed_pknca_data$intervals == Inf, 1e99)
            processed_pknca_data$options <- replace(processed_pknca_data$options,
                                                    processed_pknca_data$options == Inf, 1e99)

            # Make a standardized list with the PKNCA list elements
            setts_list <- list(
              intervals = processed_pknca_data$intervals,
              units = processed_pknca_data$units,
              conc_data = processed_pknca_data$conc$data,
              conc_columns = .perfect_stack(processed_pknca_data$conc$columns),
              dose_data = processed_pknca_data$dose$data,
              dose_columns = .perfect_stack(processed_pknca_data$dose$columns),
              flag_rules = processed_pknca_data$flag_rules,
              options = processed_pknca_data$options
            )

            # Save the PKNCA list object elements in different sheets
            wb <- openxlsx::createWorkbook(file)
            for (i in seq_len(length(setts_list))) {
              openxlsx::addWorksheet(wb = wb, sheetName = names(setts_list[i]))
              openxlsx::writeData(wb = wb, sheet = names(setts_list[i]), x = setts_list[[i]])
            }
            openxlsx::saveWorkbook(wb, file)
          }
        )
      }
    )
  })
}

# Create a function to pack list objects in a standard data frame format
.perfect_stack <- function(columns_list) {
  stack(unlist(columns_list)) %>%
    mutate(ind = sub("[0-9]+$", "", ind))
}
