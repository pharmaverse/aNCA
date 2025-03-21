save_settings_ui <- function(id) {
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

save_settings_server <- function(id, mydata, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$settings_save <- downloadHandler(
      filename = function() {
        fname <- paste0(mydata()$conc$data$STUDYID[1], "_aNCAsetts_", Sys.Date(),
                        ".", input$settings_save_fmt)
      },
      content = function(file) {

        mydata <- mydata()
        conc_cols <- c(unname(unlist(mydata$conc$columns)),
                       "is.included.hl", "is.excluded.hl", "REASON")
        conc_logical_cols <- sapply(mydata$conc$data[conc_cols], is.logical) |>
          which() |>
          names()

        # Save only from concentration records the ones pointing the customizations
        mydata$conc$data <- mydata$conc$data %>%
          filter(is.excluded.hl + is.included.hl > 0) %>%
          select(any_of(c(conc_cols, "DOSNO")))

        mydata$dose$data <- mydata$dose$data %>%
          select(any_of(c(unname(unlist(mydata$dose$columns)), "DOSNO")))

        # Save into mydata the information associated to the flag rules
        rule_inputs_logical <- names(parent_session$input) %>%
          keep(~startsWith(.x, "nca_setup-rule")) %>%
          sapply(., \(x) parent_session$input[[x]])

        threshold_inputs <- names(parent_session$input) %>%
          keep(~startsWith(.x, "nca_setup-") & endsWith(.x, "_threshold")) %>%
          sapply(., \(x) parent_session$input[[x]])

        mydata$flag_rules <- ifelse(rule_inputs_logical, threshold_inputs, NA)
        names(mydata$flag_rules) <- gsub("nca_setup-", "", names(mydata$flag_rules))
        mydata$flag_rules <- stack(mydata$flag_rules)

        # Save the file in the format requested by the user
        if (input$settings_save_fmt == "rds") {
          saveRDS(mydata, file)
        }

        if (input$settings_save_fmt == "xlsx") {
          # Excel files have some limitations  that need to be accounted for to prevent issues
          mydata$options <- as.data.frame(
            c(as.list(mydata$options$single.dose.aucs),
              mydata$options[which(names(mydata$options) != "single.dose.aucs")])
          )
          mydata$intervals <- replace(mydata$intervals, mydata$intervals == Inf, 1e99)
          mydata$options <- replace(mydata$options, mydata$options == Inf, 1e99)

          # Make a standardized list with the PKNCA list elements
          setts_list <- list(
            intervals = mydata$intervals,
            units = mydata$units,
            conc_data = mydata$conc$data,
            conc_columns = perfect_stack(mydata$conc$columns),
            dose_data = mydata$dose$data,
            dose_columns = perfect_stack(mydata$dose$columns),
            flag_rules = mydata$flag_rules,
            options = mydata$options
          )

          # Save the PKNCA list object elements in different sheets
          wb <- openxlsx::createWorkbook(file)
          for (i in seq_len(length(setts_list))) {
            openxlsx::addWorksheet(wb = wb, sheetName = names(setts_list[i]))
            openxlsx::writeData(wb = wb, sheet = names(setts_list[i]), x = setts_list[[i]])
          }
          openxlsx::saveWorkbook(wb, file)
        }
      }
    )
  })
}

# Create a function to pack list objects in a standard data frame format
.perfect_stack <- function(columns_list) {
  stack(unlist(columns_list)) %>%
    mutate(ind = sub("[0-9]+$", "", ind))
}