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

save_settings_server <- function(id, mydata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$settings_save <- downloadHandler(
      filename = function() {
        paste0(mydata()$conc$data$STUDYID[1], "_aNCAsetts_", Sys.Date(), ".xlsx")
      },
      content = function(file) {

        mydata <- mydata()

        conc_cols <- c(unname(unlist(mydata$conc$columns)),
                       "is.included.hl", "is.excluded.hl", "REASON")
        conc_logical_cols <- sapply(mydata$conc$data[conc_cols], is.logical) |>
          which() |>
          names()

        mydata$conc$data <- mydata$conc$data %>%
          select(any_of(c(conc_cols, "DOSNO")))
        mydata$conc$data <- mydata$conc$data[rowSums(mydata$conc$data[, conc_logical_cols]) > 0, ]

        mydata$dose$data <- mydata$dose$data %>%
          select(any_of(c(unname(unlist(mydata$dose$columns)), "DOSNO")))

        ########################################################################################
        # ToDo: Flag rules needs to be modified, currently not working (no access to input names)
        # ToDo: Needs to be bound to PKNCA object or similar strategy for a better solution
        rule_inputs_logical <- names(input) %>%
          keep(~startsWith(.x, "nca_settings-rule")) %>%
          sapply(., \(x) input[[x]])

        threshold_inputs <- names(input) %>%
          keep(~startsWith(.x, "nca_settings-") & endsWith(.x, "_threshold"))

        mydata$flag_rules <- ifelse(rule_inputs_logical,
                                    sapply(threshold_inputs, \(x) input[[x]]),
                                    NA) %>%
          setNames(nm = gsub("nca_settings-(.*)_threshold$", "\\1", threshold_inputs)) %>%
          as_list() %>%
          as.data.frame()
        ########################################################################################

        if (input$settings_save_fmt == "rds") {
          saveRDS(mydata, file)
        }

        if (input$settings_save_fmt == "xlsx") {
          mydata$options <- as.data.frame(
            c(as.list(mydata$options$single.dose.aucs),
              mydata$options[which(names(mydata$options) != "single.dose.aucs")])
          )
          mydata$intervals <- replace(mydata$intervals, mydata$intervals == Inf, 1e99)
          mydata$options <- replace(mydata$options, mydata$options == Inf, 1e99)

          perfect_stack <- function(columns_list) {
            stack(unlist(columns_list)) %>%
              mutate(ind = sub("[0-9]+$", "", ind))
          }

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