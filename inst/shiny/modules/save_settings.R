save_settings_ui <- function(id) {
  ns <- NS(id)

      fluidRow(
        pickerInput(ns("settings_save_fmt"), "Download format", choices = c("xlsx", "rds"), width = "50%"),
        downloadButton(ns("settings_save"), class = "custom-download-button")
    )
}

save_settings_server <- function(id, mydata, res_nca) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$settings_save <- downloadHandler(
      filename = function() {
        paste0(mydata()$conc$data$STUDYID[1], "_aNCAsetts_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        res <- res_nca()$data

        conc_cols <- c(unname(unlist(res$conc$columns)), "is.included.hl", "is.excluded.hl", "REASON")
        conc_logical_cols <- sapply(res$conc$data[conc_cols], is.logical) |>
          which() |> names()

        res$conc$data <- res$conc$data %>%
          select(any_of(c(conc_cols, "DOSNO"))) %>%
          filter(rowSums(select(., conc_logical_cols)) > 0)

        res$dose$data <- res$dose$data %>%
          select(any_of(c(unname(unlist(res$dose$columns)), "DOSNO")))

        
        ########################################################################################
        # ToDo: Flag rules needs to be modified, currently not working due to no access to nca-settings-rule
        rule_inputs_logical <- names(input) %>%
          keep(~startsWith(.x, "nca_settings-rule")) %>%
          sapply(., \(x) input[[x]])

        threshold_inputs <- names(input) %>%
          keep(~startsWith(.x, "nca_settings-") & endsWith(.x, "_threshold"))

        res$flag_rules <- ifelse(rule_inputs_logical,
                                 sapply(threshold_inputs, \(x) input[[x]]),
                                 NA) %>%
          setNames(nm = gsub("nca_settings-(.*)_threshold$", "\\1", threshold_inputs)) %>%
          as_list() %>%
          as.data.frame()
        ########################################################################################

        if (input$settings_save_fmt == "rds") {
          saveRDS(res, file)
        }

        if (input$settings_save_fmt == "xlsx") {
          res$options <- as.data.frame(c(as.list(res$options$single.dose.aucs),
                                         res$options[which(names(res$options) != "single.dose.aucs")]))
          res$intervals <- replace(res$intervals, res$intervals == Inf, 1e99)
          res$options <- replace(res$options, res$options == Inf, 1e99)

          perfect_stack <- function(columns_list) {
            stack(unlist(columns_list)) %>%
              mutate(ind = sub("[0-9]+$", "", ind))
          }

          setts_list = list(
            intervals = res$intervals,
            units = res$units,
            conc_data = res$conc$data,
            conc_columns = perfect_stack(res$conc$columns),
            dose_data = res$dose$data,
            dose_columns = perfect_stack(res$dose$columns),
            flag_rules = res$flag_rules,
            options = res$options
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