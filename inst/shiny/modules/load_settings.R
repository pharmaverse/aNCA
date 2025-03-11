load_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(
      ns("settings_upload"),
      width = "60%",
      label = "Upload Settings",
      buttonLabel = list(icon("folder"), "Browse"),
      accept = c(".xlsx", ".rds")
    )
  )
}

load_settings_server <- function(id, mydata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$settings_upload, {
      file_path <- input$settings_upload$datapath
      
      if (tools::file_ext(file_path) == "rds") {
        setts <- readRDS(input$settings_upload$datapath)
      } else if (tools::file_ext(file_path) == "xlsx") {
        sheets <- openxlsx::getSheetNames(file_path)
        setts <- lapply(sheets, function(sheet) openxlsx::read.xlsx(file_path, sheet = sheet))
        names(setts) <- sheets
        
        setts <- list(
          conc = list(
            data = setts$conc_data,
            columns = list(
              groups = list(
                group_analyte = setts$conc_columns %>% filter(ind == "groups.group_analyte") %>% pull(values),
                group_group_vars = setts$conc_columns %>% filter(ind == "groups.group_vars") %>% pull(values)
              ),
              time = setts$conc_columns %>% filter(ind == "time") %>% pull(values),
              concentration = setts$conc_columns %>% filter(ind == "concentration") %>% pull(values),
              subject = setts$conc_columns %>% filter(ind == "subject") %>% pull(values),
              time.nominal = setts$conc_columns %>% filter(ind == "time.nominal") %>% pull(values),
              exclude = setts$conc_columns %>% filter(ind == "exclude") %>% pull(values),
              duration = setts$conc_columns %>% filter(ind == "duration") %>% pull(values),
              exclude_half.life = setts$conc_columns %>% filter(ind == "exclude_half.life") %>% pull(values)
            )
          ),
          dose = list(
            data = setts$dose_data,
            columns = list(
              groups = list(
                group_vars = setts$dose_columns %>% filter(ind == "groups.group_vars") %>% pull(values)
              ),
              time = setts$dose_columns %>% filter(ind == "time") %>% pull(values),
              dose = setts$dose_columns %>% filter(ind == "dose") %>% pull(values),
              time.nominal = setts$dose_columns %>% filter(ind == "time.nominal") %>% pull(values),
              exclude = setts$dose_columns %>% filter(ind == "exclude") %>% pull(values),
              duration = setts$dose_columns %>% filter(ind == "duration") %>% pull(values)
            )
          ),
          intervals = setts$intervals,
          options = setts$options,
          units = setts$units,
          flag_rules = setts$flag_rules
        )
      }
      
      update_with_setts_selector_selected <- function(var_setts_col, var_data_col, setts.df, data.df, session, inputId) {
        if (!is.null(var_data_col) && !is.null(var_setts_col)) {
          vals_data <- unique(data.df[[var_data_col]])
          vals_setts <- unique(setts.df[[var_setts_col]])
          
          selected_vals <- intersect(vals_setts, vals_data)
          
          if (length(selected_vals) < length(vals_setts)) {
            showNotification(
              validate("The ", attr(data.df[[var_data_col]], "label"), ":",
                       setdiff(analytes_setts, selected_vals),
                       "selected in the settings file is not present in the data."),
              type = "warning"
            )
          }
          
          updateSelectInput(
            session,
            inputId = inputId,
            choices = data.df[[var_data_col]],
            selected = selected_vals
          )
        } else return()
      }
      
      data <- mydata()
      
      update_with_setts_selector_selected("DOSNO", "DOSNO", setts.df = setts, data.df = mydata()$conc$data, session = session, inputId = "select_dosno")
      update_with_setts_selector_selected(var_setts_col = setts$conc$columns$groups$group_analyte,
                                          var_data_col = mydata()$conc$columns$groups$group_analyte,
                                          setts.df = setts$intervals, data.df = mydata()$conc$data, session, "select_analyte")
      update_with_setts_selector_selected("PCSPEC", "PCSPEC", setts, data, session, "select_pcspec")
      
      updateSelectInput(
        session,
        inputId = "method",
        label = "Extrapolation Method:",
        choices = c("lin-log", "lin up/log down", "linear"),
        selected = setts$options$auc.method
      )
      
      all_param_options <- setdiff(names(PKNCA::get.interval.cols()), c("start", "end"))
      
      params_setts <- setts$intervals %>%
        filter(type_interval == "main") %>%
        mutate(across(all_of(all_param_options), ~ifelse(. == FALSE & is.logical(.), NA, .))) %>%
        select(any_of(all_param_options)) %>%
        select_if(~any(!is.na(.))) %>%
        names()
      
      updatePickerInput(
        session,
        inputId = ns("nca_params"),
        selected = params_setts
      )
      
      updatePickerInput(
        session,
        inputId = "nca_params",
        selected = params_setts
      )
      
      data$units <- dplyr::left_join(data$units, setts$units, 
                                     by = c("ANALYTE", "PPTESTCD"),
                                     suffix = c(".data", ".setts")) %>%
        mutate(across(ends_with(".setts"), ~coalesce(., get(str_replace(cur_column(), ".setts", ".data"))))) %>%
        select(-ends_with(".data")) %>%
        rename_with(~str_remove(., ".setts"))
      
      if (any(grepl("start.*", setts$intervals$impute))) {
        updateCheckboxInput(session, inputId = "should_impute_c0", value = TRUE)
      } else {
        updateCheckboxInput(session, inputId = "should_impute_c0", value = FALSE)
      }
      
      dose_time_col <- setts$dose$columns$time
      intervals_userinput_setts <- setts$intervals %>%
        ungroup() %>%
        filter(type_interval == "manual") %>%
        left_join(setts$dose$data, by = c(unlist(unname(setts$dose$columns$groups)),"DOSNO")) %>%
        mutate(start = start - !!sym(dose_time_col),
               end = end - !!sym(dose_time_col)) %>%
        select(start, end) %>%
        filter(!duplicated(paste0(start, end)))
      
      if ((nrow(intervals_userinput_setts) > 0)) {
        updateCheckboxInput(session, inputId = "AUCoptions",
                            label = "Select Partial AUC", value = TRUE)
        
        for (i in seq_along(nrow(intervals_userinput_setts))) {
          if (auc_counter() >= i){
            removeUI(selector = paste0("#", ns(paste0("AUC_", i))))
          }
          
          insertUI(
            selector = paste0("#", ns("AUCInputs")),
            where = "beforeEnd",
            ui = partial_auc_input(
              id = paste0("AUC_", auc_counter()),
              ns = ns,
              min_sel_value = as.numeric(intervals_userinput_setts$start[i]),
              max_sel_value = as.numeric(intervals_userinput_setts$end[i])
            )
          )
        }
        auc_counter(nrow(intervals_userinput_setts))
      }
      
      if (!is.na(setts$flag_rules$adj.r.squared)) {
        updateCheckboxInput(session, inputId = "rule_adj.r.squared", value = TRUE)
        updateNumericInput(session = session, inputId = "adj.r.squared_threshold", value = setts$flag_rules$adj.r.squared)
      } else {
        updateCheckboxInput(session, inputId = "rule_adj.r.squared", value = FALSE)
      }
      
      if (!is.null(setts$options$flag_rules$aucpext.obs)) {
        updateCheckboxInput(session, inputId = "rule_aucpext.obs", value = TRUE)
        updateNumericInput(session, inputId = "aucpext.obs_threshold", value = setts$lag_rules$aucpext.obs)
      } else {
        updateCheckboxInput(session, inputId = "rule_aucpext.obs", value = FALSE)
      }
      
      if (!is.null(setts$options$flag_rules$aucpext.pred)) {
        updateCheckboxInput(session, inputId = "rule_aucpext.pred", value = TRUE)
        updateNumericInput(session, inputId = "aucpext.pred_threshold", value = setts$flag_rules$aucpext.pred)
      } else {
        updateCheckboxInput(session, inputId = "rule_aucpext.pred", value = FALSE)
      }
      
      if (!is.null(setts$options$flag_rules$span.ratio)) {
        updateCheckboxInput(session, inputId = "rule_span.ratio", value = TRUE)
        updateNumericInput(session, inputId = "span.ratio_threshold", value = setts$flag_rules$span.ratio)
      } else {
        updateCheckboxInput(session, inputId = "rule_span.ratio", value = FALSE)
      }
      
      conc_setts_columns <- unname(unlist(setts$conc$columns[c("groups", "time", "concentration")]))
      
      if (nrow(setts$conc$data) > 0) {
        data$conc$data <- dplyr::left_join(data$conc$data,
                                           setts$conc$data %>% select(any_of(c(conc_setts_columns, "is.included.hl", "is.excluded.hl", "exclude_half.life"))), 
                                           by = conc_setts_columns,
                                           suffix = c(".data", ".setts")) %>%
          mutate(across(ends_with(".setts"), ~coalesce(., get(str_replace(cur_column(), ".setts", ".data"))))) %>%
          select(-ends_with(".data")) %>%
          rename_with(~str_remove(., ".setts"))
        
        mismatched_rows <- setts$conc$data %>%
          anti_join(data$conc$data, by = conc_setts_columns)
        
        if (nrow(mismatched_rows) > 0) {
          showModal(modalDialog(
            title = "Mismatched Data Points",
            tags$h4("The following data points in the settings file do not match the uploaded dataset.
                    The slopes for these profiles will be reset to best slope:"),
            DTOutput(ns("mismatched_table")),
            easyClose = TRUE,
            footer = NULL
          ))
          
          output$mismatched_table <- DT::renderDT({
            datatable(mismatched_points %>% select(-IX))
          })
        }
      }
    })
  })
}