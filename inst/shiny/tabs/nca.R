# SERVER LOGIC OF NAVBAR NCA TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This tab is the heart of the application. In this navbar tab we can setup, run and view
# results of the NCA. In the setup tabset we select the anaylte to be processed,
# setup the NCA setting and can do manual slope selection or point exclusion. In the result tabset
# we can view the NCA results, slope caclulation und exclusions table.
# TABSET: Setup ================================================================

# When data is mapped by the user, a PKNCAdata object is created (mydata),
mydata <- reactiveVal(NULL)
observeEvent(data(), priority = 2, {

  req(data())

  # Define explicetely input columns until there are input definitions
  group_columns <- intersect(colnames(data()), c("STUDYID", "PCSPEC", "ROUTE", "DRUG"))
  usubjid_column <- "USUBJID"
  time_column <- "AFRLT"
  dosno_column <- "DOSNO"
  route_column <- "ROUTE"
  analyte_column <- "ANALYTE"
  matrix_column <- "PCSPEC"

  # Segregate the data into concentration and dose records
  df_conc <- format_pkncaconc_data(ADNCA = data(),
                                   group_columns = c(group_columns, usubjid_column, analyte_column),
                                   time_column = time_column) %>%
    dplyr::arrange(across(all_of(c(usubjid_column, time_column))))


  df_dose <- format_pkncadose_data(pkncaconc_data = df_conc,
                                   group_columns = c(group_columns, usubjid_column),
                                   time_column = time_column,
                                   dosno_column = dosno_column,
                                   since_lastdose_time_column = "ARRLT")

  # Define initially a inclusions/exclusions for lambda slope estimation (with no input)
  df_conc$is.excluded.hl <- FALSE
  df_conc$is.included.hl <- FALSE
  df_conc$REASON <- NA  # Exclusions will have preferential reason statements than inclusions
  df_conc$exclude_half.life <- FALSE

  # Make the PKNCA concentration and dose objects
  myconc <- PKNCA::PKNCAconc(
    df_conc,
    formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / ANALYTE,
    exclude_half.life = "exclude_half.life",
    time.nominal = "NFRLT"
  )

  mydose <- PKNCA::PKNCAdose(
    data = df_dose,
    formula = DOSEA ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID,
    route = route_column,
    time.nominal = "NFRLT",
    duration = "ADOSEDUR"
  )

  # Combine the PKNCA objects into the PKNCAdata object
  # TODO think of case with different units for different analytes
  mydata <- PKNCA::PKNCAdata(
    data.conc = myconc,
    data.dose = mydose,
    units = PKNCA::pknca_units_table(
      concu = myconc$data$AVALU[1],
      doseu = myconc$data$DOSEU[1],
      amountu = myconc$data$AVALU[1],
      timeu = myconc$data$RRLTU[1]
    )
  )

  # Redefine units for each analyte and for potential customizations
  unique_analytes <- unique(mydata$conc$data[[mydata$conc$columns$groups$group_analyte]])
  analyte_column <- mydata$conc$columns$groups$group_analyte
  mydata$units <- tidyr::crossing(mydata$units,
                                  !!sym(analyte_column) := unique_analytes)  %>%
    dplyr::mutate(PPSTRESU = PPORRESU, conversion_factor = 1)

  mydata(mydata)
})

# TAB: NCA Settings -------------------------------------------------------------
# Specifies how to run NCA (method, dose/analyte selection, AUC intervals, parameter units, settings upload)
# Other modules within: units_table_server()
nca_settings_server("nca_settings", data, mydata)


# Define a profiles per patient
profiles_per_patient <- reactive({
  req(mydata())
  # Check if res_nca() is available and valid
  if (!is.null(res_nca())) {
    res_nca()$result %>%
      mutate(USUBJID = as.character(USUBJID),
             DOSNO = as.character(DOSNO)) %>%
      group_by(USUBJID, ANALYTE, PCSPEC) %>%
      summarise(DOSNO = unique(DOSNO), .groups = "drop") %>%
      unnest(DOSNO)  # Convert lists into individual rows
  } else {
    mydata()$conc$data %>%
      mutate(USUBJID = as.character(USUBJID)) %>%
      group_by(USUBJID, ANALYTE, PCSPEC) %>%
      summarise(DOSNO = list(unique(DOSNO)), .groups = "drop")
  }
})

# NCA running and obtention of results
myres <- reactiveVal(NULL)
res_nca <- reactiveVal(NULL)
observeEvent(input$nca, {
  req(mydata())

  withProgress(message = "Calculating NCA...", value = 0, {
    tryCatch({
      myres <- PKNCA::pk.nca(data = mydata(), verbose = FALSE)

      # Increment progress to 100% after NCA calculations are complete
      incProgress(0.5, detail = "NCA calculations complete!")

      # Make the starts and ends of results relative to last dose using the dose data
      myres$result <- myres$result %>%
        inner_join(select(mydata()$dose$data, -exclude)) %>%
        mutate(start = start - !!sym(mydata()$dose$columns$time),
                      end = end - !!sym(mydata()$dose$columns$time)) %>%
        select(names(myres$result))

      # Return the results and update panel to show results page
      res_nca(myres)
      updateTabsetPanel(session, "ncapanel", selected = "Results")

    }, error = function(e) {
      full_error <- e$parent$message
      showNotification(
        paste("Error during NCA calculation:", full_error),
        type = "error",
        duration = NULL
      )
    })
  })
})

# TABSET: Results ==============================================================

# In the result tabset we can view the NCA results, slope caclulation und exclusions table.

# TAB: NCA Results -------------------------------------------------------------

# Create a reactive expression to store the reshaped form of the results that
# will be displayed in the UI
final_res_nca <- reactiveVal(NULL)

# creative final_res_nca, aiming to present the results in a more comprehensive way
observeEvent(res_nca(), {
  req(res_nca())
  # Create a reshaped object that will be used to display the results in the UI
  final_res_nca <- pivot_wider_pknca_results(res_nca())

  # Get all inputs which are TRUE and start with 'rule_'
  for (rule_input in grep("^rule_", names(input), value = TRUE)) {
    if (!input[[rule_input]]) next

    pptestcd <- rule_input |>
      gsub("^rule_", "", x = _) |>
      gsub("_", ".", x = _, fixed = TRUE)
    if (startsWith(pptestcd, "auc")) {
      final_res_nca[[paste0("flag_", pptestcd)]] <- {
        final_res_nca[[pptestcd]] >= input[[paste0(pptestcd, "_threshold")]]
      }
    } else {
      final_res_nca[[paste0("flag_", pptestcd)]] <- {
        final_res_nca[[pptestcd]] <= input[[paste0(pptestcd, "_threshold")]]
      }
    }
  }

  # Include units for all column names
  dict_pttestcd_with_units <- res_nca()$result %>%
    select(PPTESTCD, PPSTRESU) %>%
    unique() %>%
    pull(PPSTRESU, PPTESTCD)

  final_res_nca <- final_res_nca %>%
    rename_with(~ifelse(
      gsub("_.*", "", .x) %in% names(dict_pttestcd_with_units),
      paste0(.x, "[", dict_pttestcd_with_units[gsub("_.*", "", .x)], "]"),
      .x
    ))

  # Sort alphabetically all columns but the grouping and the exclude columns
  group_cols <- c(unname(unique(c(unlist(res_nca()$data$conc$columns$groups),
                                  unlist(res_nca()$data$dose$columns$groups)))))
  int_cols <- c("DOSNO", "start", "end")
  exclude_cols <- names(final_res_nca)[startsWith(names(final_res_nca), "exclude.")]
  param_cols <- names(final_res_nca)[endsWith(names(final_res_nca), "]")]
  other_cols <- setdiff(names(final_res_nca), c(group_cols, int_cols, exclude_cols, param_cols))
  final_res_nca <- final_res_nca %>%
    dplyr::select(any_of(c(group_cols, int_cols, param_cols, exclude_cols, other_cols)))

  # Create a reshaped object
  final_res_nca(
    final_res_nca %>%
      # Create a flag_ column when input$rule_r2adj is TRUE, and make it TRUE when the only column
      # that starts with r2adj is below the threshold input$r2adj_threshold
      mutate(flag_onlyFalse = FALSE)  %>%
      # Create a flagged column that informs if any of the variables starting as 'flag_' are TRUE
      # Only if there is at least one column that stats with flag_
      mutate(
        flagged = case_when(
          rowSums(is.na(select(., starts_with("flag_")))) > 0 ~ "MISSING",
          rowSums(select(., starts_with("flag_")), na.rm = TRUE) > 0 ~ "FLAGGED",
          TRUE ~ "ACCEPTED"
        )
      )
  )

  # Allow user to choose the parameters to display of final_res_nca
  updatePickerInput(
    session = session,
    inputId = "params",
    label = "Select Parameters :",
    choices = sort(colnames(final_res_nca())),
    selected = sort(colnames(final_res_nca()))
  )
})

# Render the reshaped results as a DT datatable
output$myresults <- DT::renderDataTable({
  req(final_res_nca())
  DT::datatable(
    data = final_res_nca(),
    extensions = "FixedHeader",
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
      fixedHeader = TRUE,
      columnDefs = list(list(
        visible = FALSE, targets = setdiff(colnames(final_res_nca()), input$params)
      ))
    )
  ) %>%
    formatStyle(
      "flagged",
      target = "row",
      backgroundColor = styleEqual(c("FLAGGED", "MISSING"), c("#f5b4b4", "#cbaddd"))
    )
})


# Download final results
observeEvent(input$download, {
  showModal(modalDialog(
    title = "Please enter the path to the folder on Improve for your results:",
    textInput("pathresults", "Path:"),
    actionButton("go", "GO"),
    footer = modalButton("Close")
  ))
})

## Add download locally option

output$local_download_NCAres <- downloadHandler(
  filename = function() {
    paste0(mydata()$conc$data$STUDYID[1], "PK_Parameters.csv")
  },
  content = function(file) {
    old_wd <- getwd()  # save old working directory
    tempdir <- tempdir()  # create a temporary directory
    setwd(tempdir)  # change working directory to temporary directory

    write.csv(final_res_nca(), file, row.names = FALSE)

    setwd(old_wd)  # change working directory back to original
  }
)

# Save the project settings
output$settings_save <- downloadHandler(
  filename = function() {
    paste(mydata()$conc$data$STUDYID[1], "NCA_settings.csv", sep = "_")
  },
  content = function(file) {

    # Get the data settings from the NCA results (data run)
    myconc <- res_nca()$data$conc

    # Create a settings file that the user can download/upload
    #for establishing the same configuration
    setts_lambda <- myconc$data %>%
      # Identify the points that the user has manually selected for the half-life calculation
      mutate(
        TYPE = case_when(is.excluded.hl ~ "Exclusion", is.included.hl ~ "Selection", TRUE ~ NA)
      ) %>%
      filter(is.excluded.hl | is.included.hl)  %>%
      select(any_of(c(
        unname(unlist(myconc$columns$groups)),
        "IX",
        myconc$columns$time,
        myconc$columns$concentration,
        "TYPE",
        "REASON"
      )))

    # Make sure that there is at least one row so the settings can be considered
    if (nrow(setts_lambda) == 0) {
      setts_lambda <- setts_lambda %>%
        add_row()
    }

    # Consider the intervals defined by the user for the AUC calculation
    input_names_aucmin <- grep("^timeInputMin_", names(input), value = TRUE)
    input_names_aucmax <- grep("^timeInputMax_", names(input), value = TRUE)
    auc_mins <- unlist(lapply(input_names_aucmin, function(name) input[[name]]))
    auc_maxs <- unlist(lapply(input_names_aucmax, function(name) input[[name]]))

    # Include the rule settings as additional columns
    setts <- setts_lambda %>%
      mutate(
        ANALYTE %in% input$select_analyte,
        doses_selected = ifelse(
          !is.null(input$select_dosno),
          paste0(input$select_dosno, collapse = ","),
          unique(mydata()$conc$data$DOSNO)
        ),
        method = input$method,
        adj.r.squared_threshold = ifelse(
          input$rule_adj_r_squared, input$adj.r.squared_threshold, NA
        ),
        aucpext.obs_threshold = ifelse(
          input$rule_aucpext_obs, input$aucpext.obs_threshold, NA
        ),
        aucpext.pred_threshold = ifelse(
          input$rule_aucpext_pred, input$aucpext.pred_threshold, NA
        ),
        span.ratio_threshold = ifelse(
          input$rule_span_ratio, input$span.ratio_threshold, NA
        ),
        auc_mins = if (is.null(auc_mins)) NA else paste(auc_mins, collapse = ","),
        auc_maxs = if (is.null(auc_maxs)) NA else paste(auc_maxs, collapse = ",")
      )

    write.csv(setts, file, row.names = FALSE)
  },
  contentType = "text/csv"
)

# Keep the UI table constantly actively updated
observe({
  for (input_name in grep(
    "(TYPE|PATIENT|PROFILE|IXrange|REASON)_Ex\\d+$", names((input)), value = TRUE
  )) {
    observeEvent(input[[input_name]], {
      # Get the ID of the exclusion
      id <- gsub("_(Ex\\d+)$", "", input_name)

      # Update the reactive list of exclusion IDs
      manual_slopes <- manual_slopes()
      set_selected_value(
        manual_slopes[manual_slopes$id == id, ], paste0(input[[input_name]])
      ) <- manual_slopes[manual_slopes$id == id, ]
      manual_slopes(manual_slopes)

    })
  }
})

# TAB: Slopes ------------------------------------------------------------------

# Slope settings
output$preslopesettings <- DT::renderDataTable({
  # Reshape results and only choose the columns that are relevant to half life calculation
  preslopesettings <- pivot_wider_pknca_results(res_nca())  %>%
    select(
      any_of(c("USUBJID", "DOSNO", "ANALYTE", "PCSPEC")),
      starts_with("lambda.z"),
      starts_with("span.ratio"),
      starts_with("half.life"),
      "exclude.lambda.z"
    )

  # Render the DT datatable object
  DT::datatable(
    data = preslopesettings,
    extensions = "FixedHeader",
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
      pageLength = -1,
      fixedHeader = TRUE
    )
  ) %>%
    formatStyle(
      "exclude.lambda.z",
      target = "row",
      backgroundColor = styleEqual(NA, NA, default = "#f5b4b4")
    )
})

# Slope selector module #
slope_rules <- slope_selector_server(
  "slope_selector",
  mydata,
  res_nca,
  profiles_per_patient,
  pk_nca_trigger,
  reactive(input$settings_upload)
)

#' display rules on Results -> rules tab
output$manual_slopes2 <- renderTable({
  slope_rules()
})

# CDISC ------------------------------------------------------------------------

# Parameter datasets ------------------------------------------------------------------------

# Create CDISC parameter datasets (PP, ADPP)
observeEvent(res_nca(), {
  CDISC <- export_cdisc(res_nca())

  output$pp_dataset <- DT::renderDataTable({
    DT::datatable(
      data = CDISC$pp,
      rownames = FALSE,
      extensions = c("FixedHeader", "Buttons"),
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        fixedHeader = TRUE,
        autoWidth = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(
            extend = "copy",
            title = paste0("PP_Dataset", "_", Sys.Date())
          ),
          list(
            extend = "csv",
            title = paste0("PP_Dataset", "_", Sys.Date())
          ),
          list(
            extend = "excel",
            title = paste0("PP_Dataset", "_", Sys.Date())
          )
        )
      )
    )
  }, server = FALSE)

  output$adpp_dataset <- DT::renderDataTable({
    DT::datatable(
      data = CDISC$adpp,
      extensions = c("FixedHeader", "Buttons"),
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
        fixedHeader = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(
            extend = "copy",
            title = paste0("ADPP_Dataset", "_", Sys.Date())
          ),
          list(
            extend = "csv",
            title = paste0("ADPP_Dataset", "_", Sys.Date())
          ),
          list(
            extend = "excel",
            title = paste0("ADPP_Dataset", "_", Sys.Date())
          )
        )
      )
    )
  })
})
