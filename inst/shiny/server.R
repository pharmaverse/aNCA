# This script sources the server logic from the tabs folder
# Define server logic
function(input, output, session) {
  # DATA ----
  data <- tab_data_server("data")
  # NCA ----
  source(system.file("shiny/tabs/nca.R", package = "aNCA"), local = TRUE)
  # OUTPUT ----
  source(system.file("shiny/tabs/outputs.R", package = "aNCA"), local = TRUE)
  # shiny functions #
  system.file("shiny/functions", package = "aNCA") |>
    list.files(full.names = TRUE) |>
    sapply(source)



  #' TODO: migrated this from data tab, dunno what to do yet
  # Update analyte selection input based on the data
  observeEvent(data(), {
    updateSelectInput(
      session,
      inputId = "analyte",
      label = "Choose the analyte :",
      choices = unique(data()$ANALYTE)
    )
  })

  # When an analyte is selected and the user clicks the "Submit" button,
  # create the PKNCA data object
  mydata <- reactiveVal(NULL)
  observeEvent(input$submit_analyte, priority = 2, {
    print("trigered")
    # Segregate the data into concentration and dose records
    df_conc <- create_conc(data(), input$analyte, input$proftype)
    df_dose <- create_dose(df_conc)

    # Define initially a inclusions/exclusions for lambda slope estimation (with no input)
    df_conc$is.excluded.hl <- FALSE
    df_conc$is.included.hl <- FALSE
    df_conc$REASON <- NA  # Exclusions will have preferential reason statements than inclusions
    df_conc$exclude_half.life <- FALSE

    # Make the PKNCA concentration and dose objects
    myconc <- PKNCA::PKNCAconc(
      df_conc,
      formula = AVAL ~ TIME | STUDYID + PCSPEC + ANALYTE + USUBJID / DOSNO,
      exclude_half.life = "exclude_half.life",
      time.nominal = "NFRLT"
    )

    mydose <- PKNCA::PKNCAdose(
      data = df_dose,
      formula = DOSEA ~ TIME | STUDYID + PCSPEC + ANALYTE + USUBJID + DOSNO,
      route = ifelse(toupper(df_dose$IQROUTE) == "EXTRAVASCULAR", "extravascular", "intravascular"),
      time.nominal = "NFRLT",
      duration = "ADOSEDUR"
    )

    # Combine the PKNCA objects into the PKNCAdata object
    # TODO think of case with different units for different analytes
    mydata <- PKNCA::PKNCAdata(
      data.conc = myconc,
      data.dose = mydose,
      units = PKNCA::pknca_units_table(
        concu = myconc$data$PCSTRESU[1],
        doseu = myconc$data$DOSEU[1],
        amountu = myconc$data$PCSTRESU[1],
        timeu = myconc$data$RRLTU[1]
      )
    )
    mydata(mydata)
  })


  # Display the PKNCA data object for the user (concentration records)
  output$datatable <- DT::renderDataTable({
    req(mydata())
    DT::datatable(
      data = mydata()$conc$data,
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        lengthMenu = list(c(10, 25, -1), c("10", "25", "All"))
      )
    )
  })

  # UPLOAD PRECINICAL

  # Custom preclinical file upload

  observeEvent(input$preclinical_upload, {
    # Switch to the 'NCA' > 'Setup' tab
    updateNavbarPage(session, "page", selected = "NCA")
    updateTabsetPanel(session, "ncapanel", selected = "Setup")
  })
}
