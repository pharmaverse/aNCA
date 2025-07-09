#' UI function for the variable browser
#' This function creates the UI for the variable browser module.
#' @param id The module ID
#' @returns A Shiny tag list containing the UI elements for the variable browser
#' 
#' @details
#' The UI consists of a tabset panel with a single tab for ADNCA data.
#' It includes a variable browser for the User to select variables in the data
#' which then generates a histogram or bar chart displaying the distribution of the selected variable.
#' There is also a summary table underneath containing summary statistics of the variable.

variable_browser_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    teal.widgets::standard_layout(
      output = fluidRow(
        column(
          6,
          teal.widgets::white_small_well(
            uiOutput(ns("ui_variable_browser")),
          )
        ),
        column(
          6,
          teal.widgets::white_small_well(
            tags$div(
              class = "block",
              uiOutput(ns("ui_histogram_display"))
            ),
            tags$div(
              class = "block",
              uiOutput(ns("ui_numeric_display"))
            ),
            uiOutput(ns("plot_ui")),
            tags$br(),
            teal.widgets::panel_item(
              title = "Plot settings",
              collapsed = TRUE,
              selectInput(
                inputId = ns("ggplot_theme"), label = "ggplot2 theme",
                choices = c(
                  "bw", "grey", "linedraw", "light", "dark", "minimal", "classic", "void"
                ),
                selected = "bw"
              ),
              fluidRow(
                column(6, sliderInput(
                  inputId = ns("font_size"), label = "font size",
                  min = 5L, max = 30L, value = 15L, step = 1L, ticks = FALSE
                )),
                column(6, sliderInput(
                  inputId = ns("label_rotation"), label = "rotate x labels",
                  min = 0L, max = 90L, value = 45L, step = 1, ticks = FALSE
                ))
              )
            ),
            tags$br(),
            teal.widgets::get_dt_rows(ns("variable_summary_table"), ns("variable_summary_table_rows")),
            DT::dataTableOutput(ns("variable_summary_table"))
          )
        )
      )
    )
  )
}


#' Variable browser server function
#' This function handles the server-side logic for the variable browser module.
#' @param id The module ID
#' @param data_list_reactive A reactive list containing the datasets
#' @param ggplot2_args_param A parameter for ggplot2 arguments
#' @returns A Shiny module server function that manages the variable browser logic.
variable_browser_server <- function(id, data_list_reactive,
                                    ggplot2_args_param = teal.widgets::ggplot2_args()) {
  moduleServer(id, function(input, output,
               session) {
    
    ns <- session$ns
  
    .unique_records_for_factor <- 30
    .unique_records_default_as_factor <- 6
    
    varname_numeric_as_factor <- reactiveValues()
    
    output$ui_variable_browser <- renderUI({
      tabsetPanel(
        id = ns("tabset_panel"),
        tabPanel(
          "ADNCA",
          tags$div(
            class = "mt-4",
            textOutput(ns("dataset_summary_ADNCA"))
          ),
          tags$div(
            class = "mt-4",
            teal.widgets::get_dt_rows(
              ns("variable_browser_ADNCA"),
              ns("variable_browser_ADNCA_rows")
            ),
            DT::dataTableOutput(ns("variable_browser_ADNCA"), width = "100%")
          )
        )
      )
    })
    
    columns_names <- new.env()
    
    plot_var <- reactiveValues(data = NULL, variable = list())
    
    observe({
      establish_updating_selection("ADNCA", input, plot_var, columns_names)
    })
    
    validation_checks <- validate_input(input, plot_var, data_list_reactive, "ADNCA")
    
    plotted_data <- reactive({
      validation_checks()
      get_plotted_data(input, plot_var, data_list_reactive, "ADNCA")
    })
    
    treat_numeric_as_factor <- reactive({
      if (is_num_var_short(.unique_records_for_factor, input, plotted_data)) {
        input$numeric_as_factor
      } else {
        FALSE
      }
    })
    
    # Render content for the single tab
    observe({
      render_single_tab_content(
        dataset_name = "ADNCA",
        parent_dataname = "ADNCA",
        output = output,
        data_list_reactive = data_list_reactive,
        input = input,
        columns_names = columns_names,
        plot_var = plot_var
      )
    })
    
    all_ggplot2_args <- reactive({
      user_text <- teal.widgets::ggplot2_args(
        theme = list(
          "text" = ggplot2::element_text(size = input$font_size),
          "axis.text.x" = ggplot2::element_text(angle = input$label_rotation, hjust = 1)
        )
      )
      req(input$ggplot_theme)
      user_theme <- utils::getFromNamespace(sprintf("theme_%s", input$ggplot_theme), ns = "ggplot2")
      user_theme <- user_theme()
      user_theme <- user_theme[grep("strip.text.y.left", names(user_theme), fixed = TRUE, invert = TRUE)]
      
      teal.widgets::resolve_ggplot2_args(
        user_plot = user_text,
        user_default = teal.widgets::ggplot2_args(theme = user_theme),
        module_plot = ggplot2_args_param
      )
    })
    
    output$ui_numeric_display <- renderUI({
      validation_checks()
      dataname <- "ADNCA" # Using the direct parameter
      varname <- plot_var$variable[[dataname]]
      df <- data_list_reactive[[dataname]]
      
      numeric_ui <- tagList(
        fluidRow(
          tags$div(
            class = "col-md-4",
            tags$br(),
            shinyWidgets::switchInput(
              inputId = ns("display_density"),
              label = "Show density",
              value = `if`(is.null(isolate(input$display_density)), TRUE, isolate(input$display_density)),
              width = "50%",
              labelWidth = "100px",
              handleWidth = "50px"
            )
          ),
          tags$div(
            class = "col-md-4",
            tags$br(),
            shinyWidgets::switchInput(
              inputId = ns("remove_outliers"),
              label = "Remove outliers",
              value = `if`(is.null(isolate(input$remove_outliers)), FALSE, isolate(input$remove_outliers)),
              width = "50%",
              labelWidth = "100px",
              handleWidth = "50px"
            )
          ),
          tags$div(
            class = "col-md-4",
            uiOutput(ns("outlier_definition_slider_ui"))
          )
        ),
        tags$div(
          class = "ml-4",
          uiOutput(ns("ui_density_help")),
          uiOutput(ns("ui_outlier_help"))
        )
      )
      
      observeEvent(input$numeric_as_factor, ignoreInit = TRUE, {
        varname_numeric_as_factor[[plot_var$variable[[dataname]]]] <- input$numeric_as_factor
      })
      
      if (is.numeric(df[[varname]])) {
        unique_entries <- length(unique(df[[varname]]))
        if (unique_entries < .unique_records_for_factor && unique_entries > 0) {
          list(
            checkboxInput(
              ns("numeric_as_factor"),
              "Treat variable as factor",
              value = `if`(
                is.null(varname_numeric_as_factor[[varname]]),
                unique_entries < .unique_records_default_as_factor,
                varname_numeric_as_factor[[varname]]
              )
            ),
            conditionalPanel(
              condition = "input.numeric_as_factor === false",
              ns = ns,
              numeric_ui
            )
          )
        } else if (unique_entries > 0) {
          numeric_ui
        }
      } else {
        NULL
      }
    })
    
    output$ui_histogram_display <- renderUI({
      validation_checks()
      dataname <- "ADNCA" # Using the direct parameter
      varname <- plot_var$variable[[dataname]]
      df <- data_list_reactive[[dataname]]

      numeric_ui <- tagList(fluidRow(
        tags$div(
          class = "col-md-4",
          shinyWidgets::switchInput(
            inputId = ns("remove_NA_hist"),
            label = "Remove NA values",
            value = FALSE,
            width = "50%",
            labelWidth = "100px",
            handleWidth = "50px"
          )
        )
      ))
      
      var <- df[[varname]]
      if (anyNA(var) && (is.factor(var) || is.character(var) || is.logical(var))) {
        groups <- unique(as.character(var))
        len_groups <- length(groups)
        if (len_groups >= .unique_records_for_factor) {
          NULL
        } else {
          numeric_ui
        }
      } else {
        NULL
      }
    })
    
    output$outlier_definition_slider_ui <- renderUI({
      req(input$remove_outliers)
      sliderInput(
        inputId = ns("outlier_definition_slider"),
        tags$div(
          class = "teal-tooltip",
          tagList(
            "Outlier definition:",
            icon("circle-info"),
            tags$span(
              class = "tooltiptext",
              paste(
                "Use the slider to choose the cut-off value to define outliers; the larger the value the",
                "further below Q1/above Q3 points have to be in order to be classed as outliers"
              )
            )
          )
        ),
        min = 1,
        max = 5,
        value = 3,
        step = 0.5
      )
    })
    
    output$ui_density_help <- renderUI({
      req(is.logical(input$display_density))
      if (input$display_density) {
        tags$small(helpText(paste(
          "Kernel density estimation with gaussian kernel",
          "and bandwidth function bw.nrd0 (R default)"
        )))
      } else {
        NULL
      }
    })
    
    output$ui_outlier_help <- renderUI({
      req(is.logical(input$remove_outliers), input$outlier_definition_slider)
      if (input$remove_outliers) {
        tags$small(
          helpText(
            withMathJax(paste0(
              "Outlier data points (\\( X \\lt Q1 - ", input$outlier_definition_slider, "\\times IQR \\) or
               \\(Q3 + ", input$outlier_definition_slider, "\\times IQR \\lt X\\))
               have not been displayed on the graph and will not be used for any kernel density estimations, ",
              "although their values remain in the statisics table below."
            ))
          )
        )
      } else {
        NULL
      }
    })
    
    variable_plot_r <- reactive({
      req(plotted_data())
      display_density <- `if`(is.null(input$display_density), FALSE, input$display_density)
      remove_outliers <- `if`(is.null(input$remove_outliers), FALSE, input$remove_outliers)
      
      if (remove_outliers) {
        req(input$outlier_definition_slider)
        outlier_definition <- as.numeric(input$outlier_definition_slider)
      } else {
        outlier_definition <- 0
      }
      
      plot_var_summary(
        var = plotted_data()$data,
        var_lab = plotted_data()$var_description,
        wrap_character = 15,
        numeric_as_factor = treat_numeric_as_factor(),
        remove_NA_hist = input$remove_NA_hist,
        display_density = display_density,
        outlier_definition = outlier_definition,
        records_for_factor = .unique_records_for_factor,
        ggplot2_args = all_ggplot2_args()
      )
    })
    
    output$plot_ui <- renderUI({
      req(plot_var$variable[["ADNCA"]])
      teal.widgets::plot_with_settings_ui(ns(paste0("variable_plot_", plot_var$variable[["ADNCA"]])))
    })
    
    observe({
      req(plot_var$variable[["ADNCA"]])
      teal.widgets::plot_with_settings_srv(
        id = paste0("variable_plot_", plot_var$variable[["ADNCA"]]),
        plot_r = variable_plot_r,
        height = c(500, 200, 2000)
      )
    })
    
    output$variable_summary_table <- DT::renderDataTable({
      var_summary_table(
        plotted_data()$data,
        treat_numeric_as_factor(),
        input$variable_summary_table_rows,
        if (!is.null(input$remove_outliers) && input$remove_outliers) {
          req(input$outlier_definition_slider)
          as.numeric(input$outlier_definition_slider)
        } else {
          0
        }
      )
    })
  
  })
}