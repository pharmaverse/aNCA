#' This module provides a user interface and server function for additional analysis outside of NCA
#' calculations. It includes functions for blood-plasma partitioning, matrix ratios, excretion,
#' AUC ratios, and metabolite-parent ratios.
#' @param id A character string used to uniquely identify the module.
#' @param data A PKNCA data object that returns the data list containing the concentration and
#' dose data.
#' @param grouping_vars A character vector of grouping variables to use for the analysis.

# UI function for the non-nca analysis
additional_analysis_ui <- function(id) {
  ns <- NS(id)

  navset_pill(
    id = ns("non_nca_tabs"),
    nav_panel(
      title = "Concentration Ratios",
      value = "matrix_ratio_analysis",
      non_nca_ratio_ui(
        ns("matrix_ratio_analysis"),
        title = "Matrix Ratios",
        select_label1 = "Choose Numerator Specimens",
        select_label2 = "Choose Denominator Specimens"
      )
    ),
    nav_panel(
      title = "Excretion",
      value = "excretion_analysis",
      card(
        card_header("Excretion Analysis"),
        card_body(

          p("To be added")
        )
      )
    ),
    nav_panel(
      title = "AUC Ratios",
      value = "auc_analysis",
      card(
        p("To be added")
      ),
    ),
    nav_panel(
      title = "Metabolite-Parent Ratios",
      value = "metabolite_analysis",
      card(
        card_header("Metabolite-Parent Ratios"),
        card_body(

          p("To be added")
        )
      )
    )
  )
}

# Server function for the module
additional_analysis_server <- function(id, data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Call module for Matrix Ratio Analysis
    non_nca_ratio_server(id = "matrix_ratio_analysis", data, grouping_vars)

  })
}
