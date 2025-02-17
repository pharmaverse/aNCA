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

  tagList(
    navset_pill(
      id = ns("non_nca_tabs"),
      nav_panel(
        title = "Blood-Plasma Partitioning",
        value = "bpp_analysis",
        non_nca_ratio_ui(
          ns("bpp_analysis"),
          title = "Blood-Plasma Partitioning",
          select_label1 = "Choose Blood column for BPP",
          select_label2 = "Choose Plasma column for BPP",
          multiple = FALSE
        )
      ),
      nav_panel(
        title = "Matrix Ratios",
        value = "matrix_ratio_analysis",
        non_nca_ratio_ui(
          ns("matrix_ratio_analysis"),
          title = "Matrix Ratios",
          select_label1 = "Choose Tissues",
          select_label2 = "Choose Plasma",
          multiple = TRUE
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
          card_header("Bioavailability Calculations"),
          card_body(

            p("To be added")
          )
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
      ),
    )
  )
}

# Server function for the module
additional_analysis_server <- function(id, data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Call module for BPP Analysis
    non_nca_ratio_server(id = "bpp_analysis", data, grouping_vars,
                         func = single_matrix_ratio)

    # Call module for Matrix Ratio Analysis
    non_nca_ratio_server(id = "matrix_ratio_analysis", data, grouping_vars,
                         func = multiple_matrix_ratios)

  })
}
