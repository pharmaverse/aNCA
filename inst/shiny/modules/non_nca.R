# UI function for the non-nca analysis 
non_nca_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      id = ns("non_nca_tabs"),
      tabPanel(
        title = "BPP Analysis",
        value = "bpp_analysis",
        card(
          card_header("BPP Analysis"),
          card_body(
            p("Placeholder for BPP Analysis content.")
          )
        )
      ),
      tabPanel(
        title = "Excretion Analysis",
        value = "excretion_analysis",
        card(
          card_header("Excretion Analysis"),
          card_body(
            p("Placeholder for Excretion Analysis content.")
          )
        )
      ),
      tabPanel(
        title = "Matrix Ratio Analysis",
        value = "matrix_ratio_analysis",
        card(
          card_header("Matrix Ratio Analysis"),
          card_body(
            p("Placeholder for Matrix Ratio Analysis content.")
          )
        )
      )
    )
  )
}

# Server function for the module
non_nca_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

  })
}
