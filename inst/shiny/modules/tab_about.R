#' About Page Module
#'
#' Displays package info, links, citation, authors, license, and session info.
#'
#' @param id Module ID.

tab_about_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "max-width: 800px; margin: 0 auto; padding: 1em;",

    # --- Version banner ---
    card(
      card_header("aNCA"),
      card_body(
        div(
          style = "display: flex; align-items: center; gap: 20px;",
          tags$img(
            src = "logos/aNCA_logo.png",
            alt = "aNCA logo",
            width = "80px"
          ),
          div(
            tags$h4("(Pre-)Clinical NCA in a Dynamic Shiny App"),
            uiOutput(ns("version_info"))
          )
        )
      )
    ),

    # --- Links ---
    card(
      card_header("Links"),
      card_body(
        tags$ul(
          class = "list-unstyled",
          .about_link("Package website", "https://pharmaverse.github.io/aNCA/"),
          .about_link("CRAN", "https://cran.r-project.org/package=aNCA"),
          .about_link("GitHub", "https://github.com/pharmaverse/aNCA"),
          .about_link("Bug reports", "https://github.com/pharmaverse/aNCA/issues"),
          .about_link("Changelog", "https://pharmaverse.github.io/aNCA/news/"),
          .about_link(
            "Getting started",
            "https://pharmaverse.github.io/aNCA/articles/aNCA.html"
          ),
          .about_link("PKNCA", "https://cran.r-project.org/package=PKNCA"),
          .about_link("pharmaverse", "https://pharmaverse.org/")
        )
      )
    ),

    # --- Citation ---
    card(
      card_header("Citation"),
      card_body(
        tags$p("If you use aNCA in a publication, please cite:"),
        uiOutput(ns("citation_text")),
        tags$p(
          style = "margin-top: 0.5em;",
          "aNCA builds on PKNCA. Please also cite: Denney, Duvvuri, and ",
          "Buckeridge (2015). ",
          tags$a(
            href = "https://doi.org/10.1007/s10928-015-9432-2",
            target = "_blank",
            "doi:10.1007/s10928-015-9432-2"
          )
        )
      )
    ),

    # --- Authors ---
    card(
      card_header("Authors"),
      card_body(
        uiOutput(ns("authors_list"))
      )
    ),

    # --- License ---
    card(
      card_header("License"),
      card_body(
        tags$p(
          "Apache License 2.0",
          tags$br(),
          "Copyright \u00A9 2023 F. Hoffmann-La Roche AG"
        )
      )
    ),

    # --- Session Info ---
    card(
      card_header("Session Info"),
      card_body(
        actionButton(
          ns("copy_session_info"),
          "Copy session info to clipboard",
          icon = icon("clipboard")
        ),
        tags$br(), tags$br(),
        verbatimTextOutput(ns("session_info"))
      )
    )
  )
}

tab_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Version info
    output$version_info <- renderUI({
      anca_ver <- as.character(utils::packageVersion("aNCA"))
      r_ver <- R.version.string
      pknca_ver <- as.character(utils::packageVersion("PKNCA"))
      tags$div(
        tags$p(
          style = "margin: 0;",
          paste0("aNCA ", anca_ver),
          tags$br(),
          r_ver,
          tags$br(),
          paste0("PKNCA ", pknca_ver)
        )
      )
    })

    # Citation
    output$citation_text <- renderUI({
      cit <- utils::citation("aNCA")
      cit_text <- format(cit, style = "text")
      tags$blockquote(
        style = "border-left: 3px solid #ccc; padding-left: 1em; color: #555;",
        tags$p(cit_text)
      )
    })

    # Authors from DESCRIPTION
    output$authors_list <- renderUI({
      desc <- utils::packageDescription("aNCA")
      authors <- eval(parse(text = desc$`Authors@R`))
      author_items <- lapply(authors, function(a) {
        if (!"aut" %in% a$role) return(NULL)
        name <- paste(a$given, a$family)
        orcid <- a$comment[["ORCID"]]
        if (!is.null(orcid) && nchar(orcid) > 0) {
          tags$li(
            name, " ",
            tags$a(
              href = paste0("https://orcid.org/", orcid),
              target = "_blank",
              tags$img(
                src = "https://orcid.org/sites/default/files/images/orcid_16x16.png",
                alt = "ORCID",
                style = "vertical-align: middle;"
              )
            )
          )
        } else {
          tags$li(name)
        }
      })
      tags$ul(Filter(Negate(is.null), author_items))
    })

    # Session info display
    session_info_text <- reactive({
      paste(utils::capture.output(utils::sessionInfo()), collapse = "\n")
    })

    output$session_info <- renderText({
      session_info_text()
    })

    # Copy to clipboard via JS
    observeEvent(input$copy_session_info, {
      session$sendCustomMessage("copy_to_clipboard", session_info_text())
      showNotification("Session info copied to clipboard", type = "message")
    })
  })
}

# Helper to build a link list item
.about_link <- function(label, url) {
  tags$li(
    style = "margin-bottom: 0.3em;",
    tags$a(href = url, target = "_blank", label),
    tags$span(
      style = "color: #999; font-size: 0.85em; margin-left: 0.5em;",
      url
    )
  )
}
