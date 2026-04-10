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
          .about_link(
            "Getting started",
            "https://pharmaverse.github.io/aNCA/articles/aNCA.html"
          ),
          .about_link("CRAN", "https://cran.r-project.org/package=aNCA"),
          .about_link("GitHub", "https://github.com/pharmaverse/aNCA"),
          .about_link("Bug reports", "https://github.com/pharmaverse/aNCA/issues"),
          .about_link("Changelog", "https://pharmaverse.github.io/aNCA/news/"),
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
          "aNCA builds on PKNCA. Please also cite:"
        ),
        uiOutput(ns("pknca_citation_text"))
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
        div(
          style = "display: flex; gap: 8px;",
          actionButton(
            ns("copy_session_info"),
            "Copy to clipboard",
            icon = icon("clipboard")
          ),
          actionButton(
            ns("toggle_session_info"),
            "Show details",
            icon = icon("chevron-down")
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("session_info_panel"),
            style = "margin-top: 1em;",
            verbatimTextOutput(ns("session_info"))
          )
        )
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
          paste0("Requires at least: ", r_ver),
          tags$br(),
          paste0("PKNCA ", pknca_ver)
        )
      )
    })

    # Citation
    output$citation_text <- renderUI({
      cit_text <- tryCatch(
        {
          cit <- utils::citation("aNCA")
          paste(format(cit, style = "text"), collapse = "\n\n")
        },
        error = function(e) {
          ver <- tryCatch(
            as.character(utils::packageVersion("aNCA")),
            error = function(e) "0.1.0"
          )
          paste0(
            "Suekuer E, Rodriguez GJ, Baertschi P, Spinner J, Kolomanski M, ",
            "Aspridis L, Legras V (2025). ",
            "\"aNCA: A Dynamic Shiny App to perform NCA.\" ",
            "Version ", ver, "; License: Apache-2.0. ",
            "https://github.com/pharmaverse/aNCA"
          )
        }
      )
      tags$blockquote(
        style = "border-left: 3px solid #ccc; padding-left: 1em; color: #555;",
        tags$p(cit_text)
      )
    })

    output$pknca_citation_text <- renderUI({
      cit <- utils::citation("PKNCA")
      cit_text <- paste(format(cit, style = "text"), collapse = "\n\n")
      tags$blockquote(
        style = "border-left: 3px solid #ccc; padding-left: 1em; color: #555;",
        tags$p(cit_text)
      )
    })

    # Authors from DESCRIPTION
    output$authors_list <- renderUI({
      desc <- utils::packageDescription("aNCA")
      authors <- tryCatch(
        eval(parse(text = desc$`Authors@R`)),
        error = function(e) list()
      )
      author_items <- lapply(authors, function(a) {
        if (!"aut" %in% a$role) {
          return(NULL)
        }
        name <- paste(a$given, a$family)
        orcid <- a$comment[["ORCID"]]
        if (!is.null(orcid) && nchar(orcid) > 0) {
          tags$li(
            name, " ",
            tags$a(
              href = paste0("https://orcid.org/", orcid),
              target = "_blank",
              tags$img(
                src = "https://info.orcid.org/wp-content/uploads/2020/12/ORCIDiD_icon16x16.png",
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

    # Session info — computed once per session (doesn't change at runtime)
    session_info_text <- tryCatch(
      paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"),
      error = function(e) paste("Session info unavailable:", e$message)
    )

    output$session_info <- renderText({
      session_info_text
    })

    # Copy to clipboard via JS
    observeEvent(input$copy_session_info, {
      session$sendCustomMessage("copy_to_clipboard", session_info_text)
      showNotification("Session info copied to clipboard", type = "message")
    })

    # Toggle session info visibility
    session_info_visible <- reactiveVal(FALSE)
    observeEvent(input$toggle_session_info, {
      session_info_visible(!session_info_visible())
      shinyjs::toggle("session_info_panel")
      if (session_info_visible()) {
        updateActionButton(session, "toggle_session_info",
          label = "Hide details", icon = icon("chevron-up")
        )
      } else {
        updateActionButton(session, "toggle_session_info",
          label = "Show details", icon = icon("chevron-down")
        )
      }
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
