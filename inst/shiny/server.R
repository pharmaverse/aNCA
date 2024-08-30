# This script sources the server logic from the tabs folder

# Define server logic
function(input, output, session) {

  # LOGIN ----
  #
  # loginError <- reactiveVal("") # Initialize reactive value to hold error message
  #
  # observeEvent(input$login, {
  #   showModal(modalDialog(
  #     title = "Please enter your details",
  #     textInput("username", "Username:"),
  #     passwordInput("password", "Password:"),
  #     textInput("path", "Path:"),
  #     actionButton("signin", "GO"),
  #     footer = modalButton("Close")
  #   ))
  # })
  #
  # output$loginError <- renderText({
  #   loginError() # Display error message
  # })
  #
  # observeEvent(input$signin, {
  #
  #     httr::set_config(httr::config(ssl_verifypeer = 0L))
  #     httr::set_config(httr::config(ssl_verifyhost = 0L))
  #
  #     improveRcore::improveLogin(
  #       repo="https://valmse-rep1-prod.roche.com:8443/repository",
  #       user= input$username,
  #       password = input$password,
  #       shortEntityId = "",
  #       secure=F)
  #
  # })


  # DATA ----

  source(system.file("shiny/tabs/data.R", package = "aNCA"), local = TRUE)

  # NCA ----

  source(system.file("shiny/tabs/nca.R", package = "aNCA"), local = TRUE)
  #source("autoslideR/autoslider.R", local = TRUE)

  # OUTPUT ----

  source(system.file("shiny/tabs/outputs.R", package = "aNCA"), local = TRUE)

}

#Testing git editing

