#' Creates editing widget of appropriate type.
#' @param opt_def Definition of the option
#' @param opt_id  Id of the option
#' @param session Session object for namespacing the widgets
#' @returns Shiny widget with appropriate type, label and options
create_edit_widget <- function(opt_def, opt_id, session = shiny::getDefaultReactiveDomain()) {
  if (grepl(".group_label", opt_id)) {
    return(tags$h1(opt_def, class = "tlg-group-label"))
  }

  switch(
    opt_def$type,
    text = {
      tlg_option_text_ui(session$ns(opt_id), opt_def)
    },
    numeric = {
      tlg_option_numeric_ui(session$ns(opt_id), opt_def)
    },
    select = {
      tlg_option_select_ui(session$ns(opt_id), opt_def)
    }
  )
}

get_option_function <- function(opt_def) {
  if (opt_def$type == "text") return(tlg_option_text_server)
  if (opt_def$type == "numeric") return(tlg_option_numeric_server)
  if (opt_def$type == "select") return(tlg_option_select_server)
}