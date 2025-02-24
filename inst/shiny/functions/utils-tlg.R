#' Creates editing widget of appropriate type.
#' @param opt_def Definition of the option
#' @param opt_id  Id of the option
#' @param session Session object for namespacing the widgets
#' @returns Shiny widget with appropriate type, label and options
create_edit_widget <- function(opt_def, opt_id, session = shiny::getDefaultReactiveDomain()) {
  if (grepl(".group_label", opt_id)) {
    return(tags$h1(opt_def, class = "tlg-group-label"))
  }

  label <- if (is.null(opt_def$label)) opt_id else opt_def$label

  switch(
    opt_def$type,
    text = {
      textInput(
        session$ns(opt_id),
        label = label,
        value = opt_def$default
      )
    },
    numeric = {
      numericInput(
        session$ns(opt_id),
        label = label,
        value = opt_def$default
      )
    },
    select = {
      choices <- {
        if (isTRUE(opt_def$choices == ".colnames")) {
          names(session$userData$data())
        } else if (length(opt_def$choices) == 1 && grepl("^\\$", opt_def$choices)) {
          unique(session$userData$data()[, sub("^\\$", "", opt_def$choices)])
        } else {
          opt_def$choices
        }
      }

      selected <- {
        if (!is.null(opt_def$default)) {
          if (opt_def$default == ".all") {
            choices
          } else {
            opt_def$default
          }
        } else {
          ""
        }
      }

      selectInput(
        session$ns(opt_id),
        label = label,
        selected = selected,
        choices = c("", choices),
        multiple = isTRUE(opt_def$multiple)
      )
    }
  )
}