#' Flexible Violin/Box Plot
#'
#' This function generates a  violin or box plot based on the provided data,
#' parameter, and dose information.
#'
#' @param result_data         A list containing the data to be plotted. It should have a `data`
#'                            element with dose information and a `formatted` element with the
#'                            data to be plotted.
#' @param parameter           A string specifying the parameter to be plotted.
#' @param doses_included      A vector of doses to be included in the plot.
#' @param dosenumber_included A vector of dose numbers to be included in the plot.
#' @param columns_to_hover    A character vector indicating the column names from result_data that
#'                            should be used to identify when hovering the plotly outputs
#' @param box                 A logical value indicating whether to plot a box plot (`TRUE`) or a
#'                            violin plot (`FALSE`). Default is `TRUE`.
#' @param plotly              A logical value indicating wether the output is plotly (TRUE, default) 
#'                            or ggplot otherwise (FALSE)
#'
#' @return A plotly object representing the violin or box plot.
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @export
flexible_violinboxplot <- function(boxplotdata,
                                   parameter,
                                   xvars,
                                   colorvars,
                                   varvalstofilter,
                                   columns_to_hover,
                                   box = TRUE,
                                   plotly = TRUE) {

  # Variables to use to filter
  vals_tofilter <- gsub(".*: (.*)", "\\1", varvalstofilter)
  vars_tofilter <-  gsub("(.*): .*", "\\1", varvalstofilter)
  var_types <- sapply(vars_tofilter, \(col_id) class(boxplotdata[[col_id]]), USE.NAMES = FALSE)

  filter_text <- paste0(
    sapply(unique(vars_tofilter), \(varid) {
      vartype <- class(boxplotdata[[varid]])
      paste0(
        varid,
        " %in% as.",
        vartype,
        "(c('", paste0(vals_tofilter[vars_tofilter == varid], collapse = "','"), "'))"
      )
    }), collapse = " & "
  )

  # Filter the data
  box_data <- boxplotdata %>%
    filter(
      eval(parse(text = filter_text)),
      PPTESTCD == parameter
    )

  # Hover text to identify each point
  hover_text <- apply(box_data[columns_to_hover] %>%
                        mutate(across(where(is.numeric), \(x) round(x, digits = 2))),
                      MARGIN = 1,
                      function(row) {
                        paste(names(row), row, sep = ": ", collapse = "<br>")
                      })

  # ylabel of violin/boxplot
  ylabel <- {
    if (box_data$PPSTRESU[1] == "unitless" ||
          is.na(box_data$PPSTRESU[1]) ||
          is.null(box_data$PPSTRESU)) {
      parameter
    } else {
      paste(parameter, " [", box_data$PPSTRESU[1], "]")
    }
  }

  # Make the plot
  p <- ggplot(
    data = box_data %>% arrange(!!!syms(colorvars)),
    aes(
      x = interaction(!!!syms(xvars), sep = "\n"),
      y = PPSTRES,
      color = interaction(!!!syms(colorvars))
    )
  )

  #  Make boxplot or violin
  if (box) {
    p <- p + geom_boxplot()
  } else {
    p <- p + geom_violin()
  }

  # Include points, labels and theme
  p <- p +
    geom_point(position = position_jitterdodge()) +
    # facet_wrap(~STUDYID) +
    labs(
      x = paste(xvars, collapse = ", "),
      y = ylabel,
      color = paste(colorvars, collapse = ", ")
    ) +
    theme_bw() +
    theme(legend.position = "right",
          panel.spacing = unit(3, "lines"),
          strip.text = element_text(size = 10))
  

  # Make plotly with hover features
  if (plotly) {
    ggplotly(p + aes(text = hover_text), tooltip = "text")
  } else {
    p
  }
}
