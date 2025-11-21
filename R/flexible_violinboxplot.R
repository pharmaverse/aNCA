#' Flexible Violin/Box Plot
#'
#' This function generates a  violin or box plot based on the provided data,
#' parameter, and dose information.
#'
#' @param res_nca            A PKNCA results object containing the results and concentration data.
#' @param parameter           A string specifying the parameter to be plotted.
# TODO(mateusz): I have added the following three parameters as they were missing, this needs to
#                be checked over.
#' @param xvars               Variables for the x axis.
#' @param colorvars           Variables for the color aesthetic.
#' @param varvalstofilter     Character vector specifying which variable and value to pre-filter
#'                            as `colname: value`. By default is NULL (no pre-filtering)
#' @param columns_to_hover    A character vector indicating the column names from result_data that
#'                            should be used to identify when hovering the plotly outputs
#' @param box                 A logical value indicating whether to plot a box plot (`TRUE`) or a
#'                            violin plot (`FALSE`). Default is `TRUE`.
#' @param plotly              A logical value defining if the output is plotly (TRUE, default)
#'                            or ggplot otherwise (FALSE)
#' @param seed                An integer value to set the seed for reproducibility of jittering. Default (NULL) will use the current R seed.
#'
#' @return A plotly object representing the violin or box plot.
#' @import dplyr
#' @import ggplot2
#' @export
flexible_violinboxplot <- function(res_nca,
                                   parameter,
                                   xvars,
                                   colorvars,
                                   varvalstofilter = NULL,
                                   columns_to_hover,
                                   box = TRUE,
                                   plotly = TRUE,
                                   seed = NULL) {

  group_columns <- group_vars(res_nca$data$conc)
  boxplotdata <- left_join(
    res_nca$result,
    res_nca$data$conc$data %>%
      distinct(across(all_of(group_columns)), .keep_all = TRUE),
    by = group_columns,
    keep = FALSE,
    suffix = c("", ".concdata")
  ) %>%
    # Intervals should also be considered as differentiated options each
    mutate(
      PPTESTCD = ifelse(
        startsWith(PPTESTCD, "aucint"),
        paste0(PPTESTCD, "_", start, "-", end),
        PPTESTCD
      )
    )

  # Variables to use to filter
  if (!is.null(varvalstofilter)) {
    vals_tofilter <- gsub(".*: (.*)", "\\1", varvalstofilter)
    vars_tofilter <-  gsub("(.*): .*", "\\1", varvalstofilter)
    var_types <- sapply(
      vars_tofilter,
      function(col_id) class(boxplotdata[[col_id]]), USE.NAMES = FALSE
    )

    filter_text <- paste0(
      sapply(unique(vars_tofilter), function(varid) {
        vartype <- class(boxplotdata[[varid]])
        paste0(
          varid,
          " %in% as.",
          vartype,
          "(c('", paste0(vals_tofilter[vars_tofilter == varid], collapse = "','"), "'))"
        )
      }), collapse = " & "
    )
  } else {
    filter_text <- "TRUE"
  }

  # Filter the data
  box_data <- boxplotdata %>%
    filter(
      eval(parse(text = filter_text)),
      PPTESTCD == parameter
    )

  # Hover text to identify each point
  hover_text <- apply(box_data[columns_to_hover] %>%
                        mutate(across(where(is.numeric), function(x) round(x, digits = 2))),
                      MARGIN = 1,
                      function(row) {
                        paste(names(row), row, sep = ": ", collapse = "<br>")
                      })

  # ylabel of violin/boxplot
  ylabel <- {
    if (box_data$PPSTRESU[1] == "unitless" ||
          box_data$PPSTRESU[1] == "" ||
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
    geom_point(position = position_jitterdodge(seed = seed)) +
    # facet_wrap(~STUDYID) +
    labs(
      x = paste(xvars, collapse = ", "),
      y = ylabel,
      color = paste(colorvars, collapse = ", ")
    ) +
    scale_color_brewer(type = "qual") +
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
