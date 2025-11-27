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
#' @param seed                An integer value to set the seed for reproducibility of jittering.
#' Default (NULL) will use the current R seed.
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

  # Create filter expression
  filter_expr <- create_filter_expr(boxplotdata, varvalstofilter)

  # Filter data
  box_data <- boxplotdata %>%
    filter(
      !!filter_expr,
      PPTESTCD == parameter
    )

  # Verify that PPSTRES exists and is not NA, otherwise return empty plot
  if (!is_PPSTRES_valid(box_data)) {
    return(ggplot() +
             labs(title = paste("No data available for parameter:", parameter)) +
             theme_minimal())
  }

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


#' Helper function create text used to filter data frame
#' @param boxplotdata Data frame to be filtered
#' @param varvalstofilter Character vector specifying which variable and value to pre-filter
#' as `colname: value`. By default is NULL (no pre-filtering)
#' @importFrom rlang expr sym
#' @returns A string representing the filter expression
create_filter_expr <- function(boxplotdata, varvalstofilter) {
  if (is.null(varvalstofilter)) return(expr(TRUE))

  # 1. Parse inputs
  vars <- gsub(": .*", "", varvalstofilter)
  vals <- gsub(".*: ", "", varvalstofilter)

  # 2. Build expressions for each unique variable
  cond_list <- lapply(unique(vars), function(v) {
    # Get values for this specific variable
    current_vals <- vals[vars == v]

    # Dynamic type casting based on the target column
    # (Performs the cast immediately so the expression contains the correct types)
    col_class <- class(boxplotdata[[v]])[1]
    cast_fn <- match.fun(paste0("as.", col_class))
    clean_vals <- cast_fn(current_vals)

    # Create expression: var %in% c(val1, val2...)
    expr(!!sym(v) %in% !!clean_vals)
  })

  # 3. Combine all conditions with '&'
  reduce(cond_list, ~ expr(!!.x & !!.y))
}

# Check if data is valid
is_PPSTRES_valid <- function(box_data) { #nolint
  "PPSTRES" %in% colnames(box_data) && !all(is.na(box_data$PPSTRES))
}
