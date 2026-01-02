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
#' @param tooltip_vars        A character vector indicating the column names from result_data that
#'                            should be used to identify when hovering the plotly outputs.
#' @param labels_df           A data.frame used for label lookups in tooltips.
#'                            Defaults to metadata_nca_variables.
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
                                   tooltip_vars,
                                   labels_df = metadata_nca_variables,
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
  filter_expr <- .create_filter_expr(boxplotdata, varvalstofilter)

  # Filter data
  box_data <- boxplotdata %>%
    filter(
      !!filter_expr,
      PPTESTCD == parameter
    )

  # Check boxplot data validity; return a plot with an error message if invalid
  error_boxplot <- .check_boxplot_data(box_data, parameter)
  if (!is.null(error_boxplot)) return(error_boxplot)

  # --- Tooltip Construction ---
  box_data <- .handle_tooltips(box_data, tooltip_vars, labels_df)

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
      color = interaction(!!!syms(colorvars)),
      text = tooltip_text
    )
  )

  #  Make boxplot or violin
  if (box) {
    p <- p + geom_boxplot(
      aes(
        x = interaction(!!!syms(xvars), sep = "\n"),
        y = PPSTRES,
        color = interaction(!!!syms(colorvars))
      ),
      inherit.aes = FALSE
    )
  } else {

    p <- p + geom_violin(
      aes(
        x = interaction(!!!syms(xvars), sep = "\n"),
        y = PPSTRES,
        color = interaction(!!!syms(colorvars))
      ),
      inherit.aes = FALSE,
      drop = FALSE
    )
  }

  # Include points, labels and theme
  p <- p +
    geom_point(position = position_jitterdodge(seed = seed)) +
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
    ggplotly(p, tooltip = "text")
  } else {
    p
  }
}


#' Helper function create text used to filter data frame
#' @param boxplotdata Data frame to be filtered
#' @param varvalstofilter Character vector specifying which variable and value to pre-filter
#' as `colname: value`. By default is NULL (no pre-filtering)
#' @importFrom rlang expr sym
#' @importFrom purrr reduce
#' @returns  The filter expression
.create_filter_expr <- function(boxplotdata, varvalstofilter) {
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

#' Check validity of boxplot data for plotting
#'
#' Internal function to check if box_data is empty or lacks valid PPSTRES values.
#' Returns an error plot if invalid, otherwise NULL.
#'
#' @param box_data Data frame to check.
#' @param parameter Parameter name for error message context.
#' @return NULL if valid, or a ggplot error plot if invalid.
#' @noRd
.check_boxplot_data <- function(box_data, parameter) {
  # Check for empty data
  if (is.null(box_data) || nrow(box_data) == 0) {
    error_msg <- paste("No data available")
    return(error_plot(error_msg))
  }
  # Check for invalid PPSTRES values
  if (!"PPSTRES" %in% colnames(box_data) || all(is.na(box_data$PPSTRES))) {
    error_msg <- paste("No data available for parameter:", parameter)
    return(error_plot(error_msg))
  }
  NULL
}
