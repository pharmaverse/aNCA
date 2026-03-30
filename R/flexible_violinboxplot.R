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
#' @param show_excluded       Logical. If `TRUE`, excluded records (those with a populated
#'                            `exclude` column) are overlaid as cross-shaped points. They are
#'                            never included in box/violin statistics. Default is `FALSE`.
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
                                   seed = NULL,
                                   show_excluded = FALSE) {

  prepared <- .prepare_boxplot_data(res_nca, parameter, varvalstofilter)

  error_boxplot <- .check_boxplot_data(prepared$included, parameter)
  if (!is.null(error_boxplot)) return(error_boxplot)

  box_data <- .handle_tooltips(prepared$included, tooltip_vars, labels_df)
  excluded_data <- if (show_excluded && nrow(prepared$excluded) > 0) {
    .handle_tooltips(prepared$excluded, tooltip_vars, labels_df)
  } else {
    prepared$excluded
  }

  ylabel <- .build_ylabel(parameter, box_data$PPSTRESU[1])

  shared_aes <- aes(
    x = interaction(!!!syms(xvars), sep = "\n"),
    y = PPSTRES,
    color = interaction(!!!syms(colorvars))
  )

  p <- ggplot(
    data = box_data %>% arrange(!!!syms(colorvars)),
    aes(
      x = interaction(!!!syms(xvars), sep = "\n"),
      y = PPSTRES,
      color = interaction(!!!syms(colorvars)),
      text = tooltip_text
    )
  ) +
    .geom_distribution(box, shared_aes) +
    geom_point(position = position_jitterdodge(seed = seed)) +
    .geom_excluded(excluded_data, show_excluded, colorvars, xvars, seed) +
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

  if (plotly) ggplotly(p, tooltip = "text") else p
}


#' Prepare and split data for boxplot/violin
#'
#' Joins results with concentration data, filters by parameter, and splits
#' into included and excluded records.
#'
#' @param res_nca PKNCA results object.
#' @param parameter Parameter to filter on.
#' @param varvalstofilter Filter values (passed to `.create_filter_expr`).
#' @returns List with `included` and `excluded` data frames.
#' @noRd
.prepare_boxplot_data <- function(res_nca, parameter, varvalstofilter) {
  group_columns <- group_vars(res_nca$data$conc)
  boxplotdata <- left_join(
    res_nca$result,
    res_nca$data$conc$data %>%
      distinct(across(all_of(group_columns)), .keep_all = TRUE),
    by = group_columns,
    keep = FALSE,
    suffix = c("", ".concdata")
  ) %>%
    mutate(
      PPTESTCD = ifelse(
        startsWith(PPTESTCD, "aucint"),
        paste0(PPTESTCD, "_", start, "-", end),
        PPTESTCD
      )
    )

  filter_expr <- .create_filter_expr(boxplotdata, varvalstofilter)
  filtered <- boxplotdata %>%
    filter(!!filter_expr, PPTESTCD == parameter)

  is_excluded <- !is.na(filtered[["exclude"]]) & filtered[["exclude"]] != ""
  list(
    included = filtered[!is_excluded, , drop = FALSE],
    excluded = filtered[is_excluded, , drop = FALSE]
  )
}

#' Add boxplot or violin geometry
#' @param box Logical. TRUE for boxplot, FALSE for violin.
#' @param shared_aes Shared aesthetic mapping for x, y, color.
#' @returns A ggplot2 geom layer.
#' @noRd
.geom_distribution <- function(box, shared_aes) {
  if (box) {
    geom_boxplot(mapping = shared_aes, inherit.aes = FALSE)
  } else {
    geom_violin(mapping = shared_aes, inherit.aes = FALSE, drop = FALSE)
  }
}

#' Overlay excluded records as cross-shaped points
#' @param excluded_data Data frame of excluded records (with tooltip_text).
#' @param show_excluded Logical toggle.
#' @param colorvars Character vector of color variables.
#' @param xvars Character vector of x-axis variables.
#' @param seed Jitter seed.
#' @returns A geom_point layer or NULL.
#' @noRd
.geom_excluded <- function(excluded_data, show_excluded, colorvars, xvars, seed) {
  if (!show_excluded || nrow(excluded_data) == 0) return(NULL)
  geom_point(
    data = excluded_data %>% arrange(!!!syms(colorvars)),
    aes(
      x = interaction(!!!syms(xvars), sep = "\n"),
      y = PPSTRES,
      color = interaction(!!!syms(colorvars)),
      text = tooltip_text
    ),
    shape = 4, size = 3,
    position = position_jitterdodge(seed = seed),
    inherit.aes = FALSE
  )
}

#' Build y-axis label from parameter name and unit
#' @param parameter Parameter name string.
#' @param unit Unit string (may be NA, NULL, empty, or "unitless").
#' @returns Formatted y-axis label.
#' @noRd
.build_ylabel <- function(parameter, unit) {
  if (is.null(unit) || is.na(unit) || unit == "" || unit == "unitless") {
    parameter
  } else {
    paste(parameter, " [", unit, "]")
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
