#' Generate a Lambda Slope Plot
#'
#' This function generates a lambda slope plot using pharmacokinetic data. It calculates relevant
#' lambda parameters and visualizes the data points used for lambda calculation, along with
#' a linear regression line and additional plot annotations.
#'
#' @param conc_pknca_df  Data frame containing the concentration data
#'                      (default is `mydata$conc$data`).
#' @param row_values     A list containing the values for the column_names used for filtering.
#' @param myres          A PKNCAresults object containing the results of the NCA analysis
#' @param r2adj_threshold Numeric value representing the R-squared adjusted threshold for
#'                      determining the subtitle color (default is 0.7).
#' @param time_column   The name of the time column in the concentration data frame.
#'                      (default is "AFRLT").
#'
#' @return A plotly object representing the lambda slope plot.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item{Creates duplicates of the pre-dose and last doses of concentration data.}
#'   \item{Filters and arranges the input data to obtain relevant lambda calculation information.}
#'   \item{Identifies the data points used for lambda calculation.}
#'   \item{Calculates the fitness, intercept, and time span of the half-life estimate.}
#'   \item{
#'     Determines the subtitle color based on the R-squared adjusted value and half-life estimate.
#'   }
#'   \item{
#'     Generates a ggplot object with the relevant data points,
#'     linear regression line, and annotations.
#'   }
#'   \item{Converts the ggplot object to a plotly object for interactive visualization.}
#' }
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   plot <- lambda_slope_plot(conc_pknca_df = mydata$conc$data,
#'                             row_values = list(USUBJID = "001", STUDYID = "A", DOSENO = 1),
#'                             myres = res_nca,
#'                             r2adj_threshold = 0.7)
#'   plot
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom plotly ggplotly layout config style add_trace
#' @importFrom rlang set_names
#' @export
lambda_slope_plot <- function(
  conc_pknca_df,
  row_values,
  myres = myres,
  r2adj_threshold = 0.7,
  time_column = "AFRLT"
) {

  column_names <- names(row_values)
  grouping_names <- setdiff(column_names, "ATPTREF")
  #Create duplicates for predose and last dose points per profile
  conc_pknca_df <- dose_profile_duplicates(conc_pknca_df, grouping_names)
  #Obtain values for slopes selection
  lambda_res <- myres$result %>%
    filter(if_all(all_of(column_names), ~ .x == row_values[[deparse(substitute(.x))]])) %>%
    arrange(across(all_of(column_names)), start_dose, desc(end_dose)) %>%
    filter(!duplicated(paste0(!!!rlang::syms(column_names), PPTESTCD)))

  lambda_z_n_points <- as.numeric(lambda_res$PPSTRES[lambda_res$PPTESTCD == "lambda.z.n.points"])
  if (is.na(lambda_z_n_points)) lambda_z_n_points <- 0

  grouping_values <- row_values[grouping_names]

  lambda_z_ix_rows <- conc_pknca_df %>%
    ungroup() %>%
    filter(
      if_all(all_of(grouping_names), ~ .x == row_values[[deparse(substitute(.x))]]),
      !exclude_half.life,
      !!sym(time_column) >= sum(
        subset(
          lambda_res,
          lambda_res$PPTESTCD == "lambda.z.time.first",
          select = c("start", "PPSTRES")
        )
      )
    ) %>%
    arrange(IX) %>%
    slice(0:lambda_z_n_points)

  #Obtain parameter values for subtitle
  r2_value <- signif(as.numeric(lambda_res$PPSTRES[lambda_res$PPTESTCD == "r.squared"]), 3)
  r2adj_value <- signif(as.numeric(lambda_res$PPSTRES[lambda_res$PPTESTCD == "adj.r.squared"]), 3)
  half_life_value <- signif(as.numeric(lambda_res$PPSTRES[lambda_res$PPTESTCD == "half.life"]), 3)
  time_span <- signif(
    abs(dplyr::last(lambda_z_ix_rows[[time_column]])
        - dplyr::first(lambda_z_ix_rows[[time_column]])), 3
  )

  subtitle_color <- ifelse(
    r2adj_value < r2adj_threshold | half_life_value > (time_span / 2),
    "red",
    "black"
  )
  subtitle_text <- paste0(
    "    R<sup>2</sup><sub>adj</sub>: ", r2adj_value,
    "    HL \u03BB<sub>z</sub> = ", half_life_value, " ",
    lambda_res$PPSTRESU[lambda_res$PPTESTCD == "half.life"],
    "    (T<sub>", lambda_z_ix_rows$IX[nrow(lambda_z_ix_rows)], "</sub> - T<sub>",
    lambda_z_ix_rows$IX[1], "</sub>)/2 = ", time_span / 2, " ",
    lambda_res$PPSTRESU[lambda_res$PPTESTCD == "half.life"]
  )

  #Create error text if Cmax used in calculation
  cmax_error_text <- NULL
  tmax_value <- lambda_res$PPSTRES[lambda_res$PPTESTCD == "tmax"]
  lower_limit <- lambda_res$PPSTRES[lambda_res$PPTESTCD == "lambda.z.time.first"]
  if (!is.na(tmax_value) && !is.na(lower_limit) && tmax_value >= lower_limit) {
    subtitle_color <- "red"
    cmax_error_text <- list(
      text = "Cmax should not be included in lambda calculation",
      font = list(size = 15, color = "red", family = "times"),
      x = 1,
      y = 1,
      xref = "paper",
      yref = "paper",
      xanchor = "right",
      yanchor = "top",
      showarrow = FALSE
    )
  }

  # Create the title
  title_text <- paste(
    paste0(column_names, ": ", sapply(column_names, function(col) row_values[[col]])),
    collapse = ", "
  )

  #Filter the data set for subject profile
  plot_data <- conc_pknca_df %>%
    ungroup() %>%
    filter(
      if_all(all_of(grouping_names), ~ .x == grouping_values[[deparse(substitute(.x))]])
    ) %>%
    arrange(IX) %>%
    mutate(
      IX_shape = ifelse(is.excluded.hl, "excluded", "included"),
      IX_stroke = ifelse(is.excluded.hl, 4, 1),
      IX_color = case_when(
        is.excluded.hl ~ "excluded",
        IX %in% lambda_z_ix_rows$IX ~ "hl.included",
        TRUE ~ "hl.excluded"
      )
    ) %>%
    filter(AVAL > 0) %>%
    as.data.frame()

  if (nrow(plot_data) == 0) {
    warning("Not enough data for plotting. Returning empty plot.")
    return(.plotly_empty_plot("No valid lambda calculations available"))
  }

  #Create initial plot
  ggplot_obj <- plot_data %>%
    ggplot(aes(x = ARRLT, y = AVAL)) +
    geom_line(color = "gray70", linetype = "solid", linewidth = 1) +
    geom_smooth(
      data = subset(plot_data, IX_color == "hl.included"),
      method = "lm",
      se = FALSE,
      formula = y ~ x,
      color = "green3",
      linetype = "solid",
      linewidth = 1
    ) +
    geom_point(
      data = plot_data,
      aes(shape = IX_shape, color = IX_color, stroke = IX_stroke),
      size = 5
    ) +
    labs(
      title = title_text,
      y = paste0("Log10 Concentration (", conc_pknca_df $PCSTRESU[1], ")"),
      x = paste0("Actual Time Post Dose (", conc_pknca_df $RRLTU[1], ")")
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 15, family = "serif"),
      legend.position = "none",
      axis.text = element_text(size = 15),
      axis.title.x = element_text(size = 15, family = "serif", margin = margin(t = 0)),
      axis.title.y = element_text(size = 15, family = "serif", margin = margin(r = 10)),
      panel.border = element_rect(colour = "gray35", fill = NA, linewidth = 1),
      panel.grid.major = element_line(colour = "gray90"),
      plot.margin = margin(t = 5, r = 5, b = 35, l = 5)
    ) +
    scale_shape_manual(values = c("included" = 16, "excluded" = 3)) +
    scale_color_manual(values = c(
      "hl.included" = "green4", "hl.excluded" = "black", "excluded" = "red3"
    )) +
    scale_y_log10()

  plotly_obj <- ggplotly(ggplot_obj) %>%
    layout(
      margin = list(t = 80),
      annotations = list(
        list(
          text = subtitle_text,
          showarrow = TRUE,
          arrowcolor = "transparent",
          xref = "paper",
          yref = "paper",
          xanchor = "right",
          yanchor = "top",
          font = list(size = 15, color = subtitle_color, family = "times"),
          x = 1,
          y = 1
        ),
        cmax_error_text
      ),
      hoverlabel = list(font = list(family = "times", size = 20))
    ) %>%
    config(mathjax = "cdn")

  num_traces <- length(plotly_obj$x$data)

  for (i in seq_len(num_traces)) {
    plotly_obj <- plotly_obj %>%
      style(hovertext = ~paste0("Data Point: ", IX), hoverinfo = "none", traces = i)
  }

  customdata <- apply(
    plot_data[, c(column_names, "IX"), drop = FALSE],
    1,
    function(row) as.list(set_names(row, c(column_names, "IX")))
  )

  # Add tracing for interactive plots
  plotly_obj %>%
    add_trace(
      plot_data,
      x = ~ARRLT, y = ~log10(AVAL),
      customdata = customdata,
      text = ~paste0(
        "Data Point: ", IX, "\n",
        "(", round(ARRLT, 1), " , ", signif(AVAL, 3), ")"
      ),
      type = "scatter",
      mode = "markers",
      name = "Data Points",
      hoverinfo = "text",
      marker = list(color = case_when(
        plot_data$is.excluded.hl ~ "red",
        plot_data$IX %in% lambda_z_ix_rows$IX ~ "green",
        TRUE ~ "black"
      ), size = 12, opacity = 0), # Make points semi-transparent
      showlegend = FALSE
    )
}
