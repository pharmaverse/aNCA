#' Generate a Lambda Slope Plot
#'
#' This function generates a lambda slope plot using pharmacokinetic data. It calculates relevant
#' lambda parameters and visualizes the data points used for lambda calculation, along with
#' a linear regression line and additional plot annotations.
#'
#' @param res_pknca_df   Data frame containing the results of the pharmacokinetic non-compartmental
#'                      analysis (default is `PKNCA::pk.nca(.)$result`).
#' @param conc_pknca_df  Data frame containing the concentration data
#'                      (default is `mydata$conc$data`).
#' @param column_names   A character vector containing the column names used for grouping the data.
#' @param row_values     A list containing the values for the column_names used for filtering.
#'                      (default is `patient`).
#' @param R2ADJTHRESHOL Numeric value representing the R-squared adjusted threshold for determining
#'                      the subtitle color (default is 0.7).
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
#'   plot <- lambda_slope_plot(res_pknca_df = myres$result,
#'                             conc_pknca_df = mydata$conc$data,
#'                             column_names = c("USUBJID" , "STUDYID", "DOSENO"),
#'                             row_values = list(USUBJID = "001", STUDYID = "A", DOSENO = 1),
#'                             R2ADJTHRESHOL = 0.7)
#'   plot
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @export
lambda_slope_plot <- function(
  res_pknca_df = myres$result,
  conc_pknca_df = mydata$conc$data,
  row_values,
  R2ADJTHRESHOL = 0.7
) {

  column_names <- names(row_values)
  #Create duplicates for predose and last dose points per profile
  conc_pknca_df <- create_duplicates(conc_pknca_df, column_names)
  #Obtain values for slopes selection
  lambda_res <- res_pknca_df %>%
    filter(if_all(all_of(column_names), ~ .x == row_values[[deparse(substitute(.x))]])) %>%
    arrange(across(all_of(column_names)), start, desc(end)) %>%
    filter(!duplicated(paste0(!!!rlang::syms(column_names), PPTESTCD)))

  lambda_z_n_points <- as.numeric(lambda_res$PPORRES[lambda_res$PPTESTCD == "lambda.z.n.points"])
  if (is.na(lambda_z_n_points)) lambda_z_n_points <- 0

  row_values <- row_values[column_names]

  lambda_z_ix_rows <- conc_pknca_df %>%
    ungroup() %>%
    mutate(ARRLT = round(ARRLT, 3)) %>%
    filter(
      across(all_of(column_names), ~ .x == row_values[[cur_column()]]),
      !exclude_half.life,
      ARRLT >= round(
        sum(
          subset(
            lambda_res,
            lambda_res$PPTESTCD == "lambda.z.time.first",
            select = c("start", "PPORRES")
          )
        ),
        3
      )
    ) %>%
    arrange(IX) %>%
    slice(0:lambda_z_n_points)

  #Obtain parameter values for subtitle
  r2_value <- signif(as.numeric(lambda_res$PPORRES[lambda_res$PPTESTCD == "r.squared"]), 3)
  r2adj_value <- signif(as.numeric(lambda_res$PPORRES[lambda_res$PPTESTCD == "adj.r.squared"]), 3)
  half_life_value <- signif(as.numeric(lambda_res$PPORRES[lambda_res$PPTESTCD == "half.life"]), 3)
  time_span <- signif(
    abs(lambda_z_ix_rows$ARRLT[nrow(lambda_z_ix_rows)] - lambda_z_ix_rows$ARRLT[1]), 3
  )

  subtitle_color <- ifelse(
    r2adj_value < R2ADJTHRESHOL | half_life_value > (time_span / 2),
    "red",
    "black"
  )
  subtitle_text <- paste0(
    "    R<sup>2</sup><sub>adj</sub>: ", r2adj_value,
    "    HL \u03BB<sub>z</sub> = ", half_life_value, "h",
    "    (T<sub>", lambda_z_ix_rows$IX[nrow(lambda_z_ix_rows)], "</sub> - T<sub>",
    lambda_z_ix_rows$IX[1], "</sub>)/2 = ", time_span / 2, "h"
  )

  #Create error text if Cmax used in calculation
  cmax_error_text <- NULL
  cmax_value <- lambda_res$PPORRES[lambda_res$PPTESTCD == "cmax"]
  if (!is.na(cmax_value) && cmax_value <= max(lambda_z_ix_rows$AVAL, na.rm = TRUE)) {
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

  #Filter the data set for patient profile
  plot_data <- conc_pknca_df %>%
    ungroup() %>%
    filter(
      across(all_of(column_names), ~ .x == row_values[[cur_column()]])
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
  p <- plot_data %>%
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
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20, family = "serif"),
      legend.position = "none",
      axis.text = element_text(size = 15),
      axis.title.x = element_text(size = 15, family = "serif", margin = margin(t = 0)),
      axis.title.y = element_text(size = 15, family = "serif", margin = margin(r = 10)),
      panel.border = element_rect(colour = "gray35", fill = NA, size = 1),
      panel.grid.major = element_line(colour = "gray90"),
      plot.margin = margin(t = 5, r = 5, b = 35, l = 5)
    ) +
    scale_shape_manual(values = c("included" = 16, "excluded" = 3)) +
    scale_color_manual(values = c(
      "hl.included" = "green4", "hl.excluded" = "black", "excluded" = "red3"
    )) +
    scale_y_log10()

  pl <- ggplotly(p) %>%
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

  num_traces <- length(pl$x$data)

  for (i in seq_len(num_traces)) {
    pl <- pl %>%
      style(hovertext = ~paste0("Data Point: ", IX), hoverinfo = "none", traces = i)
  }

  customdata <- apply(
    plot_data[, c(column_names, "IX"), drop = FALSE],
    1,
    function(row) jsonlite::toJSON(as.list(setNames(row, c(column_names, "IX"))), auto_unbox = TRUE)
  )

  # Add tracing for interactive plots
  pl <- pl %>%
    add_trace(
      plot_data,
      x = ~ARRLT, y = ~log10(AVAL),
      customdata = customdata,
      text = ~paste0(
        "Data Point: ", IX, "\n",
        "(", signif(ARRLT, 2), " , ", signif(AVAL, 2), ")"
      ),
      type = "scatter",
      mode = "markers",
      name = "Data Points",
      hoverinfo = "text",
      marker = list(color = case_when(
        plot_data$is.excluded.hl ~ "red",
        plot_data$IX %in% lambda_z_ix_rows$IX ~ "green",
        TRUE ~ "black"
      ), size = 15, opacity = 0), # Make points semi-transparent
      showlegend = FALSE
    )

  pl
}