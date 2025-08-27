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
  grouping_names <- setdiff(column_names, "NCA_PROFILE")
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


get_halflife_plot <- function(o_nca, add_annotations = TRUE) {
  # Ensure result columns are present
  if (!"PPSTRES" %in% names(o_nca$result)) {
    o_nca$result$PPSTRES <- o_nca$result$PPORRES
    if ("PPORRESU" %in% names(o_nca$result)) {
      o_nca$result$PPSTRESU <- o_nca$result$PPORRESU
    }
  }
 
  # Get grouping structure for lambda.z
  groups <- getGroups(o_nca %>% dplyr::filter(PPTESTCD == "lambda.z")) %>% unique()
  groups <- o_nca$result %>%
    select(any_of(c(group_vars(o_nca), "start", "end", "PPTESTCD"))) %>%
    dplyr::filter(PPTESTCD == "lambda.z") %>%
    select(-PPTESTCD) %>%
    unique()

  plots <- vector("list", nrow(groups))

  for (i in seq_len(nrow(groups))) {
    group <- groups[i, ]
    group_vars <- setdiff(names(group), c("start", "end"))
    # Subset data for this group
    df <- merge(o_nca$data$conc$data, group[, group_vars, drop = FALSE])

    # Column names
    time_col <- o_nca$data$conc$columns$time
    conc_col <- o_nca$data$conc$columns$concentration
    timeu_col <- o_nca$data$conc$columns$timeu
    concu_col <- o_nca$data$conc$columns$concu
    exclude_hl_col <- o_nca$data$conc$columns$exclude_half.life
    if (is.null(exclude_hl_col)) {
      o_nca$data$conc$data[["exclude_half.life"]] <- FALSE
      exclude_hl_col <- "exclude_half.life"
    }
    exclude_msg_col <- o_nca$data$conc$columns$exclude
    if (is.null(exclude_msg_col)) {
      exclude_msg_col <- "exclude"
    }

    # Filter and order by time
    df <- df[df[[time_col]] >= group$start & df[[time_col]] <= group$end, ]
    df[["ROWID"]] <- seq_len(nrow(df))
    df <- df[order(df[[time_col]]), ]
    df$IX <- seq_len(nrow(df))
    
    # Prepare NCA object for this group
    group_nca <- o_nca
    group_nca$data$conc$data <- df
    group_nca$result <- merge(group_nca$result, group[, group_vars, drop = FALSE])

    # Extract NCA results for annotation
    get_res <- function(testcd) group_nca$result$PPORRES[group_nca$result$PPTESTCD == testcd]
    get_unit <- function(testcd) group_nca$result$PPSTRESU[group_nca$result$PPTESTCD == testcd]
    start <- unique(group_nca$result$start) # this has to have a better way
    tlast <- get_res("tlast")
    half_life <- get_res("half.life")
    adj.r.squared <- get_res("adj.r.squared")
    lz_time_first <- get_res("lambda.z.time.first")
    lz_time_last <- get_res("lambda.z.time.last")
    time_span <- lz_time_last - lz_time_first
    span_ratio <- get_res("span.ratio")
    exclude_calc_reason <- group_nca$result$exclude[group_nca$result$PPTESTCD == "half.life"]

    #########################################################
    # TODO (Gerardo): This complication in the code comes from a PKNCA issue
    # Once solved in a new version this can be simplified
    group_nca_for_fun <- group_nca
    group_nca_for_fun$result <- group_nca_for_fun$result %>%
      mutate(
        PPORRES = ifelse(
          PPTESTCD %in% c("lambda.z.time.first", "lambda.z.time.last", "tlast"),
          PPORRES + start,
          PPORRES
        ),
        PPSTRES = ifelse(
          PPTESTCD %in% c("lambda.z.time.first", "lambda.z.time.last", "tlast"),
          PPSTRES + start,
          PPSTRES
        )
      )
    if (!is.na(half_life)) {
      is_lz_used <- get_halflife_points(group_nca_for_fun)
    } else {
      is_lz_used <- rep(NA_real_, nrow(df))
    }
    #########################################################

    # Compute the points to depict the lambda fit line (if there is)
    if (!is.na(half_life)) {
      df_fit <- df[is_lz_used, ]
      fit <- lm(as.formula(paste0("log10(", conc_col,") ~ ", time_col)), df_fit)
      fit_line_data <- data.frame(x = c(lz_time_first + start, tlast + start))
      colnames(fit_line_data) <- time_col
      fit_line_data$y <- predict(fit, fit_line_data)
    } else {
      fit_line_data <- data.frame(
        x = c(start, start),
        y = c(0, 0)
      )
      colnames(fit_line_data)[1] <- time_col
    }

    # Plot data
    plot_data <- df
    plot_data$color <- case_when(
      is.na(is_lz_used) ~ "black",
      !is.na(is_lz_used) & is_lz_used ~ "green",
      !is.na(is_lz_used) & !is_lz_used  ~ "red",
      TRUE ~ "black"
    )
    title <- paste0(paste0(group_vars, ": "), group[, group_vars, drop = FALSE], collapse = ", ")
    xlab <- if (!is.null(timeu_col)) paste0(time_col, " (", unique(plot_data[[timeu_col]]), ")") else time_col
    ylab <- if (!is.null(concu_col)) paste0(conc_col, " (", unique(plot_data[[concu_col]]), ")") else conc_col
    subtitle_text <- paste0(
      "R<sup>2</sup><sub>adj</sub> = ", signif(adj.r.squared, 2),
      "&nbsp;&nbsp;&nbsp;&nbsp;",
      #"ln(2)/ \u03BB<sub>z</sub> = ", signif(half_life, 2), " ", get_unit("half.life"),
      #"&#9;&#9;",
      #"(T<sub>", df$IX[which(df[[time_col]] == lz_time_first)],
      #"</sub> - T<sub>", df$IX[which(df[[time_col]] == lz_time_last)], "</sub>)/(ln(2)/ \u03BB<sub>z</sub>) =",
      "span ratio = ",
      signif(span_ratio, 2)
    )
    if (is.na(half_life)) {
      subtitle_text <- exclude_calc_reason
    }

    # Build plotly object
    p <- plotly::plot_ly() %>%
      plotly::add_lines(
        data = fit_line_data,
        x = ~get(time_col),
        y = ~10^y,
        line = list(color = "green", width = 2),
        name = "Fit",
        inherit = FALSE,
        showlegend = TRUE
      ) %>%
      plotly::layout(
        title = title,
        xaxis = list(
          title = xlab,
          linecolor = "black",
          gridcolor = "white",
          zeroline = FALSE
        ),
        yaxis = list(
          title = ylab,
          type = "log",
          tickformat = "f",
          linecolor = "black",
          gridcolor = "white",
          zeroline = FALSE
        ),
        annotations = if (add_annotations) list(
          text = subtitle_text,
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          y = 1
        ) else NULL
      ) %>%
      plotly::add_trace(
        data = plot_data,
        x = ~plot_data[[time_col]],
        y = ~plot_data[[conc_col]],
        text = ~paste0(
          "Data Point: ", plot_data[["IX"]], "\n",
          "(",
          get(time_col),
          ", ",
          signif(get(conc_col), 3),
          ")"
        ),
        hoverinfo = "text",
        showlegend = FALSE,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = plot_data$color,
          size = 15,
          symbol = ifelse(plot_data[[exclude_hl_col]], "x", "circle"),
          size = 20
        ),
        customdata = ~df[, c(group_vars, time_col)] # Returns the row number in the object
      )

    plots[[i]] <- plotly_build(p)
    names(plots)[i] <- title
  }
  return(plots)
}
