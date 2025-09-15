#' Create a Plotly Half-life Plot
#'
#' Generates a plotly plot for NCA half-life visualization, with a fit line and scatter points.
#'
#' @param fit_line_data Data frame for the fit line (must have columns for time and y)
#' @param plot_data Data frame for the scatter points
#' @param time_col Name of the time column (string)
#' @param conc_col Name of the concentration column (string)
#' @param title Plot title
#' @param xlab X axis label
#' @param ylab Y axis label
#' @param subtitle_text Subtitle/annotation (HTML allowed)
#' @param color Vector of colors for points (same length as plot_data)
#' @param symbol Vector of marker symbols for points (same length as plot_data)
#' @param group_vars Character vector of grouping variable names (for customdata)
#' @param add_annotations Logical, whether to add the subtitle annotation
#' @param text Optional vector of hover text for points (same length as plot_data)
#' @return A plotly object
get_halflife_plots_single <- function(
  fit_line_data,
  plot_data,
  time_col,
  conc_col,
  title,
  xlab,
  ylab,
  subtitle_text,
  color,
  symbol,
  group_vars,
  add_annotations = TRUE,
  text = NULL
) {
  if (is.null(text)) {
    text <- paste0(
      "Data Point: ", seq_len(nrow(plot_data)), "\n(",
      plot_data[[time_col]], ", ", signif(plot_data[[conc_col]], 3), ")"
    )
  }
  plotly::plot_ly() %>%
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
      annotations = list(
        text = subtitle_text,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        y = 1
      )
    ) %>%
    plotly::add_trace(
      data = plot_data,
      x = ~plot_data[[time_col]],
      y = ~plot_data[[conc_col]],
      text = text,
      hoverinfo = "text",
      showlegend = FALSE,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = color,
        size = 15,
        symbol = symbol,
        size = 20
      ),
      customdata = apply(
        plot_data[, c(group_vars, "ROWID"), drop = FALSE],
        1,
        function(row) as.list(setNames(row, c(group_vars, "ROWID")))
      )
    ) %>%
    plotly::plotly_build()
}

get_halflife_plots <- function(pknca_data, add_annotations = TRUE) {

  # If the input has empty concentration or intervals, just return an empty list
  if (nrow(pknca_data$conc$data) == 0 || nrow(pknca_data$intervals) == 0) {
    return(list(plots = list(), data = list()))
  }

  # Identify column names
  time_col <- pknca_data$conc$columns$time
  conc_col <- pknca_data$conc$columns$concentration
  timeu_col <- pknca_data$conc$columns$timeu
  concu_col <- pknca_data$conc$columns$concu
  exclude_hl_col <- pknca_data$conc$columns$exclude_half.life

  # Make sure to create a default exclude half life column if it does not exist
  if (is.null(exclude_hl_col)) {
    pknca_data$conc$data[["exclude_half.life"]] <- FALSE
    exclude_hl_col <- "exclude_half.life"
  }

  # Adjust the input to compute half-life & show original row number
  pknca_data$conc$data$ROWID <- seq_len(nrow(pknca_data$conc$data))
  pknca_data$intervals <- pknca_data$intervals %>%
    filter(type_interval == "main", half.life) %>%
    unique()
  o_nca <- suppressWarnings(PKNCA::pk.nca(pknca_data))

  if (!"PPSTRES" %in% names(o_nca$result)) {
    o_nca$result$PPSTRES <- o_nca$result$PPORRES
    if ("PPORRESU" %in% names(o_nca$result)) {
      o_nca$result$PPSTRESU <- o_nca$result$PPORRESU
    }
  }

  # Prepare an object with all plot information
  wide_output <- o_nca
  wide_output$result <- wide_output$result %>%
    filter(PPTESTCD %in% c("lambda.z.time.first", "lambda.z.time.last", "lambda.z", "adj.r.squared", "span.ratio")) %>%
    select(-any_of(c("PPORRESU", "PPSTRESU", "PPSTRES")))
  wide_output <- as.data.frame(wide_output, out_format = "wide") %>%
    unique()

  d_conc_with_res <- merge(
    pknca_data$conc$data %>%
      select(!!!syms(c(group_vars(pknca_data), time_col, conc_col, timeu_col, concu_col, exclude_hl_col, "ROWID"))),
    wide_output,
    all.x = TRUE,
    by = c(group_vars(pknca_data))
  ) %>%
    dplyr::filter(.[[time_col]] >= start & .[[time_col]] <= end)

  # Mark points used in half-life calculation
  info_per_plot_list <- d_conc_with_res %>%
    # Indicate plot details
    dplyr::mutate(
      subtitle = ifelse(
        is.na(lambda.z),
        exclude,
        paste0(
          "R<sup>2</sup><sub>adj</sub> = ",
          signif(adj.r.squared, 2),
          "&nbsp;&nbsp;&nbsp;&nbsp;",
          "span ratio = ",
          signif(span.ratio, 2)
        )
      ),
      xlab = ifelse(
        !is.null(timeu_col),
        paste0(time_col, " (", unique(.[[timeu_col]]), ")"),
        time_col
      ),
      ylab = ifelse(
        !is.null(concu_col),
        paste0(conc_col, " (", unique(.[[concu_col]]), ")"),
        conc_col
      )
    ) %>%
    # Mark points used in half-life calculation
    mutate(
      lambda.z.time.first = lambda.z.time.first + start,
      lambda.z.time.last = lambda.z.time.last + start,
      is_halflife_used = .[[time_col]] >= lambda.z.time.first &
        .[[time_col]] <= lambda.z.time.last &
        !.[[exclude_hl_col]]
    ) %>%
    group_by(!!!syms(c(group_vars(pknca_data), "start", "end"))) %>%
    mutate(
      is_halflife_used = if (any(is.na(lambda.z.time.first))) {
        NA
      } else {
        is_halflife_used
      }
    ) %>%
    ungroup()

    info_per_plot_list <- info_per_plot_list %>%
      mutate(
        color = ifelse(is.na(is_halflife_used), "black",
                       ifelse(is_halflife_used, "green", "red")),
        symbol = ifelse(.[[exclude_hl_col]], "x", "circle")
      ) %>%
      group_by(!!!syms(c(group_vars(pknca_data), "start", "end"))) %>%
      group_split()

    plot_list <- list()
    data_list <- list()
    for (i in seq_len(length(info_per_plot_list))) {
      df <- info_per_plot_list[[i]]

      # Create line data
      if (any(!is.na(df$is_halflife_used))) {
      df_fit <- df[df$is_halflife_used, ]
      fit <- lm(as.formula(paste0("log10(", conc_col, ") ~ ", time_col)), df_fit)
      fit_line_data <- data.frame(x = c(df$lambda.z.time.first[1], max(df[[time_col]])))
      colnames(fit_line_data) <- time_col
      fit_line_data$y <- predict(fit, fit_line_data)
      } else {
        fit_line_data <- data.frame(
          x = c(df$start[1], df$start[1]),
          y = c(0, 0)
        )
        colnames(fit_line_data)[1] <- time_col
      }

      # Unique plot ID based on grouping variables and interval times
      plotid_vars <- c(group_vars(pknca_data), "start", "end")
      plotid <- paste0(
          paste0(plotid_vars, ": ",
          df[1, plotid_vars, drop = FALSE],
          collapse = ", "
        )
      )

      # Create the plot
      plot_list[[plotid]] <- get_halflife_plots_single(
        fit_line_data = fit_line_data,
        plot_data = df,
        time_col = time_col,
        conc_col = conc_col,
        title = paste0(
          paste0(group_vars(pknca_data), ": "),
          df[1, group_vars(pknca_data), drop = FALSE],
          collapse = ", "
        ),
        xlab = df$xlab[1],
        ylab = df$ylab[1],
        subtitle_text = df$subtitle[1],
        color = df$color,
        symbol = df$symbol,
        group_vars = group_vars(pknca_data),
        add_annotations = add_annotations
      )
      data_list[[plotid]] <- df
    }
    return(list(plots = plot_list, data = data_list))
}
