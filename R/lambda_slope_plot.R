
get_halflife_plot <- function(pknca_data, add_annotations = TRUE) {
  pknca_data$conc$data$ROWID <- seq_len(nrow(pknca_data$conc$data))
  o_nca <- suppressWarnings(PKNCA::pk.nca(pknca_data))
  if (!"PPSTRES" %in% names(o_nca$result)) {
    o_nca$result$PPSTRES <- o_nca$result$PPORRES
    if ("PPORRESU" %in% names(o_nca$result)) {
      o_nca$result$PPSTRESU <- o_nca$result$PPORRESU
    }
  }
  o_nca$result <- o_nca$result %>%
    unique() %>%
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
  
  groups <- o_nca$result %>%
    select(any_of(c(group_vars(o_nca), "start", "end", "PPTESTCD"))) %>%
    dplyr::filter(PPTESTCD == "lambda.z") %>%
    select(-PPTESTCD) %>%
    unique()
  n_groups <- nrow(groups)
  plots <- vector("list", n_groups)
  if (n_groups == 0) {
    print(difftime(Sys.time() - time0))
    return(plots)
  }
  # Precompute column names and helper functions
  time_col <- o_nca$data$conc$columns$time
  conc_col <- o_nca$data$conc$columns$concentration
  timeu_col <- o_nca$data$conc$columns$timeu
  concu_col <- o_nca$data$conc$columns$concu
  exclude_hl_col <- o_nca$data$conc$columns$exclude_half.life
  if (is.null(exclude_hl_col)) {
    o_nca$data$conc$data[["exclude_half.life"]] <- FALSE
    exclude_hl_col <- "exclude_half.life"
  }
  
  include_hl_col <- o_nca$data$conc$columns$exclude_half.life
  if (is.null(include_hl_col)) {
    o_nca$data$conc$data[["include_half.life"]] <- FALSE
    exclude_hl_col <- "include_half.life"
  }
  
  
  # Helper functions for extracting results
  get_res_fun <- function(result_df, testcd) result_df$PPSTRES[result_df$PPTESTCD == testcd]
  get_unit_fun <- function(result_df, testcd) result_df$PPSTRESU[result_df$PPTESTCD == testcd]
  # 1. Pre-split data by group (avoid repeated merges)
  group_vars_all <- setdiff(names(groups), c("start", "end"))
  # Create a key for each row in conc data and in groups
  conc_data <- o_nca$data$conc$data
  if (length(group_vars_all) > 0) {
    conc_data$.__groupkey__ <- apply(conc_data[, group_vars_all, drop = FALSE], 1, function(row) paste(row, collapse = "|"))
    groups$.__groupkey__ <- apply(groups[, group_vars_all, drop = FALSE], 1, function(row) paste(row, collapse = "|"))
  } else {
    conc_data$.__groupkey__ <- "__all__"
    groups$.__groupkey__ <- "__all__"
  }
  # Pre-split conc_data by group key
  conc_data_split <- split(conc_data, conc_data$.__groupkey__)
  
  
  # 2. Build the first plot fully
  i <- 1
  group <- groups[i, ]
  group_vars <- group_vars_all
  group_key <- group$.__groupkey__
  df <- conc_data_split[[group_key]]
  df <- df[df[[time_col]] >= group$start & df[[time_col]] <= group$end, ]
  df <- df[order(df[[time_col]]), ]
  df$IX <- seq_len(nrow(df))
  group_nca <- o_nca
  group_nca$data$conc$data <- df
  group_nca$result <- merge(group_nca$result, group)
  get_res <- function(testcd) get_res_fun(group_nca$result, testcd)
  get_unit <- function(testcd) get_unit_fun(group_nca$result, testcd)
  start <- unique(group_nca$result$start)
  tlast <- get_res("tlast")
  half_life <- get_res("half.life")
  adj.r.squared <- get_res("adj.r.squared")
  lz_time_first <- get_res("lambda.z.time.first")
  lz_time_last <- get_res("lambda.z.time.last")
  time_span <- lz_time_last - lz_time_first
  span_ratio <- get_res("span.ratio")
  exclude_calc_reason <- group_nca$result$exclude[group_nca$result$PPTESTCD == "half.life"]
  
  if (!is.na(half_life)) {
    is_lz_used <- get_halflife_points2(
      group_nca$data$conc$data,
      include_hl_col,
      time_col,
      lz_time_first,
      lz_time_last,
      exclude_hl_col
    )
    df_fit <- df[is_lz_used, ]
    fit <- lm(as.formula(paste0("log10(", conc_col,") ~ ", time_col)), df_fit)
    fit_line_data <- data.frame(x = c(lz_time_first, tlast))
    colnames(fit_line_data) <- time_col
    fit_line_data$y <- predict(fit, fit_line_data)
  } else {
    is_lz_used <- rep(NA_real_, nrow(df))
    fit_line_data <- data.frame(
      x = c(start, start),
      y = c(0, 0)
    )
    colnames(fit_line_data)[1] <- time_col
  }
  
  plot_data <- df
  # 4. Vectorize color, symbol, text assignment
  plot_data$color <- ifelse(is.na(is_lz_used), "black",
                            ifelse(is_lz_used, "green", "red"))
  plot_data$symbol <- ifelse(plot_data[[exclude_hl_col]], "x", "circle")
  plot_data$text <- paste0(
    "Data Point: ", plot_data[["IX"]], "\n",
    "(",
    plot_data[[time_col]],
    ", ",
    signif(plot_data[[conc_col]], 3),
    ")"
  )
  title <- paste0(paste0(group_vars, ": "), group[, group_vars, drop = FALSE], collapse = ", ")
  xlab <- if (!is.null(timeu_col)) paste0(time_col, " (", unique(plot_data[[timeu_col]]), ")") else time_col
  ylab <- if (!is.null(concu_col)) paste0(conc_col, " (", unique(plot_data[[concu_col]]), ")") else conc_col
  subtitle_text <- paste0(
    "R<sup>2</sup><sub>adj</sub> = ", signif(adj.r.squared, 2),
    "&nbsp;&nbsp;&nbsp;&nbsp;",
    "span ratio = ",
    signif(span_ratio, 2)
  )
  if (is.na(half_life)) {
    subtitle_text <- exclude_calc_reason
  }
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
      text = ~plot_data$text,
      hoverinfo = "text",
      showlegend = FALSE,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = plot_data$color,
        size = 15,
        symbol = plot_data$symbol,
        size = 20
      ),
      customdata = apply(
        plot_data[, c(group_vars, "ROWID", "IX"), drop = FALSE],
        1,
        function(row) as.list(set_names(row, c(group_vars, "ROWID", "IX")))
      )
    )
  plots[[1]] <- plotly::plotly_build(p)
  names(plots)[1] <- paste0(title, ", start: ", group$start, ", end: ", group$end)
  # For the rest, update the plotly object
  # Clone the first plot and update data/layout
  p_i <- plots[[1]]
  if (n_groups > 1) {
    for (i in 2:n_groups) {
      group <- groups[i, ]
      group_vars <- group_vars_all
      group_key <- group$.__groupkey__
      df <- conc_data_split[[group_key]]
      df <- df[df[[time_col]] >= group$start & df[[time_col]] <= group$end, ]
      df <- df[order(df[[time_col]]), ]
      df$IX <- seq_len(nrow(df))
      group_nca <- o_nca
      group_nca$data$conc$data <- df
      group_nca$result <- merge(group_nca$result, group)
      
      start <- unique(group_nca$result$start)
      tlast <- get_res("tlast")
      half_life <- get_res("half.life")
      adj.r.squared <- get_res("adj.r.squared")
      lz_time_first <- get_res("lambda.z.time.first")
      lz_time_last <- get_res("lambda.z.time.last")
      time_span <- lz_time_last - lz_time_first
      span_ratio <- get_res("span.ratio")
      exclude_calc_reason <- group_nca$result$exclude[group_nca$result$PPTESTCD == "half.life"]

      if (!is.na(half_life)) {
        is_lz_used <- get_halflife_points2(
          group_nca$data$conc$data,
          include_hl_col,
          time_col,
          lz_time_first,
          lz_time_last,
          exclude_hl_col
        )
        df_fit <- df[is_lz_used, ]
        fit <- lm(as.formula(paste0("log10(", conc_col,") ~ ", time_col)), df_fit)
        fit_line_data <- data.frame(x = c(lz_time_first, tlast))
        colnames(fit_line_data) <- time_col
        fit_line_data$y <- predict(fit, fit_line_data)
      } else {
        is_lz_used <- rep(NA_real_, nrow(df))
        fit_line_data <- data.frame(
          x = c(start, start),
          y = c(0, 0)
        )
        colnames(fit_line_data)[1] <- time_col
      }
      # Vectorize color, symbol, text assignment
      plot_data <- df
      plot_data$color <- ifelse(is.na(is_lz_used), "black",
                                ifelse(is_lz_used, "green", "red"))
      plot_data$symbol <- ifelse(plot_data[[exclude_hl_col]], "x", "circle")
      plot_data$text <- paste0(
        "Data Point: ", plot_data[["IX"]], "\n",
        "(",
        plot_data[[time_col]],
        ", ",
        signif(plot_data[[conc_col]], 3),
        ")"
      )
      title <- paste0(paste0(group_vars, ": "), group[, group_vars, drop = FALSE], collapse = ", ")
      xlab <- if (!is.null(timeu_col)) paste0(time_col, " (", unique(plot_data[[timeu_col]]), ")") else time_col
      ylab <- if (!is.null(concu_col)) paste0(conc_col, " (", unique(plot_data[[concu_col]]), ")") else conc_col
      subtitle_text <- paste0(
        "R<sup>2</sup><sub>adj</sub> = ", signif(adj.r.squared, 2),
        "&nbsp;&nbsp;&nbsp;&nbsp;",
        "span ratio = ",
        signif(span_ratio, 2)
      )
      if (is.na(half_life)) {
        subtitle_text <- exclude_calc_reason
      }
      # Update traces
      # 1: fit line, 2: scatter
      p_i$x$data[[1]]$x <- fit_line_data[[time_col]]
      p_i$x$data[[1]]$y <- 10^fit_line_data$y
      p_i$x$data[[2]]$x <- plot_data[[time_col]]
      p_i$x$data[[2]]$y <- plot_data[[conc_col]]
      p_i$x$data[[2]]$marker$color <- plot_data$color
      p_i$x$data[[2]]$marker$symbol <- plot_data$symbol
      p_i$x$data[[2]]$text <- plot_data$text
      p_i$x$data[[2]]$customdata <- apply(
        plot_data[, c(group_vars, "ROWID", "IX"), drop = FALSE],
        1,
        function(row) as.list(set_names(row, c(group_vars, "ROWID", "IX")))
      ) %>% unname()
      # Update layout
      p_i$x$layout$title <- title
      p_i$x$layout$xaxis$title <- xlab
      p_i$x$layout$yaxis$title <- ylab
      if (add_annotations) {
        p_i$x$layout$annotations[[1]]$text <- subtitle_text
      }

      plots[[i]] <- p_i
      names(plots)[i] <- paste0(title, ", start: ", group$start, ", end: ", group$end)
    }
  }
  
  return(plots)
}


#' Custom fast version of get_halflife_points
#' @param data concentration data.frame
#' @param include_hl_col column name for include_half.life
#' @param time_col column name for time
#' @param lz_time_first numeric
#' @param lz_time_last numeric
#' @param exclude_hl_col column name for exclude_half.life
#' @return logical vector
get_halflife_points2 <- function(data, include_hl_col, time_col, lz_time_first, lz_time_last, exclude_hl_col) {
  if (any(data[[include_hl_col]])) {
    data[[include_hl_col]]
  } else {
    data[[time_col]] >= lz_time_first & data[[time_col]] <= lz_time_last & !data[[exclude_hl_col]]
  }
}

