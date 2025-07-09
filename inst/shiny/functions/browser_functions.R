
#' Create a summary table for a variable
#' @param x A vector containing the variable data
#' @param numeric_as_factor Logical indicating whether to treat numeric variables as factors
#' @param dt_rows Number of rows to display in the summary table
#' @param outlier_definition Numeric value defining the outlier threshold
#' 
#' @returns A DataTable object containing the summary statistics or counts for the variable
var_summary_table <- function(x, numeric_as_factor, dt_rows, outlier_definition) {
  if (is.null(dt_rows)) {
    dt_rows <- 10
  }
  if (is.numeric(x) && !numeric_as_factor) {
    req(!any(is.infinite(x)))
    
    x <- remove_outliers_from(x, outlier_definition)
    
    qvals <- round(stats::quantile(x, na.rm = TRUE, probs = c(0.25, 0.5, 0.75), type = 2), 2)
    
    summary <-
      data.frame(
        Statistic = c("min", "Q1", "median", "mean", "Q3", "max", "sd", "n"),
        Value = c(
          round(min(x, na.rm = TRUE), 2),
          qvals[1],
          qvals[2],
          round(mean(x, na.rm = TRUE), 2),
          qvals[3],
          round(max(x, na.rm = TRUE), 2),
          round(stats::sd(x, na.rm = TRUE), 2),
          length(x[!is.na(x)])
        )
      )
    
    DT::datatable(summary, rownames = FALSE, options = list(dom = "<t>", pageLength = dt_rows))
  } else if (is.factor(x) || is.character(x) || (is.numeric(x) && numeric_as_factor) || is.logical(x)) {
    if (is.numeric(x)) {
      x <- factor(x, levels = sort(unique(x)))
    }
    
    level_counts <- table(x)
    max_levels_signif <- nchar(level_counts)
    
    if (!all(is.na(x))) {
      levels <- names(level_counts)
      counts <- sprintf(
        "%s [%.2f%%]",
        format(level_counts, width = max_levels_signif), prop.table(level_counts) * 100
      )
    } else {
      levels <- character(0)
      counts <- numeric(0)
    }
    
    summary <- data.frame(
      Level = levels,
      Count = counts,
      stringsAsFactors = FALSE
    )
    
    summary <- summary[order(summary$Count, decreasing = TRUE), ]
    
    dom_opts <- if (nrow(summary) <= 10) {
      "<t>"
    } else {
      "<lf<t>ip>"
    }
    DT::datatable(summary, rownames = FALSE, options = list(dom = dom_opts, pageLength = dt_rows))
  } else if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    summary <-
      data.frame(
        Statistic = c("min", "median", "max"),
        Value = c(
          min(x, na.rm = TRUE),
          stats::median(x, na.rm = TRUE),
          max(x, na.rm = TRUE)
        )
      )
    DT::datatable(summary, rownames = FALSE, options = list(dom = "<t>", pageLength = dt_rows))
  } else {
    NULL
  }
}

#' Plot a summary of a variable
#' @param var A vector containing the variable data
#' @param var_lab A label for the variable
#' @param wrap_character Optional character width for wrapping text in the plot
#' @param numeric_as_factor Logical indicating whether to treat numeric variables as factors
#' @param display_density Logical indicating whether to display density in the plot
#' @param remove_NA_hist Logical indicating whether to remove NA values from the histogram
#' @param outlier_definition Numeric value defining the outlier threshold
#' @param records_for_factor Integer defining the minimum number of unique records for factor variables
#' @param ggplot2_args A list of additional ggplot2 arguments
#' 
#' @returns A ggplot object or a grid grob for the variable summary plot
plot_var_summary <- function(var,
                             var_lab,
                             wrap_character = NULL,
                             numeric_as_factor,
                             display_density = is.numeric(var),
                             remove_NA_hist = FALSE,
                             outlier_definition,
                             records_for_factor,
                             ggplot2_args) {
  checkmate::assert_character(var_lab)
  checkmate::assert_numeric(wrap_character, null.ok = TRUE)
  checkmate::assert_flag(numeric_as_factor)
  checkmate::assert_flag(display_density)
  checkmate::assert_logical(remove_NA_hist, null.ok = TRUE)
  checkmate::assert_number(outlier_definition, lower = 0, finite = TRUE)
  checkmate::assert_integerish(records_for_factor, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")
  
  #grid::grid.newpage()
  
  plot_main <- if (is.factor(var) || is.character(var) || is.logical(var)) {
    groups <- unique(as.character(var))
    len_groups <- length(groups)
    if (len_groups >= records_for_factor) {
      grid::textGrob(
        sprintf(
          "%s unique values\n%s:\n %s\n ...\n %s",
          len_groups,
          var_lab,
          paste(utils::head(groups), collapse = ",\n "),
          paste(utils::tail(groups), collapse = ",\n ")
        ),
        x = grid::unit(1, "line"),
        y = grid::unit(1, "npc") - grid::unit(1, "line"),
        just = c("left", "top")
      )
    } else {
      if (!is.null(wrap_character)) {
        var <- stringr::str_wrap(var, width = wrap_character)
      }
      var <- if (isTRUE(remove_NA_hist)) as.vector(stats::na.omit(var)) else var
      ggplot2::ggplot(data.frame(var), ggplot2::aes(x = forcats::fct_infreq(as.factor(var)))) +
        ggplot2::geom_bar(
          stat = "count", ggplot2::aes(fill = ifelse(is.na(var), "withcolor", "")), show.legend = FALSE
        ) +
        ggplot2::scale_fill_manual(values = c("gray50", "tan"))
    }
  } else if (is.numeric(var)) {
    validate(need(any(!is.na(var)), "No data left to visualize."))
    
    var <- var[which(!is.na(var))]
    
    validate(need(!any(is.infinite(var)), "Cannot display graph when data includes infinite values"))
    
    if (numeric_as_factor) {
      var <- factor(var)
      ggplot2::ggplot(NULL, ggplot2::aes(x = var)) +
        ggplot2::geom_histogram(stat = "count")
    } else {
      if (outlier_definition != 0) {
        number_records <- length(var)
        var <- remove_outliers_from(var, outlier_definition)
        number_outliers <- number_records - length(var)
        outlier_text <- paste0(
          number_outliers, " outliers (",
          round(number_outliers / number_records * 100, 2),
          "% of non-missing records) not shown"
        )
        validate(need(
          length(var) > 1,
          "At least two data points must remain after removing outliers for this graph to be displayed"
        ))
      }
      binwidth <- get_bin_width(var)
      p <- ggplot2::ggplot(data = data.frame(var = var), ggplot2::aes(x = var, y = ggplot2::after_stat(count))) +
        ggplot2::geom_histogram(binwidth = binwidth) +
        ggplot2::scale_y_continuous(
          sec.axis = ggplot2::sec_axis(
            trans = ~ . / nrow(data.frame(var = var)),
            labels = scales::percent,
            name = "proportion (in %)"
          )
        )
      
      if (display_density) {
        p <- p + ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count * binwidth)))
      }
      
      if (outlier_definition != 0) {
        p <- p + ggplot2::annotate(
          geom = "text",
          label = outlier_text,
          x = Inf, y = Inf,
          hjust = 1.02, vjust = 1.2,
          color = "black",
          size = ggplot2_args[["theme"]][["text"]][["size"]] / 3.5
        )
      }
      p
    }
  } else if (inherits(var, "Date") || inherits(var, "POSIXct") || inherits(var, "POSIXlt")) {
    var_num <- as.numeric(var)
    binwidth <- get_bin_width(var_num, 1)
    p <- ggplot2::ggplot(data = data.frame(var = var), ggplot2::aes(x = var, y = ggplot2::after_stat(count))) +
      ggplot2::geom_histogram(binwidth = binwidth)
  } else {
    grid::textGrob(
      paste(strwrap(
        utils::capture.output(utils::str(var)),
        width = .9 * grid::convertWidth(grid::unit(1, "npc"), "char", TRUE)
      ), collapse = "\n"),
      x = grid::unit(1, "line"), y = grid::unit(1, "npc") - grid::unit(1, "line"), just = c("left", "top")
    )
  }
  
  dev_ggplot2_args <- teal.widgets::ggplot2_args(
    labs = list(x = var_lab)
  )
  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    ggplot2_args,
    module_plot = dev_ggplot2_args
  )
  
  if (is.ggplot(plot_main)) {
    if (is.numeric(var) && !numeric_as_factor) {
      plot_main <- plot_main +
        theme_light() +
        list(
          labs = do.call("labs", all_ggplot2_args$labs),
          theme = do.call("theme", all_ggplot2_args$theme)
        )
    } else {
      plot_main <- plot_main +
        theme_light() +
        list(
          labs = do.call("labs", all_ggplot2_args$labs),
          theme = do.call("theme", all_ggplot2_args$theme)
        )
    }
    plot_main <- ggplot2::ggplotGrob(plot_main)
  }
  
  #grid::grid.draw(plot_main)
  plot_main
}


#' Render the content for a single tab in the variable browser
#' @param dataset_name The name of the dataset to be displayed
#' @param parent_dataname The name of the parent dataset, if any
#' @param output The Shiny output object
#' @param data_list A reactive list containing the datasets
#' @param input The Shiny input object
#' @param columns_names A reactive list containing the column names for each dataset
#' @param plot_var A reactive list containing the variable to be plotted
#' 
#' @returns A rendered tab content for the variable browser
render_single_tab_content <- function(dataset_name, parent_dataname, output, data_list, input, columns_names, plot_var) {
  render_tab_header(dataset_name, output, data_list)
  
  render_tab_table(
    dataset_name = dataset_name,
    parent_dataname = parent_dataname,
    output = output,
    data = data_list,
    input = input,
    columns_names = columns_names,
    plot_var = plot_var
  )
}


#' Render the header for a tab in the variable browser
#' @param dataset_name The name of the dataset to be displayed
#' @param output The Shiny output object
#' @param data_list A reactive list containing the datasets
#' 
#' @returns A rendered text output containing the dataset summary
render_tab_header <- function(dataset_name, output, data_list) {
  dataset_ui_id <- paste0("dataset_summary_", dataset_name)
  output[[dataset_ui_id]] <- renderText({
    df <- data_list[[dataset_name]]
    
    sprintf(
      "Dataset with %s rows and %s variables",
      nrow(df),
      ncol(df)
    )
  })
}

#' Render the table for a tab in the variable browser
#' @param dataset_name The name of the dataset to be displayed
#' @param parent_dataname The name of the parent dataset, if any
#' @param output The Shiny output object
#' @param data_list A reactive list containing the datasets
#' @param input The Shiny input object
#' @param columns_names A reactive list containing the column names for each dataset
#' @param plot_var A reactive list containing the variable to be plotted
#' 
#' @returns A rendered DataTable containing the variable information
render_tab_table <- function(dataset_name, parent_dataname, output, data_list, input, columns_names, plot_var) {
  table_ui_id <- paste0("variable_browser_", dataset_name)
  
  output[[table_ui_id]] <- DT::renderDataTable({
    df <- data_list[[dataset_name]]
    
    get_vars_df <- function(input, dataset_name, parent_name, data_list_inner) {
      data_cols <- colnames(df)
      if (isTRUE(input$show_parent_vars)) {
        data_cols
      } else if (dataset_name != parent_name && parent_name %in% names(data_list_inner)) {
        setdiff(data_cols, colnames(data_list_inner[[parent_name]]))
      } else {
        data_cols
      }
    }
    
    if (length(parent_dataname) > 0) {
      df_vars <- get_vars_df(input, dataset_name, parent_dataname, data_list)
      df <- df[df_vars]
    }
    
    if (is.null(df) || ncol(df) == 0) {
      columns_names[[dataset_name]] <- character(0)
      df_output <- data.frame(
        Type = character(0),
        Variable = character(0),
        Label = character(0),
        Missings = character(0),
        stringsAsFactors = FALSE
      )
    } else {

      labels <- sapply(names(df), function(col_name) attr(df[[col_name]], "label") %||% col_name)
      
      columns_names[[dataset_name]] <- names(labels)
      
      missings <- vapply(
        df,
        var_missings_info,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      )
      
      icons <- vapply(df, function(x) class(x)[1L], character(1L))
      
      join_keys_object <- teal.data::join_keys(data_list)
      if (!is.null(join_keys_object)) {
        keys_list <- as.list(join_keys_object) # Convert to a simple list
        if (!is.null(keys_list[[dataset_name]])) {
          primary_keys_for_dataset <- unlist(keys_list[[dataset_name]])
          if (length(primary_keys_for_dataset) > 0) {
            icons[intersect(primary_keys_for_dataset, colnames(df))] <- "primary_key"
          }
        }
      }
      icons <- variable_type_icons(icons)
      
      df_output <- data.frame(
        Type = icons,
        Variable = names(labels),
        Label = labels,
        Missings = missings,
        stringsAsFactors = FALSE
      )
    }
    
    selected_ix <- 1
    selected_page_ix <- 0
    
    isolated_variable <- isolate(plot_var$variable[[dataset_name]])
    
    if (!is.null(isolated_variable)) {
      index <- which(columns_names[[dataset_name]] == isolated_variable)[1]
      if (!is.null(index) && !is.na(index) && length(index) > 0) selected_ix <- index
    }
    
    table_id_sel <- paste0(table_ui_id, "_state")
    dt_state <- isolate(input[[table_id_sel]])
    if (selected_ix != 1 && !is.null(dt_state)) {
      selected_page_ix <- floor(selected_ix / dt_state$length) * dt_state$length
    }
    
    DT::datatable(
      df_output,
      escape = FALSE,
      rownames = FALSE,
      selection = list(mode = "single", target = "row", selected = selected_ix),
      options = list(
        fnDrawCallback = htmlwidgets::JS("function() { HTMLWidgets.staticRender(); }"),
        pageLength = input[[paste0(table_ui_id, "_rows")]],
        displayStart = selected_page_ix
      )
    )
  })
}

