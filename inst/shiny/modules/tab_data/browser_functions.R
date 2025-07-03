library(shiny)
library(DT)
library(sparkline)
library(ggplot2)
library(stringr)
library(forcats)
library(checkmate)
library(teal.widgets)
library(teal.reporter)
library(teal.data)
library(lifecycle)
library(dplyr)
library(shinyWidgets)
library(teal) # Added for teal::validate_* functions

# --- Helper functions (extracted from the tm_variable_browser's internal functions) ---

ggplot_themes <- c(
  "grey", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"
)

var_missings_info <- function(x) {
  sprintf("%s [%s%%]", sum(is.na(x)), round(mean(is.na(x) * 100), 2))
}

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
  
  grid::grid.newpage()
  
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
  
  grid::grid.draw(plot_main)
  plot_main
}

is_num_var_short <- function(.unique_records_for_factor, input, data_for_analysis) {
  length(unique(data_for_analysis()$data)) < .unique_records_for_factor && !is.null(input$vb_numeric_as_factor)
}

validate_input <- function(input, plot_var, data_list_reactive, dataset_name) {
  reactive({
    req(dataset_name)
    varname <- plot_var$variable[[dataset_name]]
    
    validate(need(dataset_name, "No data selected"))
    validate(need(varname, "No variable selected"))
    df <- data_list_reactive()[[dataset_name]]
    # These functions are in the `teal` package, which is now loaded.
    teal::validate_has_data(df, 1)
    teal::validate_has_variable(varname = varname, data = df, "Variable not available")
    
    TRUE
  })
}

get_plotted_data <- function(input, plot_var, data_list_reactive, dataset_name) {
  varname <- plot_var$variable[[dataset_name]]
  df <- data_list_reactive()[[dataset_name]]
  
  var_description <- attr(df[[varname]], "label") %||% varname
  list(data = df[[varname]], var_description = var_description)
}

render_single_tab_content <- function(dataset_name, parent_dataname, output, data_list_reactive, input, columns_names, plot_var) {
  render_tab_header(dataset_name, output, data_list_reactive)
  
  render_tab_table(
    dataset_name = dataset_name,
    parent_dataname = parent_dataname,
    output = output,
    data = data_list_reactive,
    input = input,
    columns_names = columns_names,
    plot_var = plot_var
  )
}

# CORRECTED RENDER_TAB_HEADER
render_tab_header <- function(dataset_name, output, data_list_reactive) {
  dataset_ui_id <- paste0("vb_dataset_summary_", dataset_name)
  output[[dataset_ui_id]] <- renderText({
    df <- data_list_reactive()[[dataset_name]]
    join_keys_info <- "N/A"
    
    # CORRECTED: Use the teal.data::join_keys() accessor function
    if (inherits(data_list_reactive(), "teal_data")) {
      keys_obj <- teal.data::join_keys(data_list_reactive())
      keys <- keys_obj[dataset_name] # Extract keys for the specific dataset
      
      # Check if there are any keys and they are not empty
      if (length(keys) > 0 && length(unlist(keys)) > 0) {
        # This will get a character vector of the primary keys.
        # The structure can be complex, unlist is a simple way to display them.
        join_keys_info <- paste(names(unlist(keys)), collapse = ", ")
      }
    }
    
    sprintf(
      "Dataset with %s rows and %s variables (Join keys: %s)",
      nrow(df),
      ncol(df),
      join_keys_info
    )
  })
}


# CORRECTED RENDER_TAB_TABLE
render_tab_table <- function(dataset_name, parent_dataname, output, data_list_reactive, input, columns_names, plot_var) {
  table_ui_id <- paste0("vb_variable_browser_", dataset_name)
  
  output[[table_ui_id]] <- DT::renderDataTable({
    df <- data_list_reactive()[[dataset_name]]
    
    get_vars_df <- function(input, dataset_name, parent_name, data_list_reactive_inner) {
      data_cols <- colnames(df)
      if (isTRUE(input$vb_show_parent_vars)) {
        data_cols
      } else if (dataset_name != parent_name && parent_name %in% names(data_list_reactive_inner())) {
        setdiff(data_cols, colnames(data_list_reactive_inner()[[parent_name]]))
      } else {
        data_cols
      }
    }
    
    if (length(parent_dataname) > 0) {
      df_vars <- get_vars_df(input, dataset_name, parent_dataname, data_list_reactive)
      df <- df[df_vars]
    }
    
    if (is.null(df) || ncol(df) == 0) {
      columns_names[[dataset_name]] <- character(0)
      df_output <- data.frame(
        Type = character(0),
        Variable = character(0),
        Label = character(0),
        Missings = character(0),
        Sparklines = character(0),
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
      
      # CORRECTED: Use the teal.data::join_keys() accessor
      join_keys_object <- teal.data::join_keys(data_list_reactive())
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
      
      sparklines_html <- vapply(
        df,
        create_sparklines,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      )
      
      df_output <- data.frame(
        Type = icons,
        Variable = names(labels),
        Label = labels,
        Missings = missings,
        Sparklines = sparklines_html,
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


establish_updating_selection <- function(dataset_name, input, plot_var, columns_names) {
  table_ui_id <- paste0("vb_variable_browser_", dataset_name)
  table_id_sel <- paste0(table_ui_id, "_rows_selected")
  observeEvent(input[[table_id_sel]], {
    plot_var$data <- dataset_name
    plot_var$variable[[dataset_name]] <- columns_names[[dataset_name]][input[[table_id_sel]]]
  })
}

get_bin_width <- function(x_vec, scaling_factor = 2) {
  x_vec <- x_vec[!is.na(x_vec)]
  qntls <- stats::quantile(x_vec, probs = c(0.1, 0.25, 0.75, 0.9), type = 2)
  iqr <- qntls[3] - qntls[2]
  binwidth <- max(scaling_factor * iqr / length(x_vec) ^ (1 / 3), sqrt(qntls[4] - qntls[1]))
  binwidth <- ifelse(binwidth == 0, 1, binwidth)
  x_span <- diff(range(x_vec))
  if (isTRUE(x_span / binwidth >= 2)) binwidth else x_span / 2
}

remove_outliers_from <- function(var, outlier_definition) {
  if (outlier_definition == 0) {
    return(var)
  }
  q1_q3 <- stats::quantile(var, probs = c(0.25, 0.75), type = 2, na.rm = TRUE)
  iqr <- q1_q3[2] - q1_q3[1]
  var[var >= q1_q3[1] - outlier_definition * iqr & var <= q1_q3[2] + outlier_definition * iqr]
}

create_sparklines <- function(arr, width = 150, ...) {
  if (all(is.null(arr))) {
    return("")
  }
  UseMethod("create_sparklines")
}

create_sparklines.logical <- function(arr, ...) {
  create_sparklines(as.factor(arr))
}

create_sparklines.numeric <- function(arr, width = 150, ...) {
  if (any(is.infinite(arr))) {
    return(as.character(tags$code("infinite values", class = "text-blue")))
  }
  if (length(arr) > 100000) {
    return(as.character(tags$code("Too many rows (>100000)", class = "text-blue")))
  }
  
  arr <- arr[!is.na(arr)]
  sparkline::spk_chr(unname(arr), type = "box", width = width, ...)
}

create_sparklines.character <- function(arr, ...) {
  return(create_sparklines(as.factor(arr)))
}

# MINOR BUG FIX in this function
create_sparklines.factor <- function(arr, width = 150, bar_spacing = 5, bar_width = 20, ...) {
  decreasing_order <- TRUE
  
  counts <- table(arr)
  if (length(counts) >= 100) {
    return(as.character(tags$code("> 99 levels", class = "text-blue")))
  } else if (length(counts) == 0) {
    return(as.character(tags$code("no levels", class = "text-blue")))
  } else if (length(counts) == 1) {
    return(as.character(tags$code("one level", class = "text-blue")))
  }
  
  counts <- sort(counts, decreasing = decreasing_order, method = "radix")
  # Corrected: length[counts] to length(counts)
  max_value <- if (decreasing_order) counts[1] else counts[length(counts)]
  max_value <- unname(max_value)
  
  sparkline::spk_chr(
    unname(counts),
    type = "bar",
    chartRangeMin = 0,
    chartRangeMax = max_value,
    width = width,
    barWidth = bar_width,
    barSpacing = bar_spacing,
    tooltipFormatter = custom_sparkline_formatter(names(counts), as.vector(counts))
  )
}

create_sparklines.Date <- function(arr, width = 150, bar_spacing = 5, bar_width = 20, ...) {
  arr_num <- as.numeric(arr)
  arr_num <- sort(arr_num, decreasing = FALSE, method = "radix")
  binwidth <- get_bin_width(arr_num, 1)
  bins <- floor(diff(range(arr_num)) / binwidth) + 1
  if (all(is.na(bins))) {
    return(as.character(tags$code("only NA", class = "text-blue")))
  } else if (bins == 1) {
    return(as.character(tags$code("one date", class = "text-blue")))
  }
  counts <- as.vector(unname(base::table(cut(arr_num, breaks = bins))))
  max_value <- max(counts)
  
  start_bins <- as.integer(seq(1, length(arr_num), length.out = bins))
  labels_start <- as.character(as.Date(arr_num[start_bins], origin = as.Date("1970-01-01")))
  labels <- paste("Start:", labels_start)
  
  sparkline::spk_chr(
    unname(counts),
    type = "bar",
    chartRangeMin = 0,
    chartRangeMax = max_value,
    width = width,
    barWidth = bar_width,
    barSpacing = bar_spacing,
    tooltipFormatter = custom_sparkline_formatter(labels, counts)
  )
}

create_sparklines.POSIXct <- function(arr, width = 150, bar_spacing = 5, bar_width = 20, ...) {
  arr_num <- as.numeric(arr)
  arr_num <- sort(arr_num, decreasing = FALSE, method = "radix")
  binwidth <- get_bin_width(arr_num, 1)
  bins <- floor(diff(range(arr_num)) / binwidth) + 1
  if (all(is.na(bins))) {
    return(as.character(tags$code("only NA", class = "text-blue")))
  } else if (bins == 1) {
    return(as.character(tags$code("one date-time", class = "text-blue")))
  }
  counts <- as.vector(unname(base::table(cut(arr_num, breaks = bins))))
  max_value <- max(counts)
  
  start_bins <- as.integer(seq(1, length(arr_num), length.out = bins))
  labels_start <- as.character(format(as.POSIXct(arr_num[start_bins], origin = as.Date("1970-01-01")), "%Y-%m-%d"))
  labels <- paste("Start:", labels_start)
  
  sparkline::spk_chr(
    unname(counts),
    type = "bar",
    chartRangeMin = 0,
    chartRangeMax = max_value,
    width = width,
    barWidth = bar_width,
    barSpacing = bar_spacing,
    tooltipFormatter = custom_sparkline_formatter(labels, counts)
  )
}

create_sparklines.POSIXlt <- function(arr, width = 150, bar_spacing = 5, bar_width = 20, ...) {
  arr_num <- as.numeric(arr)
  arr_num <- sort(arr_num, decreasing = FALSE, method = "radix")
  binwidth <- get_bin_width(arr_num, 1)
  bins <- floor(diff(range(arr_num)) / binwidth) + 1
  if (all(is.na(bins))) {
    return(as.character(tags$code("only NA", class = "text-blue")))
  } else if (bins == 1) {
    return(as.character(tags$code("one date-time", class = "text-blue")))
  }
  counts <- as.vector(unname(base::table(cut(arr_num, breaks = bins))))
  max_value <- max(counts)
  
  start_bins <- as.integer(seq(1, length(arr_num), length.out = bins))
  labels_start <- as.character(format(as.POSIXct(arr_num[start_bins], origin = as.Date("1970-01-01")), "%Y-%m-%d"))
  labels <- paste("Start:", labels_start)
  
  sparkline::spk_chr(
    unname(counts),
    type = "bar",
    chartRangeMin = 0,
    chartRangeMax = max_value,
    width = width,
    barWidth = bar_width,
    barSpacing = bar_spacing,
    tooltipFormatter = custom_sparkline_formatter(labels, counts)
  )
}

create_sparklines.default <- function(arr, width = 150, ...) {
  as.character(tags$code("unsupported variable type", class = "text-blue"))
}

custom_sparkline_formatter <- function(labels, counts) {
  htmlwidgets::JS(
    sprintf(
      "function(sparkline, options, field) {
        return 'ID: ' + %s[field[0].offset] + '<br>' + 'Count: ' + %s[field[0].offset];
        }",
      jsonlite::toJSON(labels),
      jsonlite::toJSON(counts)
    )
  )
}

variable_type_icons <- function(x) {
  gsub("numeric", '<i>&#xf1d4;</i>',
       gsub("factor", '<i>&#xf036;</i>',
            gsub("character", '<i>&#xf035;</i>',
                 gsub("Date", '<i>&#xf073;</i>',
                      gsub("POSIXct", '<i>&#xf073;</i>',
                           gsub("POSIXlt", '<i>&#xf073;</i>',
                                gsub("logical", '<i>&#xf03d;</i>',
                                     gsub("primary_key", '<i>&#xf084;</i>', x, fixed = TRUE), fixed = TRUE),
                                fixed = TRUE),
                           fixed = TRUE),
                      fixed = TRUE),
                 fixed = TRUE),
            fixed = TRUE),
       fixed = TRUE)
}

# --- Standalone Variable Browser UI and Server Functions ---

# UI function for the variable browser
variable_browser_ui <- function(dataname, pre_output = NULL, post_output = NULL) {
  id_prefix <- "vb_" # Variable Browser ID prefix
  
  tagList(
    shinyjs::useShinyjs(),
    
    teal.widgets::standard_layout(
      output = fluidRow(
        htmlwidgets::getDependency("sparkline"),
        column(
          6,
          teal.widgets::white_small_well(
            uiOutput(paste0(id_prefix, "ui_variable_browser")),
            shinyjs::hidden({
              checkboxInput(paste0(id_prefix, "show_parent_vars"), "Show parent dataset variables", value = FALSE)
            })
          )
        ),
        column(
          6,
          teal.widgets::white_small_well(
            tags$div(
              class = "block",
              uiOutput(paste0(id_prefix, "ui_histogram_display"))
            ),
            tags$div(
              class = "block",
              uiOutput(paste0(id_prefix, "ui_numeric_display"))
            ),
            teal.widgets::plot_with_settings_ui(paste0(id_prefix, "variable_plot")),
            tags$br(),
            teal.widgets::panel_item(
              title = "Plot settings",
              collapsed = TRUE,
              selectInput(
                inputId = paste0(id_prefix, "ggplot_theme"), label = "ggplot2 theme",
                choices = ggplot_themes,
                selected = "grey"
              ),
              fluidRow(
                column(6, sliderInput(
                  inputId = paste0(id_prefix, "font_size"), label = "font size",
                  min = 5L, max = 30L, value = 15L, step = 1L, ticks = FALSE
                )),
                column(6, sliderInput(
                  inputId = paste0(id_prefix, "label_rotation"), label = "rotate x labels",
                  min = 0L, max = 90L, value = 45L, step = 1, ticks = FALSE
                ))
              )
            ),
            tags$br(),
            teal.widgets::get_dt_rows(paste0(id_prefix, "variable_summary_table"), paste0(id_prefix, "variable_summary_table_rows")),
            DT::dataTableOutput(paste0(id_prefix, "variable_summary_table"))
          )
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

# Corrected variable_browser_server function
variable_browser_server <- function(input, output, session, data_list_reactive, dataname_param, parent_dataname_param, ggplot2_args_param = teal.widgets::ggplot2_args()) {
  
  id_prefix <- "vb_" # Variable Browser ID prefix
  
  .unique_records_for_factor <- 30
  .unique_records_default_as_factor <- 6
  
  varname_numeric_as_factor <- reactiveValues()
  
  output[[paste0(id_prefix, "ui_variable_browser")]] <- renderUI({
    req(dataname_param)
    tabsetPanel(
      id = paste0(id_prefix, "tabset_panel"),
      tabPanel(
        dataname_param,
        tags$div(
          class = "mt-4",
          textOutput(paste0(id_prefix, "dataset_summary_", dataname_param))
        ),
        tags$div(
          class = "mt-4",
          teal.widgets::get_dt_rows(
            paste0(id_prefix, "variable_browser_", dataname_param),
            paste0(id_prefix, "variable_browser_", dataname_param, "_rows")
          ),
          DT::dataTableOutput(paste0(id_prefix, "variable_browser_", dataname_param), width = "100%")
        )
      )
    )
  })
  
  observe({
    shinyjs::toggle(
      id = paste0(id_prefix, "show_parent_vars"),
      condition = length(parent_dataname_param) > 0 && parent_dataname_param %in% names(data_list_reactive())
    )
  })
  
  columns_names <- new.env()
  
  plot_var <- reactiveValues(data = NULL, variable = list())
  
  observe({
    # Only establish for the single dataname
    establish_updating_selection(dataname_param, input, plot_var, columns_names)
  })
  
  validation_checks <- validate_input(input, plot_var, data_list_reactive, dataname_param)
  
  plotted_data <- reactive({
    validation_checks()
    get_plotted_data(input, plot_var, data_list_reactive, dataname_param)
  })
  
  treat_numeric_as_factor <- reactive({
    if (is_num_var_short(.unique_records_for_factor, input, plotted_data)) {
      input[[paste0(id_prefix, "numeric_as_factor")]]
    } else {
      FALSE
    }
  })
  
  # Render content for the single tab
  observe({
    render_single_tab_content(
      dataset_name = dataname_param,
      parent_dataname = parent_dataname_param,
      output = output,
      data_list_reactive = data_list_reactive,
      input = input,
      columns_names = columns_names,
      plot_var = plot_var
    )
  })
  
  all_ggplot2_args <- reactive({
    user_text <- teal.widgets::ggplot2_args(
      theme = list(
        "text" = ggplot2::element_text(size = input[[paste0(id_prefix, "font_size")]]),
        "axis.text.x" = ggplot2::element_text(angle = input[[paste0(id_prefix, "label_rotation")]], hjust = 1)
      )
    )
    req(input[[paste0(id_prefix, "ggplot_theme")]])
    user_theme <- utils::getFromNamespace(sprintf("theme_%s", input[[paste0(id_prefix, "ggplot_theme")]]), ns = "ggplot2")
    user_theme <- user_theme()
    user_theme <- user_theme[grep("strip.text.y.left", names(user_theme), fixed = TRUE, invert = TRUE)]
    
    teal.widgets::resolve_ggplot2_args(
      user_plot = user_text,
      user_default = teal.widgets::ggplot2_args(theme = user_theme),
      module_plot = ggplot2_args_param
    )
  })
  
  output[[paste0(id_prefix, "ui_numeric_display")]] <- renderUI({
    validation_checks()
    dataname <- dataname_param # Using the direct parameter
    varname <- plot_var$variable[[dataname]]
    df <- data_list_reactive()[[dataname]]
    
    numeric_ui <- tagList(
      fluidRow(
        tags$div(
          class = "col-md-4",
          tags$br(),
          shinyWidgets::switchInput(
            inputId = paste0(id_prefix, "display_density"),
            label = "Show density",
            value = `if`(is.null(isolate(input[[paste0(id_prefix, "display_density")]])), TRUE, isolate(input[[paste0(id_prefix, "display_density")]])),
            width = "50%",
            labelWidth = "100px",
            handleWidth = "50px"
          )
        ),
        tags$div(
          class = "col-md-4",
          tags$br(),
          shinyWidgets::switchInput(
            inputId = paste0(id_prefix, "remove_outliers"),
            label = "Remove outliers",
            value = `if`(is.null(isolate(input[[paste0(id_prefix, "remove_outliers")]])), FALSE, isolate(input[[paste0(id_prefix, "remove_outliers")]])),
            width = "50%",
            labelWidth = "100px",
            handleWidth = "50px"
          )
        ),
        tags$div(
          class = "col-md-4",
          uiOutput(paste0(id_prefix, "outlier_definition_slider_ui"))
        )
      ),
      tags$div(
        class = "ml-4",
        uiOutput(paste0(id_prefix, "ui_density_help")),
        uiOutput(paste0(id_prefix, "ui_outlier_help"))
      )
    )
    
    observeEvent(input[[paste0(id_prefix, "numeric_as_factor")]], ignoreInit = TRUE, {
      varname_numeric_as_factor[[plot_var$variable[[dataname]]]] <- input[[paste0(id_prefix, "numeric_as_factor")]]
    })
    
    if (is.numeric(df[[varname]])) {
      unique_entries <- length(unique(df[[varname]]))
      if (unique_entries < .unique_records_for_factor && unique_entries > 0) {
        list(
          checkboxInput(
            paste0(id_prefix, "numeric_as_factor"),
            "Treat variable as factor",
            value = `if`(
              is.null(varname_numeric_as_factor[[varname]]),
              unique_entries < .unique_records_default_as_factor,
              varname_numeric_as_factor[[varname]]
            )
          ),
          conditionalPanel(
            paste0("!input.", id_prefix, "numeric_as_factor"),
            ns = function(id) paste0(id_prefix, id),
            numeric_ui
          )
        )
      } else if (unique_entries > 0) {
        numeric_ui
      }
    } else {
      NULL
    }
  })
  
  output[[paste0(id_prefix, "ui_histogram_display")]] <- renderUI({
    validation_checks()
    dataname <- dataname_param # Using the direct parameter
    varname <- plot_var$variable[[dataname]]
    df <- data_list_reactive()[[dataname]]
    
    numeric_ui <- tagList(fluidRow(
      tags$div(
        class = "col-md-4",
        shinyWidgets::switchInput(
          inputId = paste0(id_prefix, "remove_NA_hist"),
          label = "Remove NA values",
          value = FALSE,
          width = "50%",
          labelWidth = "100px",
          handleWidth = "50px"
        )
      )
    ))
    
    var <- df[[varname]]
    if (anyNA(var) && (is.factor(var) || is.character(var) || is.logical(var))) {
      groups <- unique(as.character(var))
      len_groups <- length(groups)
      if (len_groups >= .unique_records_for_factor) {
        NULL
      } else {
        numeric_ui
      }
    } else {
      NULL
    }
  })
  
  output[[paste0(id_prefix, "outlier_definition_slider_ui")]] <- renderUI({
    req(input[[paste0(id_prefix, "remove_outliers")]])
    sliderInput(
      inputId = paste0(id_prefix, "outlier_definition_slider"),
      tags$div(
        class = "teal-tooltip",
        tagList(
          "Outlier definition:",
          icon("circle-info"),
          tags$span(
            class = "tooltiptext",
            paste(
              "Use the slider to choose the cut-off value to define outliers; the larger the value the",
              "further below Q1/above Q3 points have to be in order to be classed as outliers"
            )
          )
        )
      ),
      min = 1,
      max = 5,
      value = 3,
      step = 0.5
    )
  })
  
  output[[paste0(id_prefix, "ui_density_help")]] <- renderUI({
    req(is.logical(input[[paste0(id_prefix, "display_density")]]))
    if (input[[paste0(id_prefix, "display_density")]]) {
      tags$small(helpText(paste(
        "Kernel density estimation with gaussian kernel",
        "and bandwidth function bw.nrd0 (R default)"
      )))
    } else {
      NULL
    }
  })
  
  output[[paste0(id_prefix, "ui_outlier_help")]] <- renderUI({
    req(is.logical(input[[paste0(id_prefix, "remove_outliers")]]), input[[paste0(id_prefix, "outlier_definition_slider")]])
    if (input[[paste0(id_prefix, "remove_outliers")]]) {
      tags$small(
        helpText(
          withMathJax(paste0(
            "Outlier data points (\\( X \\lt Q1 - ", input[[paste0(id_prefix, "outlier_definition_slider")]], "\\times IQR \\) or
             \\(Q3 + ", input[[paste0(id_prefix, "outlier_definition_slider")]], "\\times IQR \\lt X\\))
             have not been displayed on the graph and will not be used for any kernel density estimations, ",
            "although their values remain in the statisics table below."
          ))
        )
      )
    } else {
      NULL
    }
  })
  
  variable_plot_r <- reactive({
    display_density <- `if`(is.null(input[[paste0(id_prefix, "display_density")]]), FALSE, input[[paste0(id_prefix, "display_density")]])
    remove_outliers <- `if`(is.null(input[[paste0(id_prefix, "remove_outliers")]]), FALSE, input[[paste0(id_prefix, "remove_outliers")]])
    
    if (remove_outliers) {
      req(input[[paste0(id_prefix, "outlier_definition_slider")]])
      outlier_definition <- as.numeric(input[[paste0(id_prefix, "outlier_definition_slider")]])
    } else {
      outlier_definition <- 0
    }
    
    plot_var_summary(
      var = plotted_data()$data,
      var_lab = plotted_data()$var_description,
      wrap_character = 15,
      numeric_as_factor = treat_numeric_as_factor(),
      remove_NA_hist = input[[paste0(id_prefix, "remove_NA_hist")]],
      display_density = display_density,
      outlier_definition = outlier_definition,
      records_for_factor = .unique_records_for_factor,
      ggplot2_args = all_ggplot2_args()
    )
  })
  
  teal.widgets::plot_with_settings_srv(
    id = paste0(id_prefix, "variable_plot"),
    plot_r = variable_plot_r,
    height = c(500, 200, 2000)
  )
  
  output[[paste0(id_prefix, "variable_summary_table")]] <- DT::renderDataTable({
    var_summary_table(
      plotted_data()$data,
      treat_numeric_as_factor(),
      input[[paste0(id_prefix, "variable_summary_table_rows")]],
      if (!is.null(input[[paste0(id_prefix, "remove_outliers")]]) && input[[paste0(id_prefix, "remove_outliers")]]) {
        req(input[[paste0(id_prefix, "outlier_definition_slider")]])
        as.numeric(input[[paste0(id_prefix, "outlier_definition_slider")]])
      } else {
        0
      }
    )
  })
}


# --- Your Simple Shiny App ---

# Define UI for the combined application
ui <- fluidPage(
  titlePanel("Simple Shiny App with Variable Browser"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num", "Enter a number:", value = 1),
      actionButton("square_btn", "Get Square"),
      tags$hr(), # Separator for visual clarity
      
      # Directly embed the variable browser UI
    ),
    mainPanel(
      textOutput("result"),
      
      variable_browser_ui(
        dataname = "ADPC"
      )
    )
  )
)

# Define server logic for the combined application
server <- function(input, output, session) {
  # Your existing app logic
  observeEvent(input$square_btn, {
    num <- input$num
    square <- num^2
    output$result <- renderText({
      paste("The square of", num, "is", square)
    })
  })
  
  # Call the server function for the variable browser
  data_for_vb <- reactive({
    # --- IMPORTANT ---
    # Please ensure this path is correct for your system.
    data_path <- "C:/Users/legrasv/Downloads/ADPC_GO43075.rds"
    
    # Check if the file exists before attempting to read
    if (!file.exists(data_path)) {
      shiny::showNotification(
        paste("Dataset file not found at:", data_path),
        type = "error",
        duration = NULL
      )
      req(FALSE, cancelOutput = TRUE) # Stop execution if file not found
    }
    
    # CORRECTED: Use the proper teal.data constructor.
    adpc_data <- readRDS(data_path)
    
    # Ensure column labels exist, as the helper functions rely on them.
    for (col_name in names(adpc_data)) {
      current_label <- attr(adpc_data[[col_name]], "label")
      if (is.null(current_label) || current_label == "") {
        attr(adpc_data[[col_name]], "label") <- col_name
      }
    }
    
    # Create an empty join_keys object since there are none for this single dataset.
    jks <- teal.data::join_keys()
    
    # Use the constructor to create the final S4 teal_data object.
    data_list <- teal.data::teal_data(
      ADPC = adpc_data,
      join_keys = jks
    )
    
    data_list
  })
  
  # Pass the reactive expression directly
  variable_browser_server(
    input = input,
    output = output,
    session = session,
    data_list_reactive = data_for_vb,
    dataname_param = "ADPC",
    parent_dataname_param = "ADPC"
  )
}


# Run the application
shinyApp(ui = ui, server = server)