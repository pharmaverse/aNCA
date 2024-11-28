#' Wrapper around aNCA::pkcg01() function. Calls the function with `LIN` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the ploting function.
#' @returns ggplot2 object for pckg01.
#' @export
g_pkconc_ind_lin <- function(data, ...) {
  pkcg01(adpc = data, scale = "LIN", ...)
}

#' Wrapper around aNCA::pkcg01() function. Calls the function with `LOG` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the ploting function.
#' @returns ggplot2 object for pckg01.
#' @export
g_pkconc_ind_log <- function(data, ...) {
  pkcg01(adpc = data, scale = "LOG")
}

#' Generate PK Concentration-Time Profile Plots
#'
#' This function generates a list of ggplots for PK concentration-time profiles.
#'
#' @param adpc            A data frame containing the data.
#' @param xvar            A character string of the variable name for the x-axis.
#' @param yvar            A character string of the variable name for the y-axis.
#' @param xvar_unit       A character string of the unit for the x-axis variable.
#' @param yvar_unit       A character string of the unit for the y-axis variable.
#' @param color_var       A character string of the variable name for the color.
#' @param color_var_label A character string of the color label.
#' @param xbreaks_var     A character string of the x-axis breaks.
#' @param xmin            A numeric value specifying the minimum x-axis limit.
#' @param xmax            A numeric value specifying the maximum x-axis limit.
#' @param ymin            A numeric value for the minimum y-axis limit.
#' @param ymax            A numeric value for the maximum y-axis limit.
#' @param xlab            Character for x-axis label. Defaults: `xvar` label & `xvar_unit`.
#' @param ylab            Character for y-axis label. Defaults: `yvar` label & `yvar_unit`.
#' @param footnote        A character string of a manual footnote for the plot.
#' @param plotgroup_vars  A character vector of the variables to group data.
#' @param plotgroup_names A character vector of the grouping variable names.
#' @param options         A list of additional options (e.g., display scale).
#' @param studyid         A character string specifying the study ID variable.
#' @param trt_var         A character string specifying the treatment variable.
#' @returns A list of ggplot objects for each unique group.
#' @importFrom dplyr mutate across rowwise ungroup group_by n
#' @importFrom ggplot2 aes scale_x_continuous labs
#' @importFrom tern g_ipp     # Can be substituted by regular ggplot easily!!
#' @importFrom checkmate assert_numeric
#' @importFrom scales breaks_log label_log trans_breaks trans_formats
#' @importFrom ggh4x scale_y_facet
#'
#' @examples
#' \dontrun {
#'   adpc <- read.csv("inst/shiny/data/DummyRO_ADNCA.csv")
#'   attr(adpc[["AFRLT"]], "label") <- "Actual time from first dose"
#'   attr(adpc[["AVAL"]], "label") <- "Analysis val
#'
#'   plots_lin <- pkcg01(adpc = adpc, xmax = 1)
#'   plots_log <- pkcg01(adpc = adpc, color_var = "USUBJID", scale = "LOG)
#'   plots_sbs <- pkcg01(
#'     adpc = adpc,
#'     color_var = "USUBJID",
#'     xbreaks_var = "NFRLT",
#'     xmin = 100, xmax = 1000,
#'     scale = "SBS"
#'   )
#' }
#'
#' @export
#' @author Gerardo Rodriguez
pkcg01 <- function(
  adpc = data(),
  xvar = "AFRLT",
  yvar = "AVAL",
  xvar_unit = "RRLTU",
  yvar_unit = "AVALU",
  color_var = NULL,
  color_var_label = NULL,
  xbreaks_var = "NFRLT",
  xbreaks_mindist = 0.5,
  xmin = NA,
  xmax = NA,
  ymin = NA,
  ymax = NA,
  # xlab = substitute(paste0(attr(adpc[[xvar]], "label"),
  #                          " (", unique(adpc[[xvar_unit]]), ")")),
  # ylab = substitute(paste0(attr(adpc[[yvar]], "label"),
  #                          " (", unique(adpc[[yvar_unit]]), ")")),
  xlab = paste0(xvar, " [", xvar_unit, "]"),
  ylab = paste0(yvar, " [", yvar_unit, "]"),
  footnote = NULL,
  # Inputs to split-by/seggregate plots
  plotgroup_vars = c("ROUTE", "PCSPEC", "PARAM", "USUBJID"),
  plotgroup_names = c("Route", "Specimen", "Analyte", "Subject ID"),

  # Specific inputs (needs metadata specification),
  scale = c("LIN", "LOG", "SBS")[1],
  studyid = "STUDYID",
  trt_var = "TRT01A"
) {

  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  ymin <- as.numeric(ymin)
  ymax <- as.numeric(ymax)

  # Title for the plots based on display option
  title <- paste0(
    "Plot of PK Concentration-Time Profile ",
    dplyr::case_when(
      scale == "LIN" ~ "linear",
      scale == "LOG" ~ "logarithmic",
      TRUE ~ "linear and logarithmic"
    ),
    " scale"
  )

  # Include in data figure details: title, subtitle, footnote/caption
  adpc <- add_figure_details(
    adpc = adpc,
    title = title,
    collapse_subtitle = ", ",
    studyid = studyid, # Includes cohort in title
    trt_var = trt_var, # Includes treatment in subtitle
    plotgroup_vars = plotgroup_vars,
    plotgroup_names = plotgroup_names,
    xvar_unit = xvar_unit,
    xmin = as.numeric(xmin),
    xmax = as.numeric(xmax),
    footnote = footnote
  )

  # Construct the reference ggplot object
  plot_data <- adpc %>% filter(id_plot == id_plot[1])

  plot <- tern::g_ipp(
    df = plot_data,
    xvar = xvar,
    yvar = yvar,
    xlab = xlab,
    ylab = ylab,
    id_var = "subtitle",
    add_baseline_hline = FALSE,
    yvar_baseline = yvar,
    title = unique(plot_data$title),
    subtitle = unique(plot_data$subtitle),
    caption = unique(plot_data$footnote),
    plotting_choices = "separate_by_obs"
  )[[1]]

  # Provide limits and additional potential future aesthetic customizations
  plot <- plot +
    aes(color = NULL) +
    theme(
      plot.title = element_text(family = "sans", size = 14, color = "black"),
      plot.subtitle = element_text(family = "sans", size = 11, color = "black")
    ) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))

  # Ensure x breaks labels do not overlap graphically
  plot <- plot +
    scale_x_continuous(
      guide = guide_axis(n.dodge = 1),
      breaks = filter_breaks(
        x_breaks = plot_data[[xbreaks_var]],
        min_cm_distance = xbreaks_mindist,
        plot = plot
      ),
      labels = \(x) ifelse(x %% 1 == 0, as.character(as.integer(x)), as.character(x))
    )

  # Add color when specified
  if (!is.null(color_var)) {
    plot <- plot +
      aes(color = !!sym(color_var)) +
      theme(legend.position = "none")
  }

  # Add color legend only when neccessary
  if (!is.null(color_var_label) && length(color_var) > 1) {
    plot <- plot +
      labs(color = if (!is.null(color_var_label)) color_var_label else color_var) +
      theme(legend.position = "bottom")
  }


  if (scale == "LOG") {
    # Create LOG version of data and plot
    adpc <- adpc %>%
      dplyr::mutate(across(all_of(yvar), ~ ifelse(. < 1e-3, 1e-3, .)))

    plot <- plot %+% dplyr::filter(adpc, id_plot == id_plot[1]) +
      scale_y_continuous(
        trans = scales::log10_trans(),
        breaks = scales::trans_breaks("log10", \(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      annotation_logticks(sides = "l") +
      labs(y = paste0("Log 10 - ", plot$labels$y))
  }

  if (scale == "SBS") {
    # Create SBS version of data and plot
    adpc <- rbind(adpc, adpc) %>%
      dplyr::mutate(
        view = c(rep("Linear view", nrow(adpc)), rep("Semilogarithmic view (Log10)", nrow(adpc))),
        !!sym(yvar) := ifelse(
          !!sym(yvar) < 1e-3 & view == "Semilogarithmic view (Log10)", yes = 1e-3, no = !!sym(yvar)
        )
      )

    plot <- plot %+% dplyr::filter(adpc, id_plot == unique(id_plot)[1]) +
      facet_wrap(~ view, scales = "free_y") +
      annotation_logticks(sides = "l", linewidth = 0.1, alpha = c(0, 1)) +
      ggh4x::scale_y_facet(
        view == "Semilogarithmic view (Log10)",
        trans  = "log10",
        breaks = scales::breaks_log(),
        labels = scales::label_log()
      )
  }

  # Create the list of plots for each unique group
  plot_list <- list()
  for (id_val in unique(adpc[["id_plot"]])) {

    plot_data <- adpc %>% dplyr::filter(id_plot ==  id_val)
    plot_list <- c(plot_list, list(plot %+% plot_data))
  }

  # Define IDs to differentiate each group of plots
  names(plot_list) <- unique(adpc[["id_plot"]])

  # Return the list of plots as output of the function
  return(plot_list)
}

#' Add Figure Details to Data Frame
#'
#' This function adds figure details; title, subtitle, and caption to the data.
#'
#' @param adpc            A data frame containing the data.
#' @param plotgroup_vars  A character vector of the grouping data variables.
#' @param plotgroup_names A character vector for the grouping variables names.
#' @param studyid         A character string specifying the study ID variable.
#' @param xvar_unit       A character string for the unit for the x-axis variable.
#' @param xmin            A numeric value specifying the minimum x-axis limit.
#' @param xmax            A numeric value specifying the maximum x-axis limit.
#' @param footnote        A character string specifying plot's manual footnote.
#' @param trt_var         A character string specifying the treatment variable.
#' @param title           A character string specifying the title for the plot.
#' @returns A data frame with added figure details.
#' @importFrom dplyr mutate across rowwise ungroup group_by n
#' @author Gerardo Rodriguez
add_figure_details <- function(
  adpc,
  title = "", # Specified by metadata
  collapse_subtitle = "\n",
  studyid = NULL, # Include or not in t
  trt_var, # Include or not in subtitle
  plotgroup_vars,
  plotgroup_names,
  xvar_unit,
  xmin = NA,
  xmax = NA,
  footnote = NULL
) {
  adpc %>%
    mutate(across(all_of(plotgroup_vars), as.character)) %>%
    rowwise() %>%
    dplyr::mutate(
      title = if (is.null(studyid)) title else paste0(title, ", by Cohort: ", !!sym(studyid)),
      subtitle = paste(
        paste(c(plotgroup_names), ": ", c_across(all_of(plotgroup_vars)), sep = ""),
        collapse = collapse_subtitle
      ),
      footnote = {
        footnote <- ifelse(
          is.na(xmax),
          yes = "",
          no = paste0("Plot not showing observations beyond ", xmax, " ", !!sym(xvar_unit), ".")
        )
        if (!is.na(xmin)) {
          footnote <- paste0(
            footnote, "\nPlot not showing observations before ", xmin, " ", !!sym(xvar_unit), "."
          )
        }

        if (!is.null(footnote))
          footnote <- footnote

        footnote
      }
    ) %>%
    ungroup() %>%
    dplyr::mutate(id_plot = interaction(!!!syms(plotgroup_vars))) %>%
    dplyr::group_by(!!!syms(c(trt_var, plotgroup_vars))) %>%
    dplyr::mutate(
      subtitle = paste0("Treatment Group: ", !!sym(trt_var), " (N=", n(), ")\n", subtitle)
    ) %>%
    ungroup()
}

#' Filter Breaks for X-Axis
#'
#' Filters X-axis for consecutive breaks with at least the specified distance.
#'
#' @param x_breaks        A numeric vector of x-axis breaks.
#' @param plot            A ggplot object used to extract plot dimensions and scales.
#' @param min_cm_distance A numeric of the minimum distance between breaks.
#' @returns A numeric vector of filtered x-axis breaks.
#' @importFrom ggplot2 ggplot_build ggplot_gtable
#' @importFrom grid convertUnit
#' @author Gerardo Rodriguez
filter_breaks <- function(x_breaks = NA, plot = plot, min_cm_distance = 0.5) {
  x_breaks <- unique(na.omit(sort(x_breaks)))
  plot_build <- ggplot_build(plot)
  plot_table <- ggplot_gtable(plot_build)

  # Extract x-axis scale information
  x_scale <- plot_build$layout$panel_params[[1]]$x.range

  # Identify the panel grob
  panel_index <- which(sapply(plot_table$grobs, \(x) grepl("panel", x$name)))

  if (length(panel_index) == 0) {
    stop("Error: Panel grob not found.")
  }
  panel <- plot_table$grobs[[panel_index]]

  # Extract the panel border grob to get the width
  panel_border <- panel$children[[
    which(sapply(panel$children, \(x) grepl("panel.border", x$name)))
  ]]

  # Convert panel width to cm
  panel_width_cm <- grid::convertUnit(panel_border$width, unitTo =  "cm", valueOnly = TRUE)

  # Filter only breaks that satisfy the minimum distance
  filt_breaks <- x_breaks[1]

  for (i in 2:length(x_breaks)) {
    # Take latest selected break and calculate its distance
    b0 <- filt_breaks[length(filt_breaks)]
    bdist <- (x_breaks[i] - b0) / diff(x_scale) * panel_width_cm

    if (bdist >= min_cm_distance) {
      filt_breaks <- c(filt_breaks, x_breaks[i])
    }
  }

  filt_breaks
}