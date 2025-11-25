#' Wrapper around aNCA::pkcg01() function. Calls the function with `LIN` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg01.
#' @export
g_pkcg01_lin <- function(data, ...) {
  pkcg01(adnca = data, scale = "LIN", ...)
}

#' Wrapper around aNCA::pkcg01() function. Calls the function with `LOG` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg01.
#' @export
g_pkcg01_log <- function(data, ...) {
  pkcg01(adnca = data, scale = "LOG", ...)
}

#' Generate PK Concentration-Time Profile Plots
#'
#' This function generates a list of ggplots for PK concentration-time profiles.
#'
#' @param adnca            A data frame containing the data.
#' @param xvar            A character string of the variable name for the x-axis.
#' @param yvar            A character string of the variable name for the y-axis.
#' @param xvar_unit       A character string of the unit for the x-axis variable.
#' @param yvar_unit       A character string of the unit for the y-axis variable.
#' @param color_var       A character string of the variable name for the color.
#' @param color_var_label A character string of the color label.
#' @param xbreaks_var     A character string of the x-axis breaks.
#' @param xbreaks_mindist A numeric value for `xbreak_mindist`.
#' @param xmin            A numeric value for the minimum x-axis limit.
#' @param xmax            A numeric value for the maximum x-axis limit.
#' @param ymin            A numeric value for the minimum y-axis limit.
#' @param ymax            A numeric value for the maximum y-axis limit.
#' @param xlab            Character for x-axis label. Defaults: `xvar` label & `xvar_unit`.
#' @param ylab            Character for y-axis label. Defaults: `yvar` label & `yvar_unit`.
#' @param title           Character for plot title.
#' @param subtitle        Character for plot subtitle.
#' @param footnote        A character string of a manual footnote for the plot.
#' @param plotgroup_vars  A character vector of the variables to group data.
#' @param plotgroup_names A character vector of the grouping variable names.
#' @param scale           Scale for the Y axis, either "LIN" or "LOG".
#' @param studyid         A character string specifying the study ID variable.
#' @param trt_var         A character string specifying the treatment variable.
#' @param plotly          Logical indicating whether to return plotly objects. Defaults to TRUE.
#' @returns A list of ggplot or plotly objects for each unique group.
#' @importFrom dplyr mutate across rowwise ungroup group_by n
#' @importFrom ggplot2 aes scale_x_continuous labs
#' @importFrom tern g_ipp
#' @importFrom stats setNames
#'
#' @examples
#' adnca <- read.csv(system.file("shiny/data/Dummy_data.csv", package = "aNCA"))
#' attr(adnca[["AFRLT"]], "label") <- "Actual time from first dose"
#' attr(adnca[["AVAL"]], "label") <- "Analysis val"
#'
#' plots_lin <- pckg01(adnca = adnca, xmax = 1)
#' plots_log <- pckg01(adnca = adnca, color_var = "USUBJID", scale = "LOG")
#' plots_sbs <- pckg01(
#'   adnca = adnca,
#'   color_var = "USUBJID",
#'   xbreaks_var = "NFRLT",
#'   xmin = 100,
#'   xmax = 1000,
#'   scale = "SBS"
#' )
#'
#' @export
#' @author Gerardo Rodriguez
pkcg01 <- function(
  adnca = data(),
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
  xlab = paste0("!", xvar, " [$", xvar_unit, "]"),
  ylab = paste0("!", yvar, " [$", yvar_unit, "]"),
  title = NULL,
  subtitle = NULL,
  footnote = NULL,
  # Inputs to split-by/seggregate plots
  plotgroup_vars = c("ROUTE", "PCSPEC", "PARAM", "USUBJID"),
  # TODO(mateusz): Refactor when label attributes are implemented.
  plotgroup_names = list(
    "ROUTE" = "Route",
    "PCSPEC" = "Specimen",
    "PARAM" = "Analyte",
    "USUBJID" = "Subject ID"
  ),

  # Specific inputs (needs metadata specification),
  scale = c("LIN", "LOG", "SBS")[1],
  studyid = "STUDYID",
  trt_var = "TRT01A",
  plotly = TRUE
) {
  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  ymin <- as.numeric(ymin)
  ymax <- as.numeric(ymax)

  # Ensure color_var is interpreted as a factor
  if (!is.null(color_var)) {
    adnca[[color_var]] <- as.factor(adnca[[color_var]])
  }

  # save col labels, as further adnca tranformations cause them to be lost #
  adnca_grouped <- adnca %>%
    mutate(across(all_of(plotgroup_vars), as.character)) %>%
    dplyr::mutate(id_plot = interaction(!!!syms(plotgroup_vars))) %>%
    filter(!is.na(id_plot))

  # reapply col labels to grouped data and make sure all variables are labeled #
  old_labels <- c(formatters::var_labels(adnca), id_plot = NA)
  formatters::var_labels(adnca_grouped) <- ifelse(!is.na(old_labels),
                                                  old_labels,
                                                  names(adnca_grouped))

  # Construct the reference ggplot object
  plot_data <- adnca_grouped %>% filter(id_plot == id_plot[1])

  plot <- tern::g_ipp(
    df = plot_data,
    xvar = xvar,
    yvar = yvar,
    xlab = paste0(parse_annotation(plot_data, xlab), collapse = ","),
    ylab = paste0(parse_annotation(plot_data, ylab), collapse = ","),
    id_var = "USUBJID",
    add_baseline_hline = FALSE,
    yvar_baseline = yvar,
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
        plot_data[[xbreaks_var]],
        min_cm_distance = xbreaks_mindist,
        plot = plot
      ),
      labels = function(x) ifelse(x %% 1 == 0, as.character(as.integer(x)), as.character(x))
    )

  # Add color when specified
  if (!is.null(color_var)) {
    plot <- plot +
      aes(color = !!sym(color_var)) +
      theme(legend.position = "none")

    # Add color legend only when neccessary
    if (length(unique(adnca[[color_var]])) > 1) {

      # Make sure the variable is interpreted as a factor
      adnca[[color_var]] <- as.factor(adnca[[color_var]])

      # Add to the plot the color_var and color_var_label
      plot <- plot +
        labs(color = if (!is.null(color_var_label)) color_var_label else color_var) +
        theme(legend.position = "bottom")
    }
  }

  if (scale == "LOG") {
    adnca_grouped[[yvar]] <- ifelse(
      adnca_grouped[[yvar]] < 1e-3,
      yes = 1e-3, no = adnca_grouped[[yvar]]
    )

    if (!plotly) {
      plot <- plot +
        scale_y_continuous(
          transform = "log10",
          labels = function(x) ifelse(x == 1e-3, yes = 0, no = x)
        )
    }
  }

  if (scale == "SBS") {
    if (!requireNamespace("ggh4x", quietly = FALSE))
      stop(
        "Side-by-side view requires `ggh4x` package, please install it with ",
        "`install.packages('ggh4x')`"
      )
    if (!requireNamespace("scales", quietly = FALSE))
      stop(
        "Side-by-side view requires `scales` package, please install it with ",
        "`install.packages('scales')`"
      )

    # Create SBS version of data and plot
    adnca_grouped <- bind_rows(
      adnca_grouped %>% dplyr::mutate(view = "Linear view"),
      adnca_grouped %>% dplyr::mutate(view = "Semilogarithmic view (Log10)")
    ) %>%
      dplyr::mutate(
        !!sym(yvar) := ifelse(
          !!sym(yvar) < 1e-3 & view == "Semilogarithmic view (Log10)",
          yes = 1e-3, no = !!sym(yvar)
        )
      )

    plot$data <- plot_data
    plot <- plot +
      facet_wrap(~ view, scales = "free_y") +
      ggh4x::scale_y_facet(
        view == "Semilogarithmic view (Log10)",
        trans  = "log10",
        labels = function(x) ifelse(x == 1e-3, yes = 0, no = x)
      )
  }

  # Create the list of plots for each unique group
  plots <- lapply(unique(adnca_grouped[["id_plot"]]), function(id_val) {
    plot_data <- adnca_grouped %>% dplyr::filter(id_plot == id_val)

    title <- generate_title(plot_data, title, scale, studyid)
    subtitle <- generate_subtitle(plot_data, subtitle, trt_var, plotgroup_vars, plotgroup_names)

    title_text <- paste0(title, "<br>", "<sup>", subtitle, "</sup>")
    title_margin <- (0.5 * length(unlist(strsplit(title_text, "\n|<br>"))))

    plot$data <- plot_data

    #' magic numbers for footnote position and margin, work in app up to 4 lines
    footnote <- {
      if (is.null(footnote)) {
        ""
      } else {
        parse_annotation(plot_data, footnote)
      }
    }
    footnote_y <- 0.1 + (0.05 * length(unlist(strsplit(footnote, "\n|<br>"))))
    if (plotly) {

      plotly_plot <- plot +
        theme(
          # add margin to make space for subtitle and footnote #
          plot.margin = margin(
            title_margin,
            0,
            footnote_y * 5,
            0,
            "cm"
          )
        )

      plotly_plot <- plotly_plot %>%
        # This because of no spec of parse annotation generates warning is.na()
        ggplotly(
          tooltip = c("x", "y"),
          dynamicTicks = if (scale != "SBS") TRUE else FALSE,
          #' NOTE: might require some fine tuning down the line, looks fine now
          height = 500 + (footnote_y * 25) + title_margin * 50
        ) %>%
        layout(
          # title and subtitle #
          title = list(text = title_text),
          # footnote #
          annotations = list(
            x = 0,
            y =  -footnote_y,
            text = footnote,
            showarrow = FALSE,
            yref = "paper",
            xref = "paper",
            align = "left",
            parse = TRUE
          )
        )

      if (scale == "LOG") {
        plotly_plot <- plotly_plot %>%
          layout(yaxis = list(
            type = "log",
            autorange = TRUE,
            tickformat = "~r"
          ))
      }

      plotly_plot
    } else {
      plot +
        labs(
          title = title,
          subtitle = subtitle,
          caption = footnote
        )
    }
  })
  plots %>%
    setNames(unique(adnca[["id_plot"]]))
}

# Helper Function for Title Generation
generate_title <- function(plot_data, title, scale, studyid) {
  if (is.null(title)) {
    paste0(
      "Plot of PK Concentration-Time Profile ",
      dplyr::case_when(
        scale == "LIN" ~ "linear",
        scale == "LOG" ~ "logarithmic",
        TRUE ~ "linear and logarithmic"
      ),
      " scale, by Cohort: ", unique(plot_data[[studyid]])
    )
  } else {
    parse_annotation(plot_data, title)
  }
}

# Helper Function for Subtitle Generation
generate_subtitle <- function(plot_data, subtitle, trt_var, plotgroup_vars, plotgroup_names) {
  if (is.null(subtitle)) {
    paste0(
      "Treatment Group: ", unique(plot_data[[trt_var]]), " (N=", nrow(plot_data), ")<br>",
      paste(
        unlist(unname(plotgroup_names[plotgroup_vars])), ": ",
        unique(plot_data[, plotgroup_vars]),
        sep = "", collapse = ", "
      )
    )
  } else {
    parse_annotation(plot_data, subtitle)
  }
}
#' Wrapper around aNCA::pkcg02() function. Calls the function with `LIN` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg02.
#' @export
g_pkcg02_lin <- function(data, ...) {
  pkcg02(adnca = data, scale = "LIN", ...)
}

#' Wrapper around aNCA::pkcg02() function. Calls the function with `LOG` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg02.
#' @export
g_pkcg02_log <- function(data, ...) {
  pkcg02(adnca = data, scale = "LOG", ...)
}

#' Generate Combined PK Concentration-Time Profile Plot by Cohort
#'
#' This function generates a list of plotly objects PK concentration-time profiles by group
#'
#' @param adnca            A data frame containing the data.
#' @param xvar            A character string of the variable name for the x-axis.
#' @param yvar            A character string of the variable name for the y-axis.
#' @param xvar_unit       A character string of the unit for the x-axis variable.
#' @param yvar_unit       A character string of the unit for the y-axis variable.
#' @param color_var       A character string of the variable name for the color.
#' @param color_var_label A character string of the color label.
#' @param xbreaks_var     A character string of the x-axis breaks.
#' @param xbreaks_mindist A numeric value for `xbreak_mindist`.
#' @param xmin            A numeric value for the minimum x-axis limit.
#' @param xmax            A numeric value for the maximum x-axis limit.
#' @param ymin            A numeric value for the minimum y-axis limit.
#' @param ymax            A numeric value for the maximum y-axis limit.
#' @param xlab            Character for x-axis label. Defaults: `xvar` label & `xvar_unit`.
#' @param ylab            Character for y-axis label. Defaults: `yvar` label & `yvar_unit`.
#' @param title           Character for plot title.
#' @param subtitle        Character for plot subtitle.
#' @param footnote        A character string of a manual footnote for the plot.
#' @param plotgroup_vars  A character vector of the variables to group data.
#' @param plotgroup_names A character vector of the grouping variable names.
#' @param scale           Scale for the Y axis, either "LIN" or "LOG".
#' @param studyid         A character string specifying the study ID variable.
#' @param trt_var         A character string specifying the treatment variable.
#' @param plotly          Logical indicating whether to return plotly objects. Defaults to TRUE.
#' @returns A list of ggplot or plotly objects for each unique group.
#' @importFrom dplyr mutate across rowwise ungroup group_by n
#' @importFrom ggplot2 aes scale_x_continuous labs
#' @importFrom tern g_ipp
#' @importFrom stats setNames

#' @examples
#' adnca <- read.csv(system.file("shiny/data/Dummy_data.csv", package = "aNCA"))
#' attr(adnca[["AFRLT"]], "label") <- "Actual time from first dose"
#' attr(adnca[["AVAL"]], "label") <- "Analysis value"
#'
#' plots <- pkcg02(adnca)
#' plots_log <- pkcg02(adnca, scale = "LOG")
#' plots_custom <- pkcg02(adnca, xmin = 0, xmax = 48, title = "PK Profile", footnote = "Study X")
#' plotly::plotly_build(plots[[1]]) # View the first plot
#'
#' @export
#' @author Kezia Kobana
pkcg02 <- function(
  adnca = data(),
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
  xlab = paste0("!", xvar, " [$", xvar_unit, "]"),
  ylab = paste0("!", yvar, " [$", yvar_unit, "]"),
  title = NULL,
  subtitle = NULL,
  footnote = NULL,
  plotgroup_vars = c("ROUTE", "PCSPEC", "PARAM", "TRT01A"),
  plotgroup_names = list(
    "ROUTE" = "Route",
    "PCSPEC" = "Specimen",
    "PARAM" = "Analyte",
    "TRT01A" = "Treatment"
  ),
  scale = c("LIN", "LOG", "SBS")[1],
  studyid = "STUDYID",
  trt_var = "TRT01A",
  plotly = TRUE
) {

  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  ymin <- as.numeric(ymin)
  ymax <- as.numeric(ymax)

  # Ensure color_var is interpreted as a factor
  if (!is.null(color_var)) {
    adnca[[color_var]] <- as.factor(adnca[[color_var]])
  }

  # save col labels, as further adnca tranformations cause them to be lost #
  adnca_grouped <- adnca %>%
    mutate(across(all_of(plotgroup_vars), as.character)) %>%
    dplyr::mutate(id_plot = interaction(!!!syms(plotgroup_vars)))

  # reapply col labels to grouped data and make sure all variables are labeled #
  old_labels <- c(formatters::var_labels(adnca), id_plot = NA)
  formatters::var_labels(adnca_grouped) <- ifelse(!is.na(old_labels),
                                                  old_labels,
                                                  names(adnca_grouped))

  # Construct the reference ggplot object
  plot_data <- adnca_grouped %>% filter(id_plot == id_plot[1])

  plot <- tern::g_ipp(
    df = plot_data,
    xvar = xvar,
    yvar = yvar,
    xlab = paste0(parse_annotation(plot_data, xlab), collapse = ","),
    ylab = paste0(parse_annotation(plot_data, ylab), collapse = ","),
    id_var = "USUBJID",
    add_baseline_hline = FALSE,
    yvar_baseline = yvar,
    plotting_choices = "all_in_one"
  )

  # Provide limits and additional potential future aesthetic customizations
  plot <- plot +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = "sans", size = 14, color = "black"),
      plot.subtitle = ggplot2::element_text(family = "sans", size = 11, color = "black")
    ) +
    ggplot2::coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))


  # Ensure x breaks labels do not overlap graphically
  plot <- plot + ggplot2::scale_x_continuous(
    guide = ggplot2::guide_axis(n.dodge = 1),
    breaks = filter_breaks(
      plot_data[[xbreaks_var]],
      min_cm_distance = xbreaks_mindist,
      plot = plot
    ),
    labels = function(x) ifelse(x %% 1 == 0, as.character(as.integer(x)), as.character(x))
  )

  # Add color when specified
  if (!is.null(color_var)) {
    plot <- plot +
      ggplot2::aes(color = !!sym(color_var)) +
      ggplot2::theme(legend.position = "none")

    # Make sure the variable is interpreted as a factor
    adnca[[color_var]] <- as.factor(adnca[[color_var]])

    # Add to the plot the color_var and color_var_label
    plot <- plot +
      ggplot2::labs(color = if (!is.null(color_var_label)) color_var_label else color_var) +
      ggplot2::theme(legend.position = "bottom")
  }

  if (scale == "LOG") {
    adnca_grouped[[yvar]] <- ifelse(
      adnca_grouped[[yvar]] < 1e-3,
      yes = 1e-3, no = adnca_grouped[[yvar]]
    )

    if (!plotly) {
      plot <- plot +
        scale_y_continuous(
          transform = "log10",
          labels = function(x) ifelse(x == 1e-3, yes = 0, no = x)
        )
    }
  }

  if (scale == "SBS") {
    if (!requireNamespace("ggh4x", quietly = FALSE))
      stop(
        "Side-by-side view requires `ggh4x` package, please install it with ",
        "`install.packages('ggh4x')`"
      )
    if (!requireNamespace("scales", quietly = FALSE))
      stop(
        "Side-by-side view requires `scales` package, please install it with ",
        "`install.packages('scales')`"
      )

    # Create SBS version of data and plot
    adnca_grouped <- bind_rows(
      adnca_grouped %>% dplyr::mutate(view = "Linear view"),
      adnca_grouped %>% dplyr::mutate(view = "Semilogarithmic view (Log10)")
    ) %>%
      dplyr::mutate(
        !!sym(yvar) := ifelse(
          !!sym(yvar) < 1e-3 & view == "Semilogarithmic view (Log10)",
          yes = 1e-3, no = !!sym(yvar)
        )
      )

    plot$data <- plot_data
    plot <- plot +
      facet_wrap(~ view, scales = "free_y") +
      ggh4x::scale_y_facet(
        view == "Semilogarithmic view (Log10)",
        trans  = "log10",
        labels = function(x) ifelse(x == 1e-3, yes = 0, no = x)
      )
  }

  # Create the list of plots for each unique group
  plots <- lapply(unique(adnca_grouped[["id_plot"]]), function(id_val) {
    plot_data <- adnca_grouped %>% dplyr::filter(id_plot == id_val)

    title <- generate_title(plot_data, title, scale, studyid)
    subtitle <- generate_subtitle(plot_data, subtitle, trt_var, plotgroup_vars, plotgroup_names)

    title_text <- paste0(title, "<br>", "<sup>", subtitle, "</sup>")
    title_margin <- (0.5 * length(unlist(strsplit(title_text, "\n|<br>"))))

    plot$data <- plot_data

    #' magic numbers for footnote position and margin, work in app up to 4 lines
    footnote <- {
      if (is.null(footnote)) {
        ""
      } else {
        parse_annotation(plot_data, footnote)
      }
    }
    footnote_y <- 0.1 + (0.05 * length(unlist(strsplit(footnote, "\n|<br>"))))
    if (plotly) {
      suppressWarnings({
        plotly_plot <- plot +
          theme(
            # add margin to make space for subtitle and footnote #
            plot.margin = margin(
              title_margin,
              0,
              footnote_y * 5,
              0,
              "cm"
            )
          )

        # This because of no spec of parse annotation generates warning is.na()
        plotly_plot <- ggplotly(
          plotly_plot,
          tooltip = c("x", "y"),
          dynamicTicks = if (scale != "SBS") TRUE else FALSE,
          #' NOTE: might require some fine tuning down the line, looks fine now
          height = 500 + (footnote_y * 25) + title_margin * 50
        ) %>%
          layout(
            # title and subtitle #
            title = list(text = title_text),
            # footnote #
            annotations = list(
              x = 0,
              y =  -footnote_y,
              text = footnote,
              showarrow = FALSE,
              yref = "paper",
              xref = "paper",
              align = "left",
              parse = TRUE
            )
          )

        if (scale == "LOG") {
          plotly_plot <- plotly_plot %>%
            layout(yaxis = list(
              type = "log",
              autorange = TRUE,
              tickformat = "~r"
            ))
        }

        plotly_plot
      })
    } else {
      plot +
        labs(
          title = title,
          subtitle = subtitle,
          caption = footnote
        )
    }
  })
  plots %>%
    setNames(unique(adnca[["id_plot"]]))

}
