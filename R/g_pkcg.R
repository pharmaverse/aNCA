#' Wrapper around aNCA::pkcg01() function. Calls the function with `LIN` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg01.
#' @export
g_pkcg01_lin <- function(data, ...) {
  pkcg01(adpc = data, scale = "LIN", ...)
}

#' Wrapper around aNCA::pkcg01() function. Calls the function with `LOG` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg01.
#' @export
g_pkcg01_log <- function(data, ...) {
  pkcg01(adpc = data, scale = "LOG", ...)
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
#' @param xbreaks_mindist A numeric value for minimum distance between x-axis breaks in centimeters.
#' @param xmin            A numeric value specifying the minimum x-axis limit.
#' @param xmax            A numeric value specifying the maximum x-axis limit.
#' @param ymin            A numeric value for the minimum y-axis limit.
#' @param ymax            A numeric value for the maximum y-axis limit.
#' @param xlab            Character for x-axis label. Defaults: `xvar` label & `xvar_unit`.
#' @param ylab            Character for y-axis label. Defaults: `yvar` label & `yvar_unit`.
#' @param title           A character string for plot title.
#' @param subtitle        A character string for plot subtitle.
#' @param footnote        A character string of a manual footnote for the plot.
#' @param plotgroup_vars  A character vector of the variables to group data.
#' @param plotgroup_names A character vector of the grouping variable names.
#' @param studyid         A character string specifying the study ID variable.
#' @param scale           A character string:"LIN", "LOG", or "SBS", defining the y-axis scale.
#' @param trt_var         A character string specifying the treatment variable.
#' @returns A list of ggplot objects for each unique group.
#' @importFrom dplyr mutate across rowwise ungroup group_by n
#' @importFrom ggplot2 aes scale_x_continuous labs
#' @importFrom tern g_ipp
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#'   adpc <- read.csv("inst/shiny/data/DummyRO_ADNCA.csv")
#'   attr(adpc[["AFRLT"]], "label") <- "Actual time from first dose"
#'   attr(adpc[["AVAL"]], "label") <- "Analysis val"
#'
#'   plots_lin <- pckg01(adpc = adpc, xmax = 1)
#'   plots_log <- pckg01(adpc = adpc, color_var = "USUBJID", scale = "LOG")
#'   plots_sbs <- pckg01(
#'     adpc = adpc,
#'     color_var = "USUBJID",
#'     xbreaks_var = "NFRLT",
#'     xmin = 100,
#'     xmax = 1000,
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
  trt_var = "TRT01A"
) {
  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  ymin <- as.numeric(ymin)
  ymax <- as.numeric(ymax)

  # save col labels, as further adpc tranformations cause them to be lost #
  col_labels <- purrr::map(adpc, ~ attr(.x, "label"))

  adpc_grouped <- adpc %>%
    mutate(across(all_of(plotgroup_vars), as.character)) %>%
    rowwise() %>%
    dplyr::mutate(id_plot = interaction(!!!syms(plotgroup_vars)))

  # reapply col labels to grouped data #
  adpc_grouped <- purrr::map2_dfc(adpc_grouped, names(adpc_grouped), ~ {
    attr(.x, "label") <- col_labels[[.y]]
    .x
  })

  # Construct the reference ggplot object
  plot_data <- adpc_grouped %>% filter(id_plot == id_plot[1])

  plot <- tern::g_ipp(
    df = plot_data,
    xvar = xvar,
    yvar = yvar,
    xlab = parse_annotation(plot_data, xlab),
    ylab = parse_annotation(plot_data, ylab),
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
      labels = \(x) ifelse(x %% 1 == 0, as.character(as.integer(x)), as.character(x))
    )

  # Add color when specified
  if (!is.null(color_var)) {
    plot <- plot +
      ggplot2::aes(color = !!sym(color_var))

    if (length(unique(plot_data[[color_var]])) >= 1) {
      plot <- plot +
        ggplot2::theme(legend.position = "bottom")
    }
  }

  # Modify legend label when specified
  if (!is.null(color_var_label)) {
    plot <- plot +
      ggplot2::labs(color = color_var_label) +
      ggplot2::theme(legend.position = "bottom")
  }

  if (scale == "LOG") {
    plot <- plot +
      labs(y = paste0("Log 10 - ", plot$labels$y))
  }

  if (scale == "SBS") {
    if (!requireNamespace("ggh4x", silently = TRUE))
      stop(
        "Side-by-side view requires `ggh4x` package, please install it with ",
        "`install.packages('ggh4x')`"
      )
    if (!requireNamespace("scales", silently = TRUE))
      stop(
        "Side-by-side view requires `scales` package, please install it with ",
        "`install.packages('scales')`"
      )

    # Create SBS version of data and plot
    adpc_grouped <- rbind(adpc_grouped, adpc_grouped) %>%
      dplyr::mutate(
        view = c(
          rep("Linear view", nrow(adpc_grouped)),
          rep("Semilogarithmic view (Log10)", nrow(adpc_grouped))
        ),
        !!sym(yvar) := ifelse(
          !!sym(yvar) < 1e-3 & view == "Semilogarithmic view (Log10)", yes = 1e-3, no = !!sym(yvar)
        )
      )

    plot <- plot %+% dplyr::filter(adpc_grouped, id_plot == unique(id_plot)[1]) +
      facet_wrap(~ view, scales = "free_y") +
      ggh4x::scale_y_facet(
        view == "Semilogarithmic view (Log10)",
        trans  = "log10",
        breaks = scales::breaks_log(),
        labels = scales::label_log()
      )
  }

  # Create the list of plots for each unique group
  lapply(unique(adpc_grouped[["id_plot"]]), \(id_val) {
    plot_data <- adpc_grouped %>% dplyr::filter(id_plot ==  id_val)

    title <- {
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

    subtitle <- {
      if (is.null(subtitle)) {
        paste0(
          "Treatment Group: ", unique(plot_data[[trt_var]]), " (N=", nrow(plot_data), ")<br>",
          # TODO(mateusz): Refactor when label attributes are implemented.
          paste(
            unlist(unname(plotgroup_names[plotgroup_vars])), ": ",
            unique(plot_data[, plotgroup_vars]),
            sep = "",  collapse = ", "
          )
        )
      } else {
        parse_annotation(plot_data, subtitle)
      }
    }

    title_text <- paste0(title, "<br>", "<sup>", subtitle, "</sup>")
    title_margin <- (0.5 * length(unlist(strsplit(title_text, "\n|<br>"))))

    #' magic numbers for footnote position and margin, work in app up to 4 lines
    footnote <- {
      if (is.null(footnote)) {
        ""
      } else {
        parse_annotation(plot_data, footnote)
      }
    }
    footnote_y <- 0.1 + (0.05 * length(unlist(strsplit(footnote, "\n|<br>"))))

    suppressWarnings({
      plotly_plot <- plot %+%
        plot_data %+%
        theme(
          # add margin to make space for subtitle and footnote #
          plot.margin = margin(
            title_margin,
            0,
            footnote_y * 5,
            0,
            "cm"
          )
        ) %>%
        ggplotly(
          tooltip = c("x", "y"),
          dynamicTicks = TRUE,
          #' NOTE: might require some fine tuning down the line, looks fine now
          height = 500 + (footnote_y * 25) + title_margin * 50
        ) %>%
        layout(
          legend = list(
            orientation = "h",
            y = -0.2 # Move the legend down to avoid overlap
          ),
          yaxis = list(autorange = FALSE),
          xaxis = list(autorange = FALSE),
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
          layout(yaxis = list(type = "log"))
      }
    })
    plotly_plot
  }) |>
    setNames(unique(adpc[["id_plot"]]))
}
#' Wrapper around aNCA::pkcg02() function. Calls the function with `LIN` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg02.
#' @export
g_pkcg02_lin <- function(data, ...) {
  pkcg02(adpc = data, scale = "LIN", ...)
}

#' Wrapper around aNCA::pkcg02() function. Calls the function with `LOG` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg02.
#' @export
g_pkcg02_log <- function(data, ...) {
  pkcg02(adpc = data, scale = "LOG", ...)
}

#' Generate Combined PK Concentration-Time Profile Plot by Cohort
#'
#' This function generates a list of plotly objects PK concentration-time profiles by group
#'
#' @param adpc            A data frame containing the data.
#' @param xvar            A character string of the variable name for the x-axis.
#' @param yvar            A character string of the variable name for the y-axis.
#' @param xvar_unit       A character string of the unit for the x-axis variable.
#' @param yvar_unit       A character string of the unit for the y-axis variable.
#' @param color_var       A character string of the variable name for the color.
#' @param color_var_label A character string of the color label.
#' @param xbreaks_var     A character string of the x-axis breaks.
#' @param xbreaks_mindist A numeric value for minimum distance between x-axis breaks in centimeters.
#' @param xmin            A numeric value specifying the minimum x-axis limit.
#' @param xmax            A numeric value specifying the maximum x-axis limit.
#' @param ymin            A numeric value for the minimum y-axis limit.
#' @param ymax            A numeric value for the maximum y-axis limit.
#' @param xlab            Character for x-axis label. Defaults: `xvar` label & `xvar_unit`.
#' @param ylab            Character for y-axis label. Defaults: `yvar` label & `yvar_unit`.
#' @param title           A character string for plot title.
#' @param subtitle        A character string for plot subtitle.
#' @param footnote        A character string of a manual footnote for the plot.
#' @param plotgroup_vars  A character vector of the variables to group data.
#' @param plotgroup_names A character vector of the grouping variable names.
#' @param studyid         A character string specifying the study ID variable.
#' @param scale           A character string:"LIN", "LOG", or "SBS", defining the y-axis scale.
#' @param trt_var         A character string specifying the treatment variable.
#' @returns A list of ggplot objects for each unique group.
#' @importFrom dplyr mutate across rowwise ungroup group_by n
#' @importFrom ggplot2 aes scale_x_continuous labs
#' @importFrom tern g_ipp
#' @importFrom stats setNames

#' @examples
#' \dontrun{
#'   adpc <- read.csv(system.file("shiny/data/DummyRO_ADNCA.csv", package = "aNCA"))
#'   attr(adpc[["AFRLT"]], "label") <- "Actual time from first dose"
#'   attr(adpc[["AVAL"]], "label") <- "Analysis value"
#'
#'   plots <- pkcg02(adpc)
#'   plots_log <- pkcg02(adpc, scale = "LOG")
#'   plots_custom <- pkcg02(adpc, xmin = 0, xmax = 48, title = "PK Profile", footnote = "Study XYZ")
#'   plotly::plotly_build(plots[[1]]) # View the first plot
#' }
#'
#' @export
#' @author Kezia Kobana
pkcg02 <- function(
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
  trt_var = "TRT01A"
) {

  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  ymin <- as.numeric(ymin)
  ymax <- as.numeric(ymax)


  col_labels <- purrr::map(adpc, ~ attr(.x, "label"))

  adpc_grouped <- adpc %>%
    dplyr::mutate(across(c(all_of(plotgroup_vars), "USUBJID"), as.character)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(id_plot = interaction(!!!syms(plotgroup_vars)))

  adpc_grouped <- purrr::map2_dfc(adpc_grouped, names(adpc_grouped), ~ {
    attr(.x, "label") <- col_labels[[.y]]
    .x
  })

  plot_data <- adpc_grouped %>% dplyr::filter(id_plot == id_plot[1])

  plot <- tern::g_ipp(
    df = plot_data,
    xvar = xvar,
    yvar = yvar,
    xlab = parse_annotation(plot_data, xlab),
    ylab = parse_annotation(plot_data, ylab),
    id_var = "USUBJID",
    add_baseline_hline = FALSE,
    yvar_baseline = yvar,
    plotting_choices = "all_in_one"
  )

  plot <- plot +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = "sans", size = 14, color = "black"),
      plot.subtitle = ggplot2::element_text(family = "sans", size = 11, color = "black")
    ) +
    ggplot2::coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    ggplot2::scale_x_continuous(
      guide = ggplot2::guide_axis(n.dodge = 1),
      breaks = filter_breaks(
        plot_data[[xbreaks_var]],
        min_cm_distance = xbreaks_mindist,
        plot = plot
      ),
      labels = \(x) ifelse(x %% 1 == 0, as.character(as.integer(x)), as.character(x))
    )

  # Add color when specified
  if (!is.null(color_var)) {
    plot <- plot +
      ggplot2::aes(color = !!sym(color_var))

    if (length(unique(plot_data[[color_var]])) >= 1) {
      plot <- plot +
        ggplot2::theme(legend.position = "bottom")
    }
  }

  # Modify legend label when specified
  if (!is.null(color_var_label)) {
    plot <- plot +
      ggplot2::labs(color = color_var_label) +
      ggplot2::theme(legend.position = "bottom")
  }

  if (scale == "LOG") {
    plot <- plot +
      ggplot2::labs(y = paste0("Log 10 - ", plot$labels$y))

  }
  if (scale == "SBS") {
    if (!requireNamespace("ggh4x", silently = TRUE))
      stop(
        "Side-by-side view requires `ggh4x` package, please install it with ",
        "`install.packages('ggh4x')`"
      )
    if (!requireNamespace("scales", silently = TRUE))
      stop(
        "Side-by-side view requires `scales` package, please install it with ",
        "`install.packages('scales')`"
      )

    # Create SBS version of data and plot
    adpc_grouped <- rbind(adpc_grouped, adpc_grouped) %>%
      dplyr::mutate(
        view = c(
          rep("Linear view", nrow(adpc_grouped)),
          rep("Semilogarithmic view (Log10)", nrow(adpc_grouped))
        ),
        !!sym(yvar) := ifelse(
          !!sym(yvar) < 1e-3 & view == "Semilogarithmic view (Log10)", yes = 1e-3, no = !!sym(yvar)
        )
      )

    plot <- plot %+% dplyr::filter(adpc_grouped, id_plot == unique(id_plot)[1]) +
      ggplot2::facet_wrap(~view, scales = "free_y") +
      ggh4x::scale_y_facet(
        view == "Semilogarithmic view (Log10)",
        trans = "log10",
        breaks = scales::breaks_log(),
        labels = scales::label_log()
      )
  }

  lapply(unique(adpc_grouped[["id_plot"]]), \(id_val) {
    plot_data <- adpc_grouped %>% dplyr::filter(id_plot ==  id_val)

    title <- {
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

    subtitle <- {
      if (is.null(subtitle)) {
        paste0(
          # TODO(mateusz): Refactor when label attributes are implemented.
          paste(
            unlist(unname(plotgroup_names[plotgroup_vars])), ": ",
            unique(plot_data[, plotgroup_vars]),
            sep = "",  collapse = ", "
          ),
          " (N=", length(unique(plot_data$USUBJID)), ")"
        )
      } else {
        parse_annotation(plot_data, subtitle)
      }
    }

    title_text <- paste0(title, "<br>", "<sup>", subtitle, "</sup>")
    title_margin <- (0.5 * length(unlist(strsplit(title_text, "\n|<br>"))))

    #' magic numbers for footnote position and margin, work in app up to 4 lines
    footnote <- {
      if (is.null(footnote)) {
        ""
      } else {
        parse_annotation(plot_data, footnote)
      }
    }
    footnote_y <- 0.1 + (0.05 * length(unlist(strsplit(footnote, "\n|<br>"))))

    suppressWarnings({
      plotly_plot <- plot %+%
        plot_data %+%
        theme(
          # add margin to make space for subtitle and footnote #
          plot.margin = margin(
            title_margin,
            0,
            footnote_y * 5,
            0,
            "cm"
          )
        ) %>%
        ggplotly(
          tooltip = c("x", "y"),
          dynamicTicks = TRUE,
          #' NOTE: might require some fine tuning down the line, looks fine now
          height = 500 + (footnote_y * 25) + title_margin * 50
        ) %>%
        layout(
          legend = list(
            orientation = "h",
            y = -0.2 # Move the legend down to avoid overlap
          ),
          yaxis = list(autorange = FALSE),
          xaxis = list(autorange = FALSE),
          # title and subtitle #
          title = list(text = title_text),
          # footnote #
          annotations = list(
            x = 0,
            y = -footnote_y - 0.2,
            text = footnote,
            showarrow = FALSE,
            yref = "paper",
            xref = "paper",
            align = "left",
            parse = TRUE
          )
        )
    })
    if (scale == "LOG") {
      plotly_plot <- plotly_plot %>%
        layout(yaxis = list(type = "log"))
    }

    plotly_plot
  }) |>
    setNames(unique(adpc[["id_plot"]]))
}

#' Wrapper around aNCA::pkcg03() function. Calls the function with `LIN` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg03.
#' @export
g_pkcg03_lin <- function(data, ...) {
  pkcg03(adpc = data, scale = "LIN", ...)
}

#' Wrapper around aNCA::pkcg03() function. Calls the function with `LOG` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg01.
#' @export
g_pkcg03_log <- function(data, ...) {
  pkcg03(adpc = data, scale = "LOG", ...)
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
#' @param xbreaks_mindist A numeric value for minimum distance between x-axis breaks in centimeters.
#' @param xmin            A numeric value specifying the minimum x-axis limit.
#' @param xmax            A numeric value specifying the maximum x-axis limit.
#' @param ymin            A numeric value for the minimum y-axis limit.
#' @param ymax            A numeric value for the maximum y-axis limit.
#' @param xlab            Character for x-axis label. Defaults: `xvar` label & `xvar_unit`.
#' @param ylab            Character for y-axis label. Defaults: `yvar` label & `yvar_unit`.
#' @param title           A character string for plot title.
#' @param subtitle        A character string for plot subtitle.
#' @param footnote        A character string of a manual footnote for the plot.
#' @param plotgroup_vars  A character vector of the variables to group data.
#' @param plotgroup_names A character vector of the grouping variable names.
#' @param studyid         A character string specifying the study ID variable.
#' @param scale           A character string:"LIN", "LOG", or "SBS", defining the y-axis scale.
#' @param trt_var         A character string specifying the treatment variable.
#' @returns A list of ggplot objects for each unique group.
#' @importFrom dplyr mutate across rowwise ungroup group_by n
#' @importFrom ggplot2 aes scale_x_continuous labs
#' @importFrom tern g_ipp
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#'   adpc <- read.csv("inst/shiny/data/DummyRO_ADNCA.csv")
#'   attr(adpc[["AFRLT"]], "label") <- "Actual time from first dose"
#'   attr(adpc[["AVAL"]], "label") <- "Analysis val"
#'
#'   plots_lin <- pckg01(adpc = adpc, xmax = 1)
#'   plots_log <- pckg01(adpc = adpc, color_var = "USUBJID", scale = "LOG")
#'   plots_sbs <- pckg01(
#'     adpc = adpc,
#'     color_var = "USUBJID",
#'     xbreaks_var = "NFRLT",
#'     xmin = 100,
#'     xmax = 1000,
#'     scale = "SBS"
#'   )
#' }
#'
#' @export
#' @author Gerardo Rodriguez
pkcg03 <- function(
    adpc = data(),
    xvar = "NFRLT",
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
    plotgroup_vars = c("ROUTE", "PCSPEC", "PARAM"),
    # TODO(mateusz): Refactor when label attributes are implemented.
    plotgroup_names = list(
      "ROUTE" = "Route",
      "PCSPEC" = "Specimen",
      "PARAM" = "Analyte"
    ),
    summary_method = "Mean_sdi",
    whiskers_lwr_upr = c("Both","Upper","Lower")[1],
    # Specific inputs (needs metadata specification),
    scale = c("LIN", "LOG", "SBS")[1],
    studyid = "STUDYID",
    mean_group_var = "DOSEA"
) {
  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  ymin <- as.numeric(ymin)
  ymax <- as.numeric(ymax)
  
  
  # Validate summary_method
  summary_settings <- list(
    Mean_ci    = list(mid = "mean", interval = "mean_ci", whiskers = c("mean_ci_lwr", "mean_ci_upr")),
    Mean_sdi   = list(mid = "mean", interval = "mean_sdi", whiskers = c("mean_sdi_lwr", "mean_sdi_upr")),
    Mean_se    = list(mid = "mean", interval = "mean_sei", whiskers = c("mean_sei_lwr", "mean_sei_upr")),
    Median_ci = list(mid = "median", interval = "median_ci", whiskers = c("median_ci_lwr", "median_ci_upr"))
  )
  if (!summary_method %in% names(summary_settings)) {
    stop("Invalid summary_method. Choose from: ", paste(names(summary_settings), collapse = ", "))
  }
  mid_value <- summary_settings[[summary_method]]$mid
  interval_value <- summary_settings[[summary_method]]$interval
  if (whiskers_lwr_upr=="Both"){
    whiskers_value <- summary_settings[[summary_method]]$whiskers
  } else if (whiskers_lwr_upr=="Upper"){
    whiskers_value <- summary_settings[[summary_method]]$whiskers[2]
  } else {
    whiskers_value <- summary_settings[[summary_method]]$whiskers[1]
  }
  # save col labels, as further adpc tranformations cause them to be lost #
  col_labels <- purrr::map(adpc, ~ attr(.x, "label"))
  
  adpc_grouped <- adpc %>%
    mutate(across(all_of(plotgroup_vars), as.character)) %>%
    rowwise() %>%
    dplyr::mutate(id_plot = interaction(!!!syms(plotgroup_vars)))
  
  # reapply col labels to grouped data #
  adpc_grouped <- purrr::map2_dfc(adpc_grouped, names(adpc_grouped), ~ {
    attr(.x, "label") <- col_labels[[.y]]
    .x
  })
  
  # Construct the reference ggplot object
  plot_data <- adpc_grouped %>% filter(id_plot == id_plot[1])
  
  plot <- tern::g_lineplot(
    df = plot_data,
    variables =  tern::control_lineplot_vars(
      x = xvar,
      y = yvar,
      group_var = mean_group_var ,
      paramcd = "PARAM",
      y_unit = yvar_unit ,
      subject_var = "USUBJID",
    ),
    alt_counts_df = plot_data,
    mid = mid_value,
    interval = interval_value,
    whiskers = whiskers_value,
    x_lab = parse_annotation(plot_data, xlab),
    y_lab = parse_annotation(plot_data, ylab),,
    y_lab_add_paramcd = FALSE,
    y_lab_add_unit = FALSE,
    title = "Plot of Mean and 95% Confidence Limits by Visit",
    subtitle = "xxx",
    caption = NULL
  )
  print(plot[[1]])
  # Provide limits and additional potential future aesthetic customizations
  plot <- plot +
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
      labels = \(x) ifelse(x %% 1 == 0, as.character(as.integer(x)), as.character(x))
    )
  
  # Add color when specified
  if (!is.null(color_var)) {
    plot <- plot +
      ggplot2::aes(color = !!sym(color_var))
    
    if (length(unique(plot_data[[color_var]])) >= 1) {
      plot <- plot +
        ggplot2::theme(legend.position = "bottom")
    }
  }
  
  # Modify legend label when specified
  if (!is.null(color_var_label)) {
    plot <- plot +
      ggplot2::labs(color = color_var_label) +
      ggplot2::theme(legend.position = "bottom")
  }
  
  if (scale == "LOG") {
    plot <- plot +
      labs(y = paste0("Log 10 - ", plot$labels$y))
  }
  
  if (scale == "SBS") {
    if (!requireNamespace("ggh4x", silently = TRUE))
      stop(
        "Side-by-side view requires `ggh4x` package, please install it with ",
        "`install.packages('ggh4x')`"
      )
    if (!requireNamespace("scales", silently = TRUE))
      stop(
        "Side-by-side view requires `scales` package, please install it with ",
        "`install.packages('scales')`"
      )
    
    # Create SBS version of data and plot
    adpc_grouped <- rbind(adpc_grouped, adpc_grouped) %>%
      dplyr::mutate(
        view = c(
          rep("Linear view", nrow(adpc_grouped)),
          rep("Semilogarithmic view (Log10)", nrow(adpc_grouped))
        ),
        !!sym(yvar) := ifelse(
          !!sym(yvar) < 1e-3 & view == "Semilogarithmic view (Log10)", yes = 1e-3, no = !!sym(yvar)
        )
      )
    
    plot <- plot %+% dplyr::filter(adpc_grouped, id_plot == unique(id_plot)[1]) +
      facet_wrap(~ view, scales = "free_y") +
      ggh4x::scale_y_facet(
        view == "Semilogarithmic view (Log10)",
        trans  = "log10",
        breaks = scales::breaks_log(),
        labels = scales::label_log()
      )
  }
  
  # Create the list of plots for each unique group
  lapply(unique(adpc_grouped[["id_plot"]]), \(id_val) {
    plot_data <- adpc_grouped %>% dplyr::filter(id_plot ==  id_val)
    plot_2 <- tern::g_lineplot(
      df = plot_data,
      variables =  tern::control_lineplot_vars(
        x = xvar,
        y = yvar,
        group_var = mean_group_var ,
        paramcd = "PARAM",
        y_unit = yvar_unit ,
        subject_var = "USUBJID",
      ),
      alt_counts_df = plot_data,
      mid = mid_value,
      interval = interval_value,
      whiskers = whiskers_value,
      x_lab = parse_annotation(plot_data, xlab),
      y_lab = parse_annotation(plot_data, ylab),,
      y_lab_add_paramcd = FALSE,
      y_lab_add_unit = FALSE,
      title = "Plot of Mean and 95% Confidence Limits by Visit",
      subtitle = "xxx",
      caption = NULL
    )
    method_label <- dplyr::case_when(
      summary_method == "Mean_se" & whiskers_lwr_upr == "Both"  ~ "Mean ± SE",
      summary_method == "Mean_se" & whiskers_lwr_upr == "Upper" ~ "Mean + SE",
      summary_method == "Mean_se" & whiskers_lwr_upr == "Lower" ~ "Mean - SE",
      
      summary_method == "Mean_sdi" & whiskers_lwr_upr == "Both"  ~ "Mean ± SD",
      summary_method == "Mean_sdi" & whiskers_lwr_upr == "Upper" ~ "Mean + SD",
      summary_method == "Mean_sdi" & whiskers_lwr_upr == "Lower" ~ "Mean - SD",
      
      summary_method == "Mean_ci" & whiskers_lwr_upr == "Both"  ~ "Mean and 95% Confidence Limits",
      summary_method == "Mean_ci" & whiskers_lwr_upr == "Upper" ~ "Mean and Upper 95% Confidence Limit",
      summary_method == "Mean_ci" & whiskers_lwr_upr == "Lower" ~ "Mean and Lower 95% Confidence Limit",
      
      summary_method == "Median_ci" & whiskers_lwr_upr == "Both"  ~ "Median and 95% Confidence Limits",
      summary_method == "Median_ci" & whiskers_lwr_upr == "Upper" ~ "Median and Upper 95% Confidence Limit",
      summary_method == "Median_ci" & whiskers_lwr_upr == "Lower" ~ "Median and Lower 95% Confidence Limit",
      
    )
    
    
    title <- {
      if (is.null(title)) {
        paste0(
          method_label," plot of PK concentrations vs time ",
          dplyr::case_when(
            scale == "LIN" ~ "linear",
            scale == "LOG" ~ "logarithmic",
            TRUE ~ "linear and logarithmic"
          ),
          " scale, by ", parse_annotation(plot_data,paste0("!", mean_group_var)),": ",unique(plot_data[[studyid]])
        )
      } else {
        parse_annotation(plot_data, title)
      }
    }
    
    subtitle <- {
      if (is.null(subtitle)) {
        paste0(
          #"Treatment Group: ", unique(plot_data[[mean_group_var]]), " (N=", nrow(plot_data), ")<br>",
          # TODO(mateusz): Refactor when label attributes are implemented.
          paste(
            unlist(unname(plotgroup_names[plotgroup_vars])), ": ",
            unique(plot_data[, plotgroup_vars]),
            sep = "",  collapse = ", "
          )
        )
      } else {
        parse_annotation(plot_data, subtitle)
      }
    }
    
    title_text <- paste0(title, "<br>", "<sup>", subtitle, "</sup>")
    title_margin <- (0.5 * length(unlist(strsplit(title_text, "\n|<br>"))))
    
    #' magic numbers for footnote position and margin, work in app up to 4 lines
    footnote <- {
      if (is.null(footnote)) {
        ""
      } else {
        parse_annotation(plot_data, footnote)
      }
    }
    footnote_y <- 0.1 + (0.05 * length(unlist(strsplit(footnote, "\n|<br>"))))
    
    suppressWarnings({
      plotly_plot <- plot %+%
        plot_2[[1]] %+%
        theme(
          # add margin to make space for subtitle and footnote #
          plot.margin = margin(
            title_margin,
            0,
            footnote_y * 5,
            0,
            "cm"
          )
        ) %>%
        ggplotly(
          tooltip = c("x", "y"),
          dynamicTicks = TRUE,
          #' NOTE: might require some fine tuning down the line, looks fine now
          height = 500 + (footnote_y * 25) + title_margin * 50
        ) %>%
        layout(
          legend = list(
            orientation = "h"
            ,
            y = -0.2 # Move the legend down to avoid overlap
          ),
          yaxis = list(autorange = FALSE),
          xaxis = list(autorange = FALSE),
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
          layout(yaxis = list(type = "log"))
      }
    })
    plotly_plot
  }) |>
    setNames(unique(adpc[["id_plot"]]))
}