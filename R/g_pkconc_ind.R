#' Wrapper around aNCA::pkcg01() function. Calls the function with `LIN` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg01.
#' @export
g_pkconc_ind_lin <- function(data, ...) {
  pkcg01(adpc = data, scale = "LIN", ...)
}

#' Wrapper around aNCA::pkcg01() function. Calls the function with `LOG` scale argument.
#' @param data Data to be passed into the plotting function.
#' @param ...  Any other parameters to be passed into the plotting function.
#' @returns ggplot2 object for pkcg01.
#' @export
g_pkconc_ind_log <- function(data, ...) {
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
#' @param xbreaks_mindist A numeric value for `xbreak_mindist`.
#' @param xmin            A numeric value specifying the minimum x-axis limit.
#' @param xmax            A numeric value specifying the maximum x-axis limit.
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
#'   plots_lin <- pkcg01(adpc = adpc, xmax = 1)
#'   plots_log <- pkcg01(adpc = adpc, color_var = "USUBJID", scale = "LOG")
#'   plots_sbs <- pkcg01(
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

    plotly_plot
  }) |>
    setNames(unique(adpc[["id_plot"]]))
}
