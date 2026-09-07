#' ADPP Exclusions Shiny Module
#'
#' UI and server logic for excluding PK parameter rows from summary outputs.
#' Users select rows from the table (or click points in the integrated
#' boxplots) and mark them for exclusion. Excluded rows are flagged via
#' PPSUMXF = "Y" in ADPP.
#'
#' Colour coding of excluded records (shared by table rows and plot crosses):
#' - Red:    default flag-rule exclusions
#' - Yellow: custom (user) exclusions
#' - Orange: both flag and manual
#'
# Colour palette defined in R/classify_exclusion.R (EXCL_TYPE_COLORS).

parameter_exclusions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 10,
        div(
          style = "display: flex; gap: 8px; align-items: center;",
          textInput(
            ns("exclusion_reason"),
            label = NULL,
            placeholder = "Enter exclusion reason"
          ),
          actionButton(
            ns("add_exclusion"),
            label = "Add",
            class = "btn btn-primary btn-sm"
          )
        )
      ),
      column(
        width = 2,
        dropdown(
          div(
            style = "min-width:340px; max-width:480px;",
            tags$h2("ADPP Exclusions Help",
                    style = "font-size:1.2em; margin-bottom:8px;"),
            p(
              "Exclude PK parameter rows from summary statistics and",
              "applicable plots while retaining them in ADPP and listings."
            ),
            tags$ul(
              tags$li("Select one or more PK parameters to display."),
              tags$li(
                "Select rows in the table (or click points in the boxplots)",
                "and provide a reason, then click Add."
              ),
              tags$li(tags$b("Red"), ": default flag-rule exclusion."),
              tags$li(tags$b("Yellow"), ": custom (user) exclusion."),
              tags$li(tags$b("Orange"), ": both flag and manual exclusion."),
              tags$li(
                "Excluded rows are marked PPSUMXF = \"Y\" in ADPP and are",
                "removed from summary statistics and applicable plots."
              )
            ),
            p("Remove manual exclusions anytime by clicking the X button.")
          ),
          style = "unite",
          right = TRUE,
          icon = icon("question"),
          status = "primary"
        )
      )
    ),
    uiOutput(ns("exclusion_list_ui")),
    div(
      class = "results-legend",
      style = "display:flex; gap:12px; align-items:center; margin:8px 0;",
      div(style = "font-weight:600; font-size:0.95em; margin-right:8px;",
          "Exclusion Colors:"),
      .legend_swatch(
        aNCA:::EXCL_TYPE_COLORS[["flag"]], "Flag exclusion",
        "Excluded by a default flag rule (e.g. R2ADJ, AUCPEO)."
      ),
      .legend_swatch(
        aNCA:::EXCL_TYPE_COLORS[["manual"]], "Custom exclusion",
        "Excluded manually by the user."
      ),
      .legend_swatch(
        aNCA:::EXCL_TYPE_COLORS[["both"]], "Flag + custom",
        "Excluded by both a flag rule and a manual exclusion."
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        position = "right", open = TRUE,
        selectInput(
          inputId = ns("filter_pps"),
          label = "Filter PPs:",
          choices = c(
            "All" = "all",
            "With a flagged result" = "flagged",
            "With an outlier" = "outlier"
          ),
          selected = "all"
        ),
        uiOutput(ns("select_pps_ui_wrapper")),
        uiOutput(ns("group_xvars_ui_wrapper")),
        uiOutput(ns("select_colorvars_ui_wrapper")),
        switchInput(
          inputId = ns("violinplot_toggle_switch"),
          label = "",
          value = TRUE,
          onLabel = "Boxplot",
          offLabel = "Violinplot"
        )
      ),
      uiOutput(ns("boxplots_ui"))
    ),
    card(reactable_ui(ns("param_table")), class = "border-0 shadow-none")
  )
}

# Build the display data frame for the parameter exclusions table.
# Derives PPSUMXF/PPSUMRSN from the PKNCA exclude column, then layers
# manual exclusions on top. Adds two internal columns:
#   .row_id    stable row index into result_df (survives PP filtering)
#   .excl_type "flag" / "manual" / "both" / "none" for row colouring
.build_param_display <- function(result_df, group_cols, manual_exclusions) {
  # Rename manual interval parameters (e.g. AUCINT -> AUCINT_0-12) so the
  # table PPTESTCD matches the boxplot selectors. Row order is preserved, so
  # .row_id below still aligns with the original result rows.
  if (all(c("type_interval", "start_dose", "end_dose") %in% names(result_df))) {
    result_df <- aNCA:::rename_interval_params(result_df)
  }

  display_cols <- c(
    "PPTESTCD", group_cols, "ATPTREF",
    "PPTEST", "PPORRES", "PPORRESU",
    "PPSTRESN", "PPSTRESU"
  )
  available_cols <- intersect(display_cols, names(result_df))
  df <- result_df[, available_cols, drop = FALSE]

  n <- nrow(df)

  # Default flag exclusions live in the PKNCA `exclude` column.
  flag_vals <- result_df[["exclude"]]
  if (!is.null(flag_vals)) {
    flag_vals[is.na(flag_vals)] <- ""
  } else {
    flag_vals <- rep("", n)
  }
  is_flag <- flag_vals != ""

  # Manual (user) exclusions are supplied as a list; indices reference
  # result_df row positions.
  excl_info <- .build_exclusion_reasons(manual_exclusions, n_rows = n)
  is_manual <- rep(FALSE, n)
  exclude_vals <- flag_vals
  if (length(excl_info$indices) > 0) {
    for (j in seq_along(excl_info$indices)) {
      idx <- excl_info$indices[j]
      reason <- excl_info$reasons[j]
      is_manual[idx] <- TRUE
      existing <- exclude_vals[idx]
      exclude_vals[idx] <- if (existing == "") reason else paste(existing, reason, sep = "; ")
    }
  }

  ppsum <- aNCA:::.derive_ppsum_flags(exclude_vals)
  df$PPSUMXF <- ppsum$PPSUMXF
  df$PPSUMRSN <- ppsum$PPSUMRSN

  df <- apply_labels(df, type = "ADPP")

  # Internal columns (hidden in the table) — appended after labelling so
  # they are never relabelled or exported.
  df$.row_id <- seq_len(n)
  df$.excl_type <- aNCA:::.classify_exclusion(is_flag, is_manual)
  df
}

# .build_exclusion_reasons and .render_exclusion_table are defined in
# inst/shiny/functions/utils-exclusions.R and shared with general_exclusions.

# Sort the ADPP exclusions display by parameter while preserving the stable
# `.row_id` used to connect plot clicks back to result rows.
.sort_param_display <- function(df) {
  if (!"PPTESTCD" %in% names(df)) return(df)
  df[order(df$PPTESTCD, seq_len(nrow(df)), na.last = TRUE), , drop = FALSE]
}

.adpp_excl_log <- function(...) {
  message("[ADPP exclusions] ", paste(..., collapse = ""))
}

.adpp_excl_event_summary <- function(ev) {
  if (is.null(ev)) return("<NULL>")

  fields <- paste(names(ev), collapse = ",")
  key <- if (is.null(ev$key)) "<NULL>" else paste(ev$key, collapse = "|")
  curve <- if (is.null(ev$curveNumber)) "<NULL>" else paste(ev$curveNumber, collapse = "|")
  point <- if (is.null(ev$pointNumber)) "<NULL>" else paste(ev$pointNumber, collapse = "|")

  paste0(
    "fields={", fields, "}; key=", key,
    "; curveNumber=", curve,
    "; pointNumber=", point
  )
}

# Return the visible table row corresponding to a clicked Plotly point.
# Plot points carry `.row_id` as their Plotly key, so duplicate/similar
# PPSTRESN values can still resolve to the exact ADPP row.
.clicked_display_row <- function(ev, display_df) {
  if (is.null(ev) || is.null(ev$key)) return(NA_integer_)
  if (!".row_id" %in% names(display_df)) return(NA_integer_)

  row_id <- suppressWarnings(as.integer(ev$key[[1]]))
  if (is.na(row_id)) return(NA_integer_)

  match(row_id, display_df$.row_id)
}

# Return the stable result row id carried by a clicked Plotly point.
.clicked_row_id <- function(ev) {
  if (is.null(ev) || is.null(ev$key)) return(NA_integer_)
  suppressWarnings(as.integer(ev$key[[1]]))
}

# Highlight the table row matching a clicked boxplot point.
.highlight_clicked_row <- function(ev, display_df, selected_plot_row_id,
                                   session, ns) {
  row_id <- .clicked_row_id(ev)
  row <- .clicked_display_row(ev, display_df)
  .adpp_excl_log(
    "clicked row_id=", row_id,
    "; ",
    "resolved clicked key to visible row=", row,
    "; displayed rows=", nrow(display_df)
  )
  if (is.na(row)) return(invisible())

  selected_plot_row_id(row_id)
  .adpp_excl_log("stored plot-selected row_id=", row_id)

  updateReactable(ns("param_table-table"), selected = row)
  .adpp_excl_log(
    "called updateReactable for outputId=", ns("param_table-table"),
    "; selected=", row
  )
  invisible()
}

parameter_exclusions_server <- function(id, res_nca) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exclusion_list <- reactiveVal(list())
    xbtn_counter <- reactiveVal(0)
    prev_fingerprint <- reactiveVal(NULL)
    selected_plot_row_id <- reactiveVal(NA_integer_)

    # Clear exclusions only when result structure changes (row count or columns),
    # not on every recomputation (e.g. unit changes that preserve row identity).
    observeEvent(res_nca(), {
      res <- res_nca()$result
      fp <- paste(nrow(res), paste(names(res), collapse = ","))
      if (!identical(fp, prev_fingerprint())) {
        exclusion_list(list())
        xbtn_counter(0)
        selected_plot_row_id(NA_integer_)
        prev_fingerprint(fp)
      }
    })

    # res_nca tagged with this module's manual exclusions (.pp_excl markers),
    # so the embedded boxplots show manual exclusions as yellow crosses.
    res_nca_tagged <- reactive({
      req(res_nca())
      excl_info <- .build_exclusion_reasons(exclusion_list())
      .apply_param_exclusions(res_nca(), excl_info)$tagged
    })

    # Full display table (all parameters). Carries hidden .row_id / .excl_type.
    param_data_full <- reactive({
      req(res_nca())
      group_cols <- unique(unlist(unname(
        res_nca()$data$conc$columns$groups
      )))
      .build_param_display(res_nca()$result, group_cols, exclusion_list())
    })

    # -- Boxplot / PP selector inputs ------------------------------------------

    # Choices available in "Select PPs", narrowed by the "Filter PPs" control.
    pp_choices <- reactive({
      req(res_nca())
      result_data <- res_nca()$result
      if (all(c("type_interval", "start_dose", "end_dose") %in%
                names(result_data))) {
        result_data <- aNCA:::rename_interval_params(result_data)
      }
      all_params <- unique(result_data$PPTESTCD)

      switch(input$filter_pps %||% "all",
        flagged = {
          excl <- result_data[["exclude"]]
          has_flag <- !is.na(excl) & excl != ""
          intersect(all_params, unique(result_data$PPTESTCD[has_flag]))
        },
        outlier = params_with_outliers(
          res_nca_tagged(),
          group_cols = unique(c(
            input$selected_xvars_boxplot, input$selected_colorvars_boxplot
          ))
        ),
        all_params
      )
    })

    # Render selectors when results change (populate X/color grouping choices).
    observeEvent(res_nca(), {
      conc_dose_cols <- unique(c(
        names(res_nca()$data$conc$data),
        names(res_nca()$data$dose$data)
      ))
      default_group <- c(
        res_nca()$data$dose$columns$dose,
        res_nca()$data$conc$columns$groups$group_analyte
      )

      selector_label(
        input = input, output = output, session = session,
        choices = conc_dose_cols, initial_selection = default_group,
        selector_ui_wrapper = "group_xvars_ui_wrapper",
        id = "selected_xvars_boxplot",
        label = "Select X grouping variables:",
        metadata_type = "variable"
      )
      selector_label(
        input = input, output = output, session = session,
        choices = conc_dose_cols, initial_selection = default_group,
        selector_ui_wrapper = "select_colorvars_ui_wrapper",
        id = "selected_colorvars_boxplot",
        label = "Select coloring variables:",
        metadata_type = "variable"
      )
    })

    # Render/refresh the "Select PPs" multi-select as the filtered choices
    # change. Preserve the user's current selection where still valid.
    observeEvent(pp_choices(), {
      choices <- pp_choices()
      current <- input$selected_pps_boxplot
      keep <- intersect(current, choices)
      default_sel <- if (length(keep) > 0) {
        keep
      } else if ("CMAX" %in% choices) {
        "CMAX"
      } else if (length(choices) > 0) {
        choices[1]
      } else {
        character(0)
      }

      selector_label(
        input = input, output = output, session = session,
        choices = choices, initial_selection = default_sel,
        selector_ui_wrapper = "select_pps_ui_wrapper",
        id = "selected_pps_boxplot",
        label = "Select PPs:",
        metadata_type = "parameter",
        multiple = TRUE
      )
    }, ignoreNULL = FALSE)

    # -- Exclusion table (filtered by selected PPs) ----------------------------

    # Display rows for the selected PPs only. .row_id preserves the mapping
    # back to the full result so selection still records correct indices.
    param_data <- reactive({
      req(param_data_full())
      sel <- input$selected_pps_boxplot
      df <- param_data_full()
      if (!is.null(sel) && length(sel) > 0 && "PPTESTCD" %in% names(df)) {
        df <- df[df$PPTESTCD %in% sel, , drop = FALSE]
      }
      df <- .sort_param_display(df)
      df$.plot_clicked <- df$.row_id %in% selected_plot_row_id()
      df
    })

    # Row id (into the full result) for each displayed row, in display order.
    displayed_row_ids <- reactive({
      req(param_data())
      param_data()$.row_id
    })

    param_table_state <- reactable_server(
      "param_table",
      param_data,
      selection = "multiple",
      onClick = "select",
      borderless = TRUE,
      defaultPageSize = 25,
      pageSizeOptions = function(data) unique(c(25, 50, 100, nrow(data))),
      # Keep internal columns in the data (needed for coloring) but hide them.
      columns = function(data) {
        defs <- define_cols(data)
        defs[[".row_id"]] <- reactable::colDef(show = FALSE)
        defs[[".excl_type"]] <- reactable::colDef(show = FALSE)
        defs[[".plot_clicked"]] <- reactable::colDef(show = FALSE)
        defs
      },
      # Colour exclusion rows, and strongly outline the point clicked in
      # the plot. The outline is server-side state, so it remains visible
      # even when reactable's client-side selected style is unavailable.
      rowStyle = function(x) {
        types <- x$.excl_type
        clicked <- x$.plot_clicked
        function(index) {
          style <- list()
          color <- aNCA:::.exclusion_type_color(types[index])
          if (!is.na(color)) {
            style$background <- color
          }
          if (isTRUE(clicked[index])) {
            style$background <- if (!is.na(color)) color else "#D8ECFF"
            style$boxShadow <- "inset 4px 0 0 #0072B2"
            style$outline <- "2px solid #0072B2"
            style$outlineOffset <- "-2px"
            style$fontWeight <- "600"
          }
          if (length(style) > 0) style else NULL
        }
      }
    )

    # Add exclusion when button is pressed. Table selection indices are
    # translated to full-result row ids via displayed_row_ids().
    observeEvent(input$add_exclusion, {
      rows_sel <- param_table_state()$selected
      reason <- input$exclusion_reason
      if (nzchar(reason)) {
        row_ids <- displayed_row_ids()[rows_sel]
        row_ids <- row_ids[!is.na(row_ids)]

        plot_row_id <- selected_plot_row_id()
        if (length(row_ids) == 0 &&
            !is.na(plot_row_id) &&
            plot_row_id %in% displayed_row_ids()) {
          row_ids <- plot_row_id
        }

        if (length(row_ids) == 0) return(invisible())

        current <- exclusion_list()
        xbtn_id <- paste0("remove_param_excl_", xbtn_counter() + 1)
        xbtn_counter(xbtn_counter() + 1)
        new_entry <- list(list(
          reason = reason, rows = row_ids, xbtn_id = xbtn_id
        ))
        exclusion_list(append(current, new_entry))
        selected_plot_row_id(NA_integer_)
        updateTextInput(session, "exclusion_reason", value = "")
        updateReactable(ns("param_table-table"), selected = NA)
      }
    })

    # -- Boxplots (one per selected PP) ----------------------------------------

    # Parameter names can contain characters invalid in Shiny IDs (e.g. the
    # interval suffix in "AUCINT_0-12"). Map each parameter to a safe slot id.
    safe_slot <- function(param) paste0("box_", gsub("[^A-Za-z0-9]", "_", param))
    slot_source <- function(slot) paste0("box_src_", slot)

    # Maintain a stable slot -> parameter mapping for the current selection.
    slot_map <- reactive({
      sel <- input$selected_pps_boxplot
      if (is.null(sel)) sel <- character(0)
      setNames(as.list(sel), vapply(sel, safe_slot, character(1)))
    })

    output$boxplots_ui <- renderUI({
      sel <- input$selected_pps_boxplot
      if (is.null(sel) || length(sel) == 0) {
        return(p("Select one or more PK parameters to display boxplots."))
      }
      tagList(lapply(sel, function(param) {
        plotlyOutput(ns(paste0(safe_slot(param), "_plot")), height = "350px")
      }))
    })

    # Render a boxplot output for every selected parameter. Each plot's plotly
    # source id encodes its slot so click events can be traced back.
    observe({
      sel <- input$selected_pps_boxplot
      req(sel)
      req(res_nca())
      req(input$selected_xvars_boxplot)
      req(input$selected_colorvars_boxplot)

      for (param in sel) {
        local({
          local_param <- param
          slot <- safe_slot(local_param)
          output[[paste0(slot, "_plot")]] <- renderPlotly({
            source <- slot_source(slot)
            .adpp_excl_log(
              "rendering plot parameter=", local_param,
              "; slot=", slot,
              "; source=", source
            )
            p <- flexible_violinboxplot(
              res_nca = res_nca_tagged(),
              parameter = local_param,
              xvars = input$selected_xvars_boxplot,
              colorvars = input$selected_colorvars_boxplot,
              varvalstofilter = NULL,
              tooltip_vars = unname(unlist(res_nca()$data$conc$columns$groups)),
              box = input$violinplot_toggle_switch,
              show_excluded = TRUE,
              plotly_source = source
            )
            if (inherits(p, "plotly")) {
              p <- plotly::event_register(p, "plotly_click")
              p <- htmlwidgets::onRender(p, "
                function(el, x) {
                  console.log('[ADPP exclusions] plotly rendered', {
                    id: el.id,
                    source: x.source
                  });
                  if (el.__adppExclClickLoggerAttached) return;
                  el.__adppExclClickLoggerAttached = true;
                  el.on('plotly_click', function(data) {
                    var points = (data && data.points) || [];
                    console.log('[ADPP exclusions] browser plotly_click', {
                      id: el.id,
                      source: x.source,
                      point_count: points.length,
                      keys: points.map(function(pt) { return pt.key; }),
                      curve_numbers: points.map(function(pt) { return pt.curveNumber; }),
                      point_numbers: points.map(function(pt) { return pt.pointNumber; })
                    });
                  });
                }
              ")
              .adpp_excl_log(
                "registered plotly_click for source=", source,
                "; widget source=", p$x$source
              )
            } else {
              .adpp_excl_log("plot output is not plotly for parameter=", local_param)
            }
            p
          })
        })
      }
    })

    # Click-to-highlight: register one click observer per slot (once), guarding
    # against duplicate registration when the selection changes.
    registered_click_slots <- reactiveVal(character(0))
    observe({
      map <- slot_map()
      already <- registered_click_slots()
      new_slots <- setdiff(names(map), already)
      .adpp_excl_log(
        "slot map updated; active sources=",
        paste(vapply(names(map), slot_source, character(1)), collapse = ","),
        "; new slots=", paste(new_slots, collapse = ",")
      )
      for (slot in new_slots) {
        local({
          local_slot <- slot
          source <- slot_source(local_slot)
          .adpp_excl_log(
            "registering server click observer for slot=", local_slot,
            "; source=", source
          )
          observeEvent(
            plotly::event_data("plotly_click", source = source),
            {
              ev <- plotly::event_data("plotly_click", source = source)
              .adpp_excl_log(
                "server plotly_click source=", source,
                "; ", .adpp_excl_event_summary(ev)
              )
              req(ev)
              .highlight_clicked_row(
                ev, param_data(), selected_plot_row_id, session, ns
              )
            },
            ignoreInit = TRUE
          )
        })
      }
      if (length(new_slots) > 0) {
        registered_click_slots(union(already, new_slots))
      }
    })

    # Track which remove buttons already have observers to avoid duplicates
    registered_xbtns <- reactiveVal(character(0))

    # Register observers for new remove buttons (shared helper)
    observe({
      .register_remove_observers(exclusion_list, registered_xbtns, input)
    })

    output$exclusion_list_ui <- renderUI({
      tbl <- .render_exclusion_table(exclusion_list(), ns)
      if (is.null(tbl)) return(NULL)
      tagList(
        tbl,
        tags$script("setTimeout(function(){ Shiny.bindAll(); }, 100);")
      )
    })

    reactive(.build_exclusion_reasons(exclusion_list()))
  })
}
