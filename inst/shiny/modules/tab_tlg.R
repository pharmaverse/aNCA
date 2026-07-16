#' Tab handling Tables, Listings and Graphs.
#'
#' @details
#' Tab provides the user with a selection of TLGs and allows the display and customization of
#' various tables, lists and graphs. The definitions table as well as all customization options
#' are based on `tlg.yaml` file in the root directory of the application. This module calls on
#' `tlg_module`s for each TLG in submitted order, creating a coprehensive UI for all visualizations.
#'
#' Read more in the contributing guide.
#'
#' @param id ID of the module
#' @param data ADNCA data object, processed and mapped.

#' Parses TLG definitions from the yaml file, holds all definitions.
.TLG_DEFINITIONS <- {
  defs <- yaml::read_yaml(system.file("shiny/tlg.yaml", package = "aNCA"))

  defs <- purrr::imap(defs, function(opt_def, opt_id) {
    if ("template" %in% names(opt_def)) {
      template_def <- defs[[opt_def$template]]

      for (d in names(opt_def)) {
        if (d == "template") next

        if (d == "options") {
          for (o in names(opt_def$options)) {
            template_def$options[[o]] <- opt_def$options[[o]]
          }
        } else {
          template_def[[d]] <- opt_def[[d]]
        }
      }

      opt_def <- template_def
    }

    opt_def
  }) %>%
    setNames(names(defs))
}

js_close_button <- tags$button(
  type = "button",
  onclick = "$(this).closest('.modal').modal('hide');",
  `aria-label` = "Close",
  # Style updated for color and size
  style = "color: white; border: none; background: transparent; font-size: 1.2em; padding: 0;",
  icon("times")
)

tab_tlg_ui <- function(id) {
  ns <- NS(id)

  navset_pill(
    id = ns("tlg_tabs"),
    nav_panel(
      "Order details",
      card(
        style = "margin-top: 1em;",
        div(
          actionButton(ns("add_tlg"), "Add TLG"),
          actionButton(ns("remove_tlg"), "Remove TLG"),
          actionButton(ns("submit_tlg_order"), "Submit Order Details", class = "btn-primary")
        )
      ),
      card(reactable_ui(ns("selected_tlg_table"))),
    ),
    nav_panel("Tables", uiOutput(ns("tables"), class = "tlg-module"), value = "Tables"),
    nav_panel("Listings", uiOutput(ns("listings"), class = "tlg-module"), value = "Listings"),
    nav_panel("Graphs", uiOutput(ns("graphs"), class = "tlg-module"), value = "Graphs"),
    # disable loader for initial empty UI render #
    footer = tags$style(
      id = "tlg-load-hide", HTML(paste0(".tlg-module .load-container {opacity: 0;}"))
    )
  )
}

tab_tlg_server <- function(id, data, adpp = reactive(NULL)) { # nolint: cyclocomp_linter
  moduleServer(id, function(input, output, session) {
    log_trace("{session$ns(id)}: Attaching server.")

    #' Load TLG orders definitions
    tlg_order <- reactiveVal({
      purrr::map_dfr(.TLG_DEFINITIONS, ~ dplyr::tibble(
        Selection = .x$is_default,
        Type = .x$type,
        Dataset = case_when(
          .x$dataset == "ADNCA" ~ "PK Concentrations",
          .x$dataset == "ADPP" ~ "PK Parameters",
          TRUE ~ .x$dataset
        ),
        PKid = .x$pkid,
        Output = paste0("<a href='", .x$link, "' target='_blank'>", .x$description, "</a>"),
        Link = if (is.null(.x$link)) NA_character_ else .x$link,
        Label = .x$label,
        Description = .x$description,
        Condition = .x$condition,
        Footnote = NA_character_,
        Stratification = NA_character_,
        Comment = NA_character_
      )) %>%
        dplyr::mutate(id = dplyr::row_number(), .before = dplyr::everything())
    })

    # Based on the TLG list conditions for data() define the preselected rows in $Selection
    observeEvent(list(tlg_order(), data()), {
      req(data())

      # Unparsable conditions will be ignored
      new_tlg_order <- tryCatch({
        tlg_order() %>%
          mutate(
            Selection = case_when(
              Condition == "" | is.na(Condition) | is.null(Condition) ~ Selection,
              any(unique(toupper(data()$conc$data$PCSPEC)) %in% Condition) ~ TRUE,
              TRUE ~ Selection
            )
          )
      }, error = function(e) {
        tlg_order()
      })

      tlg_order(new_tlg_order)
    })

    # Columns shown to the user in the Order Details table. Internal columns
    # (PKid, Label, Description, Condition) are kept in tlg_order() but hidden:
    # Condition drives urine auto-preselect above, Label titles the nav panels,
    # and Description feeds the submit log — none need to be user-facing.
    displayed_order <- reactive({
      dplyr::filter(tlg_order(), Selection) %>%
        dplyr::select(Type, Dataset, Output, Footnote, Stratification, Comment)
    }) %>%
      bindEvent(data(), input$confirm_add_tlg, input$remove_tlg)

    selected_tlg_state <- reactable_server(
      "selected_tlg_table",
      displayed_order,
      download_buttons = c("csv", "xlsx"),
      groupBy = c("Type", "Dataset"),
      defaultExpanded = TRUE,
      wrap = TRUE,
      selection = "multiple",
      editable = c("Footnote", "Stratification", "Comment"),
      columns = function(df) {
        define_cols(df, overrides = list(Output = colDef(html = TRUE)))
      }
    )

    observeEvent(selected_tlg_state()$edit(), {
      info <- selected_tlg_state()$edit()

      new_tlg_order <- tlg_order()
      new_tlg_order[new_tlg_order$Selection, ][info$row, info$column] <- info$value
      tlg_order(new_tlg_order)
    })

    # Issue #1335: the "Add TLGs" picker was a grouped reactable whose
    # Type/Dataset columns were blank on every selectable row.  Rebuilt as a
    # catalog-style checklist -- dataset tabs (PK Concentrations / PK Parameters)
    # over one column per Type (Tables / Listings / Graphs), with a search +
    # download + select-all toolbar, per-column select-all, and a live count on
    # the confirm button.  Selection toggling is client-side JS (window.tlgAdd);
    # styling derives from the app's own tokens (primary #007bc2, tint #E7ECFA).
    # modal_group_ids -- checkboxGroupInput ids in the current modal (read on confirm)
    # modal_avail     -- the available-TLG tibble backing the CSV/XLSX downloads
    modal_group_ids <- reactiveVal(character(0))
    modal_avail <- reactiveVal(NULL)

    # Fixed left-to-right order + icon for Type columns, and dataset tab order.
    .TLG_TYPE_ORDER <- c("Table", "Listing", "Graph")
    .TLG_TYPE_ICON  <- c(Table = "table", Listing = "list-ul", Graph = "chart-line")
    .TLG_DATASET_ORDER <- c("PK Concentrations", "PK Parameters")

    # Escape a string for safe embedding inside a single-quoted JS literal.
    .js_str <- function(x) paste0("'", gsub("(['\\\\])", "\\\\\\1", x), "'")

    .build_add_checklist <- function(avail) {
      present_types <- intersect(.TLG_TYPE_ORDER, unique(avail$Type))
      datasets <- c(intersect(.TLG_DATASET_ORDER, unique(avail$Dataset)),
                    setdiff(unique(avail$Dataset), .TLG_DATASET_ORDER))

      pairs <- dplyr::distinct(avail, Type, Dataset)
      pairs <- pairs[order(match(pairs$Type, .TLG_TYPE_ORDER), pairs$Dataset), ]
      pairs$input_id <- paste0("modal_check_", seq_len(nrow(pairs)))
      modal_group_ids(pairs$input_id)

      spec_icon_html <- as.character(icon("circle-info"))

      # One dataset block for a Type column: tagged with data-dataset so the
      # tab bar can show/hide it.  The active tab is applied client-side.
      build_group_ui <- function(type, dataset, input_id) {
        rows <- dplyr::filter(avail, Type == !!type, Dataset == !!dataset)
        choice_names <- purrr::map2(rows$Description, rows$Link, function(desc, link) {
          spec_link <- if (is.na(link)) "" else paste0(
            "<a href='", link, "' target='_blank' onclick='event.stopPropagation()' ",
            "class='tlg-spec' title='View spec'>", spec_icon_html, "</a>"
          )
          HTML(paste0("<span class='tlg-desc'>", htmltools::htmlEscape(desc), "</span>", spec_link))
        })
        div(
          class = "tlg-ds", `data-dataset` = dataset,
          checkboxGroupInput(
            inputId = session$ns(input_id),
            label = NULL,
            choiceNames = choice_names,
            choiceValues = as.character(rows$id)
          )
        )
      }

      # One flex column per Type (plain flex, not the bootstrap grid, whose
      # negative row margins would misalign the columns against the toolbar).
      type_columns <- purrr::map(present_types, function(tp) {
        tp_pairs <- dplyr::rename(pairs[pairs$Type == tp, ], type = Type, dataset = Dataset)
        div(
          class = "tlg-col",
          div(
            class = "tlg-col-head",
            tags$span(
              class = "tlg-col-title",
              icon(.TLG_TYPE_ICON[[tp]]), paste0(" ", tp, "s"),
              tags$span(sum(avail$Type == tp), class = "tlg-col-count")
            ),
            tags$button(
              type = "button", class = "tlg-col-selall",
              onclick = "window.tlgAdd.colSelect(this)", "Select all"
            )
          ),
          div(
            class = "tlg-col-body",
            purrr::pmap(tp_pairs, build_group_ui),
            div(class = "tlg-col-empty", "None in this view", style = "display: none;")
          )
        )
      })

      # Dataset tab bar; first dataset active by default.  data-total feeds the
      # count badge (restored when the search box is cleared).
      tab_bar <- div(
        class = "tlg-tabs",
        purrr::imap(datasets, function(ds, i) {
          ds_total <- sum(avail$Dataset == ds)
          tags$button(
            type = "button",
            class = paste("tlg-tab", if (i == 1) "active" else ""),
            `data-dataset` = ds, `data-total` = ds_total,
            onclick = paste0("window.tlgAdd.setTab(", .js_str(ds), ", this)"),
            ds, tags$span(ds_total, class = "tlg-tab-count")
          )
        })
      )

      init_js <- paste0(
        "window.tlgAdd.tab = ", .js_str(datasets[1]), "; ",
        "window.tlgAdd.q = ''; window.tlgAdd.render();"
      )

      # Shared left inset so toolbar, tabs, column headers and checkbox rows all
      # line up on the same left edge.
      div(
        class = "tlg-add-modal",
        tags$style(HTML("
          .tlg-add-modal { --tlg-inset: 0.55em; }
          .tlg-add-modal .tlg-toolbar {
            display: flex; align-items: center; gap: 0.5em; flex-wrap: wrap;
            margin: 0 0 1.25em; padding-left: var(--tlg-inset);
          }
          .tlg-add-modal .tlg-search-input { width: 300px; max-width: 100%; }
          .tlg-add-modal .tlg-toolbar-sep { width: 1px; align-self: stretch;
            background: #e5e5e5; margin: 0.1em 0.35em; }
          .tlg-add-modal .tlg-tabs {
            display: flex; gap: 0.25em; margin: 0 0 1.25em;
            padding-left: var(--tlg-inset); border-bottom: 1px solid #e5e5e5;
          }
          .tlg-tab { background: none; border: none; padding: 0.5em 0.9em; font-weight: 600;
            color: #7b8794; border-bottom: 2px solid transparent; margin-bottom: -1px;
            cursor: pointer; }
          .tlg-tab:hover { color: #21201f; }
          .tlg-tab.active { color: #007bc2; border-bottom-color: #007bc2; }
          .tlg-tab-count {
            color: #8a8f98; font-weight: 600; font-size: 0.8em; margin-left: 0.35em;
          }
          .tlg-cols { display: flex; align-items: flex-start; gap: 1.5em; }
          .tlg-cols .tlg-col { flex: 1 1 0; min-width: 0; }
          .tlg-cols .tlg-col + .tlg-col { border-left: 1px solid #eee; padding-left: 1.5em; }
          .tlg-col-body { max-height: 55vh; overflow-y: auto; }
          .tlg-col-empty { color: #8a8f98; font-style: italic; font-size: 0.85em;
            padding: 0.6em var(--tlg-inset); }
          .tlg-add-checklist .tlg-col-head {
            display: flex; align-items: baseline; margin: 0 0 0.75em;
            padding: 0 var(--tlg-inset) 0.4em; border-bottom: 2px solid #007bc2;
          }
          .tlg-add-checklist .tlg-col-title { flex: 1 1 auto; font-size: 1em;
            font-weight: 700; color: #21201f; }
          .tlg-add-checklist .tlg-col-head svg,
          .tlg-add-checklist .tlg-col-head .fa { color: #007bc2; margin-right: 0.3em; }
          .tlg-add-checklist .tlg-col-count {
            color: #8a8f98; font-weight: 600; font-size: 0.82em; margin-left: 0.25em;
          }
          .tlg-col-selall { flex: 0 0 auto; background: none; border: none; color: #007bc2;
            font-size: 0.76em; font-weight: 600; cursor: pointer; padding: 0; }
          .tlg-col-selall:hover { text-decoration: underline; }
          .tlg-add-checklist .tlg-ds { margin-bottom: 0.5em; }
          .tlg-add-checklist .checkbox {
            margin: 0; padding: 0.4em var(--tlg-inset); border-radius: 5px;
            transition: background 0.1s;
          }
          .tlg-add-checklist .checkbox:hover { background: #f2f6fb; }
          .tlg-add-checklist .checkbox:has(input:checked) { background: #E7ECFA; }
          .tlg-add-checklist .checkbox label {
            display: flex; align-items: flex-start; gap: 0.55em; text-align: left;
            font-weight: normal; line-height: 1.4; padding-left: 0;
            color: #21201f; cursor: pointer;
          }
          .tlg-add-checklist .checkbox label > span {
            flex: 1 1 auto; display: flex; align-items: flex-start;
            justify-content: space-between; gap: 0.5em;
          }
          .tlg-add-checklist .checkbox input[type=checkbox] {
            position: static; margin: 0.22em 0 0; flex: 0 0 auto;
            width: 15px; height: 15px; accent-color: #007bc2; cursor: pointer;
          }
          .tlg-add-checklist .tlg-spec {
            flex: 0 0 auto; color: #b0b7bf; margin-top: 0.1em;
          }
          .tlg-add-checklist .tlg-spec:hover { color: #007bc2; }
          .tlg-no-matches { color: #8a8f98; font-style: italic;
            padding: 1em 0.5em; padding-left: var(--tlg-inset); }
        ")),
        div(
          class = "tlg-toolbar",
          tags$input(
            type = "text", class = "form-control tlg-search-input",
            placeholder = "Search outputs…",
            oninput = "window.tlgAdd.setQuery(this.value)"
          ),
          tags$button(type = "button", class = "btn btn-sm btn-default",
                      onclick = "window.tlgAdd.selectAll()", "Select all"),
          tags$button(type = "button", class = "btn btn-sm btn-default",
                      onclick = "window.tlgAdd.clearAll()", "Clear all"),
          div(class = "tlg-toolbar-sep"),
          downloadButton(session$ns("modal_dl_csv"), "CSV", class = "btn-sm btn-default"),
          downloadButton(session$ns("modal_dl_xlsx"), "XLSX", class = "btn-sm btn-default")
        ),
        tab_bar,
        div(class = "tlg-add-checklist tlg-cols", type_columns),
        div(class = "tlg-no-matches", "No outputs match your search.", style = "display: none;"),
        tags$script(HTML(paste0("
          window.tlgAdd = {
            q: '', tab: null,
            // visibleOnly: only toggle rows shown under the active tab / search,
            // so Select-all respects the current dataset.  Clear-all passes
            // false to wipe every dataset.
            _setChecked: function(scope, checked, visibleOnly) {
              var groups = {};
              scope.querySelectorAll('.checkbox').forEach(function(row) {
                if (visibleOnly && row.style.display === 'none') return;
                var cb = row.querySelector('input[type=checkbox]');
                if (!cb) return;
                cb.checked = checked;
                var g = cb.closest('.shiny-input-checkboxgroup');
                if (g) groups[g.id] = g;
              });
              Object.keys(groups).forEach(function(id) {
                var inp = groups[id].querySelector('input[type=checkbox]');
                if (inp) inp.dispatchEvent(new Event('change', { bubbles: true }));
              });
            },
            selectAll: function() {
              var r = document.querySelector('.tlg-add-checklist');
              if (r) this._setChecked(r, true, true);
            },
            clearAll: function() {
              var r = document.querySelector('.tlg-add-checklist');
              if (r) this._setChecked(r, false, false);
            },
            colSelect: function(btn) {
              var c = btn.closest('.tlg-col'); if (c) this._setChecked(c, true, true);
            },
            setQuery: function(v) { this.q = v || ''; this.render(); },
            setTab: function(v, btn) {
              this.tab = v;
              document.querySelectorAll('.tlg-tabs .tlg-tab').forEach(function(b) {
                b.classList.remove('active');
              });
              if (btn) btn.classList.add('active');
              this.render();
            },
            render: function() {
              var root = document.querySelector('.tlg-add-checklist');
              if (!root) return;
              var q = this.q.trim().toLowerCase();
              var tab = this.tab;
              var anyGlobal = false;
              var dsMatch = {};
              root.querySelectorAll('.tlg-ds').forEach(function(ds) {
                var name = ds.getAttribute('data-dataset');
                var inTab = (q !== '') ? true : (name === tab);
                var any = false;
                ds.querySelectorAll('.checkbox').forEach(function(it) {
                  var m = inTab && (q === '' || it.textContent.toLowerCase().indexOf(q) > -1);
                  it.style.display = m ? '' : 'none';
                  if (m) { any = true; dsMatch[name] = (dsMatch[name] || 0) + 1; }
                });
                ds.style.display = any ? '' : 'none';
                if (any) anyGlobal = true;
              });
              // Per-column: visible count, empty state, hide select-all when empty.
              root.querySelectorAll('.tlg-col').forEach(function(col) {
                var vis = 0;
                col.querySelectorAll('.checkbox').forEach(function(it) {
                  if (it.style.display !== 'none') vis++;
                });
                var cnt = col.querySelector('.tlg-col-count');
                if (cnt) cnt.textContent = vis;
                var empty = col.querySelector('.tlg-col-empty');
                if (empty) empty.style.display = vis ? 'none' : '';
                var sa = col.querySelector('.tlg-col-selall');
                if (sa) sa.style.display = vis ? '' : 'none';
              });
              // Tab badges: match count while searching, dataset total otherwise.
              document.querySelectorAll('.tlg-tabs .tlg-tab').forEach(function(t) {
                var badge = t.querySelector('.tlg-tab-count');
                if (!badge) return;
                badge.textContent = (q === '')
                  ? t.getAttribute('data-total')
                  : (dsMatch[t.getAttribute('data-dataset')] || 0);
              });
              var nm = document.querySelector('.tlg-no-matches');
              if (nm) nm.style.display = anyGlobal ? 'none' : '';
            }
          };
          ", init_js, "
        ")))
      )
    }

    # Show modal when the add_tlg button is pressed
    observeEvent(input$add_tlg, {
      avail <- dplyr::arrange(dplyr::filter(tlg_order(), !Selection), Type, Dataset)
      modal_avail(avail)

      body <- if (nrow(avail) == 0) {
        modal_group_ids(character(0))
        tags$p("All available TLGs are already in the order.")
      } else {
        .build_add_checklist(avail)
      }

      showModal(modalDialog(
        title = div(
          "Add TLGs to order",
          js_close_button,
          style = "position: relative;"
        ),
        body,
        footer = tagList(
          modalButton("Close"),
          uiOutput(session$ns("modal_confirm_ui"), inline = TRUE)
        ),
        size = "l"
      ))
    })

    # Download the available-TLG catalog shown in the modal.
    .modal_dl_data <- function() {
      df <- modal_avail()
      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(Type = character(), Dataset = character(),
                          PKid = character(), Description = character()))
      }
      dplyr::select(df, Type, Dataset, PKid, Description)
    }
    output$modal_dl_csv <- downloadHandler(
      filename = function() "available_tlgs.csv",
      content = function(file) write.csv(.modal_dl_data(), file, row.names = FALSE)
    )
    output$modal_dl_xlsx <- downloadHandler(
      filename = function() "available_tlgs.xlsx",
      content = function(file) writexl::write_xlsx(.modal_dl_data(), file)
    )

    # Confirm button with a live count of checked outputs; disabled at zero.
    output$modal_confirm_ui <- renderUI({
      n <- length(unlist(lapply(modal_group_ids(), function(gid) input[[gid]])))
      label <- if (n == 0) "Add to order" else paste0("Add ", n, " to order")
      btn <- actionButton(session$ns("confirm_add_tlg"), label, class = "btn-primary")
      if (n == 0) shinyjs::disabled(btn) else btn
    })

    # Update the Selection column when the confirm_add_tlg button is pressed
    observeEvent(input$confirm_add_tlg, {
      checked_ids <- unlist(lapply(modal_group_ids(), function(gid) input[[gid]]))
      if (length(checked_ids) > 0) {
        tlg_order_data <- tlg_order()
        tlg_order_data$Selection[tlg_order_data$id %in% as.integer(checked_ids)] <- TRUE
        tlg_order(tlg_order_data)
      }
      removeModal()
    })

    # Update the Selection column when the remove_tlg button is pressed
    observeEvent(input$remove_tlg, {
      selected_rows <- selected_tlg_state()$selected
      if (length(selected_rows) > 0) {
        tlg_order_data <- tlg_order()
        tlg_order_data$Selection[tlg_order_data$Selection][selected_rows] <- FALSE
        tlg_order(tlg_order_data)
      }
    })

    # Toggle submit button depending on whether the data is available #
    observeEvent(data(), ignoreInit = FALSE, ignoreNULL = FALSE, {
      shinyjs::toggleState("submit_tlg_order", !is.null(data()$conc$data))
      shinyjs::toggleState("submit_tlg_order_alt", !is.null(data()$conc$data))
    })

    #' change tab to first populated tab
    #' for mysterious reasons nav_select() and updateTabsetPanel() were not working,
    #' so solved this using JavaScript
    observeEvent(list(input$submit_tlg_order), ignoreInit = TRUE, {
      tab_to_switch <- pull(tlg_order_filtered()[1, "Type"]) %>% paste0("s")
      shinyjs::runjs(
        paste0("
          // change the tab to graphs //
          $(`#", session$ns("tlg_tabs"), " a[data-value='", tab_to_switch, "']`)[0].click();

          // enable spinner, as it was disabled for initial empty UI render //
          setTimeout(function() {
            $('#tlg-load-hide').remove();
          }, 500);  
        ")
      )
    })

    # Submit the TLG order, filter selected TLGs
    tlg_order_filtered <- reactive({
      req(data())
      tlg_order_filt <- tlg_order()[tlg_order()$Selection, ]
      log_debug("Submitted TLGs:\n", paste0("* ", tlg_order_filt$Description, collapse = "\n"))

      tlg_order_filt
    }) %>%
      bindEvent(c(input$submit_tlg_order))

    # Raw TLG inputs.  Individual listings must display rows excluded from
    # summaries (PKSUM1F/PPSUMFL == "Y"), so they consume these unfiltered
    # sources -- see tlg_data_key().
    conc_data_all <- reactive({
      req(data())
      data()$conc$data
    })
    adpp_data_all <- reactive({
      validate(need(
        !is.null(adpp()),
        "ADPP data is not available. Run NCA first to view PK parameter outputs."
      ))
      adpp()
    })

    # Summary-filtered variants for tables and mean plots: rows flagged
    # PKSUM1F (ADNCA) / PPSUMFL (ADPP) == "Y" are removed from summary
    # statistics and mean plots, but NOT from individual listings.
    conc_data <- reactive(filter_tlg_excluded(conc_data_all()))
    adpp_data <- reactive(filter_tlg_excluded(adpp_data_all()))

    # (dataset, type) -> data reactive.  Listings resolve to the "*_all"
    # (unfiltered) source; tables and graphs resolve to the filtered source.
    tlg_data_sources <- list(
      ADNCA     = conc_data,
      ADNCA_all = conc_data_all,
      ADPP      = adpp_data,
      ADPP_all  = adpp_data_all
    )

    # Track which module IDs have already been registered for this session.
    # tlg_module_server() calls Shiny's moduleServer(), which registers reactive
    # observers (pagination buttons, entries-per-page, etc.) every time it is
    # called.  Because renderUI re-executes on re-submit, calling
    # tlg_module_server() with the same ID a second time would accumulate
    # duplicate observers that fire multiple times per user action.
    # output$tlg_output is safely deduplicated by Shiny (second assignment
    # destroys the first), but observers are not — only this environment prevents
    # the duplication.  The environment lives inside moduleServer(), so it is
    # fresh per Shiny session and does not leak across sessions.
    .registered_modules <- new.env(parent = emptyenv())

    # Shared helper: build navset_pill_list panels for one TLG type.
    # Factored out to eliminate the copy-paste across table / graph / listing
    # renderUI blocks.  `id_suffix` must be unique per type to produce
    # deterministic, stable module IDs.
    .build_tlg_panels <- function(g_ids, type, id_suffix) {
      lapply(g_ids, function(g_id) {
        g_def     <- .TLG_DEFINITIONS[[g_id]]
        module_id <- paste0(g_id, id_suffix)
        tlg_data  <- tlg_data_sources[[tlg_data_key(type, g_def$dataset)]]

        panel_ui <- if (exists(g_def$fun)) {
          # Only register the Shiny module once per session to avoid accumulating
          # duplicate pagination observers on re-submit.
          if (!exists(module_id, envir = .registered_modules, inherits = FALSE)) {
            tlg_module_server(module_id, tlg_data, type, get(g_def$fun), g_def$options)
            assign(module_id, TRUE, envir = .registered_modules)
          }
          tlg_module_ui(session$ns(module_id), type, g_def$options)
        } else {
          tags$div(paste(tools::toTitleCase(type), "not implemented yet"))
        }

        nav_panel(g_def$label, panel_ui)
      })
    }

    # Create and render Table interface and modules
    output$tables <- renderUI({
      req(tlg_order_filtered())
      ids    <- filter(tlg_order_filtered(), Type == "Table") %>% pull("id")
      panels <- .build_tlg_panels(ids, "table", "_tbl")
      panels$"widths" <- c(2, 10)
      do.call(navset_pill_list, panels)
    })

    # Create and render Graph interface and modules
    output$graphs <- renderUI({
      req(tlg_order_filtered())
      ids    <- filter(tlg_order_filtered(), Type == "Graph") %>% pull("id")
      panels <- .build_tlg_panels(ids, "graph", "_grp")
      panels$"widths" <- c(2, 10)
      do.call(navset_pill_list, panels)
    })

    output$listings <- renderUI({
      req(tlg_order_filtered())
      ids <- filter(tlg_order_filtered(), Type == "Listing") %>% pull("id")

      if (!requireNamespace("rlistings", quietly = TRUE)) {
        panels <- list(nav_panel(
          "Listings",
          tags$div(
            class = "alert alert-warning",
            "Package 'rlistings' is not installed. Install it to view listings:",
            tags$code("install.packages('rlistings')")
          )
        ))
      } else {
        panels <- .build_tlg_panels(ids, "listing", "_lst")
      }

      panels$"widths" <- c(2, 10)
      do.call(navset_pill_list, panels)
    })
  })
}
