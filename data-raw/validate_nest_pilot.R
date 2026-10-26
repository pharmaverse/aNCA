# Run from the repository root: Rscript data-raw/validate_nest_pilot.R /tmp/nest-evidence
# Uses the checked-in example; no external study data or cached analysis is required.
devtools::load_all(".")
# Headless plotting would otherwise leave Rplots.pdf in the checkout.
grDevices::pdf(NULL)
args <- commandArgs(trailingOnly = TRUE)
dest <- if (length(args)) args[1] else file.path(tempdir(), "nest-evidence")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
raw <- read.csv("data-raw/adnca_example.csv", na.strings = c("", "NA"))
raw <- apply_labels(raw, type = "ADNCA")
nca <- PKNCA_create_data_object(raw)
types <- unique(aNCA:::.derive_study_types(nca)$type)
selections <- setNames(lapply(types, function(type) {
  if (type == "Excretion Data") "ae" else c("cmax", "tmax", "auclast", "half.life")
}), types)
nca <- PKNCA_update_data_object(
  nca, method = "lin up/log down", selected_analytes = unique(raw$PARAM),
  selected_profile = unique(raw$ATPTREF), selected_pcspec = unique(raw$PCSPEC),
  keep_interval_cols = c("TRT01A", "RACE", "SEX"), parameter_selections = selections
)
results <- PKNCA_calculate_nca(nca)
state <- export_cdisc(results, grouping_vars = c("TRT01A", "RACE", "SEX"))
params <- c("Max Conc", "AUC to Last Nonzero Conc")
adpp <- as.data.frame(state$adpp[state$adpp$PARAM %in% params, ])
splits <- c("PPCAT", "PPSPEC", "ATPTREF")
summary <- t_pkpt03_nest(adpp, title = "NEST pilot: $PPCAT", stats = c("n", "Mean", "SD"))
listing <- l_pkpl01_nest(adpp, title = "Individual PK parameters: $PPCAT")
legacy <- t_pkpt03_col(adpp, list_vars = splits, strat_var = c("TRT01A", "PARAM", "AVALU"),
                       stats = c("n", "Mean", "SD"))
stopifnot(isTRUE(all.equal(lapply(summary, attr, "tlg_data"), legacy)))
old_listing <- l_pkpl01(adpp, listgroup_vars = splits)
for (i in seq_along(listing)) {
  frame <- attr(listing[[i]], "tlg_data")
  old <- as.data.frame(old_listing[[i]])
  key <- function(x) paste(x$TRT01A, x$USUBJID, sep = "\r")
  stopifnot(setequal(key(frame), key(old)))
  old <- old[match(key(frame), key(old)), ]
  for (param in intersect(params, names(frame))) {
    stopifnot(isTRUE(all.equal(as.numeric(frame[[param]]), as.numeric(old[[param]]))))
  }
}
# Independent base-R reference: split on source keys, retain one equal value per
# subject, omit flagged/missing values, then compute sample SD and arithmetic mean.
source_splits <- split(adpp, interaction(adpp[splits], drop = TRUE, lex.order = TRUE))
reference <- list()
for (source in source_splits) {
  source <- source[is.na(source$PPSUMXF) | source$PPSUMXF != "Y", ]
  groups <- split(source, interaction(source[c("TRT01A", "PARAM", "AVALU")], drop = TRUE))
  for (group in groups) {
    group <- group[!duplicated(group$USUBJID), ]
    x <- group$AVAL[!is.na(group$AVAL)]
    if (!length(x)) next
    reference[[length(reference) + 1L]] <- data.frame(
      group[1, c(splits, "TRT01A", "PARAM", "AVALU")],
      n = length(x), Mean = round(mean(x), 3), SD = round(stats::sd(x), 3)
    )
  }
}
reference <- do.call(rbind, reference)
# All nonempty summary cells must have a matching independent numeric result.
for (i in seq_len(nrow(reference))) {
  ref <- reference[i, ]
  # Match by original split key directly (avoids ambiguities in annotated text).
  key <- paste(paste(splits, ref[1, splits], sep = ": "), collapse = " / ")
  table <- summary[[key]]
  stopifnot(!is.null(table))
  frame <- attr(table, "tlg_data")
  row <- frame[frame$TRT01A == ref$TRT01A & frame$PARAM == ref$PARAM &
                 !is.na(frame$AVALU) & frame$AVALU == ref$AVALU, ]
  stopifnot(nrow(row) == 1L, row$n == ref$n, row$Mean == ref$Mean,
            isTRUE(all.equal(as.numeric(row$SD), ref$SD)))
}
write.csv(reference, file.path(dest, "independent-summary-values.csv"), row.names = FALSE)
# Compare every retained graph cell with direct mean/SD from eligible source rows.
graphs <- g_pkcg03_lin_nest(state$adnca, plotly = FALSE)
graph_source <- split(
  as.data.frame(state$adnca),
  interaction(state$adnca[c("ROUTE", "PCSPEC", "PARAM")], drop = TRUE, lex.order = TRUE)
)
graph_checks <- 0L
for (source in graph_source) {
  key <- paste(paste(c("ROUTE", "PCSPEC", "PARAM"),
                     source[1, c("ROUTE", "PCSPEC", "PARAM")], sep = ": "), collapse = " / ")
  graph <- graphs[[key]]
  if (!inherits(graph, "ggplot")) next
  old <- g_pkcg03_lin(source, plotly = FALSE)[[1]]
  stopifnot(identical(names(graph$data), names(old$data)),
            isTRUE(all.equal(graph$data, old$data, check.attributes = FALSE)))
  source <- source[is.na(source$PKSUMXF) | source$PKSUMXF != "Y", ]
  for (i in seq_len(nrow(graph$data))) {
    row <- graph$data[i, ]
    x <- source$AVAL[source$DOSEA == row$DOSEA & source$NFRLT == row$NFRLT]
    stopifnot(isTRUE(all.equal(row$mean, mean(x, na.rm = TRUE))),
              isTRUE(all.equal(row$sd, stats::sd(x, na.rm = TRUE))))
    graph_checks <- graph_checks + 1L
  }
}
# Export a compact first-dose specimen/analyte report, plus a legacy sibling.
selected <- grepl("PPCAT: DrugA / PPSPEC: SERUM / ATPTREF: DOSE 1$", names(summary))
stopifnot(sum(selected) == 1L)
entry <- function(type, items) {
  list(type = type, items = items, def = list(label = "NEST pilot", type = type, dataset = "ADPP"))
}
entries <- list(t_pkpt03_nest = entry("table", summary[selected]),
                l_pkpl01_nest = entry("listing", listing[names(summary)[selected]]),
                g_pkcg03_lin_nest = entry("graph", graphs[1]),
                t_pkpt03_col = entry("table", legacy[selected]))
source("inst/shiny/functions/zip-utils.R")
source("inst/shiny/functions/tlg_export.R")
manifest <- write_tlg_exports(entries, file.path(dest, "exports"),
                              ggplot_formats = c("png", "pdf", "html"),
                              table_formats = c("csv", "xlsx", "pdf", "html", "docx"))
stopifnot(!any(manifest$status == "error"))
# Exercise pagination with every calculated parameter and all default statistics.
full_data <- state$adpp[state$adpp$PPCAT == "DrugA" & state$adpp$PPSPEC == "SERUM" &
                          state$adpp$ATPTREF == "DOSE 1", ]
full_entries <- list(t_pkpt03_nest = entry("table", t_pkpt03_nest(full_data)),
                     l_pkpl01_nest = entry("listing", l_pkpl01_nest(full_data)))
full_manifest <- write_tlg_exports(full_entries, file.path(dest, "full-report"),
                                   table_formats = c("pdf", "html", "docx"))
stopifnot(all(full_manifest$status == "ok"), !any(grepl("exceeds", full_manifest$note)))
zip::zipr(file.path(dest, "nest-pilot.zip"),
          list.files(file.path(dest, "exports"), full.names = TRUE),
          root = file.path(dest, "exports"))
saveRDS(list(state = state, summary = summary, listing = listing, graphs = graphs),
        file.path(dest, "validation-state.rds"))
cat("Validated", nrow(raw), "source records,", nrow(state$adpp), "ADPP records,",
    nrow(reference), "independent summary rows and", graph_checks, "graph cells.\n")
print(reference[1, ])
print(manifest[c("id", "status", "note")])
invisible(grDevices::dev.off())
