#' Raise a user-facing TLG warning.
#'
#' Marks a warning as one worth showing the user in the app.  The Shiny layer surfaces
#' conditions of class `tlg_warning` as notifications and ignores everything else, so
#' incidental warnings from ggplot2/plotly never become UI noise.
#'
#' @param ... Passed to `paste0()` to build the message.
#' @returns Invisibly `NULL`; called for the warning condition it signals.
#' @noRd
.tlg_warn <- function(...) {
  warning(warningCondition(paste0(...), class = "tlg_warning"))
}

#' Split a data frame by grouping variables and apply a function to each subset
#'
#' Common pattern used by all TLG functions that return one output object per
#' analyte/visit/specimen combination.  When `list_vars` is empty (or none of
#' the variables are present in `data`), `fn` is called on the full data frame
#' and the result is returned as a single-element named list
#' `list(all = ...)`.  Otherwise the data is split by the interaction of
#' `list_vars` columns and `fn` is applied to each subset; the results are
#' named by the interaction key.
#'
#' @param data A data frame.
#' @param list_vars Character vector of column names to split by. Absent
#'   columns are silently skipped.
#' @param fn A function that takes a data frame and returns a single output
#'   object (plot, table, listing, ...).
#'
#' @return A named list of `fn` outputs.
#' @noRd
split_and_apply <- function(data, list_vars, fn) {
  present <- intersect(list_vars, names(data))

  if (length(present) == 0) {
    return(list(all = fn(data)))
  }

  # Rows where any split column is NA are excluded: they cannot be assigned to a
  # meaningful group and would otherwise appear as a spurious "NA / PLASMA" page.
  complete_rows <- rowSums(is.na(data[, present, drop = FALSE])) == 0
  if (!all(complete_rows)) {
    .tlg_warn(
      "split_and_apply: ", sum(!complete_rows), " row(s) with NA in split ",
      "variable(s) [", paste(present, collapse = ", "), "] were excluded."
    )
    data <- data[complete_rows, , drop = FALSE]
  }

  if (nrow(data) == 0) return(list(all = fn(data)))

  # Keys carry the variable name as well as the value ("PPCAT: A" rather than "A") because they
  # become the group header above each split output, where a bare value gives the reader no way
  # to tell which variable it came from.
  split_keys <- do.call(
    interaction,
    c(
      lapply(present, function(v) paste0(v, ": ", as.character(data[[v]]))),
      list(sep = " / ", drop = TRUE)
    )
  )

  results <- lapply(levels(split_keys), function(key) {
    fn(data[split_keys == key, , drop = FALSE])
  })
  setNames(results, levels(split_keys))
}

# The analyte column is fixed to "PARAM" when the PKNCA objects are built
# (see `R/PKNCA.R`), so a ratio whose reference group is keyed on `PARAM` is one
# analyte divided by another -- a metabolite/parent ratio.  `export_cdisc()`
# moves that analyte into `PPCAT` and reuses `PARAM` for the parameter name, so
# the bracketed keys in PPANMETH are pre-export column names and have to be
# translated before they can be read off an ADPP row.
.RATIO_ANALYTE_KEY <- "PARAM"
.RATIO_ADPP_COLUMN <- c(PARAM = "PPCAT", PCSPEC = "PPSPEC")

# Columns `filter_ratio_rows()` derives.  They are not in ADPP, so the sidebar's
# `.colnames` choices token cannot offer them -- `.ratiocols` exists to append
# them for the entries that default to splitting on one.
.RATIO_DERIVED_COLS <- c("RATIO", "RATIOREF")

#' Name of the ADPP column holding the values of a PPANMETH reference key
#' @noRd
.ratio_key_column <- function(key) {
  renamed <- .RATIO_ADPP_COLUMN[key]
  unname(ifelse(is.na(renamed), key, renamed))
}

#' Split the reference groups out of a PPANMETH string
#'
#' `calculate_ratios()` stamps each ratio row with
#' `"<PPTESTCD> TO <PPTESTCD_ref> [<key>: <value>, ...]"`.  Two things make this
#' worth parsing defensively rather than with a single tidy regex: the bracket is
#' omitted when the test and reference groups are identical, and
#' `.apply_metadata_ppanmeth()` can prepend a parameter's own analysis method with
#' `"; "`, so the ratio string is not always the whole field.
#'
#' @param ppanmeth Character vector of PPANMETH values.
#' @returns A list the same length as `ppanmeth`, each element a named character
#'   vector mapping reference key to reference value (empty when there is no
#'   parseable bracket).
#' @noRd
.parse_ratio_reference <- function(ppanmeth) {
  lapply(ppanmeth, function(x) {
    empty <- setNames(character(0), character(0))
    if (is.na(x)) return(empty)

    # Anchored at the end so a prepended analysis method is ignored.
    bracket <- regmatches(x, regexpr("\\[[^][]*\\]$", x))
    if (length(bracket) == 0) return(empty)

    frags <- strsplit(substr(bracket, 2, nchar(bracket) - 1), ", ", fixed = TRUE)[[1]]
    # ", " is both the separator between pairs and a character a reference value
    # may contain, so a value like "Drug A, Extended Release" splits into a
    # fragment with no "key: " prefix.  Such a fragment belongs to the pair before
    # it: re-joining is what keeps two references that share a prefix ("Drug A, ER"
    # and "Drug A, IR") from both collapsing to "Drug A" and being pooled into one
    # table under a truncated denominator.  A leading fragment has no pair to
    # rejoin to and is dropped.
    starts_pair <- grepl("^[^:]+: ", frags)
    if (!any(starts_pair)) return(empty)
    pairs <- vapply(
      split(frags[cumsum(starts_pair) > 0], cumsum(starts_pair)[cumsum(starts_pair) > 0]),
      paste, character(1), collapse = ", "
    )

    setNames(sub("^[^:]+: ", "", pairs), sub(": .*$", "", pairs))
  })
}

#' Split the test and reference parameters out of a PPANMETH string
#'
#' The `"<PPTESTCD> TO <PPTESTCD_ref>"` half of the field, which -- unlike the
#' bracket -- is always written.  It is the only thing identifying a ratio whose
#' test and reference groups are the same: there `calculate_ratios()` omits the
#' bracket entirely, because what is being divided is one parameter by another
#' rather than one group by another.
#'
#' @param ppanmeth Character vector of PPANMETH values.
#' @returns A character matrix with columns `test` and `ref`, `NA` on rows where
#'   the pair could not be read.
#' @noRd
.parse_ratio_parameters <- function(ppanmeth) {
  # Drop a trailing reference bracket and any analysis method
  # `.apply_metadata_ppanmeth()` prepended with "; ", leaving the pair alone.
  bare <- trimws(sub("\\s*\\[[^][]*\\]$", "", ppanmeth))
  bare <- sub("^.*; ", "", bare)

  # Parameter codes never contain whitespace, and anchoring both ends means the
  # pair has to be the whole field rather than a phrase inside one.  That rules
  # out sentence-shaped text ("interpolated from dose TO last conc"); it cannot
  # rule out a bare three-word field that happens to read "X TO Y".
  matched <- regmatches(bare, regexec("^(\\S+) TO (\\S+)$", bare))
  out <- vapply(matched, function(m) {
    if (length(m) == 3) m[2:3] else c(NA_character_, NA_character_)
  }, character(2))
  matrix(t(out), ncol = 2, dimnames = list(NULL, c("test", "ref")))
}

#' Classify each PPANMETH value as a ratio row, and which family it belongs to
#'
#' Every ratio carries `"<test> TO <ref>"`, with a reference bracket appended
#' unless the test and reference groups are the same.  A row counts as a ratio
#' when that pair is the whole field, or when a reference bracket accompanies it.
#' Both halves of that test earn their place: `PPANMETH` is a permitted ADPP
#' variable carrying free-text analysis method, so a plain `grepl(" TO ", ...)`
#' read any sentence mentioning "TO" as a ratio, while a bracket on its own
#' matched an annotation with no comparison in it at all.
#'
#' @param ppanmeth Character vector of PPANMETH values.
#' @returns A character vector the same length as `ppanmeth`: `"analyte"` for a
#'   metabolite/parent ratio, `"other"` for any other reference group, `NA` for a
#'   row that is not a ratio at all.
#' @noRd
.ratio_row_type <- function(ppanmeth) {
  ppanmeth <- as.character(ppanmeth)
  refs <- .parse_ratio_reference(ppanmeth)

  # A reference bracket identifies a ratio whose codes are not bare tokens -- a
  # chained ratio carries "RACMAX (mean)" -- but only when what remains once the
  # bracket is stripped is still a comparison.  The bracket alone is not enough:
  # an analysis method such as "Interpolated [source: nominal]" contains no
  # " TO " at all, and admitting it summarized a free-text annotation under a
  # ratio heading, labelled with the bracket's value.
  has_ref <- lengths(refs) > 0 &
    grepl(" TO ", sub("\\s*\\[[^][]*\\]$", "", ppanmeth))
  is_ratio <- !is.na(ppanmeth) &
    (has_ref | !is.na(.parse_ratio_parameters(ppanmeth)[, "test"]))

  is_analyte <- vapply(refs, function(r) .RATIO_ANALYTE_KEY %in% names(r), logical(1))
  ifelse(!is_ratio, NA_character_, ifelse(is_analyte, "analyte", "other"))
}

#' Select the ratio rows written by Parameter Selection > Ratios
#'
#' Ratio rows are identified by what `calculate_ratios()` writes into `PPANMETH`
#' (see `.ratio_row_type()`), never by a `PPTESTCD`/`PARAMCD` prefix: the package
#' default code is `RA<param>`, the app only emits `MR<param>` when the reference
#' happens to be the analyte column, the code is user-editable free text, and `MR`
#' additionally collides with the mean-residence-time parameters (`MRTLST`, ...).
#'
#' Two columns are added for display, so a ratio is readable without the user
#' having to decode PPANMETH:
#' * `RATIOREF` -- the reference (denominator) group value, e.g. the parent analyte.
#'   `NA` for a same-group ratio, which has no reference group.
#' * `RATIO` -- `"<numerator> / <denominator>"`, e.g. `"Metab-DrugA / DrugA"`, or
#'   the parameter pair (`"AUCLST / CMAX"`) for a same-group ratio.
#'
#' @param data A CDISC ADPP data frame (from `export_cdisc()$adpp`).
#' @param caller Character string naming the calling function, used in error messages.
#' @param ref_type `"analyte"` keeps ratios whose reference is another analyte
#'   (metabolite/parent); `"other"` keeps the rest (treatment, dose profile, route,
#'   specimen); `"any"` keeps all ratio rows.
#'
#' @returns The ratio rows of `data`, with `RATIOREF` and `RATIO` added.
#' @noRd
filter_ratio_rows <- function(data, caller, ref_type = c("analyte", "other", "any")) {
  ref_type <- match.arg(ref_type)

  # PPANMETH is a permitted ADPP variable, so export_cdisc() drops it outright when
  # every value is missing -- which is exactly the "no ratios were set up" case.
  setup_hint <- paste0(
    "Set them up in Parameter Selection > Ratios and re-run the NCA: ratios are ",
    "computed as part of the NCA run, so adding one afterwards has no effect until ",
    "the run is repeated."
  )
  row_type <- if ("PPANMETH" %in% names(data)) .ratio_row_type(data$PPANMETH) else character(0)
  if (!any(!is.na(row_type))) {
    stop(caller, ": no ratio parameters found in the data. ", setup_hint)
  }

  wanted_type <- switch(ref_type, analyte = "analyte", other = "other", any = c("analyte", "other"))
  keep <- !is.na(row_type) & row_type %in% wanted_type
  if (!any(keep)) {
    wanted <- if (ref_type == "analyte") {
      "metabolite/parent ratios (reference group on the analyte)"
    } else {
      "treatment ratios (reference group on treatment, dose profile, route or specimen)"
    }
    # The complement of the analyte family is not necessarily treatment ratios --
    # it also holds route, specimen and same-group ones -- so only name the family
    # in the direction where it is exact.
    found <- if (ref_type == "analyte") {
      "only ratios referenced against something other than the analyte were found"
    } else {
      "only metabolite/parent ratios were found"
    }
    stop(
      caller, ": the data contains ratio parameters, but none are ", wanted, " -- ",
      found, ". ", setup_hint
    )
  }
  ratios <- data[keep, , drop = FALSE]
  refs <- .parse_ratio_reference(ratios$PPANMETH)

  # `RATIO`/`RATIOREF` are derived below and would overwrite same-named input
  # columns.  Neither is an ADPP variable, so this only happens on hand-built
  # data, but silently replacing a column the caller supplied is worse than saying so.
  clobbered <- intersect(.RATIO_DERIVED_COLS, names(ratios))
  if (length(clobbered) > 0) {
    .tlg_warn(
      caller, ": the data already has a column named ",
      paste(clobbered, collapse = " and "),
      "; it is replaced by the derived ratio label."
    )
  }

  # A ratio the NCA run could not compute (no comparable reference rows, or units
  # that would not convert) still reaches ADPP, with a missing value.  A summary
  # table renders that as a statistics row of all-NA and a listing as blank cells,
  # neither of which says why -- so name the parameters involved.  The rows are
  # kept rather than dropped: in a listing the blank cell is the honest answer for
  # that subject, and silently removing rows would understate the ratios requested.
  if (all(c("AVAL", "PARAM") %in% names(ratios))) {
    all_na <- vapply(
      split(ratios$AVAL, ratios$PARAM), function(v) all(is.na(v)), logical(1)
    )
    if (any(all_na)) {
      .tlg_warn(
        caller, ": no value was computed for ", sum(all_na), " of ",
        length(all_na), " ratio parameter(s) -- ",
        paste(names(all_na)[all_na], collapse = ", "),
        " -- so they appear empty. The NCA run found no comparable reference ",
        "rows for them, or their units could not be converted."
      )
    }
  }

  .add_ratio_labels(ratios, refs, ref_type)
}

#' Add the `RATIO` and `RATIOREF` display columns to selected ratio rows
#'
#' @param ratios Ratio rows, already narrowed to one `ref_type`.
#' @param refs Parsed reference groups for those rows, from `.parse_ratio_reference()`.
#' @param ref_type As in `filter_ratio_rows()`.
#' @returns `ratios` with the two labelled columns added. `RATIO` is never `NA`.
#' @noRd
.add_ratio_labels <- function(ratios, refs, ref_type) {
  # Every reference key is shown, including on an analyte ratio.  Keeping only the
  # analyte key there read "Metab-DrugA / DrugA" for a metabolite in urine
  # referenced against the parent in serum, hiding that the denominator is a
  # different matrix -- and `list_vars` carries the numerator's specimen, so the
  # two directions of that comparison would have shared a table.
  keys <- lapply(refs, names)

  # Denominator: the reference values parsed out of PPANMETH.  Numerator: the same
  # keys read off the row itself, which is where the test group's values live.
  ratios$RATIOREF <- .collapse_values(
    lapply(seq_along(refs), function(i) unname(refs[[i]][keys[[i]]]))
  )
  numerator <- .collapse_values(
    lapply(seq_along(refs), function(i) {
      cols <- .ratio_key_column(keys[[i]])
      # All or nothing: reading only the keys that happen to have an ADPP column
      # produced "50mg / 10mg, iv", where the two sides of the "/" describe
      # different things.  Showing the reference alone is the honest fallback.
      if (length(cols) == 0 || !all(cols %in% names(ratios))) return(character(0))
      vapply(cols, function(cl) as.character(ratios[[cl]][i]), character(1))
    })
  )

  # An unparseable bracket leaves nothing to divide by; showing whichever side is
  # known is still more use than an "NA / NA" header.
  ratios$RATIO <- ifelse(
    is.na(numerator) | is.na(ratios$RATIOREF),
    ifelse(is.na(numerator), ratios$RATIOREF, numerator),
    paste0(numerator, " / ", ratios$RATIOREF)
  )
  ratios$RATIO <- .fill_missing_ratio_labels(ratios$RATIO, ratios$PPANMETH)

  # Both columns are derived here rather than coming from ADPP, so they have no
  # entry in `metadata_nca_variables` and `apply_labels()` would fall back to the
  # bare column name -- which is what the `!VAR` annotation syntax renders.
  attr(ratios$RATIO, "label") <- if (ref_type == "analyte") {
    "Metabolite / Parent"
  } else {
    "Test / Reference"
  }
  attr(ratios$RATIOREF, "label") <- if (ref_type == "analyte") {
    "Parent (reference analyte)"
  } else {
    "Reference group"
  }
  ratios
}

#' Fill in `RATIO` labels the reference groups could not supply
#'
#' A same-group ratio carries no bracket at all, so neither side of the label is
#' known -- but it is still a real ratio of one parameter by another.  Left as `NA`
#' the row was dropped by `split_and_apply()` and the output rendered empty, so the
#' gap is filled from the parameter pair instead.
#'
#' Every gap can be filled: `.ratio_row_type()` admits a row only when one of the
#' two parsers reads it, so a row whose reference groups were unreadable is one
#' whose parameter pair was not.
#'
#' @param ratio Character vector of labels built from the reference groups.
#' @param ppanmeth The `PPANMETH` values of the same rows.
#' @returns `ratio` with no missing values.
#' @noRd
.fill_missing_ratio_labels <- function(ratio, ppanmeth) {
  if (!anyNA(ratio)) return(ratio)

  params <- .parse_ratio_parameters(ppanmeth)
  pair <- paste0(params[, "test"], " / ", params[, "ref"])
  ifelse(is.na(ratio), pair, ratio)
}

#' First non-missing value, or `NA` when there is none
#'
#' Used where several rows collapse to one cell and the choice between them is
#' arbitrary: picking a missing value over an available one is never the useful
#' arbitrary choice.
#'
#' @param x A vector.
#' @returns A length-one vector of the same type as `x`.
#' @noRd
.first_present <- function(x) {
  present <- x[!is.na(x)]
  if (length(present) == 0) x[NA_integer_][1] else present[1]
}

#' Collapse each element of a list of values into one label, `NA` when empty
#' @noRd
.collapse_values <- function(values) {
  vapply(values, function(v) {
    v <- v[!is.na(v)]
    if (length(v) == 0) NA_character_ else paste(v, collapse = ", ")
  }, character(1))
}

#' Compute descriptive statistics for a numeric vector of PK values.
#'
#' Returns a one-row data frame of n, Mean, SD, CV%, GeoMean, GeoCV%, Median,
#' Min, Max.  When `include_geo = FALSE`, the GeoMean and GeoCV_pct columns
#' are omitted (used for urine parameters that are not log-normally distributed
#' by convention).
#'
#' Used by [t_pkpt03_col()], [t_pkpt08_uri()], and [t_pkpt11_gmr()] via
#' [.build_pkpp_table()].  Placed here so future table functions can reuse it
#' without duplicating the stat logic.
#'
#' @param vals Numeric vector (NAs already handled by caller or this function).
#' @param include_geo Logical. Include GeoMean and GeoCV_pct columns.
#'   Default: `TRUE`.
#' @noRd
.summarise_adpp <- function(vals, include_geo = TRUE) { # nolint: cyclocomp_linter
  vals <- vals[!is.na(vals)]
  pos  <- vals[vals > 0]
  n    <- length(vals)
  mn   <- if (n > 0) mean(vals) else NA_real_
  s    <- if (n > 1) sd(vals)   else NA_real_
  out  <- data.frame(
    n      = n,
    Mean   = round(mn, 3),
    SD     = round(s,  3),
    CV_pct = if (!is.na(mn) && mn != 0 && !is.na(s))
      round(s / mn * 100, 1) else NA_real_,
    stringsAsFactors = FALSE
  )
  if (include_geo) {
    gm  <- if (length(pos) > 0) exp(mean(log(pos))) else NA_real_
    gs  <- if (length(pos) > 1) sd(log(pos))        else NA_real_
    out <- cbind(out, data.frame(
      GeoMean   = round(gm, 3),
      GeoCV_pct = if (!is.na(gs)) round(sqrt(exp(gs^2) - 1) * 100, 1) else NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  cbind(out, data.frame(
    Median = if (n > 0) round(median(vals), 3) else NA_real_,
    Min    = if (n > 0) round(min(vals),    3) else NA_real_,
    Max    = if (n > 0) round(max(vals),    3) else NA_real_,
    stringsAsFactors = FALSE
  ))
}

#' Human-readable display labels for the descriptive-statistic columns shared by
#' the summary tables (`t_pkct01`, and everything built via [.build_pkpp_table()]).
#'
#' The data frames keep their terse programmatic column names (`GeoMean`,
#' `CV_pct`, ...) so downstream code and tests can reference them; these labels
#' are attached as the `label` attribute and promoted to the rendered column
#' header by `define_cols(header_from_label = TRUE)`.
#' @noRd
.STAT_LABELS <- c(
  n         = "n",
  n_blq     = "Number BLQ",
  Mean      = "Mean",
  SD        = "SD",
  CV_pct    = "CV%",
  Median    = "Median",
  GeoMean   = "Geometric Mean",
  GeoCV_pct = "Geometric CV%",
  Min       = "Min",
  Max       = "Max"
)

#' Attach readable labels to the statistic columns of a summary table.
#'
#' Only statistic columns are touched; grouping/key columns (already labelled via
#' [apply_labels()]) are left as-is.  Group-comparison tables prefix each
#' statistic column with `"<level><.GROUP_SEP>"` (see [.pivot_group_blocks()]);
#' the prefix is stripped before the [.STAT_LABELS] lookup so a prefixed leaf
#' such as `"Male<SEP>Mean"` still gets the "Mean" label.  Applied AFTER the
#' `rbind` in the builders because `rbind` drops per-column attributes.
#'
#' @param df A summary-table data frame.
#' @return `df` with `label` attributes set on its statistic columns.
#' @noRd
.apply_stat_labels <- function(df) {
  for (col in names(df)) {
    base <- sub(paste0("^.*", .GROUP_SEP), "", col)
    if (base %in% names(.STAT_LABELS)) {
      attr(df[[col]], "label") <- unname(.STAT_LABELS[[base]])
    }
  }
  df
}

#' Keep only the user-selected statistic columns of a summary table.
#'
#' Summary builders always compute the full statistic block; this trims the
#' result to the statistics the user asked for, leaving every key / grouping
#' column (and, for group-comparison tables, the column order) intact.  A column
#' is a statistic column when its base name -- the part after any
#' `"<level><.GROUP_SEP>"` prefix, matching [.apply_stat_labels()] -- is one of
#' the names in [.STAT_LABELS].  Requested statistics that a given table never
#' produces (e.g. `n_blq` on ADPP tables, `GeoMean` on `t_pkpt08_uri`) are
#' silently ignored.
#'
#' When a `col_groups` attribute is present (group-comparison tables) it is
#' rebuilt against the surviving columns so the rendered two-level header never
#' references a dropped leaf; groups left with no columns are removed.
#'
#' @param df A labelled summary-table data frame.
#' @param stats Character vector of terse statistic names to keep (e.g.
#'   `c("n", "Mean", "SD")`). `NULL` or empty keeps every column unchanged.
#' @return `df` restricted to key columns plus the selected statistic columns.
#' @noRd
.select_stats <- function(df, stats = NULL) {
  if (is.null(stats) || length(stats) == 0 || ncol(df) == 0) return(df)
  base_name  <- function(col) sub(paste0("^.*", .GROUP_SEP), "", col)
  is_stat    <- vapply(names(df), function(c) base_name(c) %in% names(.STAT_LABELS), logical(1))
  keep_stat  <- vapply(names(df), function(c) base_name(c) %in% stats, logical(1))
  keep       <- !is_stat | keep_stat
  cg <- attr(df, "col_groups")
  out <- df[, keep, drop = FALSE]
  if (!is.null(cg)) {
    kept_cols <- names(df)[keep]
    cg <- lapply(cg, function(leaves) intersect(leaves, kept_cols))
    cg <- cg[vapply(cg, length, integer(1)) > 0]
    attr(out, "col_groups") <- cg
  }
  out
}

#' Sentinel separator used to prefix per-group statistic column names.
#'
#' A control character (unit separator) is used so it can never collide with
#' real column-label text (arm names, RACE strings, ...).  The prefixed names
#' are only a uniqueness device -- the group->columns mapping is carried
#' explicitly via the `col_groups` attribute (see [.make_col_groups()]), so
#' render code never has to parse these names back apart.
#' @noRd
.GROUP_SEP <- ""

#' Ordered levels of a column-group variable.
#'
#' Natural (numeric-aware) sort of the non-missing values, with `NA` coerced to
#' a literal `"NA"` level appended last -- mirroring how [.natural_sort_key()]
#' and the row-key handling keep missing values visible rather than dropping
#' them.
#'
#' @param x A vector.
#' @return A character vector of ordered levels. Blank (`""`/whitespace) and `NA`
#'   values collapse into a single `"NA"` level appended last; a literal `"NA"`
#'   string value is merged into that same level (never duplicated).
#' @noRd
.group_levels <- function(x) {
  gv <- as.character(x)
  # Blank / whitespace-only values are treated as missing so they never become
  # an empty-string ("") level -- reactable cannot group an "" column, and
  # `list(...)[[""]]` returns NULL, which crashes the render.
  gv[is.na(gv) | trimws(gv) == ""] <- NA
  has_na <- anyNA(gv)
  lv     <- unique(gv[!is.na(gv)])
  lv     <- lv[order(.natural_sort_key(lv))]
  if (has_na) lv <- c(setdiff(lv, "NA"), "NA")  # exactly one "NA" level, last
  lv
}

#' Validate a column-group (comparison) variable and return its ordered levels.
#'
#' Errors when the variable is absent, or when it collides with a variable that
#' already defines rows or table splits (which would produce degenerate,
#' duplicated groups).  Warns when the resulting table would be very wide.
#'
#' @param col_group_var Character scalar column name (the comparison variable).
#' @param data The data frame the table is built from.
#' @param reserved Character vector of variables that must NOT be reused as the
#'   comparison variable (stratification / parameter / split columns).
#' @return A character vector of ordered group levels (from [.group_levels()]).
#' @noRd
.resolve_col_group <- function(col_group_var, data, reserved) {
  if (!col_group_var %in% names(data)) {
    stop("Cannot compare in columns by '", col_group_var,
         "': it is not a column in the data.")
  }
  if (col_group_var %in% reserved) {
    stop(
      "Cannot compare in columns by '", col_group_var,
      "': it is already used to define the table rows or splits (",
      paste(reserved, collapse = ", "),
      "). Pick a different variable, or change the row/split variables."
    )
  }
  levels <- .group_levels(data[[col_group_var]])
  if (length(levels) > 6L) {
    .tlg_warn(
      "col_group_var '", col_group_var, "' has ", length(levels),
      " levels; the comparison table will be very wide and may overflow ",
      "horizontally."
    )
  }
  levels
}

#' Pivot a per-cell statistic block into side-by-side per-group blocks.
#'
#' For each level in `group_levels`, `block_fn` is applied to the matching subset
#' of `cell_df` and the resulting one-row statistic block is renamed with a
#' `"<level><.GROUP_SEP><stat>"` prefix; all blocks are then `cbind`'d together.
#' Empty subsets still yield a rectangular `n = 0 / NA` block (both
#' [.summarise_adpp()] and `.summarise_group()` handle empty input), so the
#' result is the same width regardless of which levels are populated in this cell.
#'
#' Readable leaf labels are (re-)applied by [.apply_stat_labels()] on the final
#' wide table AFTER the builder's `rbind` (which drops column attributes), so this
#' helper only computes and prefixes the blocks.
#'
#' @param cell_df Rows for a single (stratum x parameter) / row-key cell.
#' @param group_var Column whose levels become side-by-side groups.
#' @param group_levels Ordered levels defining the column order
#'   (from [.resolve_col_group()]).
#' @param block_fn Function taking a data-frame subset and returning a one-row
#'   statistic data frame (e.g. `.summarise_group`, or a `summary_fn` wrapper).
#' @return A one-row data frame of all per-group statistic blocks, side by side.
#' @noRd
.pivot_group_blocks <- function(cell_df, group_var, group_levels, block_fn) {
  gvals <- as.character(cell_df[[group_var]])
  # Match the level coercion in .group_levels(): blank/whitespace and NA both
  # map to the single "NA" level so every row lands in exactly one group.
  gvals[is.na(gvals) | trimws(gvals) == ""] <- "NA"
  blocks <- lapply(group_levels, function(lvl) {
    block <- block_fn(cell_df[gvals == lvl, , drop = FALSE])
    names(block) <- paste0(lvl, .GROUP_SEP, names(block))
    block
  })
  do.call(cbind, blocks)
}

#' Build the `col_groups` attribute map: group level -> prefixed leaf names.
#'
#' Consumed at render time by `define_col_groups()` to emit
#' `reactable::colGroup()` spanners.  Storing the exact leaf names (rather than
#' re-deriving them by parsing) means a group value containing [.GROUP_SEP] can
#' never corrupt the grouping.
#'
#' @param group_levels Ordered group levels.
#' @param stat_names Terse statistic column names of a single block
#'   (e.g. `c("n", "Mean", ...)`).
#' @return A named list (one entry per level) of prefixed leaf-name vectors.
#' @noRd
.make_col_groups <- function(group_levels, stat_names) {
  setNames(
    lapply(group_levels, function(lvl) paste0(lvl, .GROUP_SEP, stat_names)),
    group_levels
  )
}

#' Build an `order()` key that sorts embedded numbers numerically.
#'
#' `order()` on a character vector is lexical, so "DOSE 10" would sort before
#' "DOSE 2" and arms like "100 mg" before "50 mg".  This returns a key whose
#' lexical order matches natural order: numeric columns are returned unchanged
#' (already numeric-sortable), factors are returned as their level codes (so an
#' upstream-defined order is respected), and for character values each run of
#' digits is zero-padded to a fixed width.  `NA` keys sort last, as with the
#' default `order()`.
#'
#' @param x A vector (numeric, factor, or character).
#' @return A vector suitable as an argument to [order()].
#' @noRd
.natural_sort_key <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.factor(x))  return(as.integer(x))
  vapply(as.character(x), function(s) {
    if (is.na(s)) return(NA_character_)
    parts  <- regmatches(s, gregexpr("[0-9]+|[^0-9]+", s))[[1]]
    is_num <- grepl("^[0-9]+$", parts)
    parts[is_num] <- formatC(parts[is_num], width = 12, flag = "0")
    paste(parts, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' Return the label attribute of a column, falling back to the column name.
#'
#' Used by TLG plot functions to label axes.  When a column has a
#' `formatters`-style `label` attribute it is used; otherwise the column name
#' string is returned unchanged.
#'
#' @param data A data frame.
#' @param var  Character scalar column name.
#' @return A character scalar label.
#' @noRd
.get_var_label <- function(data, var) {
  lbl <- attr(data[[var]], "label")
  if (!is.null(lbl)) lbl else var
}
