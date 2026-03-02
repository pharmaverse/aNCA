#' Generate a session script code in R that can replicate the App outputs
#'
#' @param template_path Path to the R script template (e.g., script_template.R)
#' @param setts_obj The read settings object with all analysis specifications
#' or the full session object from the App.
#' @param output_path Path to write the resulting script file (e.g., "output_script.R")
#' @return The output_path (invisibly)
#' @keywords internal
#' @noRd
get_code <- function(
  setts_obj,
  output_path,
  template_path = system.file("www/templates/script_template.R", package = "aNCA")
) {

  # Helper to get value from settings_list by path (e.g., 'settings$method')
  get_session_value <- function(path) {
    parts <- strsplit(path, "\\$")[[1]]
    obj <- setts_obj
    for (p in parts) {
      if (inherits(obj[[p]], "reactive")) {
        obj <- obj[[p]]()
      } else {
        obj <- obj[[p]]
      }
      if (is.null(obj)) {
        return(NULL)
      }
    }
    obj
  }
  # Read template
  script <- readLines(template_path, warn = FALSE) %>%
    paste(collapse = "\n")

  # Find all settings_list$...
  pattern <- "settings_list(\\$[a-zA-Z0-9_]+(\\(\\))?(\\$[a-zA-Z0-9_]+)*)"
  matches <- gregexpr(pattern, script, perl = TRUE)[[1]]
  if (matches[1] == -1) {
    stop(
      "Template has no placeholders (settings_list...) to substitute.",
      "This may be due to an incorrect file path, a missing template, ",
      "or a modified template without placeholders."
    )
  }

  # Replace each match with deparsed value
  for (i in rev(seq_along(matches))) {
    start <- matches[i]
    len <- attr(matches, "match.length")[i]
    matched <- substr(script, start, start + len - 1)
    # Extract the path after settings_list$
    path <- sub("^settings_list\\$", "", matched)
    value <- get_session_value(path)

    deparsed <- clean_deparse(value, max_per_line = 15)
    script <- paste0(
      substr(script, 1, start - 1),
      deparsed,
      substr(script, start + len, nchar(script))
    )
  }

  # Split back into lines
  script_lines <- strsplit(script, "\n")[[1]]
  writeLines(script_lines, output_path)
  invisible(output_path)
}

#' Convert R objects into reproducible R code strings (internal)
#'
#' This internal S3 generic converts common R objects (data frames, lists,
#' atomic vectors, etc.) into character strings containing R code that will
#' reconstruct the object. It is used by the app script generator to
#' serialize `settings_list` values into a runnable R script.
#'
#' @param obj An R object to convert to a string of R code.
#' @param max_per_line Maximum number of elements to include per line for
#'   long vectors/lists.
#' @param min_to_rep Minimum number of repeated elements to use `rep()` for
#'   long vectors/lists.
#' @param indent Integer indentation level for multi-line outputs.
#' @return A single string containing R code that, when evaluated, will
#'   reconstruct `obj` (or a close approximation for complex types).
#' @keywords internal
clean_deparse <- function(obj, indent = 0, max_per_line = 10, min_to_rep = 3) {
  # Handle tbl_df objects as data.frame
  if (inherits(obj, "tbl_df")) obj <- as.data.frame(obj)

  # Handle trivial length-0 constructors (character(0), numeric(0), list(), data.frame(), ...)
  if (length(obj) == 0 && !is.null(obj)) {
    return(paste0(class(obj)[1], "()"))
  }
  UseMethod("clean_deparse", obj)
}

#' @noRd
clean_deparse.default <- function(obj, indent = 0, max_per_line = 10, min_to_rep = 3) {
  paste(deparse(obj, width.cutoff = 500), collapse = "")
}

#' @noRd
clean_deparse.data.frame <- function(obj, indent = 0, max_per_line = 10, min_to_rep = 3) {
  ind <- paste(rep("  ", indent), collapse = "")

  cols <- lapply(obj, function(col) {
    clean_deparse(col, indent + 1, max_per_line = max_per_line)
  })

  col_strs <- paste0(ind, "  ", names(obj), " = ", unlist(cols))
  if (length(col_strs) > 1) {
    not_last <- seq_len(length(col_strs) - 1)
    col_strs[not_last] <- paste0(col_strs[not_last], ",")
  }
  paste0("data.frame(\n", paste(col_strs, collapse = "\n"), "\n", ind, ")")
}

#' @noRd
clean_deparse.list <- function(obj, indent = 0, max_per_line = 10, min_to_rep = 3) {
  ind <- paste(rep("  ", indent), collapse = "")
  n <- length(obj)
  nms <- names(obj)
  items <- vapply(seq_len(n), FUN.VALUE = "", function(i) {
    name <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else paste0("V", i)
    # Quote name if not a valid R symbol
    if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", name)) {
      name <- sprintf('"%s"', name)
    }
    val <- obj[[i]]
    val_str <- clean_deparse(val, indent + 1, max_per_line = max_per_line)
    paste0(name, " = ", val_str)
  })
  if (length(items) > 1) {
    not_last <- seq_len(length(items) - 1)
    items[not_last] <- paste0(items[not_last], ",")
  }
  item_strs <- paste0(ind, "  ", items)
  paste0("list(\n", paste(item_strs, collapse = "\n"), "\n", ind, ")")
}

#' @noRd

clean_deparse.character <- function(obj, indent = 0, max_per_line = 10, min_to_rep = 3) {
  obj <- sprintf('"%s"', obj)
  .deparse_vector(obj, indent, max_per_line, min_to_rep)
}

#' @noRd

clean_deparse.numeric <- function(obj, indent = 0, max_per_line = 10, min_to_rep = 3) {
  obj <- sprintf("%s", obj)
  .deparse_vector(obj, indent, max_per_line, min_to_rep)
}

#' @noRd
clean_deparse.integer <- function(obj, indent = 0, max_per_line = 10, min_to_rep = 3) {
  clean_deparse.numeric(obj, indent = indent, max_per_line = max_per_line, min_to_rep = min_to_rep)
}

#' @noRd
clean_deparse.logical <- function(obj, indent = 0, max_per_line = 10, min_to_rep = 3) {
  obj <- as.character(obj)
  .deparse_vector(obj, indent, max_per_line, min_to_rep)
}

#' Internal helper to deparse atomic vectors
#' using repetition simplification (rep) and line splitting
#'
#' @noRd
.deparse_vector <- function(obj, indent = 0, max_per_line = 10, min_to_rep = 3) {
  n <- length(obj)
  if (n == 1) {
    return(obj)
  } else {
    rle_obj <- rle(obj)
    lines_obj <- c()
    for (i in seq_along(rle_obj$values)) {
      val <- rle_obj$values[i]
      len <- rle_obj$lengths[i]
      if (len >= min_to_rep) {
        rep_obj <- paste0("rep(", val, ", ", len, ")")
        lines_obj <- c(lines_obj, rep_obj)
      } else {
        lines_obj <- c(lines_obj, rep(val, len))
      }
    }
  }
  ind <- paste(rep("  ", indent), collapse = "")
  lines <- split(lines_obj, ceiling(seq_along(lines_obj) / max_per_line))
  line_strs <- vapply(lines, function(x) paste(x, collapse = ", "), "")
  if (is.list(lines) && length(lines) > 1) {
    out <- paste0(ind, "  ", line_strs, collapse = ",\n")
    paste0("c(\n", out, "\n", ind, ")")
  } else {
    paste0("c(", paste0(line_strs, collapse = ",\n"), ")")
  }
}

default_mapping <- list(
  select_STUDYID = "STUDYID",
  select_USUBJID = "USUBJID",
  select_DOSEA = "DOSEA",
  select_DOSEU = "DOSEU",
  select_DOSETRT = "DOSETRT",
  select_PARAM = "PARAM",
  select_Metabolites = "Metab-DrugA",
  select_ARRLT = "ARRLT",
  select_NRRLT = "NRRLT",
  select_AFRLT = "AFRLT",
  select_NCAwXRS = c("NCA1XRS", "NCA2XRS"),
  select_NFRLT = "NFRLT",
  select_PCSPEC = "PCSPEC",
  select_ROUTE = "ROUTE",
  select_TRTRINT = "TRTRINT",
  select_ADOSEDUR = "ADOSEDUR",
  select_Grouping_Variables = c("TRT01A", "RACE", "SEX"),
  select_RRLTU = "RRLTU",
  select_VOLUME = "VOLUME",
  select_VOLUMEU = "VOLUMEU",
  select_AVAL = "AVAL",
  select_AVALU = "AVALU",
  select_ATPTREF = "ATPTREF"
)

#' Generate a session script from settings and mapping files
#'
#' This function reads a settings yaml file and data path, and generates an R script
#' that can reproduce the session using a template.
#'
#' @param settings_file_path Path to the yaml file containing the settings list.
#' @param data_path Path to the data file to be referenced in the script.
#' @param template_path Path to the R script template file. By default, uses the one
#' installed from your aNCA package version.
#' @param output_path Path to write the resulting script file.
#' @param mapping Named list mapping variable names (default: \code{default_mapping}).
#' @param ratio_table Data frame containing ratio definitions (default: empty data frame).
#'
#' @return Invisibly returns the output_path.
#' @export
get_settings_code <- function(
  settings_file_path,
  data_path,
  output_path = "settings_code.R",
  template_path = system.file("www/templates/script_template.R", package = "aNCA"),
  # TODO: mapping & ratio_table should be included in the settings file as well
  # so they keep working as expected also from the settings file
  mapping = default_mapping,
  ratio_table = data.frame()
) {
  settings <- read_settings(settings_file_path)
  session <- list(
    settings = settings[["settings"]],
    slope_rules = settings[["slope_rules"]],
    data_path = data_path,
    mapping = mapping,
    ratio_table = ratio_table
  )
  get_code(
    template_path = template_path,
    setts_obj = session,
    output_path = output_path
  )
  invisible(output_path)
}

#' Generate a session script from a Shiny session object
#'
#' This function generates an R script that can reproduce the outputs of a Shiny app session.
#' It extracts the session's user data, substitutes it into a script template, and writes
#' the result to a file.
#'
#' @param session The Shiny session object containing user data (typically from a running app).
#' @param output_path Path to write the resulting script file (e.g., "output_script.R").
#' @param template_path Path to the R script template file. By default, uses the one
#' installed from your aNCA package version.
#' @return Invisibly returns the output_path.
#' @keywords Internal
#' @noRd
get_session_code <- function(
  session,
  output_path,
  template_path = system.file("www/templates/script_template.R", package = "aNCA")
) {
  get_code(
    template_path = template_path,
    setts_obj = session$userData,
    output_path = output_path
  )
  invisible(output_path)
}
