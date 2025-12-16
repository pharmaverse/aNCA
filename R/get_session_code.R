#' Generate a session script code in R that can replicate the App outputs
#'
#' @param template_path Path to the R script template (e.g., script_template.R)
#' @param session The session object containing userData, etc.
#' @param output_path Path to write the resulting script file (e.g., "output_script.R")
#' @return The output_path (invisibly)
#' @export
get_session_code <- function(template_path, session, output_path) {

  # Helper to get value from session$userData by path (e.g., 'settings$method')
  get_session_value <- function(path) {
    parts <- strsplit(path, "\\$")[[1]]
    obj <- session$userData
    for (p in parts) {
      if (inherits(obj[[p]], "reactive")) {
        obj <- obj[[p]]()
      } else {
        obj <- obj[[p]]
      }
      if (is.null(obj)) return(NULL)
    }
    obj
  }

  # Read template
  script <- readLines(template_path, warn = FALSE) %>%
    paste(collapse = "\n")

  # Find all session$userData$...
  pattern <- "session\\$userData(\\$[a-zA-Z0-9_]+(\\(\\))?(\\$[a-zA-Z0-9_]+)*)"
  matches <- gregexpr(pattern, script, perl = TRUE)[[1]]
  if (matches[1] == -1) {
    stop(
      "Template has no placeholders (session$userData...) to substitute.",
      "This may be due to an incorrect file path, a missing template, ",
      "or a modified template without placeholders."
    )
  }

  # Replace each match with deparsed value
  for (i in rev(seq_along(matches))) {
    start <- matches[i]
    len <- attr(matches, "match.length")[i]
    matched <- substr(script, start, start + len - 1)
    # Extract the path after session$userData$
    path <- sub("^session\\$userData\\$", "", matched)
    value <- get_session_value(path)

    deparsed <- clean_deparse(value)
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
#' serialize `session$userData` values into a runnable R script.
#'
#' @param obj An R object to convert to a string of R code.
#' @param indent Integer indentation level for multi-line outputs.
#' @return A single string containing R code that, when evaluated, will
#'   reconstruct `obj` (or a close approximation for complex types).
#' @keywords internal
clean_deparse <- function(obj, indent = 0) {
  # Handle trivial length-0 constructors (character(0), numeric(0), list(), data.frame(), ...)
  if (length(obj) == 0 && !is.null(obj)) {
    return(paste0(class(obj)[1], "()"))
  }
  # For single-element atomic numeric/integer/logical return bare representation
  if (length(obj) == 1 && class(obj)[1] %in% c("integer", "numeric", "logical")) {
    return(as.character(obj))
  }
  UseMethod("clean_deparse")
}

#' @noRd
clean_deparse.default <- function(obj, indent = 0) {
  paste(deparse(obj, width.cutoff = 500), collapse = "")
}

#' @noRd
clean_deparse.data.frame <- function(obj, indent = 0) {
  ind <- paste(rep("  ", indent), collapse = "")
  if (nrow(obj) == 0) return("data.frame()")

  cols <- lapply(obj, function(col) {
    d <- deparse(col, width.cutoff = 500) %>% paste(collapse = "")
    if (length(col) > 1 && !grepl("^c\\(", d)) d <- paste0("c(", d, ")")
    d
  })
  col_strs <- paste0(ind, "  ", names(obj), " = ", unlist(cols))
  if (length(col_strs) > 1) {
    not_last <- seq_len(length(col_strs) - 1)
    col_strs[not_last] <- paste0(col_strs[not_last], ",")
  }
  paste0("data.frame(\n", paste(col_strs, collapse = "\n"), "\n", ind, ")")
}

#' @noRd
clean_deparse.list <- function(obj, indent = 0) {
  ind <- paste(rep("  ", indent), collapse = "")
  n <- length(obj)
  if (n == 0) return("list()")
  nms <- names(obj)
  items <- vapply(seq_len(n), FUN.VALUE = "", function(i) {
    name <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else paste0("V", i)
    val <- obj[[i]]
    paste0(name, " = ", clean_deparse(val, indent + 1))
  })
  if (length(items) > 1) {
    not_last <- seq_len(length(items) - 1)
    items[not_last] <- paste0(items[not_last], ",")
  }
  item_strs <- paste0(ind, "  ", items)
  paste0("list(\n", paste(item_strs, collapse = "\n"), "\n", ind, ")")
}

#' @noRd
clean_deparse.character <- function(obj, indent = 0) {
  if (length(obj) == 1) return(sprintf('"%s"', obj))
  paste0("c(", paste(sprintf('"%s"', obj), collapse = ", "), ")")
}

#' @noRd
clean_deparse.numeric <- function(obj, indent = 0) {
  paste0("c(", paste(obj, collapse = ", "), ")")
}

#' @noRd
clean_deparse.integer <- clean_deparse.numeric

#' @noRd
clean_deparse.logical <- function(obj, indent = 0) {
  paste0("c(", paste(ifelse(obj, "TRUE", "FALSE"), collapse = ", "), ")")
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
#' This function reads a settings RDS file and data path, and generates an R script
#' that can reproduce the session using a template.
#'
#' @param settings_file_path Path to the RDS file containing the settings list.
#' @param data_path Path to the data file to be referenced in the script.
#' @param mapping Named list mapping variable names (default: \code{default_mapping}).
#' @param template_path Path to the R script template file.
#' @param output_path Path to write the resulting script file.
#'
#' @return Invisibly returns the output_path.
#' @export
get_settings_code <- function(settings_file_path, data_path, mapping = default_mapping, template_path, output_path) {
  settings <- readRDS(settings_file_path)
  session <- list(userData = list(settings = settings, data_path = data_path, mapping = mapping))
  get_session_code(
    template_path = template_path,
    session = session,
    output_path = output_path
  )
  invisible(output_path)
}
settings_file_path <- "../../Downloads/elproject/settings/settings.rds"
get_settings_code(
  settings_file_path, 
  data_path = "inst/shiny/data/example-ADNCA.csv",
  template_path = "inst/shiny/www/templates/script_template.R",
  output_path = "../../Downloads/elproject/settings/settings_code.R"
)
