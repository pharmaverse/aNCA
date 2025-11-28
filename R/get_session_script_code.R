#' Generates a session script to replicate the App outputs
#'
#' @param template_path Path to the R script template (e.g., script_template.R)
#' @param session The session object containing userData, etc.
#' @param output_path Path to write the resulting script file (e.g., "output_script.R")
#' @return The output_path (invisibly)
get_session_script_code <- function(template_path, session, output_path) {
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
  script <- readLines(template_path, warn = FALSE) |>
    paste(collapse = "\n")

  # Find all session$userData$... or session$userData[[...]] or session$userData$...$...
  # Regex for session$userData$foo or session$userData$foo$bar or session$userData[["foo"]]
  pattern <- "session\\$userData(\\$[a-zA-Z0-9_]+(\\(\\))?(\\$[a-zA-Z0-9_]+)*)"
  matches <- gregexpr(pattern, script, perl = TRUE)[[1]]
  
  # Just write the original template if there are no substitutions needed
  # with a warning message
  if (matches[1] == -1) {
    script_lines <- strsplit(script, "\n")[[1]]
    writeLines(script_lines, output_path)
    warning(
      "Please, the script template should have session$userData calls in order to work. ",
      "Did you accidentally modify the original template?"
    )
    return(invisible(output_path))
  }

  # Replace all matches with deparsed values in one pass
  match_positions <- gregexpr(pattern, script, perl = TRUE)[[1]]
  match_strings <- regmatches(script, list(match_positions))
  replacements <- lapply(match_strings, function(matched) {
    path <- sub("^session\\$userData\\$", "", matched)
    value <- get_session_value(path)
    clean_deparse(value)
  })
  regmatches(script, list(match_positions)) <- replacements
  # Split back into lines
  script_lines <- strsplit(script, "\n")[[1]]
  writeLines(script_lines, output_path)
  invisible(output_path)
}

# Helper to cleanly deparse an object (data.frame, list, etc.)
clean_deparse <- function(obj, indent = 0) {
  ind <- paste(rep("  ", indent), collapse = "")
  if (is.data.frame(obj)) {
    # Multi-line, indented, no trailing comma on last column
    cols <- lapply(obj, function(col) {
      d <- paste(deparse(col, width.cutoff = 500), collapse = "")
      if (grepl("^c\\(", d)) d else paste0("c(", d, ")")
    })
    col_strs <- paste0(ind, "  ", names(obj), " = ", unlist(cols))
    # Add comma to all but last
    if (length(col_strs) > 1) {
      col_strs[1:(length(col_strs) - 1)] <- paste0(col_strs[1:(length(col_strs) - 1)], ",")
    }
    paste0(
      "data.frame(\n",
      paste(col_strs, collapse = "\n"),
      "\n", ind, ")"
    )
  } else if (is.list(obj)) {
    # Deparse as list(a = ..., ...)
    items <- lapply(
      names(obj),
      function(nm) {
        paste0(nm, " = ", clean_deparse(obj[[nm]], indent + 1))
      }
    )
    # Add comma to all but last
    if (length(items) > 1) {
      items[1:(length(items) - 1)] <- paste0(items[1:(length(items) - 1)], ",")
    }
    item_strs <- paste0(ind, "  ", items)
    paste0(
      "list(\n",
      paste(item_strs, collapse = "\n"),
      "\n", ind, ")"
    )
  } else if (is.character(obj)) {
    if (length(obj) == 1) paste0('"', obj, '"')
    else paste0("c(", paste(sprintf('"%s"', obj), collapse = ", "), ")")
  } else if (is.numeric(obj) || is.integer(obj)) {
    if (length(obj) == 1) as.character(obj)
    else paste0("c(", paste(obj, collapse = ", "), ")")
  } else if (is.logical(obj)) {
    if (length(obj) == 1) if (obj) "TRUE" else "FALSE"
    else paste0("c(", paste(ifelse(obj, "TRUE", "FALSE"), collapse = ", "), ")")
  } else {
    paste(deparse(obj, width.cutoff = 500), collapse = "")
  }
}
