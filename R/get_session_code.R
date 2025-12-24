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
    clean_deparse(col, indent + 1)
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
  nms <- names(obj)
  items <- vapply(seq_len(n), FUN.VALUE = "", function(i) {
    name <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else paste0("V", i)
    # Quote name if not a valid R symbol
    if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", name)) {
      name <- sprintf('"%s"', name)
    }
    val <- obj[[i]]
    # Use specialized deparsers for atomic vectors
    val_str <- clean_deparse(val, indent + 1)
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

clean_deparse.character <- function(obj, indent = 0) {
  n <- length(obj)
  if (n == 1) {
    return(sprintf('"%s"', obj))
  } else if (n > 10) {
    ind <- paste(rep("  ", indent), collapse = "")
    lines <- split(obj, ceiling(seq_along(obj) / 10))
    line_strs <- vapply(lines, function(x) paste(sprintf('"%s"', x), collapse = ", "), "")
    paste0(
      "c(\n",
      paste0(ind, "  ", line_strs, collapse = ",\n"),
      "\n", ind, ")"
    )
  } else {
    paste0("c(", paste(sprintf('"%s"', obj), collapse = ", "), ")")
  }
}

#' @noRd

clean_deparse.numeric <- function(obj, indent = 0) {
  n <- length(obj)
  if (n == 1) {
    return(paste0(obj))
  } else if (n > 10) {
    ind <- paste(rep("  ", indent), collapse = "")
    lines <- split(obj, ceiling(seq_along(obj) / 10))
    line_strs <- vapply(lines, function(x) paste(x, collapse = ", "), "")
    paste0(
      "c(\n",
      paste0(ind, "  ", line_strs, collapse = ",\n"),
      "\n", ind, ")"
    )
  } else {
    paste0("c(", paste(obj, collapse = ", "), ")")
  }
}

#' @noRd
clean_deparse.integer <- clean_deparse.numeric

#' @noRd
clean_deparse.logical <- function(obj, indent = 0) {
  n <- length(obj)
  if (n == 0) {
    return("logical()")
  } else if (n == 1) {
    return(paste0(obj))
  } else {
    paste0("c(", paste(ifelse(obj, "TRUE", "FALSE"), collapse = ", "), ")")
  }
}
