#' Generate a session script code in R that can replicate the App outputs
#'
#' @param template_path Path to the R script template (e.g., script_template.R)
#' @param session The session object containing userData, etc.
#' @param output_path Path to write the resulting script file (e.g., "output_script.R")
#' @return The output_path (invisibly)
#' @export
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
  if (matches[1] == -1) {
    stop(
      "Template has no placeholders (session$userData...) to substitute.",
      "Did you accidentally modify it?"
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

# Helper to cleanly deparse an object (data.frame, list, etc.)
clean_deparse <- function(obj, indent = 0) {
  ind <- paste(rep("  ", indent), collapse = "")
  obj_class <- class(obj)[1]
  if (length(obj) == 0) {
    return(paste0(obj_class, "()"))
  }

  switch(
    obj_class,
    data.frame = {
      # Multi-line, indented representation of a data.frame
      cols <- lapply(obj, function(col) {
        d <- col |>
          deparse(width.cutoff = 500) |>
          paste(collapse = "")
        # Only wrap in c(...) if length > 1
        if (length(col) > 1 && !grepl("^c\\(", d)) {
          d <- paste0("c(", d, ")")
        }
        d
      })
      col_strs <- paste0(ind, "  ", names(obj), " = ", unlist(cols))
      if (length(col_strs) > 1) {
        not_last <- 1:(length(col_strs) - 1)
        col_strs[not_last] <- paste0(col_strs[not_last], ",")
      }
      paste0("data.frame(\n", paste(col_strs, collapse = "\n"), "\n", ind, ")")
    },

    list = {
      # Deparse as list(a = ..., ...) and handle unnamed elements by position
      n <- length(obj)
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
    },

    character = {
      if (length(obj) == 1) paste0('"', obj, '"')
      else paste0("c(", paste(sprintf('"%s"', obj), collapse = ", "), ")")
    },

    numeric = {
      if (length(obj) == 1) as.character(obj)
      else paste0("c(", paste(obj, collapse = ", "), ")")
    },
    integer = {
      if (length(obj) == 1) as.character(obj)
      else paste0("c(", paste(obj, collapse = ", "), ")")
    },

    logical = {
      if (length(obj) == 1) if (obj) "TRUE" else "FALSE"
      else paste0("c(", paste(ifelse(obj, "TRUE", "FALSE"), collapse = ", "), ")")
    },

    {
      # default: fallback to deparse
      paste(deparse(obj, width.cutoff = 500), collapse = "")
    }
  )
}
