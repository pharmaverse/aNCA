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
      if (is.null(obj)) {
        return(NULL)
      }
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
#' serialize `session$userData` values into a runnable R script.
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
  # Handle trivial length-0 constructors (character(0), numeric(0), list(), data.frame(), ...)
  if (length(obj) == 0 && !is.null(obj)) {
    return(paste0(class(obj)[1], "()"))
  }
  UseMethod("clean_deparse")
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

# TODO (Gerardo): Create a linked function
# to obtain the code from a settings file
# (#826)
