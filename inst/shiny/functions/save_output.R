#' Save aNCA Result to the Output Directory
#'
#' @param output Output object, can be a data frame, plot or a list of them.
#' @param output_path Path to the output directory (should exist or be creatable).
#' @returns Invisibly returns the file path written.
save_output <- function(output, output_path) {

  # Create output directory if it doesn't exist
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

  if (inherits(output, "data.frame")) {
    write.csv(output, file = output_path, row.names = FALSE)
  } else if (inherits(output, "ggplot")) {
    ggsave(output_path, plot = output, width = 10, height = 6)
  } else if (inherits(output, "list")) {
    dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
    for (name in names(output)) {
      if (inherits(output[[name]], "ggplot")) {
        file_name <- paste0(output_path, "/", name, ".png")
        ggsave(file_name, plot = output[[name]], width = 10, height = 6)
      } else if (inherits(output[[1]], "data.frame")) {
        file_name <- paste0(output_path, "/", name, ".csv")
        write.csv(output[[name]], file = file_name, row.names = FALSE)
      } else {
        stop("Unsupported output type in the list.")
      }
    }
  }
}