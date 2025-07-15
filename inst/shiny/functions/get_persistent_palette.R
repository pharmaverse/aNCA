#' Get a Persistent Color Palette for a Set of Variables
#'
#' @param data The full, unfiltered data.frame.
#' @param colorby_vars A character vector of one or more column names.
#' @param palette_name The name of the color theme (e.g., "default", "viridis").
#' @return A single named color vector (palette).
get_persistent_palette <- function(data, colorby_vars, palette_name = "default") {
  # Return NULL if no variables are provided
  if (is.null(colorby_vars) || length(colorby_vars) == 0) {
    return(NULL)
  }

  # Create a temporary column in the data representing the interaction of the variables
  interaction_data <- data %>%
    mutate(interaction_col = interaction(!!!syms(colorby_vars), sep = ", "))

  # Get all unique levels of this new interaction column
  all_levels <- sample(unique(na.omit(interaction_data$interaction_col)))
  n <- length(all_levels)

  if (n == 0) return(NULL)

  # Generate colors using the selected theme
  colors <- if (palette_name == "viridis") {
    viridisLite::viridis(n)
  } else if (palette_name == "spectral") {
    if (n > 11) {
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(n)
    } else {
      RColorBrewer::brewer.pal(max(n, 3), "Spectral")[1:n]
    }
  } else {
    scales::hue_pal()(n)
  }

  # Return the final named palette
  setNames(colors, all_levels)
}
