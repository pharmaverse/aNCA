# Generate filter UI for a single variable
generate_filter_ui <- function(var, col_data, ns) {
  if (is.character(col_data) || is.factor(col_data)) {
    virtualSelectInput(
      inputId = ns(paste0("filter_", var)),
      label = var,
      search = TRUE,
      choices = unique(col_data),
      selected = unique(col_data),
      optionsCount = 6,
      noOfDisplayValues = 6,
      showSelectedOptionsFirst = TRUE,
      multiple = TRUE
    )
  } else if (is.numeric(col_data)) {
    rng <- range(col_data, na.rm = TRUE)
    tagList(
      numericInput(
        inputId = ns(paste0("filter_", var, "_min")),
        label = paste("Min", var),
        value = rng[1]
      ),
      numericInput(
        inputId = ns(paste0("filter_", var, "_max")),
        label = paste("Max", var),
        value = rng[2]
      )
    )
  }
}

# Apply filter to a single variable
apply_filter <- function(df, var, input) {
  col <- df[[var]]
  if (is.character(col) || is.factor(col)) {
    selected <- input[[paste0("filter_", var)]]
    if (!is.null(selected)) {
      df <- df[col %in% selected, , drop = FALSE]
    }
  } else if (is.numeric(col)) {
    min_val <- input[[paste0("filter_", var, "_min")]]
    max_val <- input[[paste0("filter_", var, "_max")]]
    if (!is.null(min_val)) df <- df[col >= min_val, , drop = FALSE]
    if (!is.null(max_val)) df <- df[col <= max_val, , drop = FALSE]
  }
  df
}

# Wait for all required inputs to be available
wait_for_all_filter_inputs <- function(df, input) {
  for (var in names(df)) {
    col <- df[[var]]
    if (is.character(col) || is.factor(col)) {
      req(input[[paste0("filter_", var)]])
    } else if (is.numeric(col)) {
      req(input[[paste0("filter_", var, "_min")]])
      req(input[[paste0("filter_", var, "_max")]])
    }
  }
}
