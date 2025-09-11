  #' Check overlap between existing and new slope rulesets
  #'
  #' Takes in tables with existing and incoming selections and exclusions, finds any overlap and
  #' differences, edits the ruleset table accordingly.
  #'
  #' @param existing Data frame with existing selections and exclusions.
  #' @param new      Data frame with new rule to be added or removed.
  #' @param slope_groups List with column names that define the groups.
  #' @param .keep    Whether to force keep fully overlapping rulesets. If FALSE, it will be assumed
  #'                 that the user wants to remove rule if new range already exists in the dataset.
  #'                 If TRUE, in that case full range will be kept.
  #' @returns Data frame with full ruleset, adjusted for new rules.
  #' @export
  check_slope_rule_overlap <- function(existing, new, .keep = FALSE) {
    print("check_slope_rule_overlap")
    slope_groups <- setdiff(names(new), c("TYPE", "RANGE", "REASON"))
    
    # check if any rule already exists for specific subject and profile #
    existing_index <- which(
      existing$TYPE == new$TYPE &
        Reduce(`&`, lapply(slope_groups, function(col) {
          existing[[col]] == new[[col]]
        }))
    )
    
    if (length(existing_index) != 1) {
      if (length(existing_index) > 1)
        warning("More than one range for single subject, profile and rule type detected.")
      return(rbind(existing, new))
    }
    
    existing_range <- .eval_range(existing$RANGE[existing_index])
    new_range <- .eval_range(new$RANGE)
    
    is_inter <- length(intersect(existing_range, new_range)) != 0
    is_diff <- length(setdiff(new_range, existing_range)) != 0
    
    if (is_diff || .keep) {
      existing$RANGE[existing_index] <- unique(c(existing_range, new_range)) %>%
        .compress_range()
      
    } else if (is_inter) {
      existing$RANGE[existing_index] <- setdiff(existing_range, new_range) %>%
        .compress_range()
    }
    
    dplyr::filter(existing, !is.na(RANGE))
  }
  
  #' Apply Slope Rules to Update Data
  #'
  #' This function iterates over the given slopes and updates the `data$conc$data` object
  #' by setting inclusion or exclusion flags based on the slope conditions.
  #'
  #' @param data A list containing concentration data (`data$conc$data`) with columns that
  #'        need to be updated based on the slope rules.
  #' @param slopes A data frame containing slope rules, including `TYPE`, `RANGE`,
  #'        and `REASON` columns. May also have grouping columns (expected to match slope_groups)
  #' @param slope_groups A character vector specifying the group columns used for filtering.
  #'
  #' @returns description The modified `data` object with updated inclusion/exclusion flags
  #'         and reasons in `data$conc$data`.
  .update_pknca_with_rules <- function(data, slopes) {
    print(".update_pknca_with_rules")
    
    # TODO (Gerardo): If we keep RANGE as a time, we don't need NCA_PROFILE
    slope_groups <- c(group_vars(data), "NCA_PROFILE")
    time_col <- data$conc$columns$time
    exclude_hl_col <- data$conc$columns$exclude_half.life
    include_hl_col <- data$conc$columns$include_half.life
    
    # Apply each rule action
    for (i in seq_len(nrow(slopes))) {
      # Determine the time range for the points adjusted
      range <- strsplit(as.character(slopes$RANGE[i]), ":")[[1]] %>%
        as.numeric() %>%
        range()
      
      # Build the condition dynamically for group columns and time range
      pnt_idx <- which(
        Reduce(`&`, lapply(slope_groups, function(col) {
          data$conc$data[[col]] == slopes[[col]][i]
        })) &
          between(data$conc$data[[time_col]], range[[1]], range[[2]])
      )
      
      if (slopes$TYPE[i] == "Selection") {
        data$conc$data[[include_hl_col]][pnt_idx] <- TRUE
      } else if (slopes$TYPE[i] == "Exclusion") {
        data$conc$data[[exclude_hl_col]][pnt_idx] <- TRUE
      } else {
        stop("Unknown TYPE in slopes: ", slopes$TYPE[i])
      }
      
      data$conc$data$REASON[pnt_idx] <- paste0(
        data$conc$data$REASON[pnt_idx],
        rep(slopes$REASON[i], length(pnt_idx))
      )
      print(".update_pknca_with_rules")
    }
    
    data
  }
  
  # Refactored: .update_plots_with_rules now uses .update_pknca_with_rules and delegates to .update_plots_with_pknca
  .update_plots_with_rules <- function(pknca_data, manual_slopes, plot_outputs, slopes_to_update = NULL) {
    print(".update_plots_with_rules (refactored)")
    # Update the PKNCA object with manual slopes
    pknca_for_plots <- .update_pknca_with_rules(pknca_data, manual_slopes)
    # If the user does not specify which plots to update (NULL), update all plots in the manual slopes table
    if (is.null(slopes_to_update)) {
      slopes_to_update <- manual_slopes %>%
        select(any_of(c(group_vars(pknca_for_plots), "NCA_PROFILE"))) %>%
        distinct()
      print(names(plot_outputs))
    }
    # If the user specify that there are no plots to update (empty data.frame), then do not update
    if (nrow(slopes_to_update) == 0) return(plot_outputs)
    # Delegate to .update_plots_with_pknca for actual plot updating
    .update_plots_with_pknca(pknca_for_plots, plot_outputs, intervals_to_update = slopes_to_update)
  }
  
  .update_plots_with_pknca <- function(pknca_data, plot_outputs, intervals_to_update = NULL) {
    print(".update_plots_with_pknca")
    # If the user does not specify which plots to update (NULL), update all plots in the manual slopes table
    if (is.null(intervals_to_update)) {
      intervals_to_update <- pknca_data$intervals %>%
        select(any_of(c(group_vars(pknca_data), "NCA_PROFILE"))) %>%
        distinct()
      print(names(plot_outputs))
    }
    # If the user specify that there are no plots to update (empty data.frame), then do not update
    if (nrow(intervals_to_update) == 0) return(plot_outputs)
    
    # Get the intervals of the plots affected by the current rules
    pknca_data$intervals <- inner_join(
      intervals_to_update,
      pknca_data$intervals,
      by = c(group_vars(pknca_data), "NCA_PROFILE")
    )
    updated_plots <- suppressWarnings(get_halflife_plot(pknca_data))
    plot_outputs[names(updated_plots)] <- updated_plots
    plot_outputs
  }
