
data_path <- NULL

processed_pknca_data <- read_pk(data_path)
if (is.null(data_path)) stop("Please provide a processed data path within the script")

manual_slopes <- deparse1(session$userData$slope_rules$manual_slopes())
profiles_per_subject <- deparse1(session$userData$slope_rules$profiles_per_subject())
slopes_groups <- deparse1(session$userData$slope_rules$slopes_groups())

res <- processed_pknca_data %>%
  filter_slopes(
    manual_slopes,
    profiles_per_subject,
    slopes_groups,
    check_reasons = TRUE
  ) %>%
  PKNCA_calculate_nca() %>%
  add_f_to_pknca_results(settings$bioavailability)

final_results <- pivot_wider_pknca_results(results)

write_csv(final_results, file = "nca_results.csv")