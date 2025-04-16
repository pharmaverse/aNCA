# Create PKNCA results object from 0 with sample concentration and dose datasets
# Additional conditions to PKNCA assumptions need to be made:
# 1) CDISC denomination of actual and nominal time variables (AFRLT, ARRLT, NFRLT, NRRLT)
# 2) For the intervals create a column (type_interval) that differentiates between
# custom AUC ranges ("manual") and main parameter calculations ("main")
# 3) There are PPSTRES and PPSTRESU variables in the PKNCA results output
# 4) start_dose & end_dose columns expressing when the actual start and actual end
# of the dose happened. The time reference is the first dose given to the subject.
# 5) CDISC denomination of PK parameters (needed temporarily to derive LAMZNPT & LAMZMTD)
conc_data <- data.frame(
  ID = rep(1:2, each = 10), # Adjusted for 5 points per ID/DOSNO combination
  DOSNO = rep(rep(1:2, each = 5), 2),
  AFRLT = rep(1:10, 2),
  ARRLT = rep(1:5, 4),
  NFRLT = rep(1:10, 2),
  NRRLT = rep(1:5, 4),
  AVAL = c(c(10, 8, 6, 4, 2),
           c(10, 8, 6, 4, 2),
           c(100, 80, 60, 40, 20),
           c(100, 80, 60, 40, 20)),
  # Function assumes these custom additional columns are present
  is.excluded.hl = FALSE,
  is.included.hl = FALSE,
  exclude_half.life = FALSE
)

dose_data <- data.frame(
  ID = rep(1:2, each = 2),
  DOSNO = rep(1:2, 2),
  AFRLT = rep(c(0, 5), 2), # Aligned with conc_data AFRLT start and end times
  ARRLT = rep(0, 4),
  NFRLT = rep(c(0, 5), 2), # Aligned with conc_data NFRLT start and end times
  NRRLT = rep(0, 4),
  DOSE = c(rep(10, 2), rep(100, 2))
)

# Perform NCA analysis
main_intervals <- data.frame(
  start = c(0, 5),
  end = c(5, 10),
  half.life = TRUE,
  cmax = TRUE,
  aucint.last = FALSE,
  type_interval = "main",
  DOSNO = c(1, 2)
)
auc_intervals <- data.frame(
  start = c(0, 5),
  end = c(5, 10),
  half.life = FALSE,
  cmax = FALSE,
  aucint.last = TRUE,
  type_interval = "manual",
  DOSNO = c(1, 2)
)
options <- PKNCA::PKNCA.options()
options$keep_interval_cols <- c("DOSNO", "type_interval")

myres <- PKNCA::pk.nca(
  PKNCA::PKNCAdata(
    data.conc = PKNCA::PKNCAconc(conc_data, AVAL ~ AFRLT | ID),
    data.dose = PKNCA::PKNCAdose(dose_data, DOSE ~ AFRLT | ID),
    intervals = rbind(
      main_intervals,
      auc_intervals
    ),
    options = options,
    units = PKNCA::pknca_units_table(
      concu = "ng/mL", doseu = "mg/kg", amountu = "mg", timeu = "hr"
    )
  )
)
# Make preparations done by PKNCA_calculate_nca
dose_data_to_join <- select(
  myres$data$dose$data,
  -exclude,
  -myres$data$dose$data$conc$columns$groups$group_analyte
)
myres$result <- myres$result %>%
  # Function assumes dose time information is added to PKNCA results
  inner_join(
    dose_data_to_join,
    by = intersect(names(.), names(dose_data_to_join))
  ) %>%
  # Function assumes start_dose, end_dose, PPSTRES, PPSTRESU
  mutate(
    start_dose = start - !!sym(myres$data$dose$columns$time),
    end_dose = end - !!sym(myres$data$dose$columns$time),
    PPSTRESU = ifelse(PPORRESU %in% c("fraction", "unitless"), "", PPORRESU),
    PPSTRES = PPORRES,
    # Function assumes PPTESTCD is following CDISC standards
    PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")
  )

# Obtain the result
result <- pivot_wider_pknca_results(myres)








#' Validate PKNCA Parameters
#'
#' This function validates that the reshaped PKNCA results contain the expected values
#' for each parameter based on the original `myres` object.
#'
#' @param myres The original PKNCA results object.
#' @param result The reshaped results from `pivot_wider_pknca_results`.
#' @return A logical value indicating whether all checks passed.
#' @export
validate_pknca_params <- function(myres, result) {

  # Extract unique parameter names with units
  names_params <- myres$result %>%
    mutate(PPTESTCD2 = case_when(
      type_interval == "manual" ~ paste0(PPTESTCD, "_", start, "-", end),
      PPSTRESU == "" | PPSTRESU == "unitless" ~ PPTESTCD,
      TRUE ~ paste0(PPTESTCD, "[", PPSTRESU, "]")
    )) %>%
    pull(PPTESTCD2) %>%
    unique()
  
  # Iterate over each parameter and validate values
  for (param_col in names_params) {
    pptestcd <- gsub("\\[.*\\]", "", param_col)  # Remove units from parameter name
    
    if (grepl("AUCINT_[0-9]+\\-[0-9]", param_col)) {
      pptestcd <- gsub("_[0-9]+\\-[0-9]", "", param_col)
      auc_start <- gsub("AUCINT_([0-9]+)\\-[0-9]", "\\1", param_col)
      auc_end <- gsub("AUCINT_[0-9]+\\-([0-9]+)", "\\1", param_col)
      
      original_data <- myres$result %>%
        filter(start == auc_start,
               end == auc_end)
      reshaped_res <- result %>%
        filter(start == auc_start,
               end == auc_end)
    } else {
      original_data <- myres$result
      reshaped_res <- result
    }

    original_vals <- original_data %>%
      formatters::var_labels_remove() %>%
      filter(PPTESTCD == pptestcd) %>%
      pull(PPSTRES)
    result_vals <- reshaped_res %>%
      formatters::var_labels_remove() %>%
      pull(param_col)

    have_similar_vals <- all(round(result_vals, 3) %in% round(original_vals, 3))
    have_same_length <- length(result_vals) == length(original_vals)
    if (!have_similar_vals | !have_same_length) {
      stop(paste("Mismatch in values for parameter:", pptestcd))
    }
  }
  
  # If all checks pass, return TRUE
  TRUE
}





describe("pivot_wider_pknca_results", {

  it("produces a data.frame with expected format when only reshaping main intervals", {
    myres_only_main <- myres
    myres_only_main$result <- myres$result  %>%
      filter(type_interval == "main")
    result_only_main <- pivot_wider_pknca_results(myres_only_main)
    expect_s3_class(result, "data.frame")

    # Check that the result contains expected columns
    expected_columns <- c(
      "ID", "start", "end", "DOSNO", "AFRLT", "ARRLT",
      "NFRLT", "NRRLT", "DOSE", "route", "duration",
      "CMAX[ng/mL]", "TMAX[hr]", "TLST[hr]", "LAMZ[1/hr]",
      "R2", "R2ADJ", "LAMZLL[hr]",
      "LAMZNPT[count]", "CLSTP[ng/mL]", "LAMZHL[hr]",
      "LAMZSPN", "LAMZIX",
      "LAMZMTD", "Exclude"
    )
    expect_true(all(expected_columns %in% colnames(result_only_main)))

    # Check number of rows is the expected
    expect_true(nrow(result_only_main) == nrow(myres_only_main$data$dose$data))

    # Check values
    expect_true(validate_pknca_params(myres_only_main, result_only_main))
    })

  it("reshapes PKNCA results correctly when also considering AUC intervals", {
    # Check that the result is a data frame
    expect_s3_class(result, "data.frame")

    # Check that the result contains expected columns
    expected_columns <- c(
      "ID", "start", "end", "DOSNO", "AFRLT", "ARRLT",
      "NFRLT", "NRRLT", "DOSE", "route", "duration",
      "CMAX[ng/mL]", "TMAX[hr]", "TLST[hr]", "LAMZ[1/hr]",
      "R2", "R2ADJ", "LAMZLL[hr]",
      "LAMZNPT[count]", "CLSTP[ng/mL]", "LAMZHL[hr]",
      "LAMZSPN", "AUCINT_0-5", "AUCINT_5-10", "LAMZIX",
      "LAMZMTD", "Exclude"
    )
    expect_true(all(expected_columns %in% colnames(result)))
    
    expect_true(validate_pknca_params(myres, result))
  })

  it("rounds numeric values to three decimals", {
    expected_num_param_cols <- c(
      "CMAX[ng/mL]", "TMAX[hr]", "TLST[hr]", "LAMZ[1/hr]",
      "R2", "R2ADJ", "LAMZLL[hr]",
      "LAMZNPT[count]", "CLSTP[ng/mL]", "LAMZHL[hr]",
      "LAMZSPN", "AUCINT_0-5", "AUCINT_5-10"
    )
    rounded_values <- result[, expected_num_param_cols]
    expect_true(all(apply(rounded_values, 2,
                          function(x) all(na.omit(abs(x - round(x, 3))) < 1e-9))))
  })

  it("adds appropriate labels to columns", {
    labels <- formatters::var_labels(result)
    expected_labels <- c(
      ID = NA, start = NA, end = NA, DOSNO = NA, AFRLT = NA, ARRLT = NA,
      NFRLT = NA, NRRLT = NA, DOSE = NA, route = NA, duration = NA,
      `CMAX[ng/mL]` = "Max Conc", `TMAX[hr]` = "Time of CMAX",
      `TLST[hr]` = "Time of Last Nonzero Conc", `LAMZ[1/hr]` = "Lambda z",
      `R2` = "R Squared", `R2ADJ` = "R Squared Adjusted",
      `LAMZLL[hr]` = "Lambda z Lower Limit", `LAMZNPT[count]` = "Number of Points for Lambda z",
      `CLSTP[ng/mL]` = "Clast pred", `LAMZHL[hr]` = "Half-Life Lambda z",
      `LAMZSPN` = "Lambda z Span", `AUCINT_0-5` = NA, `AUCINT_5-10` = NA,
      `LAMZIX` = NA, `LAMZMTD` = NA, `Exclude` = NA
    )
    expect_equal(labels, expected_labels)
  })

  it("handles exclude values correctly", {
    # Modify myres$result to include exclude values
    myres_with_exclude <- myres
    myres_with_exclude$result <- myres_with_exclude$result %>%
      mutate(
        exclude = ifelse(ID == 1 & DOSNO == 1, "Reason 1; Reason 2", NA_character_)
      )

    # Apply pivot_wider_pknca_results
    result <- pivot_wider_pknca_results(myres_with_exclude)

    # Check that the Exclude column combines and deduplicates exclude values
    exclude_values <- result %>% filter(ID == 1 & DOSNO == 1) %>% pull(Exclude)
    expect_equal(exclude_values, "Reason 1, Reason 2")

    # Check that rows without exclude values have NA in the Exclude column
    exclude_values_na <- result %>% filter(ID == 2 & DOSNO == 2) %>% pull(Exclude)
    expect_true(is.na(exclude_values_na))
  })
})
