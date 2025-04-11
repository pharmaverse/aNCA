library(testthat)
library(PKNCA)
library(dplyr)
library(tidyr)
describe("pivot_wider_pknca_results", {

  it("reshapes PKNCA results correctly", {
    # Create input data similar to o_data in test-intervals_helpers.R
    conc_data <- data.frame(
      ID = rep(1:2, each = 10), # Adjusted for 5 points per ID/DOSNO combination
      DOSNO = rep(rep(1:2, each = 5), 2),
      AFRLT = rep(1:10, 2),
      ARRLT = rep(1:5, 4),
      NFRLT = rep(1:10, 2),
      NRRLT = rep(1:5, 4),
      AVAL = rep(c(10, 8, 6, 4, 2), 4) # Adjusted values for 5 points
    )

    dose_data <- data.frame(
      ID = rep(1:2, each = 2),
      DOSNO = rep(1:2, each = 2),
      AFRLT = rep(c(0, 5), 2), # Aligned with conc_data AFRLT start and end times
      ARRLT = rep(0, 4),
      NFRLT = rep(c(0, 5), 2), # Aligned with conc_data NFRLT start and end times
      NRRLT = rep(0, 4),
      DOSE = rep(100, 4)
    )

    # Perform NCA analysis
    main_intervals <- data.frame(
        start = c(0, 5),
        end = c(5, 10),
        half.life = TRUE,
        cmax = TRUE,
        aucint.last = FALSE,
        type_interval = "main"
    )
    auc_intervals <- data.frame(
        start = c(0, 5),
        end = c(5, 10),
        half.life = FALSE,
        cmax = FALSE,
        aucint.last = TRUE,
        type_interval = "manual"
    )
    myres <- PKNCA::pk.nca(
      PKNCA::PKNCAdata(
        data.conc = PKNCA::PKNCAconc(conc_data, AVAL~AFRLT|ID),
        data.dose = PKNCA::PKNCAdose(dose_data, DOSE~AFRLT|ID),
        intervals = rbind(
          main_intervals,
          auc_intervals
        ),
        options = list(keep_interval_cols = c("type_interval")),
        units = PKNCA::pknca_units_table(
            concu = "ng/mL", doseu = "mg/kg", amountu = "mg", timeu = "hr"
        )
    ))
    # Make preparations done by PKNCA_calculate_nca
    dose_data_to_join <- select(
        myres$data$dose$data,
        -exclude,
        -myres$data$dose$data$conc$columns$groups$group_analyte
    )
    myres$result <- myres$result %>%
        inner_join(
        dose_data_to_join,
        by = intersect(names(.), names(dose_data_to_join))
        ) %>%
        mutate(
        start_dose = start - !!sym(myres$data$dose$columns$time),
        end_dose = end - !!sym(myres$data$dose$columns$time),
        PPSTRESU = PPORRESU,
        PPSTRES = PPORRES
        )

    # Apply pivot_wider_pknca_results
    result <- pivot_wider_pknca_results(myres)

    # Check that the result is a data frame
    expect_s3_class(result, "data.frame")

    # Check that the result contains expected columns
    expected_columns <- c("ID", "cmax", "AUClast", "AUCinf_obs", "Exclude")
    expect_true(all(expected_columns %in% colnames(result)))

    # Check that the result contains expected values
    expect_true(all(result$cmax > 0))
    expect_true(all(result$AUClast > 0))
  })

  it("handles empty input gracefully", {
    # Create empty input
    empty_myres <- list(
      data = list(conc = list(data = data.frame())),
      result = data.frame()
    )

    # Apply pivot_wider_pknca_results
    result <- pivot_wider_pknca_results(empty_myres)

    # Check that the result is an empty data frame
    expect_equal(nrow(result), 0)
  })
})
