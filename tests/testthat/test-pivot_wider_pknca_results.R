library(testthat)
library(PKNCA)
library(dplyr)
library(tidyr)


# Create PKNCA results object from 0 with sample concentration and dose datasets
# Additional conditions to PKNCA assumptions need to be made:
# 1) CDISC denomination of actual and nominal time variables (AFRLT, ARRLT, NFRLT, NRRLT)
# 2) For the intervals create a column (type_interval) that differentiates between
# custom AUC ranges ("manual") and main parameter calculations ("main")
# 3) There are PPSTRES and PPSTRESU variables in the PKNCA results output
# 4) start_dose & end_dose columns expressing when the actual start and actual end
# of the dose happened. The time reference is the first dose given to the subject.
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
           c(100, 80, 60, 40, 20)) # Adjusted values for 5 points
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
myres <- PKNCA::pk.nca(
  PKNCA::PKNCAdata(
    data.conc = PKNCA::PKNCAconc(conc_data, AVAL~AFRLT|ID),
    data.dose = PKNCA::PKNCAdose(dose_data, DOSE~AFRLT|ID),
    intervals = rbind(
      main_intervals,
      auc_intervals
    ),
    options = list(keep_interval_cols = c("DOSNO", "type_interval")),
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


describe("pivot_wider_pknca_results", {

  it("reshapes PKNCA results correctly", {
    # Apply pivot_wider_pknca_results
    result <- pivot_wider_pknca_results(myres)

    # Check that the result is a data frame
    expect_s3_class(result, "data.frame")

    # Check that the result contains expected columns
    expected_columns <- c(
      "ID", "start", "end", "DOSNO", "AFRLT", "ARRLT", "NFRLT", "NRRLT", "DOSE",
      "route", "duration", "cmax[ng/mL]", "tmax[hr]", "tlast[hr]", "lambda.z[1/hr]",
      "r.squared[unitless]", "adj.r.squared[unitless]", "lambda.z.time.first[hr]",
      "lambda.z.n.points[count]", "clast.pred[ng/mL]", "half.life[hr]",
      "span.ratio[fraction]", "Exclude"
    )
    expect_true(all(expected_columns %in% colnames(result)))

    # Check the labels for the parameters are in the result
    expected_labels <- 

    # Check that the result contains expected values for a few key columns
    options <- result  %>%
      select(ID, DOSNO) %>%
      distinct()
    for (i in 1:nrow(options)) {
      id <- options$ID[i]
      dosno <- options$DOSNO[i]

      # Make a loop of the different id and dosno to check if the results are the expected
      sub_conc_data <- conc_data  %>%
        filter(ID == id, DOSNO == dosno)

      actual_result <- result %>%
        filter(ID == id, DOSNO == dosno) %>%
        formatters::var_labels_remove()

      expect_equal(actual_result[["cmax[ng/mL]"]], max(sub_conc_data$AVAL))
      expect_equal(actual_result[["tmax[hr]"]], sub_conc_data$ARRLT[which.max(sub_conc_data$AVAL)])
      expect_equal(actual_result[["lambda.z[1/hr]"]],
                  PKNCA::pk.calc.half.life(sub_conc_data$AVAL,
                                            sub_conc_data$AFRLT)$lambda.z,
                                            tolerance = 1e-3)
    }
  })

  it("handles manual AUC intervals correctly", {
    # Check if manual AUC intervals are included
    result <- pivot_wider_pknca_results(myres)
    manual_auc_columns <- grep("^AUCINT_", colnames(result), value = TRUE)
    expect_true(length(manual_auc_columns) > 0)
  })

  it("rounds numeric values to three decimals", {
    result <- pivot_wider_pknca_results(myres)
    numeric_columns <- sapply(result, is.numeric)
    rounded_values <- result[, numeric_columns]
    expect_true(all(apply(rounded_values, 2, function(x) all(abs(x - round(x, 3)) < 1e-6))))
  })

  it("adds appropriate labels to columns", {
    result <- pivot_wider_pknca_results(myres)
    expect_true(!is.null(attr(result$`cmax[ng/mL]`, "label")))
    expect_equal(attr(result$`cmax[ng/mL]`, "label"), "Maximum concentration [ng/mL]")
  })

  it("validates variable labels in the result", {
    result <- pivot_wider_pknca_results(myres)
    labels <- var_labels(result)

    expected_labels <- c(
      ID = NA, start = NA, end = NA, DOSNO = NA, AFRLT = NA, ARRLT = NA,
      NFRLT = NA, NRRLT = NA, DOSE = NA, route = NA, duration = NA,
      `cmax[ng/mL]` = "cmax", `tmax[hr]` = "tmax", `tlast[hr]` = "tlast",
      `lambda.z[1/hr]` = "lambda.z", `r.squared[unitless]` = "r.squared",
      `adj.r.squared[unitless]` = "adj.r.squared",
      `lambda.z.time.first[hr]` = "lambda.z.time.first",
      `lambda.z.n.points[count]` = "lambda.z.n.points",
      `clast.pred[ng/mL]` = "clast.pred", `half.life[hr]` = "half.life",
      `span.ratio[fraction]` = "span.ratio", Exclude = NA
    )

    expect_equal(labels, expected_labels)
  })

})
