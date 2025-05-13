# Sample concentration data for testing with three doses
conc_data <- data.frame(
  USUBJID = c("001", "001", "001", "001", "001", "001", "001", "001", "001", "001"),
  AVAL = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  DOSNOA = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3),
  ARRLT = c(-1, 0, 1, -1, 0, 1, 2, 0, 1, 2),
  AFRLT = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8),
  NRRLT = c(-1, 0, 1, -1, 0, 1, 2, 0, 1, 2),
  NFRLT = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8)
)

result <- dose_profile_duplicates(conc_data, groups = c("USUBJID", "DOSNOA"), dosno = "DOSNOA")

describe("dose_profile_duplicates", {

  it("should maintain the original columns", {
    expect_true(all(colnames(conc_data) %in% colnames(result)))
  })

  it("should add predose duplicates correctly", {
    predose_rows <- result[result$DOSNOA == 1, ]
    expect_equal(nrow(predose_rows), 4)
  })

  it("should add last dose values when predose is missing", {
    last_dose_rows <- result[result$DOSNOA == 3, ]
    expect_equal(nrow(last_dose_rows), 4)
  })

  it("should return data with IX column if only one dose is present", {
    single_dose_data <- conc_data %>%
      filter(DOSNOA == 1)

    result_single_dose <- dose_profile_duplicates(single_dose_data,
                                                  groups = c("USUBJID", "DOSNOA"),
                                                  dosno = "DOSNOA")
    expect_true("IX" %in% colnames(result_single_dose))
  })
})
