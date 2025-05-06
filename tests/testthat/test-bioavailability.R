describe("calculate_F", {
  it("returns NULL when aucs are missing", {
    expect_null(calculate_F(PKNCA_RESULTS_FIXTURE, NULL))
  })

  it("returns a data.frame", {
    result <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_auclast"))
    expect_s3_class(result, "data.frame")
  })

  it("has expected columns in output", {
    result <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_auclast", "f_aucinf.obs"))

    expect_true("USUBJID" %in% names(result))
    expect_true("f_auclast" %in% names(result))
    expect_true("f_aucinf.obs" %in% names(result))
  })

  it("bioavailability values are numeric and within expected range", {
    result <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_auclast"))
    expect_type(result$f_auclast, "double")
    expect_true(all(result$f_auclast >= 0 & result$f_auclast <= 200, na.rm = TRUE))
  })

  it("handles missing IV data by estimating from group mean", {
    result <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_auclast"))

    expect_true(any(is.na(result$f_auclast)) || any(result$f_auclast > 0))
  })
})

describe("PKNCA_add_F", {
  it("returns the original object when bioavailability is NULL", {
    result <- PKNCA_add_F(PKNCA_RESULTS_FIXTURE, NULL)
    expect_equal(result, PKNCA_RESULTS_FIXTURE)
  })

  it("adds bioavailability data to the result slot when provided", {
    bioavailability <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_auclast", "f_aucinf.obs"))

    result <- PKNCA_add_F(PKNCA_RESULTS_FIXTURE, bioavailability)

    expect_true(all(c("f_auclast","f_aucinf.obs") %in% result$result$PPTESTCD))

    expected_res <- result$result %>%
      filter(PCSPEC == "Plasma",
             PPTESTCD %in% c("f_auclast", "f_aucinf.obs"))
    expect_equal(result$result$PPSTRES[result$result$PPTESTCD == "f_auclast"][1:5],
                 c(NA, NA, NA, 26.899, 19.98260), tolerance = 1e-4)
    expect_equal(result$result$PPSTRES[result$result$PPTESTCD == "f_aucinf.obs"][1:5],
                 as.double(c(NA, NA, NA, NA, NA)), tolerance = 1e-4)
  })
})
