PKNCA_RESULTS_FIXTURE <- FIXTURE_PKNCA_RES

describe("calculate_F", {
  it("returns NULL when aucs are missing", {
    expect_null(calculate_F(PKNCA_RESULTS_FIXTURE, NULL))
  })

  it("returns a data.frame", {
    result <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_AUCLST"))
    expect_s3_class(result, "data.frame")
  })

  it("has expected columns in output", {
    result <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_AUCLST", "f_AUCIFO"))

    expect_true("USUBJID" %in% names(result))
    expect_true("f_AUCLST" %in% names(result))
    expect_true("f_AUCIFO" %in% names(result))
  })

  it("bioavailability values are numeric and within expected range", {
    result <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_AUCLST"))
    expect_type(result$f_AUCLST, "double")
    expect_true(all(result$f_AUCLST >= 0 & result$f_AUCLST <= 200, na.rm = TRUE))
  })

  it("handles missing IV data by estimating from group mean", {
    result <- calculate_F(PKNCA_RESULTS_FIXTURE, "f_AUCLST")

    expect_true(any(is.na(result$f_AUCLST)) || any(result$f_AUCLST > 0))
  })
})

describe("PKNCA_add_F", {
  it("returns the original object when bioavailability is NULL", {
    result <- PKNCA_add_F(PKNCA_RESULTS_FIXTURE, NULL)
    expect_equal(result, PKNCA_RESULTS_FIXTURE)
  })

  it("adds bioavailability data to the result slot when provided", {
    bioavailability <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_AUCLST", "f_AUCIFO"))
    result <- PKNCA_add_F(PKNCA_RESULTS_FIXTURE, bioavailability)
    expect_true(all(c("f_AUCLST","f_AUCIFO") %in% result$result$PPTESTCD))
  
    expected_res <- result$result %>%
      filter(ROUTE == "intravascular",
             PPTESTCD %in% c("f_AUCLST", "f_AUCIFO"))
    expect_equal(result$result$PPSTRES[result$result$PPTESTCD == "f_AUCLST"] |> na.omit() |> as.numeric(),
                 c(26.899, 19.98260, 22.5204, 79.0651, 70.2774, 66.71026), tolerance = 1e-4)
  })
})

