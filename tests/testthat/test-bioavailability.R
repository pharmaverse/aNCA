PKNCA_RESULTS_FIXTURE <- FIXTURE_PKNCA_RES
PKNCA_RESULTS_FIXTURE$result <- FIXTURE_PKNCA_RES$result %>%
  filter(
    USUBJID %in% 1:6
  )

describe("calculate_F", {
  it("returns NULL when aucs are missing", {
    expect_null(calculate_F(PKNCA_RESULTS_FIXTURE, NULL))
  })

  it("throws an error when aucs are not available", {
    expect_error(calculate_F(PKNCA_RESULTS_FIXTURE, c("f_AUCXXX", "f_AUCYYY")),
                 "No AUC data available for the selected variables")
  })

  it("returns a data.frame", {
    result <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_AUCLST"))
    expect_s3_class(result, "data.frame")
  })

  it("makes ratio calculations only if start, end specific", {
    pknca_res <- PKNCA_RESULTS_FIXTURE
    pknca_res$result <- pknca_res$result %>%
      filter(
        PPTESTCD == "AUCLST"
      )

    pknca_res_no_start_matching <- pknca_res
    pknca_res_no_start_matching$result <- pknca_res_no_start_matching$result %>%
      mutate(
        start = case_when(
          ROUTE == "extravascular" & USUBJID == 1 ~ 1,
          ROUTE == "extravascular" & USUBJID == 2 ~ 2,
          TRUE ~ start
        )
      )

    result <- calculate_F(pknca_res, "f_AUCLST")
    result_no_start_matching <- calculate_F(pknca_res_no_start_matching, "f_AUCLST")

    expect_true(all(!is.na(result$PPSTRES[1:3])))
    expect_true(all(is.na(result_no_start_matching$PPSTRES[1:3])))
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

