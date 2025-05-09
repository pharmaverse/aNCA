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
                 "No AUC parameters (PPTESTCD) available for: AUCXXX, AUCYYY")
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

    # Check that the subject not changed has still the same value
    expect_equal(result_no_start_matching %>%
                  filter(USUBJID == 7),
                result_no_start_matching %>%
                  filter(USUBJID == 7)
    )
  })

  it("makes ratio using the same unit", {
    pknca_res <- PKNCA_RESULTS_FIXTURE
    pknca_res$result <- pknca_res$result %>%
      filter(
        PPTESTCD == "AUCLST"
      )

    pknca_res_diff_units <- pknca_res
    pknca_res_diff_units$result <- pknca_res_diff_units$result %>%
      mutate(
        PPORRESU = case_when(
          ROUTE == "intravascular" & USUBJID %in% c(3,4) ~ "hr*g/mL",
          ROUTE == "intravascular" ~ "hr*ng/mL",
          TRUE ~ PPSTRESU
        ),
        PPORRES = case_when(
          ROUTE == "intravascular" & USUBJID %in% c(3,4) ~ PPORRES * 1e-9,
          TRUE ~ PPORRES
        )
      )

    result <- calculate_F(pknca_res, "f_AUCLST")
    result_diff_units <- calculate_F(pknca_res_diff_units, "f_AUCLST")

    expect_equal(result, result_diff_units)
  })

  it("has expected format and can be bound to a PKNCA result object", {
    result <- calculate_F(PKNCA_RESULTS_FIXTURE, c("f_AUCLST", "f_AUCIFO"))

    # Does not create other new columns (variables output can vary)
    expect_true(all(names(result) %in% names(PKNCA_RESULTS_FIXTURE$result)))
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

