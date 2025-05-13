# Load the testing data (PKNCA result)
pknca_res <- FIXTURE_PKNCA_RES
pknca_res$result <- pknca_res$result %>%
  filter(USUBJID %in% 1:6)

describe("pknca_calculate_f", {
  it("handles missing or unavailable AUCs gracefully", {
    expect_null(pknca_calculate_f(pknca_res, NULL))
    expect_error(
      pknca_calculate_f(pknca_res, c("f_AUCXXX", "f_AUCYYY")),
      "No AUC parameters \\(PPTESTCD\\) available for: AUCXXX, AUCYYY"
    )
  })

  result <- pknca_calculate_f(pknca_res, c("f_AUCLST"))

  it("returns a data.frame with expected format", {
    expect_s3_class(result, "data.frame")
    expect_true(all(names(result) %in% names(pknca_res$result)))
  })

  it("ensures bioavailability values are numeric and within the expected range", {
    actual_range <- range(result$PPORRES, na.rm = TRUE)
    expect_true(all(actual_range >= 0 & actual_range <= 100))
  })

  it("calculates ratios only when start and end match", {
    pknca_res_no_start_matching <- pknca_res
    pknca_res_no_start_matching$result <- pknca_res_no_start_matching$result %>%
      filter(PPTESTCD == "AUCLST") %>%
      mutate(
        start = case_when(
          ROUTE == "extravascular" & USUBJID == 1 ~ 1,
          ROUTE == "extravascular" & USUBJID == 2 ~ 2,
          TRUE ~ start
        )
      )

    result <- pknca_calculate_f(pknca_res, "f_AUCLST")
    result_no_start_matching <- pknca_calculate_f(pknca_res_no_start_matching, "f_AUCLST")

    expect_true(all(!is.na(result$PPSTRES[1:3])))
    expect_true(all(is.na(result_no_start_matching$PPSTRES[1:3])))
  })

  it("when needed handles unit conversions if possible for the ratio calculations", {
    pknca_res_diff_units <- pknca_res
    pknca_res_diff_units$result <- pknca_res$result %>%
      mutate(
        PPORRESU = case_when(
          ROUTE == "intravascular" & USUBJID %in% c(3, 4) ~ "hr*g/mL",
          ROUTE == "intravascular" ~ "hr*ng/mL",
          TRUE ~ PPSTRESU
        ),
        PPORRES = case_when(
          ROUTE == "intravascular" & USUBJID %in% c(3, 4) ~ PPORRES * 1e-9,
          TRUE ~ PPORRES
        )
      )

    result <- pknca_calculate_f(pknca_res, "f_AUCLST")
    result_diff_units <- pknca_calculate_f(pknca_res_diff_units, "f_AUCLST")

    expect_equal(result, result_diff_units)
  })

  it("uses intravascular and extravascular data for calculations when available", {
    pknca_res_ind_both_routes <- pknca_res
    pknca_res_ind_both_routes$result <- pknca_res_ind_both_routes$result %>%
      filter(USUBJID == 1, PPTESTCD == "AUCLST")

    pknca_res_ind_both_routes$result <- bind_rows(
      pknca_res_ind_both_routes$result,
      pknca_res_ind_both_routes$result %>% mutate(
        ROUTE = "intravascular",
        DOSNO = 2,
        PPORRES = PPORRES + 1
      ),
      pknca_res_ind_both_routes$result %>% mutate(
        ROUTE = "intravascular",
        DOSNO = 3,
        PPORRES = PPORRES - 1
      )
    )

    result <- pknca_calculate_f(pknca_res_ind_both_routes, "f_AUCLST")
    expect_equal(result$PPORRES[1], 100)
  })
})

describe("calculate_f", {
  it("returns a data.frame with one column per bioavailability parameter", {
    result <- calculate_f(pknca_res, c("f_AUCLST", "f_AUCIFO"))
    expect_s3_class(result, "data.frame")
    res_groups <- names(PKNCA::getGroups(pknca_res))
    expect_true(all(c("f_AUCLST[%]", "f_AUCIFO[%]") %in% names(result)))
  })
})
