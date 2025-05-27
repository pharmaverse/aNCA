describe("exclude_nca_by_param", {

  # Define the input
  my_result <- FIXTURE_PKNCA_RES %>%
    filter(USUBJID == 2) %>%
    mutate(PPTESTCD = translate_terms(PPTESTCD, "PPTESTCD", "PKNCA")) %>%
    filter(PPTESTCD %in% c("r.squared", "span.ratio"))

  it("excludes rows based on min_thr", {
    res_min_excluded <- PKNCA::exclude(
      my_result,
      FUN = exclude_nca_by_param("span.ratio", min_thr = 100)
    )
    expect_equal(
      as.data.frame(res_min_excluded)$exclude,
      c(NA, "Span ratio < 100", NA, "Span ratio < 100")
    )
  })

  it("does not exclude rows when min_thr is not met", {
    res_min_not_excluded <- PKNCA::exclude(
      my_result,
      FUN = exclude_nca_by_param("span.ratio", min_thr = 0.01)
    )
    expect_equal(
      as.data.frame(res_min_not_excluded)$exclude,
      rep(NA_character_, 4)
    )
  })

  it("excludes rows based on max_thr", {
    res_max_excluded <- PKNCA::exclude(
      my_result,
      FUN = exclude_nca_by_param("span.ratio", max_thr = 0.01)
    )
    expect_equal(
      as.data.frame(res_max_excluded)$exclude,
      c(NA, "Span ratio > 0.01", NA, "Span ratio > 0.01")
    )
  })

  it("does not exclude rows when max_thr is not exceeded", {
    res_max_not_excluded <- PKNCA::exclude(
      my_result,
      FUN = exclude_nca_by_param("span.ratio", max_thr = 100)
    )
    expect_equal(
      as.data.frame(res_max_not_excluded)$exclude,
      rep(NA_character_, 4)
    )
  })

  it("throws an error for invalid min_thr", {
    expect_error(
      exclude_nca_by_param("span.ratio", min_thr = "invalid"),
      "when defined min_thr must be a single numeric value"
    )
  })

  it("throws an error for invalid max_thr", {
    expect_error(
      exclude_nca_by_param(parameter = "span.ratio", max_thr = c(1, 2)),
      "when defined max_thr must be a single numeric value"
    )
  })

  it("throws an error when min_thr is greater than max_thr", {
    expect_error(
      exclude_nca_by_param("span.ratio", min_thr = 10, max_thr = 5),
      "if both defined min_thr must be less than max_thr"
    )
  })

  it("returns the original object when the parameter is not found", {
    res <- PKNCA::exclude(my_result, FUN = exclude_nca_by_param("nonexistent", min_thr = 0))
    expect_true(all(is.na(as.data.frame(res)$exclude)))
  })

  it("returns the object when the parameter's value is NA", {
    my_result_na <- my_result
    my_result_na$result$PPORRES <- NA
    res <- PKNCA::exclude(
      my_result_na,
      FUN = exclude_nca_by_param("span.ratio", min_thr = 0)
    )
    expect_true(all(is.na(as.data.frame(res)$exclude)))
  })

  # This should never happen in real code
  it("produces an error when more than 1 PPORRES is per parameter", {
    expect_error(
      exclude_nca_by_param(
        param = "r.squared",
        min_thr = 0.7
      )(data.frame(PPTESTCD = "r.squared", PPORRES = c(1, 1))),
      regexp = "Should not see more than one r.squared (please report this as a bug)",
      fixed = TRUE
    )
  })
})
