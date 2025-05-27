describe("exclude_nca_by_param", {
  
  # Define the input
  my_result <- FIXTURE_PKNCA_RES %>%
    filter(USUBJID == 2) %>%
    mutate(PPTESTCD = translate_terms(PPTESTCD, "PPTESTCD", "PKNCA")) %>%
    filter(PPTESTCD %in% c("r.squared", "span.ratio"))

  it("excludes rows based on min_value", {
    res_min_excluded <- PKNCA::exclude(
      my_result,
      FUN = exclude_nca_by_param("span.ratio", min_value = 100)
    )
    expect_equal(
      as.data.frame(res_min_excluded)$exclude,
      c(NA, "Span ratio < 100", NA, "Span ratio < 100")
    )
  })

  it("does not exclude rows when min_value is not met", {
    res_min_not_excluded <- PKNCA::exclude(my_result, FUN = exclude_nca_by_param("span.ratio", min_value = 0.01))
    expect_equal(
      as.data.frame(res_min_not_excluded)$exclude,
      rep(NA_character_, 4)
    )
  })

  it("excludes rows based on max_value", {
    res_max_excluded <- PKNCA::exclude(
      my_result,
      FUN =exclude_nca_by_param("span.ratio", max_value = 0.01)
    )
    expect_equal(
      as.data.frame(res_max_excluded)$exclude,
      c(NA, "Span ratio > 0.01", NA, "Span ratio > 0.01")
    )
  })

  it("does not exclude rows when max_value is not exceeded", {
    res_max_not_excluded <- PKNCA::exclude(
      my_result,
      FUN = exclude_nca_by_param("span.ratio", max_value = 100)
    )
    expect_equal(
      as.data.frame(res_max_not_excluded)$exclude,
      rep(NA_character_, 4)
    )
  })

  it("throws an error for invalid min_value", {
    expect_error(
      exclude_nca_by_param("span.ratio", min_value = "invalid"),
      "when defined min_value must be single numeric values"
    )
  })

  it("throws an error for invalid max_value", {
    expect_error(
      exclude_nca_by_param(parameter = "span.ratio", max_value = c(1, 2)),
      "when defined max_value must be single numeric values"
    )
  })

  it("throws an error when min_value is greater than max_value", {
    expect_error(
      exclude_nca_by_param("span.ratio", min_value = 10, max_value = 5),
      "if both defined min_value must be less than max_value"
    )
  })

  it("returns the original object when the parameter is not found", {
    res <- PKNCA::exclude(my_result, FUN = exclude_nca_by_param("nonexistent", min_value = 0))
    expect_true(all(is.na(as.data.frame(res)$exclude)))
  })

  it("returns the object when the parameter's value is NA", {
    my_result_na <- my_result
    my_result_na$result$PPORRES <- NA
    res <- PKNCA::exclude(my_result_na, FUN = exclude_nca_by_param("span.ratio", min_value = 0))
    expect_true(all(is.na(as.data.frame(res)$exclude)))
  })

  # This should never happen in real code
  it("produces an error when more than 1 PPORRES is per parameter", {
    expect_error(
      exclude_nca_by_param(param = "r.squared", min_value = 0.7)(data.frame(PPTESTCD = "r.squared", PPORRES = c(1, 1))),
      regexp = "Should not see more than one r.squared (please report this as a bug)",
      fixed = TRUE
    )
  })
})
