describe("apply_parameter_exclusions", {
  make_res <- function(n = 5) {
    list(result = data.frame(
      PPTESTCD = paste0("PARAM", seq_len(n)),
      PPSTRES = rnorm(n),
      stringsAsFactors = FALSE
    ))
  }

  it("returns result unchanged when excl_info is NULL", {
    res <- make_res()
    result <- apply_parameter_exclusions(res, NULL)
    expect_identical(result, res)
  })

  it("returns result unchanged when excl_info$indices is empty", {
    res <- make_res()
    result <- apply_parameter_exclusions(res, list(indices = integer(0), reasons = character(0)))
    expect_identical(result, res)
  })

  it("tags excluded rows with .pp_excl = TRUE and correct reason", {
    res <- make_res(3)
    excl <- list(indices = 2L, reasons = "outlier")
    result <- apply_parameter_exclusions(res, excl)
    expect_equal(result$result$.pp_excl, c(FALSE, TRUE, FALSE))
    expect_equal(result$result$.pp_excl_reason, c(NA, "outlier", NA))
  })

  it("tags multiple excluded rows correctly", {
    res <- make_res(5)
    excl <- list(indices = c(1L, 4L), reasons = c("reason1", "reason2"))
    result <- apply_parameter_exclusions(res, excl)
    expect_equal(result$result$.pp_excl, c(TRUE, FALSE, FALSE, TRUE, FALSE))
    expect_equal(result$result$.pp_excl_reason, c("reason1", NA, NA, "reason2", NA))
  })

  it("handles mismatched indices/reasons lengths gracefully", {
    res <- make_res(3)
    excl <- list(indices = 2L, reasons = c("a", "b"))
    result <- apply_parameter_exclusions(res, excl)
    expect_equal(result$result$.pp_excl, c(FALSE, TRUE, FALSE))
    # When lengths mismatch, reasons are not assigned (fallback to all NA)
    expect_equal(result$result$.pp_excl_reason, rep(NA_character_, 3))
  })

  it("preserves existing columns in res$result", {
    res <- make_res(2)
    orig_names <- names(res$result)
    excl <- list(indices = 1L, reasons = "reason")
    result <- apply_parameter_exclusions(res, excl)
    expect_true(all(orig_names %in% names(result$result)))
  })
})
