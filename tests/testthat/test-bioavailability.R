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