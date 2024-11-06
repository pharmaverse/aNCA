DATA_FIXTURE <- list(
  conc = list(
    data = data.frame(
      STUDYID = 1,
      PCSPEC = 1,
      USUBJID = rep(1:4, each = 4),
      DOSNO = 1,
      IX = rep(1:4, times = 4),
      is.included.hl = FALSE,
      is.excluded.hl = FALSE,
      exclude_half.life = FALSE,
      REASON = ""
    )
  )
)

PROFILES_FIXTURE <- list(
  "1" = list(1),
  "2" = list(1),
  "3" = list(1),
  "4" = list(1)
)

describe(".filter_slopes", {
  it("should handle slope selection", {
    selection <- data.frame(
      TYPE = rep("Selection", 2),
      PATIENT = c(1, 3),
      PROFILE = c(1, 1),
      IXrange = c("1:3", "2:4"),
      REASON = "Test selection"
    )

    res <- .filter_slopes(DATA_FIXTURE, selection, PROFILES_FIXTURE)
    expect_true(all(res$is.included.hl[c(1:3, 6:8)]))
    expect_true(all(res$REASON[c(1:3, 6:8)] == "Test selection"))
  })

  it("should handle slope exclusion", {
    exclusion <- data.frame(
      TYPE = rep("Exclusion", 2),
      PATIENT = c(2, 4),
      PROFILE = c(1, 1),
      IXrange = c("1:2", "2:3"),
      REASON = "Test exclusion"
    )

    res <- .filter_slopes(DATA_FIXTURE, exclusion, PROFILES_FIXTURE)
    expect_true(all(res$is.excluded.hl[c(5, 6, 14, 15)]))
    expect_true(all(res$REASON[c(5, 6, 14, 15)] == "Test exclusion"))
  })

  it("should throw an error for invalid data", {
    expect_error(.filter_slopes(NULL, NULL, PROFILES_FIXTURE), "Please provide valid data.")
    expect_error(.filter_slopes(list(), NULL, PROFILES_FIXTURE), "Please provide valid data.")
    expect_error(
      .filter_slopes(list(conc = list()), NULL, PROFILES_FIXTURE), "Please provide valid data."
    )
    expect_error(
      .filter_slopes(list(conc = list()), NULL, PROFILES_FIXTURE), "Please provide valid data."
    )
  })
})