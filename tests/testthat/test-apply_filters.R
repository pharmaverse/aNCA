describe("apply_filters works correctly", {
  raw_data <- mtcars

  test_that("apply_filters works with ==", {
    filter <- list(
      fitler1 = list(
        column = "mpg",
        condition = "==",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(raw_data, filter)), 2)
  })

  test_that("apply_fitlers works with >", {
    filter <- list(
      fitler1 = list(
        column = "mpg",
        condition = ">",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(raw_data, filter)), 12)
  })

  test_that("apply_fitlers works with <", {
    filter <- list(
      fitler1 = list(
        column = "mpg",
        condition = "<",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(raw_data, filter)), 18)
  })

  test_that("apply_fitlers works with >=", {
    filter <- list(
      fitler1 = list(
        column = "mpg",
        condition = ">=",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(raw_data, filter)), 14)
  })

  test_that("apply_fitlers works with <=", {
    filter <- list(
      fitler1 = list(
        column = "mpg",
        condition = "<=",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(raw_data, filter)), 20)
  })

  test_that("apply_filters works with compound filters", {
    filter <- list(
      fitler1 = list(
        column = "mpg",
        condition = "==",
        value = "21"
      ),
      filter2 = list(
        column = "wt",
        condition = "==",
        value = "2.875"
      )
    )

    expect_equal(nrow(apply_filters(raw_data, filter)), 1)

    filter <- list(
      fitler1 = list(
        column = "mpg",
        condition = ">",
        value = "21"
      ),
      filter2 = list(
        column = "disp",
        condition = "<",
        value = "100"
      )
    )

    expect_equal(nrow(apply_filters(raw_data, filter)), 5)
  })


})