describe("apply_filters works correctly", {
  test_that("apply_filters works with ==", {
    filter <- list(
      list(
        column = "mpg",
        condition = "==",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(mtcars, filter)), 2)
  })

  test_that("apply_fitlers works with >", {
    filter <- list(
      list(
        column = "mpg",
        condition = ">",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(mtcars, filter)), 12)
  })

  test_that("apply_fitlers works with <", {
    filter <- list(
      list(
        column = "mpg",
        condition = "<",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(mtcars, filter)), 18)
  })

  test_that("apply_fitlers works with >=", {
    filter <- list(
      list(
        column = "mpg",
        condition = ">=",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(mtcars, filter)), 14)
  })

  test_that("apply_fitlers works with <=", {
    filter <- list(
      list(
        column = "mpg",
        condition = "<=",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(mtcars, filter)), 20)
  })

  test_that("apply_filters works with compound filters", {
    filter <- list(
      list(
        column = "mpg",
        condition = "==",
        value = "21"
      ),
      list(
        column = "wt",
        condition = "==",
        value = "2.875"
      )
    )

    expect_equal(nrow(apply_filters(mtcars, filter)), 1)
  })

  test_that("apply_filters works with named filters", {
    filter <- list(
      filter1 = list(
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

    expect_equal(nrow(apply_filters(mtcars, filter)), 5)
  })
})

describe("apply_filters throws errors for invalid input", {
  test_that("apply_filters throws error when filter misses required fields", {
    filter <- list(
      filter1 = list(
        column = "mpg",
        condition = ">"
      )
    )

    expect_error(
      apply_filters(mtcars, filter),
      "Missing required filter fields: value"
    )
  })

  test_that("apply_filter throws error if column does not exist in data", {
    filter <- list(
      filter1 = list(
        column = "nonexistant",
        condition = ">",
        value = "21"
      )
    )

    expect_error(
      apply_filters(mtcars, filter),
      "Data is missing filtered column: nonexistant"
    )
  })
})
