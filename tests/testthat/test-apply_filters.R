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

  test_that("apply_filters works with !=", {
    filter <- list(
      list(
        column = "mpg",
        condition = "!=",
        value = "21"
      )
    )

    expect_equal(nrow(apply_filters(mtcars, filter)), 30)
  })

  test_that("apply_filters works with min-max", {
    filter <- list(
      filter1  <- list(
        column = "mpg",
        condition = "min-max",
        value = c("15", "20")
      )
    )
    expect_equal(nrow(apply_filters(mtcars, filter)), 13)
  })

  test_that("apply_filters warns with min-max argument NA", {
    filter <- list(
      filter2  <- list(
        column = "mpg",
        condition = "min-max",
        value = c("15", NA)
      )
    )
    expect_warning(apply_filters(mtcars, filter))
  })

  test_that("apply_filters skips NULL filters", {
    filters_with_null <- list(
      list(
        column = "mpg",
        condition = ">",
        value = "21"
      ),
      NULL
    )

    expect_equal(nrow(apply_filters(mtcars, filters_with_null)), 12)
  })

})

describe("apply_filters handles (NA) sentinel", {
  it("== with (NA) includes rows where column is NA", {
    df <- data.frame(x = c("a", "b", NA, NA, "a"), stringsAsFactors = FALSE)
    filter <- list(list(column = "x", condition = "==", value = "(NA)"))
    result <- apply_filters(df, filter)
    expect_equal(nrow(result), 2)
    expect_true(all(is.na(result$x)))
  })

  it("== with (NA) and other values includes both", {
    df <- data.frame(x = c("a", "b", NA, NA, "a"), stringsAsFactors = FALSE)
    filter <- list(list(column = "x", condition = "==", value = c("a", "(NA)")))
    result <- apply_filters(df, filter)
    expect_equal(nrow(result), 4)
  })

  it("!= with (NA) keeps NA rows and excludes matched values", {
    df <- data.frame(x = c("a", "b", NA, "a"), stringsAsFactors = FALSE)
    filter <- list(list(column = "x", condition = "!=", value = c("a", "(NA)")))
    result <- apply_filters(df, filter)
    expect_equal(nrow(result), 2)
    expect_equal(result$x, c("b", NA))
  })

  it("!= with only (NA) returns all rows", {
    df <- data.frame(x = c("a", "b", NA), stringsAsFactors = FALSE)
    filter <- list(list(column = "x", condition = "!=", value = "(NA)"))
    result <- apply_filters(df, filter)
    expect_equal(nrow(result), 3)
  })

  it("== without (NA) excludes NA rows", {
    df <- data.frame(x = c("a", "b", NA), stringsAsFactors = FALSE)
    filter <- list(list(column = "x", condition = "==", value = "a"))
    result <- apply_filters(df, filter)
    expect_equal(nrow(result), 1)
    expect_equal(result$x, "a")
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
