## ------------------------  Placeholder for test-fliter_breaks.R

?apply_labels
?pckg01

# from R/g_pkconc_ind.R   example

   adpc <- read.csv("inst/shiny/data/DummyRO_ADNCA.csv")
   attr(adpc[["AFRLT"]], "label") <- "Actual time from first dose"
   attr(adpc[["AVAL"]], "label") <- "Analysis val"

   plots_lin <- pckg01(adpc = adpc, xmax = 1)
   plots_log <- pckg01(adpc = adpc, color_var = "USUBJID", scale = "LOG")
   plots_sbs <- pckg01(
     adpc = adpc,
     color_var = "USUBJID",
     xbreaks_var = "NFRLT",
     xmin = 100,
     xmax = 1000,
     scale = "SBS"
   )
 }
###
p  <- ggplot(mtcars, aes(mpg)) +
  geom_histogram()

filter_breaks(plot = p)

### ------------------------  ignore ...

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
