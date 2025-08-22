library(shinytest2)

test_that("End to End test pass", {
  # based on: https://rstudio.github.io/shinytest2/articles/use-package.html#applications-in-inst
  skip_on_cran()

  appdir <- system.file(package = "aNCA", "shiny")

  test_app(appdir)
})
