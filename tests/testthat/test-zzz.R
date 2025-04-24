describe(".onLoad is indeed loading the variables", {
  # Capture the global variables set by .onLoad
  .onLoad()
  global_vars <- utils::globalVariables()

  # Check that specific variables are included
  expect_true("USUBJID" %in% global_vars)
  expect_true("PPTESTCD" %in% global_vars)
  expect_true("AFRLT" %in% global_vars)
})
