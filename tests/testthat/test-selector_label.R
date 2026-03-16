format_selector_choices <- function(choices, metadata_type = c("variable", "parameter")) {
  metadata_type <- match.arg(metadata_type)

  if (metadata_type == "variable") {
    choices_df <- metadata_nca_variables %>%
      select(Variable, Label) %>%
      distinct(Variable, .keep_all = TRUE) %>%
      filter(!is.na(Variable), Variable != "") %>%
      filter(Variable %in% choices) %>%
      rename(val = Variable, desc = Label)
  } else {
    choices_df <- metadata_nca_parameters %>%
      select(PPTESTCD, PPTEST) %>%
      distinct(PPTESTCD, .keep_all = TRUE) %>%
      filter(!is.na(PPTESTCD), PPTESTCD != "") %>%
      filter(PPTESTCD %in% choices) %>%
      rename(val = PPTESTCD, desc = PPTEST)
  }

  unname(purrr::pmap(list(choices_df$val, choices_df$desc), function(var, lab) {
    list(label = as.character(var),
         value = as.character(var),
         description = as.character(lab))
  }))
}


describe("format_selector_choices", {
  it("returns formatted list for variables", {
    result <- format_selector_choices(choices = "AGE", "variable")

    expect_equal(length(result), 1)
    expect_equal(result[[1]]$value, "AGE")
    expect_equal(result[[1]]$description, "Age")
  })

  it("returns formatted list for parameters", {
    result <- format_selector_choices(choices = "CMAX", "parameter")

    expect_equal(length(result), 1)
    expect_equal(result[[1]]$value, "CMAX")
    expect_equal(result[[1]]$description, "Max Conc")
  })

  it("handles missing/empty choices gracefully", {
    result <- format_selector_choices(choices = "FAKE", "variable")
    expect_equal(length(result), 0)
  })

  it("drops choices not found in metadata", {
    result <- format_selector_choices(
      c("USUBJID", "NOT_A_REAL_VARIABLE"),
      "variable"
    )
    vals <- vapply(result, function(x) x$value, character(1))
    expect_true("USUBJID" %in% vals)
    expect_false("NOT_A_REAL_VARIABLE" %in% vals)
  })

  it("returns empty list for empty input", {
    result <- format_selector_choices(character(0), "variable")
    expect_equal(result, list())
  })

  it("deduplicates variables that appear multiple times in metadata", {
    # Some variables appear in multiple Dataset rows
    result <- format_selector_choices("USUBJID", "variable")
    vals <- vapply(result, function(x) x$value, character(1))
    expect_equal(sum(vals == "USUBJID"), 1)
  })

  it("handles multiple parameters", {
    result <- format_selector_choices(c("CMAX", "TMAX", "AUCLST"), "parameter")
    vals <- vapply(result, function(x) x$value, character(1))
    expect_true(all(c("CMAX", "TMAX", "AUCLST") %in% vals))
  })

  it("drops parameters not in metadata", {
    result <- format_selector_choices(
      c("CMAX", "FAKE_PARAM"),
      "parameter"
    )
    vals <- vapply(result, function(x) x$value, character(1))
    expect_true("CMAX" %in% vals)
    expect_false("FAKE_PARAM" %in% vals)
  })

  it("returns empty list for unrecognized parameters", {
    result <- format_selector_choices("ZZZZZ", "parameter")
    expect_equal(result, list())
  })

  it("label and value are identical (variable name used for both)", {
    result <- format_selector_choices("USUBJID", "variable")
    expect_equal(result[[1]]$label, result[[1]]$value)
  })

  it("all values are character strings", {
    result <- format_selector_choices(c("USUBJID", "AVAL"), "variable")
    for (item in result) {
      expect_type(item$label, "character")
      expect_type(item$value, "character")
      expect_type(item$description, "character")
    }
  })

  it("rejects invalid metadata_type", {
    expect_error(
      format_selector_choices("USUBJID", "invalid_type"),
      "arg"
    )
  })
})
