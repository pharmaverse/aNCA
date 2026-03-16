selector_label <- function(choices, metadata_type = c("variable", "parameter")) {
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


describe("selector_label", {
  it("returns formatted list for variables", {
    result <- selector_label(choices = "AGE", "variable")

    expect_equal(length(result), 1)
    expect_equal(result[[1]]$value, "AGE")
    expect_equal(result[[1]]$description, "Age")
  })

  it("returns formatted list for parameters", {
    result <- selector_label(choices = "CMAX", "parameter")

    expect_equal(length(result), 1)
    expect_equal(result[[1]]$value, "CMAX")
    expect_equal(result[[1]]$description, "Max Conc")
  })

  it("handles missing/empty choices gracefully", {
    result <- selector_label(choices = "FAKE", "variable")
    expect_equal(length(result), 0)
  })

  it("drops choices not found in metadata", {
    result <- selector_label(
      c("USUBJID", "NOT_A_REAL_VARIABLE"),
      "variable"
    )
    vals <- vapply(result, function(x) x$value, character(1))
    expect_true("USUBJID" %in% vals)
    expect_false("NOT_A_REAL_VARIABLE" %in% vals)
  })

  it("returns empty list for empty input", {
    result <- selector_label(character(0), "variable")
    expect_equal(result, list())
  })

  it("deduplicates variables that appear multiple times in metadata", {
    # Some variables appear in multiple Dataset rows
    result <- selector_label("USUBJID", "variable")
    vals <- vapply(result, function(x) x$value, character(1))
    expect_equal(sum(vals == "USUBJID"), 1)
  })

  it("handles multiple parameters", {
    result <- selector_label(c("CMAX", "TMAX", "AUCLST"), "parameter")
    vals <- vapply(result, function(x) x$value, character(1))
    expect_true(all(c("CMAX", "TMAX", "AUCLST") %in% vals))
  })

  it("drops parameters not in metadata", {
    result <- selector_label(
      c("CMAX", "FAKE_PARAM"),
      "parameter"
    )
    vals <- vapply(result, function(x) x$value, character(1))
    expect_true("CMAX" %in% vals)
    expect_false("FAKE_PARAM" %in% vals)
  })

  it("returns empty list for unrecognized parameters", {
    result <- selector_label("ZZZZZ", "parameter")
    expect_equal(result, list())
  })

  it("label and value are identical (variable name used for both)", {
    result <- selector_label("USUBJID", "variable")
    expect_equal(result[[1]]$label, result[[1]]$value)
  })

  it("all values are character strings", {
    result <- selector_label(c("USUBJID", "AVAL"), "variable")
    for (item in result) {
      expect_type(item$label, "character")
      expect_type(item$value, "character")
      expect_type(item$description, "character")
    }
  })

  it("rejects invalid metadata_type", {
    expect_error(
      selector_label("USUBJID", "invalid_type"),
      "arg"
    )
  })
})
