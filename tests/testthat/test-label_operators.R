ADNCA_LABELS_FIXTURE <- data.frame(
  Variable = c("USUBJID", "AVAL", "RACE"),
  Label =  c("Unique Subject Identifier", "Analysis Value", "Race"),
  Dataset = c("ADPC", "ADPC", "ADPC")
)

data <- data.frame(
  USUBJID =  c("S1-1", "S1-2", "S1-3"),
  AVAL = c(10, 20, 30),
  RACE = as.factor(c("WHITE", "ASIAN", "ASIAN"))
)
describe("apply_labels", {
  labeled_data  <- expect_no_error(apply_labels(data, ADNCA_LABELS_FIXTURE, type = "ADPC"))
  it("applies labels to the data frame", {
    expect_equal(base::attr(labeled_data$USUBJID, "label"), "Unique Subject Identifier")
    expect_equal(base::attr(labeled_data$AVAL, "label"), "Analysis Value")
    expect_equal(base::attr(labeled_data$RACE, "label"), "Race")
  })

  it("uses column names as labels when the variable is not in the labels list", {
    EMPTY_ADNCA_LABELS_FIXTURE  <- data.frame(
      Variable = character(),
      Label = character(),
      Dataset = character()
    )
    labeled_data <- expect_no_error(apply_labels(data, EMPTY_ADNCA_LABELS_FIXTURE,
                                                 c("ADPC", "ADPC")))
    expect_equal(base::attr(labeled_data$USUBJID, "label"), "USUBJID")
    expect_equal(base::attr(labeled_data$AVAL, "label"), "AVAL")
    expect_equal(base::attr(labeled_data$RACE, "label"), "RACE")
  })


  it("does not change labels if already applied", {
    data_with_existing_labels <- data
    attr(data_with_existing_labels$USUBJID, "label") <- "Existing label for USUBJID"

    labeled_data <- apply_labels(data_with_existing_labels, ADNCA_LABELS_FIXTURE, "ADPC")
    expect_equal(base::attr(labeled_data$USUBJID, "label"), "Existing label for USUBJID")
    expect_equal(base::attr(labeled_data$AVAL, "label"), "Analysis Value")
  })
})

describe("get_label", {
  it("returns label of a heading if it exists in the label file", {
    expect_equal(get_label(ADNCA_LABELS_FIXTURE, "USUBJID", "ADPC"), "Unique Subject Identifier")
  })

  it("returns the variable name if the label does not exist", {
    expect_equal(get_label(ADNCA_LABELS_FIXTURE, "USUBJID", "ADP"), "USUBJID")
  })
})

mock_vec <- c("A", "B", "C")
attr(mock_vec, "label") <- "Example Label"

describe("as_factor_preserve_label", {
  mock_vector_as_factor <- expect_no_error(as_factor_preserve_label(mock_vec))
  it("returns object of class factor", {
    expect_s3_class(mock_vector_as_factor, "factor")
  })

  it("does not change the original label text", {
    old_label <- base::attr(mock_vector_as_factor, "label")
    new_label <- base::attr(as_factor_preserve_label(mock_vector_as_factor), "label")
    expect_equal(old_label, new_label)

  })
})

describe("has_label", {
  it("returns TRUE if has label", {
    expect_true(has_label(mock_vec))
  })
  it("returns FALSE if has no label", {
    expect_false(has_label("unlabeled_char"))
  })
})

describe("set_empty_label", {
  attr(mock_vec, "label")  <-  NULL
  expect_false(has_label(mock_vec))
  it("sets label to empty string if it does not exist", {
    mock_vec_unlabeled  <- set_empty_label(mock_vec)
    expect_identical("", base::attr(mock_vec_unlabeled, "label"))
  })
})

describe("generate_tooltip_text", {
  TEST_DATA <- data.frame(
    USUBJID = c("S1-1", "S1-2"),
    AVAL = c(10.5, NA_real_),
    RACE = as.factor(c("WHITE", "ASIAN"))
  )
  TEST_VARS <- c("USUBJID", "AVAL", "RACE")

  it("generates correct tooltip string for multiple rows and data types", {
    tooltips <- generate_tooltip_text(TEST_DATA, ADNCA_LABELS_FIXTURE, TEST_VARS, "ADPC")
    expected_output <- c(
      "<b>Unique Subject Identifier</b>: S1-1<br><b>Analysis Value</b>: 10.5<br><b>Race</b>: WHITE",
      "<b>Unique Subject Identifier</b>: S1-2<br><b>Analysis Value</b>: NA<br><b>Race</b>: ASIAN"
    )
    expect_equal(tooltips, expected_output)
  })

  it("returns an empty string for each row if tooltip_vars is empty", {
    tooltips <- generate_tooltip_text(TEST_DATA, ADNCA_LABELS_FIXTURE, character(0), "ADPC")
    expect_equal(tooltips, c("", ""))
  })

  it("returns an empty vector for data with zero rows", {
    empty_data <- TEST_DATA[0, ]
    tooltips <- generate_tooltip_text(empty_data, ADNCA_LABELS_FIXTURE, TEST_VARS, "ADPC")
    expect_equal(tooltips, character(0))
  })

  it("uses the variable name as a label if it's not in labels_df", {
    data_with_unlabeled_var <- TEST_DATA %>% mutate(AGE = c(45, 52))
    vars_with_unlabeled <- c("USUBJID", "AGE")
    expected_output <- c(
      "<b>Unique Subject Identifier</b>: S1-1<br><b>AGE</b>: 45",
      "<b>Unique Subject Identifier</b>: S1-2<br><b>AGE</b>: 52"
    )
    tooltips <- generate_tooltip_text(data_with_unlabeled_var, ADNCA_LABELS_FIXTURE,
                                      vars_with_unlabeled, "ADPC")
    expect_equal(tooltips, expected_output)
  })
})