#------------------------  apply_labels, get_label

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
    labeled_data <- expect_no_error(apply_labels(data, EMPTY_ADNCA_LABELS_FIXTURE, c("ADPC", "ADPC")))
    expect_equal(base::attr(labeled_data$USUBJID, "label"), "USUBJID")
    expect_equal(base::attr(labeled_data$AVAL, "label"), "AVAL")
    expect_equal(base::attr(labeled_data$RACE, "label"), "RACE")
  })
})

describe("get_label", {
  # Test for successful label retrieval
  it("returns label of a heading if it exists in the label file", {
    expect_equal(get_label(ADNCA_LABELS_FIXTURE, "USUBJID", "ADPC"), "Unique Subject Identifier")
  })

  # Test for fallback behavior when no label is available
  it("returns 'No label available' if the label does not exist", {
    expect_equal(get_label(ADNCA_LABELS_FIXTURE, "USUBJID", "ADP"), "No label available")
  })
})

#------------------------  as_factor_preserve_label, has_label, set_empty_label
vec <- c("A", "B", "C")
attr(vec, "label") <- "Example Label"

describe("as_factor_preserve_label", {
  it("returns object of class factor", {
    expect_equal("factor", class(as_factor_preserve_label(vec)))
  })

  it("returns ... ", {
    old_label <- base::attr(vec, "label")
    new_label <- base::attr(as_factor_preserve_label(vec), "label")
    expect_equal(old_label, new_label)

  })
})

describe("has_label", {
  it("returns TRUE if has label; FALSE otherwise", {
    expect_true(has_label(vec))
  })
})

describe("set_empty_label", {
  it("set empty label if none exists now", {
    vec_unlabeled  <- c("A", "B", "C")
    vec_unlabeled  <- set_empty_label(vec_unlabeled)
    expect_identical("", base::attr(vec_unlabeled, "label"))
  })
})

