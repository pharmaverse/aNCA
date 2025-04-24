LABELS <- data.frame(
   Variable = c("USUBJID", "AVAL"),
   Label = c("Unique Subject Identifier", "Analysis Value"),
   Dataset = c("ADPC", "ADPC")
)

  #  I added next line (data <- )
  data <- data.frame(USUBJID = 1:3, AVAL = c(10, 20, 30))
  data <- apply_labels(data, LABELS, c("ADPC", "ADPC"))
  print(base::attr(data$A, "label"))
   
#------------------------  apply_labels

describe("apply_labels", {
  
  # Test for successful label application
  it("applies labels to the data frame", {
    data <- data.frame(USUBJID = 1:3, AVAL = c(10, 20, 30))
    labeled_data <- apply_labels(data, LABELS, c("ADPC", "ADPC"))
    expect_equal(base::attr(labeled_data$USUBJID, "label"), "Unique Subject Identifier")
    expect_equal(base::attr(labeled_data$AVAL, "label"), "Analysis Value")
  })

  # PROBLEM !
  # Test for no labels applied when no matching labels exist
  it("does not apply labels if no matching labels exist", {
    data <- data.frame(USUBJID = 1:3, AVAL = c(10, 20, 30))
    LABELS <- data.frame(Variable = character(),
                         Label = character(),
                         Dataset = character())
    
    labeled_data <- apply_labels(data, LABELS, c("ADPC", "ADPC"))

    expect_null(base::attr(labeled_data$USUBJID, "label"))
    expect_null(base::attr(labeled_data$AVAL, "label"))
  })
})


# ------------------------  as_factor_preserve_label
vec <- c("A", "B", "C")
attr(vec, "label") <- "Example Label"

describe("as_factor_preserve_label", {

  it("returns object of class factor", {
    expect_equal("factor", class(as_factor_preserve_label(vec)))
     })
    
  it("returns ... ", {
    old_label = base::attr(vec, "label")
    new_label = base::attr(as_factor_preserve_label(vec), "label")
    expect_equal(old_label, new_label)

    })
  })

# ------------------------  get_label

describe("get_label", {

  # Test for successful label retrieval
  it("returns label of a heading if it exists in the label file", {
    expect_equal(get_label(LABELS, "USUBJID", "ADPC"), "Unique Subject Identifier")
  })

  # Test for fallback behavior when no label is available
  it("returns 'No label available' if the label does not exist", {
    expect_equal(get_label(LABELS, "USUBJID", "ADP"), "No label available")
  })
})
