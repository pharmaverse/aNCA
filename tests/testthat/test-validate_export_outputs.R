# Minimal metadata fixture reused for CDISC value-level checks
export_test_metadata <- data.frame(
  Dataset = c("ADNCA", "ADNCA", "ADNCA"),
  Variable = c("STUDYID", "AVAL", "USUBJID"),
  Type = c("text", "float", "Char"),
  Length = c(8, NA, 5),
  stringsAsFactors = FALSE
)

# A trivial ggplot for plot-node fixtures
make_ggplot <- function() {
  ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()
}

describe("validate_export_outputs: object-class checks", {
  it("returns zero rows when every output has the expected class", {
    output <- list(
      exploration = list(meanplot = make_ggplot(), meanplot_code = "x <- 1"),
      nca_results = list(nca_pkparam = data.frame(AVAL = 1.2)),
      CDISC = list(
        adnca = data.frame(
          STUDYID = "S1", AVAL = 1.2, USUBJID = "U1", stringsAsFactors = FALSE
        )
      )
    )
    findings <- validate_export_outputs(output, metadata = export_test_metadata)
    expect_s3_class(findings, "data.frame")
    expect_equal(names(findings), EXPORT_FINDING_COLS)
    expect_equal(nrow(findings), 0)
  })

  it("flags a plot node that is a data frame", {
    output <- list(exploration = list(meanplot = data.frame(x = 1)))
    findings <- validate_export_outputs(output)
    row <- findings[findings$Output == "exploration/meanplot", ]
    expect_equal(nrow(row), 1)
    expect_equal(row$Severity, "error")
    expect_equal(row$Check, "class")
    expect_equal(row$Expected, "plot")
    expect_equal(row$Observed, "table")
  })

  it("flags a table node that is a ggplot", {
    output <- list(nca_results = list(nca_pkparam = make_ggplot()))
    findings <- validate_export_outputs(output)
    row <- findings[findings$Output == "nca_results/nca_pkparam", ]
    expect_equal(nrow(row), 1)
    expect_equal(row$Expected, "table")
    expect_equal(row$Observed, "plot")
  })

  it("accepts a plotly object for a plot node", {
    output <- list(
      nca_results = list(boxplot = plotly::plot_ly(x = 1, y = 1))
    )
    findings <- validate_export_outputs(output)
    expect_equal(nrow(findings), 0)
  })

  it("flags an unsupported object type as unexportable", {
    output <- list(nca_results = list(nca_pkparam = lm(y ~ x, data.frame(x = 1:3, y = 1:3))))
    findings <- validate_export_outputs(output)
    row <- findings[findings$Output == "nca_results/nca_pkparam", ]
    expect_equal(nrow(row), 1)
    expect_equal(row$Severity, "error")
    expect_match(row$Observed, "lm")
  })

  it("flags a *_code node that is not a length-1 string", {
    output <- list(exploration = list(meanplot_code = c("a", "b")))
    findings <- validate_export_outputs(output)
    row <- findings[findings$Output == "exploration/meanplot_code", ]
    expect_equal(nrow(row), 1)
    expect_equal(row$Severity, "error")
  })

  it("recurses into nested lists and reports the full path", {
    output <- list(
      additional_analysis = list(matrix_ratios = make_ggplot())
    )
    findings <- validate_export_outputs(output)
    expect_true("additional_analysis/matrix_ratios" %in% findings$Output)
  })
})

describe("validate_export_outputs: obj_names filtering", {
  it("only validates leaves named in obj_names", {
    output <- list(
      nca_results = list(
        nca_pkparam = make_ggplot(),   # wrong class, but not selected
        nca_statistics = data.frame(AVAL = 1)
      )
    )
    findings <- validate_export_outputs(output, obj_names = "nca_statistics")
    expect_equal(nrow(findings), 0)
  })

  it("validates a selected leaf with a wrong class", {
    output <- list(nca_results = list(nca_pkparam = make_ggplot()))
    findings <- validate_export_outputs(output, obj_names = "nca_pkparam")
    expect_equal(nrow(findings), 1)
  })
})

describe("validate_export_outputs: CDISC value-level checks", {
  it("reports a wrong-type CDISC column as an error", {
    output <- list(
      CDISC = list(adnca = data.frame(STUDYID = 123, stringsAsFactors = FALSE))
    )
    findings <- validate_export_outputs(output, metadata = export_test_metadata)
    row <- findings[findings$Output == "CDISC/adnca", ]
    expect_equal(nrow(row), 1)
    expect_equal(row$Variable, "STUDYID")
    expect_equal(row$Check, "class")
    expect_equal(row$Severity, "error")
  })

  it("reports a CDISC length violation as an error", {
    output <- list(
      CDISC = list(
        adnca = data.frame(USUBJID = "TOO-LONG-VALUE", stringsAsFactors = FALSE)
      )
    )
    findings <- validate_export_outputs(output, metadata = export_test_metadata)
    row <- findings[findings$Output == "CDISC/adnca", ]
    expect_equal(row$Check, "length")
    expect_equal(row$Severity, "error")
  })

  it("surfaces unknown CDISC columns as non-blocking warnings", {
    output <- list(
      CDISC = list(adnca = data.frame(NOTINMETA = "x", stringsAsFactors = FALSE))
    )
    findings <- validate_export_outputs(output, metadata = export_test_metadata)
    row <- findings[findings$Output == "CDISC/adnca", ]
    expect_equal(row$Severity, "warning")
    expect_false(export_validation_blocks_save(findings))
  })

  it("only checks CDISC datasets present in obj_names", {
    output <- list(
      CDISC = list(
        adnca = data.frame(STUDYID = 123, stringsAsFactors = FALSE),
        adpp = data.frame(STUDYID = 456, stringsAsFactors = FALSE)
      )
    )
    findings <- validate_export_outputs(
      output, obj_names = "adpp", metadata = export_test_metadata
    )
    expect_true(all(findings$Output == "CDISC/adpp"))
  })

  it("combines object-class and CDISC value-level findings", {
    output <- list(
      nca_results = list(nca_pkparam = make_ggplot()),
      CDISC = list(adnca = data.frame(STUDYID = 123, stringsAsFactors = FALSE))
    )
    findings <- validate_export_outputs(output, metadata = export_test_metadata)
    expect_true("nca_results/nca_pkparam" %in% findings$Output)
    expect_true("CDISC/adnca" %in% findings$Output)
  })
})

describe("validate_export_outputs: empty and edge input", {
  it("returns an empty findings frame for NULL or empty input", {
    expect_equal(nrow(validate_export_outputs(NULL)), 0)
    expect_equal(nrow(validate_export_outputs(list())), 0)
    expect_equal(names(validate_export_outputs(NULL)), EXPORT_FINDING_COLS)
  })
})

describe("export_validation_blocks_save", {
  it("is TRUE when an error-severity finding exists", {
    output <- list(nca_results = list(nca_pkparam = make_ggplot()))
    findings <- validate_export_outputs(output)
    expect_true(export_validation_blocks_save(findings))
  })

  it("is FALSE for empty or warning-only findings", {
    expect_false(export_validation_blocks_save(validate_export_outputs(list())))
    warn_only <- validate_export_outputs(
      list(CDISC = list(adnca = data.frame(NOTINMETA = "x"))),
      metadata = export_test_metadata
    )
    expect_false(export_validation_blocks_save(warn_only))
  })
})
