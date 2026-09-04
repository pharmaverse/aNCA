# Minimal metadata fixture covering the mapping variants and a length rule
test_metadata <- data.frame(
  Dataset = c(
    "ADNCA", "ADNCA", "ADNCA", "ADNCA", "ADNCA",
    "ADPP", "ADPP"
  ),
  Variable = c(
    "STUDYID", "AVAL", "USUBJID", "PCRFTDTM", "ADURN",
    "PPSTRESN", "PARAMCD"
  ),
  Type = c(
    "text", "float", "Char", "dateTime", "duration",
    "Num", "text"
  ),
  Length = c(8, NA, 5, 30, 30, NA, 8),
  stringsAsFactors = FALSE
)

describe("validate_cdisc_types: type -> class mapping", {
  it("maps text/Char/Character/dateTime/duration to character", {
    for (t in c("text", "Char", "Character", "dateTime", "duration")) {
      expect_equal(.cdisc_expected_class(t), "character")
    }
  })

  it("maps float/integer/Num to numeric", {
    for (t in c("float", "integer", "Num")) {
      expect_equal(.cdisc_expected_class(t), "numeric")
    }
  })

  it("returns NA for unknown or empty types", {
    expect_true(is.na(.cdisc_expected_class("weird")))
    expect_true(is.na(.cdisc_expected_class("")))
    expect_true(is.na(.cdisc_expected_class(NA_character_)))
    expect_true(is.na(.cdisc_expected_class(character(0))))
  })
})

describe(".cdisc_normalise_name", {
  it("collapses digit-index instances to a template key", {
    expect_equal(.cdisc_normalise_name("NCA1XRS"), "NCA#XRS")
    expect_equal(.cdisc_normalise_name("CRIT12FL"), "CRIT#FL")
  })

  it("collapses lowercase placeholders only for metadata names", {
    expect_equal(.cdisc_normalise_name("NCAwXRS", is_metadata = TRUE), "NCA#XRS")
    expect_equal(.cdisc_normalise_name("CRIT1", is_metadata = TRUE), "CRIT#")
    # Data names keep uppercase intact; only digits are collapsed
    expect_equal(.cdisc_normalise_name("NCA2XRS"), "NCA#XRS")
  })
})

describe("validate_cdisc_types: conforming data", {
  it("returns zero rows when all columns conform", {
    cdisc_data <- list(
      adnca = data.frame(
        STUDYID = "STUDY001",
        AVAL = 1.23,
        USUBJID = "S001",
        stringsAsFactors = FALSE
      )
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = test_metadata)
    expect_s3_class(findings, "data.frame")
    expect_equal(nrow(findings), 0)
    expect_equal(names(findings), CDISC_FINDING_COLS)
  })
})

describe("validate_cdisc_types: wrong type detection", {
  it("flags a numeric column that should be character", {
    cdisc_data <- list(
      adnca = data.frame(STUDYID = 12345678, stringsAsFactors = FALSE)
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = test_metadata)
    expect_equal(nrow(findings), 1)
    expect_equal(findings$Check, "class")
    expect_equal(findings$Severity, "error")
    expect_equal(findings$Variable, "STUDYID")
    expect_equal(findings$Expected, "character")
    expect_equal(findings$Observed, "numeric")
  })

  it("flags a character column that should be numeric", {
    cdisc_data <- list(
      adnca = data.frame(AVAL = "not-a-number", stringsAsFactors = FALSE)
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = test_metadata)
    expect_equal(nrow(findings), 1)
    expect_equal(findings$Check, "class")
    expect_equal(findings$Severity, "error")
    expect_equal(findings$Expected, "numeric")
  })

  it("accepts factor columns for character variables", {
    cdisc_data <- list(
      adnca = data.frame(
        STUDYID = factor("S001"),
        stringsAsFactors = FALSE
      )
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = test_metadata)
    expect_equal(nrow(findings), 0)
  })

  it("accepts integer columns for numeric variables", {
    cdisc_data <- list(
      adnca = data.frame(AVAL = 5L, stringsAsFactors = FALSE)
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = test_metadata)
    expect_equal(nrow(findings), 0)
  })
})

describe("validate_cdisc_types: length checks", {
  it("flags character values exceeding the declared length", {
    cdisc_data <- list(
      adnca = data.frame(
        USUBJID = c("SUBJECT-TOO-LONG", "OK"),
        stringsAsFactors = FALSE
      )
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = test_metadata)
    len_finding <- findings[findings$Check == "length", ]
    expect_equal(nrow(len_finding), 1)
    expect_equal(len_finding$Severity, "error")
    expect_equal(len_finding$N_Affected, 1L)
    expect_equal(len_finding$Variable, "USUBJID")
  })

  it("does not flag length when check_length = FALSE", {
    cdisc_data <- list(
      adnca = data.frame(
        USUBJID = "SUBJECT-TOO-LONG",
        stringsAsFactors = FALSE
      )
    )
    findings <- validate_cdisc_types(
      cdisc_data, metadata = test_metadata, check_length = FALSE
    )
    expect_equal(nrow(findings[findings$Check == "length", ]), 0)
  })
})

describe("validate_cdisc_types: indexed CDISC variable families", {
  # Metadata declares the CDISC templates NCAwXRS/NCAwXRSN (w = index) and the
  # indexed family CRIT1/CRIT1FL. Exported data contains concrete instances.
  indexed_meta <- data.frame(
    Dataset = rep("ADNCA", 4),
    Variable = c("NCAwXRS", "NCAwXRSN", "CRIT1", "CRIT1FL"),
    Type = c("Character", "integer", "text", "text"),
    Length = c(200, 12, 70, 1),
    stringsAsFactors = FALSE
  )

  it("matches NCA1XRS/NCA2XRS to the NCAwXRS template (no warning)", {
    cdisc_data <- list(
      adnca = data.frame(
        NCA1XRS = "Late Sample",
        NCA2XRS = "Vomit",
        stringsAsFactors = FALSE
      )
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = indexed_meta)
    expect_equal(nrow(findings), 0)
  })

  it("matches numeric NCA1XRSN to the NCAwXRSN template", {
    cdisc_data <- list(
      adnca = data.frame(NCA1XRSN = 1L, stringsAsFactors = FALSE)
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = indexed_meta)
    expect_equal(nrow(findings), 0)
  })

  it("matches CRIT2/CRIT3FL to the CRIT1/CRIT1FL index family", {
    cdisc_data <- list(
      adnca = data.frame(
        CRIT2 = "R2ADJ < 0.8",
        CRIT3FL = "Y",
        stringsAsFactors = FALSE
      )
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = indexed_meta)
    expect_equal(nrow(findings), 0)
  })

  it("still enforces the template's class on indexed instances", {
    cdisc_data <- list(
      adnca = data.frame(NCA1XRSN = "not-a-number", stringsAsFactors = FALSE)
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = indexed_meta)
    expect_equal(nrow(findings), 1)
    expect_equal(findings$Check, "class")
    expect_equal(findings$Variable, "NCA1XRSN")
    expect_equal(findings$Expected, "numeric")
  })

  it("prefers an exact metadata match over the template", {
    meta <- rbind(
      indexed_meta,
      data.frame(
        Dataset = "ADNCA", Variable = "NCA1XRS", Type = "float", Length = 8,
        stringsAsFactors = FALSE
      )
    )
    # Exact NCA1XRS spec is numeric, so a character value must error
    cdisc_data <- list(
      adnca = data.frame(NCA1XRS = "text", stringsAsFactors = FALSE)
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = meta)
    expect_equal(findings$Check, "class")
    expect_equal(findings$Expected, "numeric")
  })

  it("keeps truly unknown non-indexed columns as warnings", {
    cdisc_data <- list(
      adnca = data.frame(DOSFRM = "TABLET", stringsAsFactors = FALSE)
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = indexed_meta)
    unk <- findings[findings$Check == "unknown_variable", ]
    expect_equal(nrow(unk), 1)
    expect_equal(unk$Variable, "DOSFRM")
  })
})

describe("validate_cdisc_types: unknown columns", {
  it("surfaces unknown columns as non-blocking warnings", {
    cdisc_data <- list(
      adnca = data.frame(
        STUDYID = "S001",
        NOTINMETA = "x",
        stringsAsFactors = FALSE
      )
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = test_metadata)
    unk <- findings[findings$Check == "unknown_variable", ]
    expect_equal(nrow(unk), 1)
    expect_equal(unk$Severity, "warning")
    expect_equal(unk$Variable, "NOTINMETA")
    expect_false(cdisc_validation_blocks_save(findings))
  })
})

describe("validate_cdisc_types: all-NA and edge inputs", {
  it("treats all-NA columns as conforming", {
    cdisc_data <- list(
      adnca = data.frame(
        AVAL = NA_real_,
        STUDYID = NA_character_,
        stringsAsFactors = FALSE
      )
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = test_metadata)
    expect_equal(nrow(findings), 0)
  })

  it("returns empty findings for NULL or empty input", {
    expect_equal(nrow(validate_cdisc_types(NULL)), 0)
    expect_equal(nrow(validate_cdisc_types(list())), 0)
    expect_equal(names(validate_cdisc_types(NULL)), CDISC_FINDING_COLS)
  })

  it("ignores list entries with unrecognised names", {
    cdisc_data <- list(
      other = data.frame(STUDYID = 1, stringsAsFactors = FALSE)
    )
    expect_equal(nrow(validate_cdisc_types(cdisc_data, metadata = test_metadata)), 0)
  })

  it("skips variables whose metadata type cannot be interpreted", {
    meta <- data.frame(
      Dataset = "ADNCA", Variable = "MYSTERY", Type = "weird", Length = NA,
      stringsAsFactors = FALSE
    )
    cdisc_data <- list(
      adnca = data.frame(MYSTERY = 1, stringsAsFactors = FALSE)
    )
    expect_equal(nrow(validate_cdisc_types(cdisc_data, metadata = meta)), 0)
  })
})

describe("validate_cdisc_types: packaged metadata smoke test", {
  it("runs against metadata_nca_variables without error", {
    cdisc_data <- list(
      adnca = data.frame(
        STUDYID = "STUDY001",
        USUBJID = "SUBJ001",
        stringsAsFactors = FALSE
      )
    )
    findings <- validate_cdisc_types(cdisc_data)
    expect_s3_class(findings, "data.frame")
    expect_equal(names(findings), CDISC_FINDING_COLS)
  })
})

describe("cdisc_validation_blocks_save", {
  it("is TRUE when an error-severity finding exists", {
    findings <- validate_cdisc_types(
      list(adnca = data.frame(STUDYID = 123, stringsAsFactors = FALSE)),
      metadata = test_metadata
    )
    expect_true(cdisc_validation_blocks_save(findings))
  })

  it("is FALSE for empty or warning-only findings", {
    expect_false(cdisc_validation_blocks_save(validate_cdisc_types(list())))
    warn_only <- validate_cdisc_types(
      list(adnca = data.frame(NOTINMETA = "x", stringsAsFactors = FALSE)),
      metadata = test_metadata
    )
    expect_false(cdisc_validation_blocks_save(warn_only))
  })
})

describe("describe_cdisc_variables", {
  it("returns one row per column with pass status for conforming data", {
    cdisc_data <- list(
      adnca = data.frame(
        STUDYID = "S001", AVAL = 1.2, USUBJID = "U1",
        stringsAsFactors = FALSE
      )
    )
    summary <- describe_cdisc_variables(cdisc_data, metadata = test_metadata)
    expect_s3_class(summary, "data.frame")
    expect_equal(nrow(summary), 3)
    expect_equal(names(summary), CDISC_SUMMARY_COLS)
    expect_true(all(summary$Status == "pass"))
    expect_true(all(c("STUDYID", "AVAL", "USUBJID") %in% summary$Variable))
  })

  it("includes the metadata label, expected and observed class", {
    cdisc_data <- list(
      adnca = data.frame(AVAL = 1.2, stringsAsFactors = FALSE)
    )
    summary <- describe_cdisc_variables(cdisc_data, metadata = test_metadata)
    row <- summary[summary$Variable == "AVAL", ]
    expect_equal(row$Expected_Class, "numeric")
    expect_equal(row$Observed_Class, "numeric")
    expect_equal(row$Type, "float")
  })

  it("marks wrong-type columns as error with a detail message", {
    cdisc_data <- list(
      adnca = data.frame(STUDYID = 123, stringsAsFactors = FALSE)
    )
    summary <- describe_cdisc_variables(cdisc_data, metadata = test_metadata)
    row <- summary[summary$Variable == "STUDYID", ]
    expect_equal(row$Status, "error")
    expect_match(row$Detail, "Expected character")
  })

  it("marks over-length columns as error and reports the longest value", {
    cdisc_data <- list(
      adnca = data.frame(USUBJID = "SUBJECT-TOO-LONG", stringsAsFactors = FALSE)
    )
    summary <- describe_cdisc_variables(cdisc_data, metadata = test_metadata)
    row <- summary[summary$Variable == "USUBJID", ]
    expect_equal(row$Status, "error")
    expect_equal(row$Longest_Value, nchar("SUBJECT-TOO-LONG"))
    expect_match(row$Detail, "exceeds declared length")
  })

  it("marks unknown columns as unknown status", {
    cdisc_data <- list(
      adnca = data.frame(NOTINMETA = "x", stringsAsFactors = FALSE)
    )
    summary <- describe_cdisc_variables(cdisc_data, metadata = test_metadata)
    row <- summary[summary$Variable == "NOTINMETA", ]
    expect_equal(row$Status, "unknown")
  })

  it("resolves indexed variables to their template label and passes", {
    indexed_meta <- data.frame(
      Dataset = "ADNCA", Variable = "NCAwXRS",
      Label = "Reason w for PK NCA Exclusion",
      Type = "Character", Length = 200, stringsAsFactors = FALSE
    )
    cdisc_data <- list(
      adnca = data.frame(NCA1XRS = "Late Sample", stringsAsFactors = FALSE)
    )
    summary <- describe_cdisc_variables(cdisc_data, metadata = indexed_meta)
    row <- summary[summary$Variable == "NCA1XRS", ]
    expect_equal(row$Status, "pass")
    expect_equal(row$Label, "Reason w for PK NCA Exclusion")
    expect_equal(row$Expected_Class, "character")
  })

  it("marks uninterpretable metadata types as skipped", {
    meta <- data.frame(
      Dataset = "ADNCA", Variable = "MYSTERY", Label = "Mystery",
      Type = "weird", Length = NA, stringsAsFactors = FALSE
    )
    cdisc_data <- list(
      adnca = data.frame(MYSTERY = 1, stringsAsFactors = FALSE)
    )
    summary <- describe_cdisc_variables(cdisc_data, metadata = meta)
    expect_equal(summary$Status, "skipped")
  })

  it("returns empty summary for NULL or empty input", {
    expect_equal(nrow(describe_cdisc_variables(NULL)), 0)
    expect_equal(nrow(describe_cdisc_variables(list())), 0)
    expect_equal(names(describe_cdisc_variables(NULL)), CDISC_SUMMARY_COLS)
  })
})

describe("cdisc_validation_report", {
  it("renders a PASS banner for conforming data", {
    findings <- validate_cdisc_types(list())
    html <- cdisc_validation_report(findings)
    expect_type(html, "character")
    expect_match(html, "PASS")
    expect_match(html, "<!DOCTYPE html>")
    expect_no_match(html, "FAIL -")
  })

  it("renders a FAIL banner and finding rows for errors", {
    findings <- validate_cdisc_types(
      list(adnca = data.frame(STUDYID = 123, stringsAsFactors = FALSE)),
      metadata = test_metadata
    )
    html <- cdisc_validation_report(findings, project = "MyProject")
    expect_match(html, "FAIL")
    expect_match(html, "MyProject")
    expect_match(html, "STUDYID")
  })

  it("escapes HTML special characters in findings", {
    findings <- validate_cdisc_types(
      list(adnca = data.frame(`A<B` = "x", check.names = FALSE)),
      metadata = test_metadata
    )
    html <- cdisc_validation_report(findings)
    expect_match(html, "A&lt;B")
  })

  it("shows the Problems section before the Variable summary section", {
    cdisc_data <- list(
      adnca = data.frame(STUDYID = "S1", AVAL = 1.2, stringsAsFactors = FALSE)
    )
    findings <- validate_cdisc_types(cdisc_data, metadata = test_metadata)
    summary <- describe_cdisc_variables(cdisc_data, metadata = test_metadata)
    html <- cdisc_validation_report(findings, summary = summary)
    expect_match(html, "<h2>Problems</h2>")
    expect_match(html, "<h2>Variable summary</h2>")
    expect_lt(
      regexpr("<h2>Problems</h2>", html, fixed = TRUE),
      regexpr("<h2>Variable summary</h2>", html, fixed = TRUE)
    )
    # The summary lists the variable and its declared class
    expect_match(html, "STUDYID")
    expect_match(html, "Conforms")
  })

  it("omits the Variable summary section when no summary is supplied", {
    html <- cdisc_validation_report(validate_cdisc_types(list()))
    expect_no_match(html, "<h2>Variable summary</h2>")
  })
})

describe("write_cdisc_validation_report", {
  it("writes the report and returns findings, path, and blocks flag", {
    tmp <- file.path(tempdir(), paste0("cdisc_val_", as.integer(runif(1, 1, 1e6))))
    cdisc_data <- list(
      adnca = data.frame(STUDYID = "S001", AVAL = 1.2, stringsAsFactors = FALSE)
    )
    res <- write_cdisc_validation_report(
      cdisc_data, target_dir = tmp, metadata = test_metadata
    )
    expect_true(file.exists(res$report_path))
    expect_false(res$blocks_save)
    expect_equal(nrow(res$findings), 0)
    expect_s3_class(res$summary, "data.frame")
    expect_equal(nrow(res$summary), 2)
    expect_true(all(res$summary$Status == "pass"))
    unlink(tmp, recursive = TRUE)
  })

  it("reports blocks_save = TRUE when data has type errors", {
    tmp <- file.path(tempdir(), paste0("cdisc_val_", as.integer(runif(1, 1, 1e6))))
    cdisc_data <- list(
      adnca = data.frame(STUDYID = 123, stringsAsFactors = FALSE)
    )
    res <- write_cdisc_validation_report(
      cdisc_data, target_dir = tmp, metadata = test_metadata
    )
    expect_true(res$blocks_save)
    expect_true(file.exists(res$report_path))
    unlink(tmp, recursive = TRUE)
  })
})
