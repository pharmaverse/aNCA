source(testthat::test_path("..", "scripts", "r-cmd-check-policy.R"))

# Diagnostic captured from PR #1548, Actions job 124290688366 (R 4.4.3).
imports_note <- paste(
  "checking package dependencies ... NOTE",
  "Imports includes 24 non-default packages.",
  "Importing from so many packages makes the package vulnerable to any of",
  "them becoming unavailable.  Move as many as possible to Suggests and",
  "use conditionally.",
  sep = "\n"
)
check_result <- function(notes = character(), errors = character(), warnings = character()) {
  list(errors = errors, warnings = warnings, notes = notes)
}

describe("R CMD check dependency NOTE exception", {
  it("accepts the captured NOTE only on R 4.4 and preserves the complete result", {
    result <- check_result(imports_note)
    for (version in c("4.4.0", "4.4.3")) {
      expect_message(
        checked <- check_r_cmd_results(result, version),
        "exactly 24 non-default Imports; NOTE retained"
      )
      expect_identical(checked, result)
    }
    for (version in c("4.3.3", "4.5.0", "4.6.0")) {
      expect_error(check_r_cmd_results(result, version), "outside the approved")
    }
  })

  it("ignores wrapping differences but rejects changed counts or extra diagnostic text", {
    wrapped <- check_result(gsub(" ", "\n  ", imports_note, fixed = TRUE))
    expect_message(check_r_cmd_results(wrapped, "4.4.3"), "Approved R 4.4 exception")
    for (count in c(23L, 25L, 124L)) {
      changed <- sub("24", as.character(count), imports_note, fixed = TRUE)
      expect_error(check_r_cmd_results(check_result(changed), "4.4.3"), "outside the approved")
    }
    for (note in c("A different NOTE", paste(imports_note, "An additional dependency problem"))) {
      expect_error(check_r_cmd_results(check_result(note), "4.4.3"), "outside the approved")
    }
  })

  it("rejects any error, warning or additional NOTE alongside the allowed NOTE", {
    failures <- list(
      check_result(imports_note, errors = "A check error"),
      check_result(imports_note, warnings = "A check warning"),
      check_result(c(imports_note, "A different NOTE")),
      check_result(c(imports_note, imports_note)),
      check_result(errors = "A check error"),
      check_result(warnings = "A check warning")
    )
    for (result in failures) {
      expect_error(check_r_cmd_results(result, "4.4.3"), "outside the approved")
    }
  })

  it("accepts clean checks and rejects malformed results", {
    clean <- check_result()
    for (version in c("4.4.3", "4.5.2", "4.6.0")) {
      expect_identical(check_r_cmd_results(clean, version), clean)
    }
    expect_error(check_r_cmd_results(NULL, "4.4.3"))
    expect_error(check_r_cmd_results(list(), "4.4.3"))
    expect_error(check_r_cmd_results(check_result(notes = NA_character_), "4.4.3"))
  })
})
