library(reactable)
library(reactable.extras)
library(logger)
source("../../functions/utils.R", local = FALSE)
source("../../modules/tab_nca/setup/manual_slopes_table.R", local = FALSE)

describe("manual_slopes_table_server", {
  it("clears the table when the last rule is removed (#1302)", {
    # Mock getReactableState so the remove observer has a selection  # nolint: object_name_linter
    orig_get_reactable_state <- get("getReactableState", envir = asNamespace("reactable"))  # nolint: object_name_linter
    assign("getReactableState", function(...) c(1), envir = .GlobalEnv)  # nolint: object_name_linter
    on.exit(assign("getReactableState", orig_get_reactable_state, envir = .GlobalEnv), add = TRUE)  # nolint: object_name_linter

    testServer(manual_slopes_table_server, args = list(
      pknca_data = shiny::reactive(NULL),
      manual_slopes_override = shiny::reactive(NULL)
    ), {
      manual_slopes(data.frame(
        TYPE = "Exclusion", RANGE = "1:3", REASON = "test",
        stringsAsFactors = FALSE
      ))
      session$setInputs(remove_rule = 1)
      expect_true(is.null(manual_slopes()) || nrow(manual_slopes()) == 0)
    })
  })

  it("preserves remaining rows' TYPE/RANGE/REASON on removal (#1302)", {
    # Mock getReactableState so the remove observer has a selection  # nolint: object_name_linter
    orig_get_reactable_state <- get("getReactableState", envir = asNamespace("reactable"))  # nolint: object_name_linter
    assign("getReactableState", function(...) c(1), envir = .GlobalEnv)  # nolint: object_name_linter
    on.exit(assign("getReactableState", orig_get_reactable_state, envir = .GlobalEnv), add = TRUE)  # nolint: object_name_linter

    testServer(manual_slopes_table_server, args = list(
      pknca_data = shiny::reactive(NULL),
      manual_slopes_override = shiny::reactive(NULL)
    ), {
      original <- data.frame(
        TYPE = c("Exclusion", "Inclusion"),
        RANGE = c("1:3", "4:6"),
        REASON = c("reason1", "reason2"),
        stringsAsFactors = FALSE
      )
      manual_slopes(original)
      session$setInputs(remove_rule = 1)
      expect_equal(
        manual_slopes()[, c("TYPE", "RANGE", "REASON")],
        original[-1, c("TYPE", "RANGE", "REASON")]
      )
    })
  })

  it("ignores edit events while suppression is active (#1302)", {
    testServer(manual_slopes_table_server, args = list(
      pknca_data = shiny::reactive(NULL),
      manual_slopes_override = shiny::reactive(NULL)
    ), {
      original <- data.frame(
        TYPE = "Exclusion", RANGE = "1:3", REASON = "test",
        stringsAsFactors = FALSE
      )
      manual_slopes(original)
      refresh_reactable(refresh_reactable() + 1)
      session$setInputs(edit_TYPE = list(row = 1, value = "spurious"))
      expect_equal(manual_slopes(), original)
    })
  })
})
