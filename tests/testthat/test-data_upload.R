describe("versioned settings upload", {
  it("restores single and selected versions outside the package namespace", {
    # Shiny sources modules outside the package namespace. Do not let load_all()
    # or the test environment provide internal constants that the app cannot see.
    upload <- list2env(list(
      req = shiny::req,
      `%||%` = rlang::`%||%`,
      showNotification = function(...) NULL,
      removeModal = function(...) NULL,
      log_success = function(...) NULL
    ), parent = baseenv())
    sys.source(
      system.file("shiny/modules/tab_data/data_upload.R", package = "aNCA"),
      envir = upload
    )
    ratios <- data.frame(
      TestParameter = "CMAX", RefParameter = "CMAX",
      TestGroups = "(all other levels)",
      RefGroups = c("PARAM: DrugA", "PARAM: Metab-DrugA"),
      AggregateSubject = "no", AdjustingFactor = 1,
      PPTESTCD = c("MRCMAX11", "MRCMAX")
    )
    payload <- list(
      mapping = list(PARAM = "ANALYTE"),
      settings = list(method = "lin up/log down", ratio_table = ratios),
      nca_ran = TRUE
    )
    version <- create_settings_version(payload, comment = "ratio run", tab = "nca")
    path <- tempfile(fileext = ".yaml")
    on.exit(unlink(path), add = TRUE)
    write_versioned_settings(list(version), path)
    content <- read_settings(path)
    versioned <- attr(content, "versioned")

    for (selected in c(FALSE, TRUE)) {
      restored <- history <- NULL
      pending <- versioned
      settings_override <- function(x) restored <<- x
      session <- list(userData = list(settings_versions = function(x) history <<- x))

      if (selected) {
        older <- version
        older$settings$ratio_table$AdjustingFactor <- 100
        pending$versions <- c(list(older), versioned$versions)
        expected_history <- pending$versions
        upload$.handle_version_restore(
          function(x) if (missing(x)) pending else pending <<- x,
          function() 2L, settings_override, session
        )
        expect_null(pending)
      } else {
        expected_history <- versioned$versions
        errors <- upload$.apply_uploaded_settings(
          list(list(type = "settings", name = "ratios.yaml", content = content)),
          character(), settings_override, function(x) pending <<- x, session, identity
        )
        expect_length(errors, 0L)
      }

      expect_equal(restored$settings$ratio_table, ratios)
      expect_equal(restored$mapping, payload$mapping)
      expect_true(restored$nca_ran)
      expect_equal(restored$tab, "nca")
      expect_false(any(c("comment", "datetime", "dataset", "anca_version") %in% names(restored)))
      expect_equal(history, expected_history)
    }
  })
})
