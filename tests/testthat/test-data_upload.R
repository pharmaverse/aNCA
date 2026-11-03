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

source(file.path(system.file("shiny", package = "aNCA"), "modules", "tab_data", "data_upload.R"),
       local = TRUE)

describe("uploaded settings restoration", {
  it("invalidates consumers when restoring the identical payload a second time", {
    payload <- list(settings = list(), tlg_labels = list(t_pkct01 = list(title = "Saved")))
    shiny::testServer(function(input, output, session) {
      restored <- shiny::reactiveVal(NULL)
      count <- shiny::reactiveVal(0L)
      shiny::observeEvent(restored(), count(count() + 1L))
    }, {
      .restore_uploaded_settings(restored, payload)
      session$flushReact()
      expect_identical(restored(), payload)
      expect_identical(count(), 1L)
      .restore_uploaded_settings(restored, payload)
      session$flushReact()
      expect_identical(restored(), payload)
      expect_identical(count(), 2L)
    })
  })
})

describe("versioned uploads with installed-package visibility", {
  it("restores single and selected versions without development-only exports", {
    app_env <- new.env(parent = baseenv())
    for (pkg in c("shiny", "logger", "purrr")) {
      list2env(mget(getNamespaceExports(pkg), asNamespace(pkg), inherits = TRUE),
               envir = app_env)
    }
    app_env$`%||%` <- rlang::`%||%`
    source(system.file("shiny/modules/tab_data/data_upload.R", package = "aNCA"),
           local = app_env)
    version <- create_settings_version(
      list(settings = list(method = "linear"),
           tlg_labels = list(t_pkct01 = list(title = "Saved title"))),
      comment = "Saved version", tab = "tlg"
    )
    payload <- list(settings = version$settings, tlg_labels = version$tlg_labels)
    attr(payload, "versioned") <- list(versions = list(version))
    upload <- list(type = "settings", name = "settings.yaml", content = payload)

    shiny::testServer(function(input, output, session) {
      restored <- shiny::reactiveVal(NULL)
      pending <- shiny::reactiveVal(NULL)
      session$userData$settings_versions <- shiny::reactiveVal(list())
    }, {
      errors <- app_env$.apply_uploaded_settings(
        list(upload), list(), restored, pending, session, session$ns
      )
      expect_length(errors, 0L)
      expect_identical(restored()$tlg_labels, version$tlg_labels)
      expect_identical(restored()$tab, "tlg")
      restored(NULL)
      pending(list(versions = list(version, version)))
      app_env$.handle_version_restore(pending, function() 2L, restored, session)
      expect_identical(restored()$tlg_labels, version$tlg_labels)
      expect_identical(restored()$tab, "tlg")
      expect_null(pending())
    })
  })
})
