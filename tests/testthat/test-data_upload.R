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
