local({
  library(shiny)
  library(dplyr)
  library(purrr)
  library(logger)
  library(reactable)
  library(reactable.extras)
  for (file in c("common/reactable.R", "tab_tlg.R", "tab_data/data_upload.R")) {
    source(system.file("shiny/modules", file, package = "aNCA"), local = TRUE)
  }
}, envir = parent.env(environment()))

tlg_upload_test_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    settings_override <- shiny::reactiveVal(NULL)
    pending_versioned <- shiny::reactiveVal(NULL)
    session$userData$settings_versions <- shiny::reactiveVal(list())
    restored_order <- shiny::reactiveVal(NULL)
    restore_count <- shiny::reactiveVal(0L)

    shiny::observeEvent(settings_override(), {
      restored_order(settings_override()$tlg_order)
      restore_count(restore_count() + 1L)
    })

    upload_settings <- function(upload) {
      .apply_uploaded_settings(
        list(upload), list(), settings_override, pending_versioned,
        session, session$ns
      )
    }
    restore_version <- function(index) {
      .handle_version_restore(
        pending_versioned, function() index, settings_override, session
      )
    }
  })
}

tlg_settings_upload_fixture <- function(versioned = FALSE) {
  payload <- list(
    settings = list(method = "linear"),
    tlg_order = list(t_pkct01 = list(
      Selection = FALSE, Footnote = "Saved footnote",
      Stratification = "TRT01A", Comment = "Saved comment"
    ))
  )
  contents <- if (versioned) {
    list(current = create_settings_version(payload, comment = "Saved order", tab = "tlg"))
  } else {
    payload
  }
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)
  yaml::write_yaml(contents, path)
  .read_uploaded_file(path, "settings.yaml")
}

describe("TLG settings uploads: repeated restore", {
  for (versioned in c(FALSE, TRUE)) {
    format_name <- if (versioned) "single-version" else "legacy"
    it(paste("reapplies identical", format_name, "settings after local edits"), {
      upload <- tlg_settings_upload_fixture(versioned)
      expect_identical(upload$status, "success")
      saved_order <- upload$content$tlg_order

      shiny::testServer(tlg_upload_test_server, {
        expect_length(upload_settings(upload), 0)
        session$flushReact()
        expect_identical(restored_order(), saved_order)
        expect_identical(restore_count(), 1L)

        edited <- saved_order
        edited$t_pkct01$Selection <- TRUE
        edited$t_pkct01$Footnote <- "Unsaved edit"
        restored_order(edited)

        expect_length(upload_settings(upload), 0)
        session$flushReact()
        expect_identical(restored_order(), saved_order)
        expect_identical(restore_count(), 2L)
      })
    })
  }

  it("reapplies the same chosen version without restoring merely on modal open", {
    upload <- tlg_settings_upload_fixture(TRUE)
    first <- attr(upload$content, "versioned")$versions[[1]]
    second <- first
    second$comment <- "Earlier order"
    second$tlg_order$t_pkct01$Footnote <- "Earlier footnote"
    attr(upload$content, "versioned") <- list(versions = list(first, second))

    shiny::testServer(tlg_upload_test_server, {
      expect_length(upload_settings(upload), 0)
      session$flushReact()
      expect_null(settings_override())
      expect_identical(restore_count(), 0L)

      restore_version(2L)
      session$flushReact()
      expect_identical(restored_order(), second$tlg_order)
      expect_identical(settings_override()$tab, "tlg")
      expect_identical(restore_count(), 1L)

      edited <- restored_order()
      edited$t_pkct01$Comment <- "Local edit"
      restored_order(edited)
      expect_length(upload_settings(upload), 0)
      session$flushReact()
      # Opening, or dismissing, the modal must leave the current order alone.
      expect_identical(restored_order(), edited)
      expect_identical(restore_count(), 1L)
      shiny::removeModal()
      session$flushReact()
      expect_identical(restored_order(), edited)

      restore_version(2L)
      session$flushReact()
      expect_identical(restored_order(), second$tlg_order)
      expect_identical(restore_count(), 2L)
    })
  })

  it("keeps current settings when a chosen version is invalid or unselected", {
    upload <- tlg_settings_upload_fixture(FALSE)
    invalid <- list(versions = list(list(comment = "Invalid", datetime = "2026-01-01")))

    shiny::testServer(tlg_upload_test_server, {
      upload_settings(upload)
      session$flushReact()
      before <- settings_override()
      pending_versioned(invalid)

      restore_version(NULL)
      session$flushReact()
      expect_identical(settings_override(), before)
      expect_identical(restore_count(), 1L)

      restore_version(1L)
      session$flushReact()
      expect_identical(settings_override(), before)
      expect_identical(restored_order(), before$tlg_order)
      expect_identical(restore_count(), 1L)
      expect_identical(pending_versioned(), invalid)
    })
  })

  it("does not clear the current order on a malformed settings file", {
    upload <- tlg_settings_upload_fixture(FALSE)
    invalid_path <- withr::local_tempfile(fileext = ".yaml")
    yaml::write_yaml(list(current = list(
      comment = "Invalid", datetime = "2026-01-01"
    )), invalid_path)

    shiny::testServer(tlg_upload_test_server, {
      upload_settings(upload)
      session$flushReact()
      before <- settings_override()

      file_error <- shiny::reactiveVal(NULL)
      fallback <- data.frame(USUBJID = "S1", AVAL = 2)
      loaded <- .process_uploaded_files(
        invalid_path, "invalid.yaml", fallback, settings_override,
        pending_versioned, file_error, session, session$ns
      )
      session$flushReact()
      expect_identical(loaded, fallback)
      expect_match(file_error(), "valid settings YAML")
      expect_identical(settings_override(), before)
      expect_identical(restored_order(), before$tlg_order)
      expect_identical(restore_count(), 1L)
    })
  })

  it("preserves loaded settings and edited TLG order when multiple files are rejected", {
    upload <- tlg_settings_upload_fixture(FALSE)
    second_upload <- upload
    second_upload$name <- "another-settings.yaml"
    current_settings <- shiny::reactiveVal(upload$content)
    data <- shiny::reactive(list(conc = list(data = data.frame(
      USUBJID = "S1", PCSPEC = "PLASMA", AVAL = 2
    ))))

    shiny::testServer(tab_tlg_server, args = list(
      data = data, settings_override = current_settings
    ), {
      session$flushReact()
      edited <- tlg_order()
      edited$Selection[1] <- TRUE
      edited$Footnote[1] <- "Unsaved local footnote"
      edited$Comment[1] <- "Unsaved local comment"
      tlg_order(edited)
      session$flushReact()
      before <- session$userData$tlg_order()

      errors <- .apply_uploaded_settings(
        list(upload, second_upload), list(), current_settings,
        shiny::reactiveVal(NULL), session, session$ns
      )
      session$flushReact()

      expect_length(errors, 1)
      expect_match(errors[[1]], "Multiple settings files detected")
      expect_identical(current_settings(), upload$content)
      expect_identical(session$userData$tlg_order(), before)
      expect_true(session$userData$tlg_order()$t_pkct01$Selection)
      expect_identical(
        session$userData$tlg_order()$t_pkct01$Footnote, "Unsaved local footnote"
      )
    })
  })
})
