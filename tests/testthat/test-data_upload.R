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
