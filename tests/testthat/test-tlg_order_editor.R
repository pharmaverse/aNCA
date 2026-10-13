local({
  library(shiny)
  library(dplyr)
  source(system.file("shiny/modules/tab_tlg.R", package = "aNCA"), local = TRUE)
}, envir = parent.env(environment()))

describe("TLG order text editors", {
  it("retains edits while paging and resets values and input keys on restore", {
    node <- Sys.which("node")
    skip_if(node == "", "Node.js is needed to execute the cell renderer regression")
    definitions <- lapply(c("Footnote", "Stratification", "Comment"), function(field) {
      id <- paste0("tlg-selected_tlg_table-edit_", field)
      list(
        id = id, field = field,
        initial = as.character(.tlg_order_edit_cell(id, 1L)),
        restored = as.character(.tlg_order_edit_cell(id, 2L))
      )
    })
    input <- withr::local_tempfile(fileext = ".json")
    jsonlite::write_json(definitions, input, auto_unbox = TRUE)
    result <- suppressWarnings(system2(
      node, c(shQuote(test_path("js", "tlg_order_editor.cjs")), shQuote(input)),
      stdout = TRUE, stderr = TRUE
    ))
    status <- attr(result, "status")
    if (is.null(status)) status <- 0L
    expect_identical(status, 0L, info = paste(result, collapse = "\n"))
  })
})
