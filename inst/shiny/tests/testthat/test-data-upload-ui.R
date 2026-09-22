describe("data upload UI helper text", {
  it("uses Shiny's default upload limit when no app option is set", {
    old_options <- options(shiny.maxRequestSize = NULL)
    on.exit(options(old_options), add = TRUE)

    expect_equal(
      .upload_file_help_text(readers = c("csv", "rds")),
      "Accepted: csv, rds - Max 5 MB"
    )
  })

  it("uses the configured Shiny upload limit", {
    old_options <- options(shiny.maxRequestSize = 30 * 1024^2)
    on.exit(options(old_options), add = TRUE)

    expect_equal(
      .upload_file_help_text(readers = c("csv", "rds")),
      "Accepted: csv, rds - Max 30 MB"
    )
  })
})
