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

  it("formats oversized upload errors", {
    expect_equal(
      .upload_size_error_message("large.csv", max_size_mb = 30),
      "large.csv was not loaded because it exceeded the maximum upload size (30 MB)."
    )
  })

  it("detects oversized files before reading them", {
    uploads <- data.frame(
      name = c("small.csv", "large.csv"),
      size = c(4, 6) * 1024^2,
      datapath = c("small-path", "large-path")
    )

    oversized <- .oversized_uploads(uploads, max_size_bytes = 5 * 1024^2)

    expect_equal(oversized$name, "large.csv")
    expect_equal(oversized$datapath, "large-path")
  })

  it("returns no oversized files when nothing was uploaded", {
    oversized <- .oversized_uploads(NULL, max_size_bytes = 5 * 1024^2)

    expect_equal(nrow(oversized), 0)
  })
})
