describe("namespace-only PKNCA extensions", {
  it("registers callable excretion parameters without attaching aNCA", {
    skip_if_not(file.exists(system.file("Meta/package.rds", package = "aNCA")),
                "Run against the installed source package")
    script <- withr::local_tempfile(fileext = ".R")
    paths <- paste(capture.output(dput(.libPaths())), collapse = "\n")
    writeLines(c(
      paste0(".libPaths(", paths, ")"),
      "stopifnot(!('package:aNCA' %in% search()))",
      "loadNamespace('aNCA')",
      "stopifnot(!('package:aNCA' %in% search()))",
      "stopifnot(all(c('ertlst', 'ertmax', 'volpk', 'fe') %in% names(PKNCA::get.interval.cols())))",
      "fun <- getAnywhere(PKNCA::get.interval.cols()[['volpk']]$FUN)$objs[[1]]",
      "stopifnot(isTRUE(all.equal(fun(c(1, 2, 3)), 6)))"
    ), script)
    output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                       c("--vanilla", shQuote(script)),
                                       stdout = TRUE, stderr = TRUE))
    expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  })
})
