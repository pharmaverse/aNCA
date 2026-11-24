describe("namespace-only PKNCA extensions", {
  it("registers callable excretion parameters with only base R attached", {
    skip_if_not(file.exists(system.file("Meta/package.rds", package = "aNCA")),
                "Run against the installed source package")
    script <- withr::local_tempfile(fileext = ".R")
    paths <- paste(capture.output(dput(.libPaths())), collapse = "\n")
    writeLines(c(
      paste0(".libPaths(", paths, ")"),
      "stopifnot(identical(search(), c('.GlobalEnv', 'Autoloads', 'package:base')))",
      "loadNamespace('aNCA')",
      "stopifnot(!('package:aNCA' %in% search()))",
      "stopifnot(all(c('ertlst', 'ertmax', 'volpk', 'fe') %in% names(PKNCA::get.interval.cols())))",
      "callback <- PKNCA::get.interval.cols()[['volpk']]$FUN",
      "owner <- if (callback %in% getNamespaceExports('PKNCA')) 'PKNCA' else 'aNCA'",
      "fun <- getExportedValue(owner, callback)",
      "stopifnot(isTRUE(all.equal(fun(c(1, 2, 3)), 6)))"
    ), script)
    output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                       c("--vanilla", "--default-packages=NULL", shQuote(script)),
                                       stdout = TRUE, stderr = TRUE))
    expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  })
})
