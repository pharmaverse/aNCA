# App helpers must work with library(aNCA), without load_all() exposing internals.
source_app_exports <- function(path) {
  env <- new.env(parent = baseenv())
  for (pkg in c("aNCA", "shiny", "logger", "stats", "rlang")) {
    for (name in getNamespaceExports(pkg)) {
      assign(name, getExportedValue(pkg, name), envir = env)
    }
  }
  sys.source(file.path(system.file("shiny", package = "aNCA"), path), envir = env)
  env
}
