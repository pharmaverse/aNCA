# Validates that every sidebar `options:` key declared in tlg.yaml maps to a
# settable argument of its TLG function.  This guards two failure modes that
# only surface at render time via `do.call()` in tlg_module_server():
#   1. "unused argument" — the option name is neither a formal argument of the
#      function nor absorbed by `...`.
#   2. "duplicate argument" — the option flows through `...` into a wrapper that
#      hardcodes that same argument in its forwarded call (e.g. t_pkct01_dose
#      forwards strat_var = "DOSEA").

describe("tlg.yaml sidebar options", {
  defs <- yaml::read_yaml(system.file("shiny/tlg.yaml", package = "aNCA"))

  # Real option widgets are named lists with a `type`; ".group_label_*" section
  # headers are plain strings and are skipped (mirrors tlg_module_server()).
  option_keys <- function(options) {
    keys <- names(options)
    keys[!vapply(options, is.character, logical(1))]
  }

  entries_with_options <- Filter(function(d) !is.null(d$options), defs)

  it("declares at least one options block (guards against silent regressions)", {
    expect_gt(length(entries_with_options), 0)
  })

  for (id in names(entries_with_options)) {
    entry <- entries_with_options[[id]]

    it(paste0("`", id, "` option keys are settable arguments of ", entry$fun), {
      fn <- getFromNamespace(entry$fun, "aNCA")
      arg_names <- names(formals(fn))
      has_dots  <- "..." %in% arg_names
      body_src  <- paste(deparse(body(fn)), collapse = "\n")

      for (key in option_keys(entry$options)) {
        if (key %in% arg_names) {
          # Bound to a named parameter — always safe, even if the wrapper passes
          # it on under the same name (e.g. l_pkpl04_mp grouping_vars/title).
          succeed()
        } else {
          # Not a formal: it can only reach the function via `...`.
          expect_true(
            has_dots,
            info = paste0(
              "Option '", key, "' on '", id, "' is not an argument of ",
              entry$fun, "() and the function has no `...` to absorb it."
            )
          )
          # And it must not be hardcoded in a forwarded call, or `do.call()`
          # would raise a duplicate-argument error.
          expect_false(
            grepl(paste0("\\b", key, "\\s*="), body_src),
            info = paste0(
              "Option '", key, "' on '", id, "' flows through `...` but ",
              entry$fun, "() hardcodes '", key, "' in a forwarded call ",
              "(duplicate-argument error at render time)."
            )
          )
        }
      }
    })
  }
})
