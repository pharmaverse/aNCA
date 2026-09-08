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

# Validates that a literal `default:` on a select option is one of that option's own
# literal `choices:`.  A mismatch leaves selectInput with no matching option, so the
# widget renders blank and the user cannot tell what value is actually in effect.

# Mirrors the template merge in inst/shiny/modules/tab_tlg.R
resolve_tlg_templates <- function(defs) {
  lapply(defs, function(entry) {
    if (!"template" %in% names(entry)) return(entry)
    base <- defs[[entry$template]]
    for (key in setdiff(names(entry), "template")) {
      if (key != "options") {
        base[[key]] <- entry[[key]]
        next
      }
      for (opt in names(entry$options)) base$options[[opt]] <- entry$options[[opt]]
    }
    base
  })
}

# `.colnames`, `.stats`, `$PARAM` etc. are resolved at runtime against the data.
is_runtime_token <- function(x) {
  length(x) == 1 && is.character(x) && grepl("^[.$]", x)
}

is_literal_value <- function(x) {
  !is.null(x) && !is_runtime_token(x)
}

is_comparable_select <- function(opt) {
  if (!is.list(opt)) return(FALSE)
  if (!identical(opt$type, "select")) return(FALSE)
  is_literal_value(opt$default) && is_literal_value(opt$choices)
}

# Select options carrying both a literal default and a literal choice list.
comparable_select_options <- function(entry) {
  if (is.null(entry$options)) return(list())
  Filter(is_comparable_select, entry$options)
}

describe("tlg.yaml select defaults", {
  resolved <- resolve_tlg_templates(
    yaml::read_yaml(system.file("shiny/tlg.yaml", package = "aNCA"))
  )

  for (id in names(resolved)) {
    for (key in names(comparable_select_options(resolved[[id]]))) {
      opt     <- resolved[[id]]$options[[key]]
      missing <- setdiff(unlist(opt$default), unlist(opt$choices))

      it(paste0("`", id, "` option '", key, "' defaults to one of its own choices"), {
        expect_equal(
          missing, character(0),
          info = paste0(
            "Option '", key, "' on '", id, "' defaults to a value that is not in its ",
            "choices (", paste(unlist(opt$choices), collapse = ", "), "). ",
            "The dropdown will render blank."
          )
        )
      })
    }
  }
})

# Issue #1430: every catalog entry must offer Title, Subtitle and Footnote in the sidebar,
# and the reference wording from the TLG catalog must be pre-filled where a static default
# is meaningful.  Counted after the template merge, since `g_pkcg02_*` and the `_log`/`_sbs`
# variants inherit their whole Labs block from `g_pkcg01_lin` / `g_pkcg03_lin`.

LAB_OPTIONS <- c("title", "subtitle", "footnote")

# The three pkcg families build title and subtitle per render (scale wording, cohort, and a
# "N=" subject count that `parse_annotation` has no token for), so a static default would be
# a regression rather than an improvement.  They are exempt from the pre-filled-title check.
DYNAMIC_TITLE_ENTRIES <- c(
  "g_pkcg01_lin", "g_pkcg01_log", "g_pkcg01_sbs",
  "g_pkcg02_lin", "g_pkcg02_log", "g_pkcg02_sbs",
  "g_pkcg03_lin", "g_pkcg03_log", "g_pkcg03_sbs",
  "p_pkcg03_lin_dose", "p_pkcg03_log_dose", "p_pkcg03_sbs_dose"
)

lab_default <- function(entry, key) entry$options[[key]]$default

# Entry ids whose `key` label carries no pre-filled default.
entries_without_default <- function(resolved, key, exclude = character()) {
  Filter(
    function(id) is.null(lab_default(resolved[[id]], key)),
    setdiff(names(resolved), exclude)
  )
}

# Column tokens other than $STUDYID found in any label default, reported as
# "<entry>.<option>: <tokens>".
disallowed_default_tokens <- function(resolved) {
  found <- lapply(names(resolved), function(id) {
    unlist(lapply(LAB_OPTIONS, function(key) {
      value <- lab_default(resolved[[id]], key)
      if (is.null(value)) return(NULL)
      tokens <- setdiff(unlist(regmatches(value, gregexpr("\\$\\w+", value))), "$STUDYID")
      if (length(tokens) == 0) return(NULL)
      paste0(id, ".", key, ": ", paste(tokens, collapse = ", "))
    }))
  })
  unlist(found)
}

# Listing entries whose title default spans more than one line.
multiline_listing_titles <- function(resolved) {
  listings <- Filter(function(e) identical(e$type, "Listing"), resolved)
  Filter(function(id) {
    value <- lab_default(listings[[id]], "title")
    !is.null(value) && grepl("\n", value, fixed = TRUE)
  }, names(listings))
}

describe("tlg.yaml label options", {
  resolved <- resolve_tlg_templates(
    yaml::read_yaml(system.file("shiny/tlg.yaml", package = "aNCA"))
  )

  for (id in names(resolved)) {
    opts <- resolved[[id]]$options

    it(paste0("`", id, "` exposes all three label options as text widgets"), {
      expect_equal(
        setdiff(LAB_OPTIONS, names(opts)), character(0),
        info = paste0(
          "'", id, "' is missing a label option, so the user cannot set it in the sidebar."
        )
      )
      for (key in LAB_OPTIONS) expect_identical(opts[[key]]$type, "text")
    })
  }

  it("every entry outside the pkcg families pre-fills a catalog title", {
    expect_equal(
      entries_without_default(resolved, "title", DYNAMIC_TITLE_ENTRIES), character(0)
    )
  })

  it("every entry pre-fills a footnote", {
    expect_equal(entries_without_default(resolved, "footnote"), character(0))
  })

  it("the pkcg families deliberately leave title and subtitle unset", {
    unset <- intersect(
      entries_without_default(resolved, "title"),
      entries_without_default(resolved, "subtitle")
    )
    expect_setequal(intersect(unset, DYNAMIC_TITLE_ENTRIES), DYNAMIC_TITLE_ENTRIES)
  })

  # `$COL` expands to `unique(data[[COL]])`, so a token naming a column that varies within
  # a split returns a vector and the label renders blank.  `$STUDYID` is the only column
  # guaranteed single-valued across a study, so it is the only token allowed in a default.
  it("defaults interpolate no column token other than $STUDYID", {
    expect_equal(disallowed_default_tokens(resolved), NULL)
  })

  # Listing titles are run through parse_annotation, which rewrites newlines to "<br>", and
  # unlike the subtitle the result is not converted back, so a newline would print a literal
  # "<br>" in the rendered listing.
  it("listing titles are single-line", {
    expect_equal(multiline_listing_titles(resolved), character(0))
  })
})
