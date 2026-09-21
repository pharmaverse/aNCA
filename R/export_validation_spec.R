# Validator IDs supported by the export preflight. The YAML specification may
# select only these static validators; it never supplies executable code.
EXPORT_VALIDATOR_IDS <- c(
  "standard_output_structure", "cdisc_schema", "settings_structure",
  "script_structure", "slides_structure", "pre_specs_structure",
  "session_info_structure"
)

# Read the installed export-validation traceability matrix.
.read_export_validation_spec <- function() {
  path <- system.file("extdata", "export-validation.yml", package = "aNCA")
  if (!nzchar(path) || !file.exists(path)) {
    return(structure(list(), class = "export_validation_spec_missing"))
  }
  read_yaml(path)
}

# Return readable errors when the traceability matrix cannot safely drive the
# export gate. The gate treats any returned error as a blocking finding.
.validate_export_validation_spec <- function(spec = .read_export_validation_spec()) {
  if (inherits(spec, "export_validation_spec_missing")) {
    return("The export validation specification is unavailable.")
  }
  rules <- spec$requirements
  if (!is.list(rules) || length(rules) == 0) {
    return("The export validation specification has no requirements.")
  }

  ids <- vapply(rules, `[[`, "", "id")
  validators <- vapply(rules, `[[`, "", "validator")
  requirements <- vapply(rules, `[[`, "", "requirement")
  blocks <- vapply(rules, function(rule) isTRUE(rule$blocks_export), logical(1))
  tests <- vapply(rules, `[[`, "", "test")
  errors <- character(0)
  if (any(!nzchar(ids)) || anyDuplicated(ids)) {
    errors <- c(errors, "Requirement IDs must be present and unique.")
  }
  if (!setequal(ids, EXPORT_VALIDATOR_IDS) || !setequal(validators, EXPORT_VALIDATOR_IDS)) {
    errors <- c(errors, "Requirements must declare each supported validator exactly once.")
  }
  if (any(requirements != "21 CFR §11.10(a)")) {
    errors <- c(errors, "Every requirement must cite 21 CFR §11.10(a).")
  }
  if (!all(blocks)) {
    errors <- c(errors, "Every requirement must block export when it fails.")
  }
  if (any(!nzchar(tests))) {
    errors <- c(errors, "Every requirement must reference a regression test.")
  }
  errors
}

# Is a named validation rule enabled by the controlled YAML specification?
.export_validator_enabled <- function(id, spec = .read_export_validation_spec()) {
  if (length(.validate_export_validation_spec(spec)) > 0) return(FALSE)
  any(vapply(spec$requirements, function(rule) identical(rule$validator, id), logical(1)))
}
