describe("export validation traceability matrix", {
  it("covers every supported non-TLG export validator under 21 CFR 11.10(a)", {
    spec <- aNCA:::.read_export_validation_spec()
    expect_equal(aNCA:::.validate_export_spec(spec), character(0))
    rules <- spec$requirements
    expect_setequal(
      vapply(rules, `[[`, "", "validator"),
      aNCA:::EXPORT_VALIDATOR_IDS
    )
    expect_true(all(vapply(rules, `[[`, "", "requirement") == "21 CFR §11.10(a)"))
    expect_true(all(vapply(rules, function(rule) isTRUE(rule$blocks_export), logical(1))))
  })
})
