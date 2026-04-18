# Test Code Coverage Report: R/PKNCA.R

**Generated:** 2026-04-07  
**Source File:** R/PKNCA.R  
**Test File:** tests/testthat/test-PKNCA.R

---

## Summary

| Metric | Value |
|--------|-------|
| **Total Coverage** | 93.33% |
| **Lines Covered** | 140 / 150 |
| **Lines Not Covered** | 10 |

---

## Functions Overview

| Function | Status | Coverage |
|----------|--------|----------|
| `PKNCA_create_data_object` | Tested | Full |
| `PKNCA_update_data_object` | Tested | Full |
| `PKNCA_calculate_nca` | Tested | Full |
| `PKNCA_impute_method_start_logslope` | Tested | Full |
| `PKNCA_impute_method_start_c1` | Tested | Full |
| `PKNCA_build_units_table` | Tested | Full |
| `ensure_column_unit_exists` | Tested | Full |
| `select_minimal_grouping_cols` | Tested | Full |
| `check_valid_pknca_data` | Tested | Full |
| `add_exclusion_reasons` | Tested | Full |
| `PKNCA_hl_rules_exclusion` | **Not Tested** | 0% |
| `remove_pp_not_requested` | **Not Tested** | 0% |

---

## Untested Functions

### 1. `PKNCA_hl_rules_exclusion` (Lines 709-731)

Entire function is not covered by tests.

```r
PKNCA_hl_rules_exclusion <- function(res, rules) {
  for (param in names(rules)) {
    if (startsWith(param, "AUCPE")) {
      exc_fun <- PKNCA::exclude_nca_by_param(...)
    } else {
      exc_fun <- PKNCA::exclude_nca_by_param(...)
    }
    res <- PKNCA::exclude(res, FUN = exc_fun)
  }
  res
}
```

**Missing coverage:**
- Line 709: `for (param in names(rules))`
- Line 710: `if (startsWith(param, "AUCPE"))`
- Line 711-719: `exclude_nca_by_param` with `max_thr` (AUCPE branch)
- Line 721-727: `exclude_nca_by_param` with `min_thr` (else branch)
- Line 729: `res <- PKNCA::exclude(res, FUN = exc_fun)`
- Line 731: `res`

---

### 2. `remove_pp_not_requested` (Lines 788-807)

Entire function is not covered by tests.

```r
remove_pp_not_requested <- function(pknca_res) {
  params <- c(setdiff(names(PKNCA::get.interval.cols()), c("start", "end")))
  params_not_requested <- pknca_res$data$intervals %>%
    pivot_longer(...) %>%
    mutate(...) %>%
    group_by(...) %>%
    summarise(...) %>%
    filter(!is_requested)

  pknca_res$result <- pknca_res$result %>%
    anti_join(params_not_requested, ...)
  pknca_res
}
```

**Missing coverage:**
- Line 788: `params <- c(...)`
- Line 790: `params_not_requested <- pknca_res$data$intervals %>% ...`
- Line 805: `pknca_res$result <- pknca_res$result %>% anti_join(...)`
- Line 807: `pknca_res`

---

## Recommendations

1. **Add tests for `PKNCA_hl_rules_exclusion`**: Create mock NCA results and test both branches (AUCPE parameters with max threshold, other parameters with min threshold).

2. **Add tests for `remove_pp_not_requested`**: Create mock PKNCA results with intervals and test the filtering of non-requested parameters.

3. **Total lines to cover**: 10 lines remain untested.

---

## Notes

- Coverage excludes comments and roxygen documentation
- Branch coverage is tracked: both branches of conditionals require individual test cases
- Tests are run using `covr::file_coverage()` with `tests/testthat/setup.R` sourced
