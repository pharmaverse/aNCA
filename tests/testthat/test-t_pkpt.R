# Shared fixture: minimal ADPP-like data frame
pkpt_data <- data.frame(
  USUBJID = rep(paste0("S", 1:6), each = 3),
  TRT01A  = rep(c("10mg", "10mg", "10mg", "50mg", "50mg", "50mg"), each = 3),
  DOSEA   = rep(c(10, 10, 10, 50, 50, 50), each = 3),
  AVISIT  = "Day 1",
  PARAM   = rep(c("Cmax", "AUClast", "Tmax"), 6),
  PARAMCD = rep(c("CMAX", "AUCLST", "TMAX"), 6),
  AVAL    = c(5, 20, 1,   6, 22, 1.2,  7, 21, 0.9,
              10, 40, 1.5, 11, 38, 1.3, 9, 42, 1.4),
  AVALU   = rep(c("ng/mL", "ng/mL*h", "h"), 6),
  PPCAT   = "Drug A Plasma",
  PPSPEC  = "Plasma",
  METABFL = NA_character_,
  stringsAsFactors = FALSE
)

# Metabolite variant: mark some rows as metabolite
pkpt_metab_data <- pkpt_data
pkpt_metab_data$METABFL[pkpt_metab_data$TRT01A == "50mg"] <- "Y"

describe("t_pkpt03_col", {
  it("returns a named list of data frames", {
    result <- t_pkpt03_col(pkpt_data)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "data.frame"))
  })

  it("contains expected statistic columns", {
    result <- t_pkpt03_col(pkpt_data)[[1]]
    expected <- c("TRT01A", "PARAM", "n", "Mean", "SD", "CV_pct",
                  "GeoMean", "GeoCV_pct", "Median", "Min", "Max")
    expect_true(all(expected %in% names(result)))
  })

  it("computes correct n per treatment and parameter", {
    result <- t_pkpt03_col(pkpt_data)[[1]]
    row_cmax_10 <- result[result$TRT01A == "10mg" & result$PARAM == "Cmax", ]
    expect_equal(row_cmax_10$n, 3)
  })

  it("computes Mean correctly", {
    result <- t_pkpt03_col(pkpt_data)[[1]]
    row <- result[result$TRT01A == "10mg" & result$PARAM == "Cmax", ]
    expect_equal(row$Mean, round(mean(c(5, 6, 7)), 3))
  })

  it("attaches readable labels to statistic columns", {
    result <- t_pkpt03_col(pkpt_data)[[1]]
    expect_equal(attr(result$GeoMean, "label"), "Geometric Mean")
    expect_equal(attr(result$GeoCV_pct, "label"), "Geometric CV%")
    expect_equal(attr(result$CV_pct, "label"), "CV%")
  })

  it("orders treatment arms naturally (9 mg before 10 mg, not lexically)", {
    nat <- pkpt_data
    nat$TRT01A <- ifelse(nat$TRT01A == "10mg", "9 mg", "10 mg")
    result <- t_pkpt03_col(nat)[[1]]
    expect_true(max(which(result$TRT01A == "9 mg")) <
                  min(which(result$TRT01A == "10 mg")))
  })

  it("returns NA for SD when only one observation", {
    one_obs <- pkpt_data[pkpt_data$USUBJID == "S1", ]
    result <- t_pkpt03_col(one_obs)[[1]]
    row <- result[result$PARAM == "Cmax", ]
    expect_true(is.na(row$SD))
  })

  it("stops with informative error when required columns are missing", {
    bad <- pkpt_data[, setdiff(names(pkpt_data), "AVAL")]
    expect_error(t_pkpt03_col(bad), "missing required columns")
  })

  it("produces one table per PPCAT when split by PPCAT (default)", {
    two_cats <- rbind(
      pkpt_data,
      transform(pkpt_data, PPCAT = "DrugB Plasma")
    )
    result <- t_pkpt03_col(two_cats)
    expect_equal(length(result), 2)
  })

  it("splits by AVISIT when explicitly passed in list_vars", {
    two_visits <- rbind(
      pkpt_data,
      transform(pkpt_data, AVISIT = "Day 7")
    )
    result <- t_pkpt03_col(two_visits, list_vars = c("AVISIT", "PPCAT"))
    expect_equal(length(result), 2)
  })
})

describe("t_pkpt03_MP_col", {
  it("filters to metabolite rows before summarizing (METABFL path)", {
    result <- t_pkpt03_MP_col(pkpt_metab_data)[[1]]
    # Only 50mg arm has METABFL set — only that arm should appear
    expect_true(all(result$TRT01A == "50mg"))
  })

  it("falls back to PPCAT when METABFL absent and PPCAT contains 'metab'", {
    # Simulate ADPP without METABFL: PPCAT identifies metabolite rows
    data_ppcat <- pkpt_data
    data_ppcat$PPCAT <- ifelse(data_ppcat$TRT01A == "50mg", "Metab-DrugA Plasma", "DrugA Plasma")
    data_ppcat <- data_ppcat[, setdiff(names(data_ppcat), "METABFL")]
    result <- t_pkpt03_MP_col(data_ppcat)[[1]]
    expect_true(all(result$TRT01A == "50mg"))
  })

  it("falls back to PARAM when METABFL and PPCAT both absent but PARAM contains 'metab'", {
    data_param <- pkpt_data
    data_param$PARAM <- ifelse(data_param$TRT01A == "50mg",
                               paste0("Metab-", data_param$PARAM),
                               data_param$PARAM)
    data_param <- data_param[, setdiff(names(data_param), c("METABFL", "PPCAT"))]
    result <- t_pkpt03_MP_col(data_param)[[1]]
    expect_true(all(result$TRT01A == "50mg"))
  })

  it("stops when METABFL absent and no 'metab' in PPCAT or PARAM", {
    data_no_metabfl <- pkpt_data[, setdiff(names(pkpt_data), "METABFL")]
    expect_error(t_pkpt03_MP_col(data_no_metabfl), "no metabolite data found")
  })

  it("stops with informative error when METABFL is all missing", {
    expect_error(t_pkpt03_MP_col(pkpt_data), "no metabolite data found")
  })
})

describe("t_pkpt07_norm", {
  it("filters to dose-normalized parameters (PARAMCD ending in D)", {
    data_with_dn <- pkpt_data
    data_with_dn$PARAMCD <- rep(c("CMAXD", "AUCTLSTD", "TMAX"), 6)
    data_with_dn$PARAM   <- rep(c("Cmax/D", "AUClast/D", "Tmax"), 6)
    result <- t_pkpt07_norm(data_with_dn)[[1]]
    expect_true(all(grepl("D$", result$PARAM) | result$PARAM == "Tmax"))
    expect_false("Tmax" %in% result$PARAM)  # TMAX doesn't end in D
  })

  it("stops with informative error when no dose-normalized params found", {
    expect_error(t_pkpt07_norm(pkpt_data), "no dose-normalized parameters")
  })
})

describe("t_pkpt08_uri", {
  it("filters to urine records before summarizing", {
    data_mixed <- pkpt_data
    data_mixed$PPSPEC[data_mixed$TRT01A == "10mg"] <- "URINE"
    result <- t_pkpt08_uri(data_mixed)[[1]]
    # Only urine rows (10mg arm) survive the filter
    expect_true(all(result$TRT01A == "10mg"))
  })

  it("stops with informative error when no urine records present", {
    expect_error(
      t_pkpt08_uri(pkpt_data),  # all Plasma
      "no urine PK parameter data found"
    )
  })

  it("warns when PPSPEC column is absent and skips the urine filter", {
    data_no_ppspec <- pkpt_data[, setdiff(names(pkpt_data), "PPSPEC")]
    expect_warning(
      t_pkpt08_uri(data_no_ppspec),
      "PPSPEC.*not found"
    )
  })
})

describe("t_pkpt11_gmr", {
  it("returns a named list of data frames", {
    result <- t_pkpt11_gmr(pkpt_data, ref_arm = "10mg")
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "data.frame"))
  })

  it("contains GMR, CI_lower, CI_upper columns", {
    result <- t_pkpt11_gmr(pkpt_data, ref_arm = "10mg")[[1]]
    expect_true(all(c("GMR", "CI_lower", "CI_upper") %in% names(result)))
  })

  it("GMR is ratio of geometric means (50mg / 10mg)", {
    result <- t_pkpt11_gmr(pkpt_data, ref_arm = "10mg")[[1]]
    row <- result[result$PARAM == "Cmax", ]
    gm_10 <- exp(mean(log(c(5, 6, 7))))
    gm_50 <- exp(mean(log(c(10, 11, 9))))
    expect_equal(row$GMR, round(gm_50 / gm_10, 3))
  })

  it("CI_lower < GMR < CI_upper", {
    result <- t_pkpt11_gmr(pkpt_data, ref_arm = "10mg")[[1]]
    expect_true(all(result$CI_lower < result$GMR, na.rm = TRUE))
    expect_true(all(result$GMR < result$CI_upper, na.rm = TRUE))
  })

  it("uses first sorted arm as reference when ref_arm is NULL", {
    result <- t_pkpt11_gmr(pkpt_data)[[1]]
    # "10mg" sorts before "50mg", so 10mg is ref → only 50mg rows appear
    expect_true(all(result$TRT01A == "50mg"))
  })

  it("stops with informative error when ref_arm not found", {
    expect_error(
      t_pkpt11_gmr(pkpt_data, ref_arm = "100mg"),
      "not found"
    )
  })

  it("stops when required columns are missing", {
    bad <- pkpt_data[, setdiff(names(pkpt_data), "AVAL")]
    expect_error(t_pkpt11_gmr(bad), "missing required columns")
  })

  it("returns NA (not NaN) CI bounds when both arms have zero within-arm log-variance", {
    # Regression: when sd(log(ref_vals)) == sd(log(trt_vals)) == 0, the Welch df
    # formula yields 0/0 = NaN.  max(NaN, 1) returns NaN in R (not 1), so qt()
    # returns NaN and CI bounds become NaN rather than NA, corrupting the table.
    zero_var_data <- data.frame(
      USUBJID = paste0("S", 1:6),
      TRT01A  = c("10mg", "10mg", "10mg", "50mg", "50mg", "50mg"),
      PARAM   = "Cmax",
      PARAMCD = "CMAX",
      AVAL    = c(5, 5, 5, 10, 10, 10),  # identical values within each arm
      AVALU   = "ng/mL",
      PPCAT   = "DrugA Plasma",
      PPSPEC  = "PLASMA",
      stringsAsFactors = FALSE
    )
    result <- t_pkpt11_gmr(zero_var_data, ref_arm = "10mg")[[1]]
    row <- result[result$PARAM == "Cmax", ]
    expect_true(is.na(row$CI_lower), info = "CI_lower should be NA, not NaN")
    expect_true(is.na(row$CI_upper), info = "CI_upper should be NA, not NaN")
    # GMR itself is still computable (ratio of identical geometric means)
    expect_true(is.finite(row$GMR))
  })

  it("returns empty data frame and warns when ref_arm absent from a sub-split", {
    # Create two-split data where ref_arm "10mg" only exists in one PPCAT
    split_data <- rbind(
      pkpt_data,
      transform(pkpt_data[pkpt_data$TRT01A == "50mg", ],
                PPCAT = "DrugB Plasma")
    )
    # "DrugB Plasma" split has only 50mg rows — ref_arm "10mg" is absent
    expect_warning(
      result <- t_pkpt11_gmr(split_data, ref_arm = "10mg"),
      "reference arm"
    )
    # The DrugA split returns a normal table; the DrugB split is empty
    drug_b_key <- grep("DrugB", names(result), value = TRUE)
    expect_equal(nrow(result[[drug_b_key]]), 0)
  })
})
