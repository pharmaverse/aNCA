describe(".mp_analyte_pairs", {
  it("uses distinct drug/flag mappings rather than concentration rows or analyte prefixes", {
    adnca <- mp_adnca_fixture()
    adnca$PARAM <- c("Compound", "Product")
    pairs <- .mp_analyte_pairs(rbind(adnca, adnca))
    expect_equal(nrow(pairs), 1)
    expect_equal(pairs$parent, "Compound")
    expect_equal(pairs$metabolite, "Product")
    expect_equal(pairs$DOSETRT, "DrugA")
  })

  it("keeps drug and study contexts separate", {
    adnca <- rbind(mp_adnca_fixture(), transform(
      mp_adnca_fixture(), STUDYID = "STUDY2", PARAM = c("DrugB", "ProductB")
    ))
    pairs <- .mp_analyte_pairs(adnca)
    expect_equal(pairs$parent[pairs$STUDYID == "STUDY2"], "DrugB")
    expect_equal(pairs$metabolite[pairs$STUDYID == "STUDY2"], "ProductB")
  })

  it("handles factor analyte names and multiple metabolites of one parent", {
    adnca <- mp_adnca_fixture()
    adnca <- rbind(adnca, transform(adnca[2, ], PARAM = "Another metabolite"))
    adnca$PARAM <- factor(adnca$PARAM)
    pairs <- .mp_analyte_pairs(adnca)
    expect_identical(pairs$parent, rep("DrugA", 2))
    expect_setequal(pairs$metabolite, c("Metab-DrugA", "Another metabolite"))
  })

  it("does not guess a parent when mapping is missing or ambiguous", {
    adnca <- mp_adnca_fixture()
    expect_error(.mp_analyte_pairs(adnca[-1, ]), "parent")
    expect_error(.mp_analyte_pairs(rbind(
      adnca, transform(adnca[1, ], PARAM = "Another parent")
    )), "ambiguous")
    expect_error(.mp_analyte_pairs(adnca[, -4]), "METABFL")
    expect_error(.mp_analyte_pairs(transform(adnca, DOSETRT = PARAM)), "DOSETRT")
  })
})

describe(".mp_check_columns", {
  it("accepts only the calculated ratio value and unit columns", {
    expect_silent(.mp_check_columns("AVAL", "AVALU"))
    for (value in list("PPSTRESN", "", NA_character_, NULL, c("AVAL", "PPSTRESN"))) {
      expect_error(.mp_check_columns(value), "value_var must be 'AVAL'")
    }
    for (unit in list("PPSTRESU", "", NA_character_, NULL, c("AVALU", "PPSTRESU"))) {
      expect_error(.mp_check_columns("AVAL", unit), "unit_var must be 'AVALU'")
    }
  })
})

describe(".mp_ratio_data", {
  ratios <- function(data, ...) .mp_ratio_data(data, "DrugA", "Metab-DrugA", "test", ...)

  it("divides matched metabolite values by parent values without configured ratios", {
    out <- ratios(mp_adpp_fixture())
    expect_equal(nrow(out), 8)
    expect_equal(out$AVAL[out$ATPTREF == "DOSE 1" & out$PARAMCD == "CMAX"], c(0.5, 0.3))
    expect_equal(out$AVAL[out$ATPTREF == "DOSE 2" & out$PARAMCD == "CMAX"], c(0.25, 0.75))
    expect_true(all(out$AVALU == "fraction"))
    expect_true(all(out$RATIO == "Metab-DrugA / DrugA"))
    expect_true(all(out$RATIOREF == "DrugA"))
  })

  it("matches parameter, study, specimen, treatment, visit and interval", {
    base <- mp_adpp_fixture()
    for (key in c("STUDYID", "PPSPEC", "TRT01A", "ROUTE", "AVISIT", "PPSTINT")) {
      one <- base
      one[[key]] <- "A"
      two <- one
      two[[key]] <- "B"
      two$AVAL[two$PPCAT == "Metab-DrugA"] <- two$AVAL[two$PPCAT == "Metab-DrugA"] * 2
      out <- ratios(rbind(one, two))
      expect_equal(nrow(out), 16, info = key)
      expect_equal(sort(out$AVAL[out[[key]] == "B"]), sort(ratios(base)$AVAL * 2), info = key)
    }
  })

  it("converts compatible units with the existing unit helper", {
    data <- mp_adpp_fixture()
    parent <- data$PPCAT == "DrugA" & data$PARAMCD == "CMAX"
    data$AVAL[parent] <- data$AVAL[parent] / 1000
    data$AVALU[parent] <- "ug/mL"
    expect_equal(ratios(data)$AVAL, ratios(mp_adpp_fixture())$AVAL)
  })

  it("skips invalid pairs but retains a valid zero numerator", {
    data <- mp_adpp_fixture()
    parent <- which(data$PPCAT == "DrugA")
    metab <- which(data$PPCAT == "Metab-DrugA")
    data$AVAL[parent[1:4]] <- c(0, NA, Inf, -Inf)
    data$AVAL[metab[5]] <- 0
    expect_warning(out <- ratios(data), "skipped 4")
    expect_equal(nrow(out), 4)
    expect_true(all(is.finite(out$AVAL)))
    expect_true(0 %in% out$AVAL)
    expect_error(ratios(transform(data, AVAL = NA_real_)), "no usable")
  })

  it("does not convert an incompatible unit or a missing parent into a numeric result", {
    data <- mp_adpp_fixture()
    data$AVALU[1] <- "h"
    expect_warning(out <- ratios(data), "skipped 1")
    expect_equal(nrow(out), 7)
    expect_warning(out <- ratios(mp_adpp_fixture()[-1, ]), "skipped 1")
    expect_equal(nrow(out), 7)
  })

  it("allows identical duplicate records but rejects conflicting matches", {
    data <- mp_adpp_fixture()
    expect_equal(ratios(rbind(data, data)), ratios(data))
    conflict <- data[1, ]
    conflict$AVAL <- 99
    expect_error(ratios(rbind(data, conflict)), "ambiguous")
  })

  it("keeps mean-residence-time parameters and excludes already-derived ratio rows", {
    data <- mp_adpp_fixture()
    data$PARAMCD[data$PARAMCD == "CMAX"] <- "MRTLST"
    data$PPANMETH <- NA_character_
    extra <- transform(data, PARAMCD = "CUSTOM", PPANMETH = "CMAX TO CMAX [PARAM: DrugA]")
    out <- ratios(rbind(data, extra))
    expect_setequal(out$PARAMCD, c("MRTLST", "AUCLST"))
    expect_equal(nrow(out), 8)
  })

  it("excludes either-side summary flags from summaries but retains them in listings", {
    data <- mp_adpp_fixture()
    data$PPSUMXF[1] <- "Y"
    data$PPSUMXF[10] <- "Y"
    all_rows <- ratios(data)
    summary_rows <- ratios(data, summary = TRUE)
    expect_equal(sum(all_rows$PPSUMXF == "Y"), 2)
    expect_equal(nrow(summary_rows), 6)
    expect_false(any(summary_rows$PPSUMXF == "Y"))
  })
})

describe("M/P TLGs: consistent individual ratios and profile preservation", {
  it("accepts actual NCA exports without configured ratio rows or visit columns", {
    parent <- FIXTURE_CONC_DATA[FIXTURE_CONC_DATA$USUBJID == 2, ]
    parent <- transform(
      parent, DOSEA = ATPTREF, DOSEU = "mg/kg", ADOSEDUR = 0,
      PCSPEC = "PLASMA", TRT01A = "Treatment A"
    )
    parent$DOSNOA <- NULL # Re-derived by the concentration-data preprocessing step.
    metabolite <- transform(parent, PARAM = "B", METABFL = "Y", AVAL = AVAL / 2)
    adnca <- rbind(parent, metabolite)
    obj <- PKNCA_update_data_object(
      PKNCA_create_data_object(adnca), method = "lin up/log down",
      selected_analytes = c("A", "B"), selected_profile = unique(adnca$ATPTREF),
      selected_pcspec = unique(adnca$PCSPEC)
    )
    obj$intervals$cmax <- TRUE
    obj$intervals$auclast <- TRUE
    adpp <- export_cdisc(PKNCA_calculate_nca(obj))$adpp
    adpp <- adpp[adpp$PARAMCD %in% c("CMAX", "AUCLST"), ]
    expect_false("AVISIT" %in% names(adpp))
    expect_true(all(c("PPSTRESN", "PPSTRESU") %in% names(adpp)))
    out <- .mp_ratio_data(adpp, "A", "B", "test")
    expect_equal(nrow(out), 4)
    expect_equal(as.numeric(out$AVAL), rep(0.5, 4))
    expect_setequal(out$ATPTREF, c(1, 2))
    for (fun in list(t_pkpt03_MP_col, l_pkpl01_mp, p_pkpg06_mp)) {
      expect_error(
        fun(adpp, parent = "A", metabolite = "B", value_var = "PPSTRESN"),
        "value_var must be 'AVAL'"
      )
    }
    expect_error(
      l_pkpl01_mp(adpp, parent = "A", metabolite = "B", unit_var = "PPSTRESU"),
      "unit_var must be 'AVALU'"
    )
  })

  it("uses every paired dose in all three outputs, even without AVISIT", {
    data <- mp_adpp_fixture()
    args <- list(data = data, parent = "DrugA", metabolite = "Metab-DrugA", value_var = "AVAL")
    tables <- do.call(t_pkpt03_MP_col, args)
    plots <- do.call(p_pkpg06_mp, args)
    listings <- do.call(l_pkpl01_mp, c(args, list(unit_var = "AVALU")))
    expect_length(tables, 2)
    expect_identical(names(tables), names(plots))
    expect_identical(names(tables), names(listings))
    for (i in seq_along(tables)) {
      expected <- if (grepl("DOSE 1", names(tables)[i])) c(0.5, 0.3) else c(0.25, 0.75)
      expect_equal(as.numeric(listings[[i]]$Cmax), expected)
      expect_equal(plots[[i]]$data$AVAL[plots[[i]]$data$PARAMCD == "CMAX"], expected)
      expect_equal(tables[[i]]$Mean[tables[[i]]$PARAM == "Cmax"], mean(expected))
      expect_equal(tables[[i]]$n[tables[[i]]$PARAM == "Cmax"], 2)
      expect_match(formatters::var_labels(listings[[i]])[["Cmax"]], "fraction")
    }
  })

  it("keeps raw column selection available for ordinary PK outputs", {
    data <- subset(mp_adpp_fixture(), PPCAT == "Metab-DrugA" & ATPTREF == "DOSE 1")
    table <- t_pkpt03_col(data, value_var = "PPSTRESN")[[1]]
    plot <- p_pkpg03_boxp(data, value_var = "PPSTRESN")[[1]]
    listing <- l_pkpl01(data, value_var = "PPSTRESN", unit_var = "PPSTRESU")[[1]]
    expect_equal(table$Mean[table$PARAM == "Cmax"], 7)
    expect_equal(plot$data$PPSTRESN[plot$data$PARAMCD == "CMAX"], c(5, 9))
    expect_equal(as.numeric(listing$Cmax), c(5, 9))
    expect_match(formatters::var_labels(listing)[["Cmax"]], "ng/mL", fixed = TRUE)
  })

  it("does not lose dose context when custom split options omit it", {
    data <- mp_adpp_fixture()
    plots <- p_pkpg06_mp(data, parent = "DrugA", metabolite = "Metab-DrugA", list_vars = "RATIO")
    expect_length(plots, 2)
    expect_true(all(grepl("ATPTREF", names(plots))))
  })

  it("retains parameters with unspecified intervals alongside interval-specific values", {
    data <- mp_adpp_fixture()
    data$PPSTINT <- ifelse(data$PARAMCD == "CMAX", NA_real_, 0)
    args <- list(data = data, parent = "DrugA", metabolite = "Metab-DrugA")
    tables <- do.call(t_pkpt03_MP_col, args)
    expect_length(tables, 4)
    expect_equal(sum(vapply(tables, function(x) sum(x$n), numeric(1))), 8)
    expect_true(any(grepl("unspecified", names(tables))))
  })

  it("keeps summary-excluded ratios in listings, but not tables or boxplots", {
    data <- mp_adpp_fixture()
    data$PPSUMXF[1] <- "Y"
    args <- list(data = data, parent = "DrugA", metabolite = "Metab-DrugA")
    table <- do.call(t_pkpt03_MP_col, args)[[1]]
    plot <- do.call(p_pkpg06_mp, args)[[1]]
    listing <- do.call(l_pkpl01_mp, args)[[1]]
    expect_equal(table$n[table$PARAM == "Cmax"], 1)
    expect_equal(table$Mean[table$PARAM == "Cmax"], 0.3)
    expect_equal(plot$data$AVAL[plot$data$PARAMCD == "CMAX"], 0.3)
    expect_equal(as.numeric(listing$Cmax), c(0.5, 0.3))
  })
})
