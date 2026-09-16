# Shared fixture: minimal ADPP-like data frame
pkpg_data <- data.frame(
  USUBJID = rep(paste0("S", 1:6), each = 2),
  TRT01A  = rep(c("10mg", "10mg", "10mg", "50mg", "50mg", "50mg"), each = 2),
  PARAM   = rep(c("Cmax", "AUClast"), 6),
  PARAMCD = rep(c("CMAX", "AUCLST"), 6),
  AVAL    = c(5, 20, 6, 22, 7, 21, 10, 40, 11, 38, 9, 42),
  AVALU   = "ng/mL",
  PPCAT   = "DrugA Plasma",
  PPSPEC  = "SERUM",
  METABFL = NA_character_,
  stringsAsFactors = FALSE
)

pkpg_metab_data <- pkpg_data
pkpg_metab_data$PPCAT <- ifelse(
  pkpg_metab_data$TRT01A == "50mg", "Metab-DrugA Plasma", "DrugA Plasma"
)
pkpg_metab_data$METABFL <- ifelse(
  pkpg_metab_data$TRT01A == "50mg", "Y", NA_character_
)

describe("p_pkpg03_boxp", {
  it("returns a named list of ggplot objects", {
    result <- p_pkpg03_boxp(pkpg_data)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "ggplot"))
  })

  it("splits output by PPCAT/PPSPEC when present", {
    result <- p_pkpg03_boxp(pkpg_data)
    expect_equal(length(result), 1)
    expect_true(grepl("DrugA Plasma", names(result)[1]))
  })

  it("produces one entry per PPCAT/PPSPEC combination", {
    two_cats <- rbind(
      pkpg_data,
      transform(pkpg_data, PPCAT = "DrugB Plasma")
    )
    result <- p_pkpg03_boxp(two_cats)
    expect_equal(length(result), 2)
  })

  it("returns list with 'all' when list_vars absent from data", {
    data_bare <- pkpg_data[, setdiff(names(pkpg_data), c("PPCAT", "PPSPEC"))]
    result <- p_pkpg03_boxp(data_bare)
    expect_equal(names(result), "all")
  })

  it("stops with informative error when required columns are missing", {
    bad <- pkpg_data[, setdiff(names(pkpg_data), "AVAL")]
    expect_error(p_pkpg03_boxp(bad), "missing required columns")
  })

  it("returns empty list when all AVAL values are NA", {
    data_na <- pkpg_data
    data_na$AVAL <- NA_real_
    result <- p_pkpg03_boxp(data_na)
    expect_equal(length(result), 0)
  })

  it("uses facets — one panel per PARAM value", {
    result <- p_pkpg03_boxp(pkpg_data)[[1]]
    facet_vars <- as.character(result$facet$params$facets)
    expect_true(any(grepl("PARAM", facet_vars)))
  })

  it("does not overlay individual points when all_points = FALSE", {
    result <- p_pkpg03_boxp(pkpg_data, all_points = FALSE)[[1]]
    layer_classes <- sapply(result$layers, function(l) class(l$geom)[1])
    expect_false(any(layer_classes == "GeomJitter"))
  })
})

describe("p_pkpg04_boxp", {
  it("returns ggplot objects like p_pkpg03_boxp", {
    result <- p_pkpg04_boxp(pkpg_data)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "ggplot"))
  })

  it("overlays individual jitter points (all_points = TRUE)", {
    result <- p_pkpg04_boxp(pkpg_data)[[1]]
    # geom_jitter registers as "GeomPoint" internally
    layer_classes <- sapply(result$layers, function(l) class(l$geom)[1])
    expect_true(any(layer_classes == "GeomPoint"))
  })

  it("suppresses boxplot outlier points when all_points = TRUE", {
    result <- p_pkpg04_boxp(pkpg_data)[[1]]
    box_layer <- result$layers[[
      which(sapply(result$layers, function(l) class(l$geom)[1]) == "GeomBoxplot")
    ]]
    expect_true(is.na(box_layer$geom_params$outlier_gp$shape))
  })
})

describe("p_pkpg06_mp", {
  it("filters to metabolite rows using METABFL (preferred path)", {
    result <- p_pkpg06_mp(pkpg_metab_data)
    # Only metabolite arm rows reach the plot — check it returns a ggplot
    expect_s3_class(result[[1]], "ggplot")
    # Plot data should only contain metabolite arm (50mg)
    plot_df <- result[[1]]$data
    expect_true(all(plot_df$TRT01A == "50mg"))
  })

  it("falls back to PPCAT grep when METABFL absent", {
    data_ppcat <- pkpg_data
    data_ppcat$PPCAT <- ifelse(
      data_ppcat$TRT01A == "50mg", "Metab-DrugA", "DrugA"
    )
    data_ppcat <- data_ppcat[, setdiff(names(data_ppcat), "METABFL")]
    result <- p_pkpg06_mp(data_ppcat)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "ggplot"))
  })

  it("falls back to PARAM grep when METABFL and PPCAT absent", {
    data_param <- pkpg_data
    data_param$PARAM <- ifelse(data_param$TRT01A == "50mg",
                               paste0("Metab-", data_param$PARAM),
                               data_param$PARAM)
    data_param <- data_param[, setdiff(names(data_param), c("METABFL", "PPCAT"))]  # nolint
    result <- p_pkpg06_mp(data_param)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "ggplot"))
  })

  it("stops with informative error when no metabolite data found", {
    data_no_metab <- pkpg_data[, setdiff(names(pkpg_data), "METABFL")]
    expect_error(p_pkpg06_mp(data_no_metab), "no metabolite data found")
  })
})

# --- p_pkpg01_cum / p_pkpg01_per fixtures -----------------------------------

pkpg01_data <- data.frame(
  USUBJID = rep(paste0("S", 1:4), each = 3),
  TRT01A  = rep(c("10mg", "10mg", "50mg", "50mg"), each = 3),
  PARAM   = rep(c("Ae 0-2h", "Ae 0-4h", "Ae 0-8h"), 4),
  PARAMCD = rep("RCAMINT", 12),
  AVAL    = c(10, 25, 40, 12, 28, 42, 20, 45, 70, 22, 48, 72),
  AVALU   = "mg",
  PPCAT   = "DrugA",
  PPSPEC  = "URINE",
  stringsAsFactors = FALSE
)

pkpg01_per_data <- data.frame(
  USUBJID = rep(paste0("S", 1:4), each = 3),
  TRT01A  = rep(c("10mg", "10mg", "50mg", "50mg"), each = 3),
  PARAM   = rep(c("Fe 0-2h", "Fe 0-4h", "Fe 0-8h"), 4),
  PARAMCD = rep("FREXINT", 12),
  AVAL    = c(5, 12, 20, 6, 14, 21, 10, 22, 35, 11, 24, 36),
  AVALU   = "fraction",
  PPCAT   = "DrugA",
  PPSPEC  = "URINE",
  stringsAsFactors = FALSE
)

describe("p_pkpg01_cum", {
  it("returns a named list of ggplot objects", {
    result <- p_pkpg01_cum(pkpg01_data)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "ggplot"))
  })

  it("returns empty list when no urine rows found", {
    data_serum <- pkpg01_data
    data_serum$PPSPEC <- "SERUM"
    expect_equal(length(p_pkpg01_cum(data_serum)), 0)
  })

  it("returns empty list when all AVAL values are NA", {
    data_na <- pkpg01_data
    data_na$AVAL <- NA_real_
    expect_equal(length(p_pkpg01_cum(data_na)), 0)
  })

  it("stops with informative error when required columns are missing", {
    bad <- pkpg01_data[, setdiff(names(pkpg01_data), "AVAL")]
    expect_error(p_pkpg01_cum(bad), "missing required columns")
  })

  it("has line and point geoms", {
    result <- p_pkpg01_cum(pkpg01_data)[[1]]
    layer_classes <- sapply(result$layers, function(l) class(l$geom)[1])
    expect_true(any(layer_classes == "GeomLine"))
    expect_true(any(layer_classes == "GeomPoint"))
  })

  it("has error bar geom", {
    result <- p_pkpg01_cum(pkpg01_data)[[1]]
    layer_classes <- sapply(result$layers, function(l) class(l$geom)[1])
    expect_true(any(layer_classes == "GeomErrorbar"))
  })

  it("splits by list_vars into one entry per combination", {
    two_cats <- rbind(
      pkpg01_data,
      transform(pkpg01_data, PPCAT = "DrugB")
    )
    result <- p_pkpg01_cum(two_cats)
    expect_equal(length(result), 2)
  })

  it("returns single 'all' entry when list_vars absent from data", {
    data_bare <- pkpg01_data[, setdiff(names(pkpg01_data), c("PPCAT", "PPSPEC"))]
    result <- suppressWarnings(p_pkpg01_cum(data_bare))
    expect_equal(names(result), "all")
  })

  it("warns when PPSPEC column is absent and skips the urine filter", {
    data_no_ppspec <- pkpg01_data[, setdiff(names(pkpg01_data), "PPSPEC")]
    expect_warning(
      p_pkpg01_cum(data_no_ppspec),
      "PPSPEC.*not found"
    )
  })

  it("filters to RCAMINT params, not FREXINT", {
    combined <- rbind(pkpg01_data, pkpg01_per_data)
    result <- p_pkpg01_cum(combined)[[1]]
    x_vals <- as.character(result$data$PARAM)
    expect_true(all(grepl("^Ae", x_vals)))
  })

  it("skips PARAMCD filter when PARAMCD column absent", {
    data_no_pcd <- pkpg01_data[, setdiff(names(pkpg01_data), "PARAMCD")]
    result <- p_pkpg01_cum(data_no_pcd)
    expect_equal(length(result), 1)
  })

  it("warns and returns empty list when paramcd_filter matches no rows", {
    expect_warning(
      result <- p_pkpg01_cum(pkpg01_data, paramcd_filter = "NONEXISTENT"),
      "no rows matched paramcd_filter"
    )
    expect_equal(length(result), 0)
  })
})

describe("p_pkpg01_per", {
  it("returns ggplot objects (delegates to p_pkpg01_cum)", {
    result <- p_pkpg01_per(pkpg01_per_data)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "ggplot"))
  })

  it("uses percent recovered title by default", {
    result <- p_pkpg01_per(pkpg01_per_data)[[1]]
    expect_true(grepl("Percent", result$labels$title, ignore.case = TRUE))
  })

  it("filters to FREXINT params, not RCAMINT", {
    combined <- rbind(pkpg01_data, pkpg01_per_data)
    result <- p_pkpg01_per(combined)[[1]]
    x_vals <- as.character(result$data$PARAM)
    expect_true(all(grepl("^Fe", x_vals)))
  })

  it("warns and returns empty list when only RCAMINT rows present (no FREXINT)", {
    expect_warning(
      result <- p_pkpg01_per(pkpg01_data),
      "no rows matched paramcd_filter"
    )
    expect_equal(length(result), 0)
  })
})

# --- p_pkpg02_doseprop fixtures ---------------------------------------------

pkpg02_data <- data.frame(
  USUBJID = rep(paste0("S", 1:6), each = 2),
  TRT01A  = rep(c("5mg", "5mg", "10mg", "10mg", "20mg", "20mg"), each = 2),
  DOSEA   = rep(c(5, 5, 10, 10, 20, 20), each = 2),
  DOSEU   = "mg",
  PARAM   = rep(c("Cmax", "AUClast"), 6),
  PARAMCD = rep(c("CMAX", "AUCLST"), 6),
  AVAL    = c(5, 20, 9.8, 39, 20, 78, 5.2, 21, 10.1, 41, 19.8, 77),
  AVALU   = "ng/mL",
  PPCAT   = "DrugA Plasma",
  PPSPEC  = "SERUM",
  stringsAsFactors = FALSE
)

describe("p_pkpg02_doseprop", {
  it("returns a named list of ggplot objects", {
    result <- p_pkpg02_doseprop(pkpg02_data)
    expect_type(result, "list")
    purrr::walk(result, ~ expect_s3_class(.x, "ggplot"))
  })

  it("stops with informative error when required columns are missing", {
    bad <- pkpg02_data[, setdiff(names(pkpg02_data), "DOSEA")]
    expect_error(p_pkpg02_doseprop(bad), "missing required columns")
  })

  it("returns empty list when all values are non-positive", {
    data_zero <- pkpg02_data
    data_zero$AVAL <- -1
    expect_equal(length(p_pkpg02_doseprop(data_zero)), 0)
  })

  it("uses facets — one panel per PARAM value", {
    result <- p_pkpg02_doseprop(pkpg02_data)[[1]]
    facet_vars <- as.character(result$facet$params$facets)
    expect_true(any(grepl("PARAM", facet_vars)))
  })

  it("has a scatter (point) layer", {
    result <- p_pkpg02_doseprop(pkpg02_data)[[1]]
    layer_classes <- sapply(result$layers, function(l) class(l$geom)[1])
    expect_true(any(layer_classes == "GeomPoint"))
  })

  it("adds regression line layer when enough data points exist", {
    result <- p_pkpg02_doseprop(pkpg02_data)[[1]]
    layer_classes <- sapply(result$layers, function(l) class(l$geom)[1])
    expect_true(any(layer_classes == "GeomLine"))
  })

  it("adds slope annotation (GeomText) per facet", {
    result <- p_pkpg02_doseprop(pkpg02_data)[[1]]
    layer_classes <- sapply(result$layers, function(l) class(l$geom)[1])
    expect_true(any(layer_classes == "GeomText"))
  })

  it("annotation x-coordinate is finite when log_scale = TRUE (default)", {
    # Regression: -Inf mapped through log10() becomes NaN/NA, causing geom_text
    # to silently drop the annotation row.  Ensure the data passed to the
    # GeomText layer has a finite x-coordinate so the label is always rendered.
    result <- p_pkpg02_doseprop(pkpg02_data)[[1]]
    geom_text_idx <- sapply(result$layers, function(l) inherits(l$geom, "GeomText"))
    text_layer <- result$layers[geom_text_idx][[1]]
    annot_data <- text_layer$data
    expect_true(all(is.finite(annot_data[["DOSEA"]])),
                info = "annotation DOSEA (x-anchor) must be finite for log10 scale to render it")
  })

  it("applies log10 scales when log_scale = TRUE (default)", {
    result <- p_pkpg02_doseprop(pkpg02_data)[[1]]
    trans_names <- sapply(result$scales$scales, function(s) s$trans$name)
    expect_true(any(trans_names == "log-10"))
  })

  it("does not apply log scales when log_scale = FALSE", {
    result <- p_pkpg02_doseprop(pkpg02_data, log_scale = FALSE)[[1]]
    trans_names <- sapply(result$scales$scales, function(s) s$trans$name)
    expect_false(any(trans_names == "log-10"))
  })

  it("splits by list_vars into one entry per PPCAT/PPSPEC combination", {
    two_specs <- rbind(
      pkpg02_data,
      transform(pkpg02_data, PPSPEC = "URINE")
    )
    result <- p_pkpg02_doseprop(two_specs)
    expect_equal(length(result), 2)
  })
})

describe("PK parameter plot subtitles", {
  labelled <- pkpg_data
  labelled$AVALU <- rep(c("ng/mL", "h*ng/mL"), 6)
  dose_labelled <- pkpg02_data
  dose_labelled$AVALU <- labelled$AVALU
  metabolite_labelled <- pkpg_metab_data
  metabolite_labelled$AVALU <- labelled$AVALU
  percent_labelled <- pkpg01_per_data
  percent_labelled$AVALU <- "%"
  cases <- list(
    p_pkpg01_cum = pkpg01_data,
    p_pkpg01_per = percent_labelled,
    p_pkpg02_doseprop = dose_labelled,
    p_pkpg03_boxp = labelled,
    p_pkpg04_boxp = labelled,
    p_pkpg06_mp = metabolite_labelled
  )
  expected <- c(
    p_pkpg01_cum = paste0(
      "Analyte: DrugA\nSpecimen: URINE\n",
      "PK Parameter: Ae 0-2h (mg), Ae 0-4h (mg), Ae 0-8h (mg)"
    ),
    p_pkpg01_per = paste0(
      "Analyte: DrugA\nSpecimen: URINE\n",
      "PK Parameter: Fe 0-2h (%), Fe 0-4h (%), Fe 0-8h (%)"
    ),
    p_pkpg02_doseprop = paste0(
      "Analyte: DrugA Plasma\nSpecimen: SERUM\n",
      "PK Parameter: Cmax (ng/mL), AUClast (h*ng/mL)"
    ),
    p_pkpg03_boxp = paste0(
      "Analyte: DrugA Plasma\nSpecimen: SERUM\n",
      "PK Parameter: Cmax (ng/mL), AUClast (h*ng/mL)"
    ),
    p_pkpg04_boxp = paste0(
      "Analyte: DrugA Plasma\nSpecimen: SERUM\n",
      "PK Parameter: Cmax (ng/mL), AUClast (h*ng/mL)"
    ),
    p_pkpg06_mp = paste0(
      "Analyte: Metab-DrugA Plasma\nSpecimen: SERUM\n",
      "PK Parameter: Cmax (ng/mL), AUClast (h*ng/mL)"
    )
  )

  it("derives context and parameter-unit pairs for all six catalog entries", {
    catalog <- yaml::read_yaml(system.file("shiny/tlg.yaml", package = "aNCA"))
    for (id in names(cases)) {
      options <- catalog[[id]]$options[c("title", "subtitle", "footnote")]
      labels <- lapply(options, function(option) option$default)
      result <- do.call(get(id), c(list(data = cases[[id]]), labels))[[1]]
      expect_equal(result$labels$subtitle, unname(expected[id]), info = id)
      expect_equal(result$labels$title, labels$title, info = id)
      expect_equal(result$labels$caption, labels$footnote, info = id)
    }
  })

  it("derives each page's analyte, specimen, visit and matching parameter units", {
    first <- transform(labelled, AVISIT = "Day 1")
    second <- transform(
      labelled, PPCAT = "DrugB", PPSPEC = "PLASMA", AVISIT = "Day 2",
      AVALU = ifelse(PARAM == "Cmax", "ug/mL", "h*ug/mL")
    )
    result <- p_pkpg03_boxp(rbind(first, second), list_vars = c("PPCAT", "AVISIT"))
    expect_equal(length(result), 2L)
    expect_equal(result[[1]]$labels$subtitle, paste0(
      "Analyte: DrugA Plasma\nSpecimen: SERUM\nVisit: Day 1\n",
      "PK Parameter: Cmax (ng/mL), AUClast (h*ng/mL)"
    ))
    expect_equal(result[[2]]$labels$subtitle, paste0(
      "Analyte: DrugB\nSpecimen: PLASMA\nVisit: Day 2\n",
      "PK Parameter: Cmax (ug/mL), AUClast (h*ug/mL)"
    ))
  })

  it("builds urine labels after specimen and parameter filters", {
    mixed <- rbind(
      pkpg01_data, percent_labelled,
      transform(pkpg01_data, PPCAT = "Other drug", PPSPEC = "SERUM", AVALU = "ng/mL")
    )
    expect_equal(p_pkpg01_cum(mixed)[[1]]$labels$subtitle, unname(expected["p_pkpg01_cum"]))
    expect_equal(p_pkpg01_per(mixed)[[1]]$labels$subtitle, unname(expected["p_pkpg01_per"]))
    expect_equal(p_pkpg01_per(pkpg01_per_data)[[1]]$labels$subtitle, paste0(
      "Analyte: DrugA\nSpecimen: URINE\n",
      "PK Parameter: Fe 0-2h (fraction), Fe 0-4h (fraction), Fe 0-8h (fraction)"
    ))
  })

  it("uses a custom parameter column without pairing units from PARAM", {
    renamed <- labelled
    renamed$PARAM <- "Unused parameter"
    renamed$PKPARAM <- rep(c("Peak", "Exposure"), 6)
    result <- p_pkpg03_boxp(renamed, param_var = "PKPARAM")[[1]]
    expect_equal(result$labels$subtitle, paste0(
      "Analyte: DrugA Plasma\nSpecimen: SERUM\n",
      "PK Parameter: Peak (ng/mL), Exposure (h*ng/mL)"
    ))
  })

  it("omits missing or blank metadata and unit parentheses", {
    bare <- labelled[, setdiff(names(labelled), c("PPCAT", "PPSPEC", "AVALU"))]
    expect_equal(p_pkpg03_boxp(bare)[[1]]$labels$subtitle, "PK Parameter: Cmax, AUClast")
    partial <- transform(
      labelled, PPCAT = NA_character_, PPSPEC = " ", AVISIT = "",
      AVALU = ifelse(PARAM == "Cmax", NA_character_, "")
    )
    expect_equal(
      p_pkpg03_boxp(partial, list_vars = character())[[1]]$labels$subtitle,
      "PK Parameter: Cmax, AUClast"
    )
  })

  it("wraps long generated subtitles without losing parameter-unit pairs", {
    many_params <- do.call(rbind, lapply(seq_len(12), function(i) {
      transform(
        labelled[labelled$PARAM == "Cmax", ],
        PARAM = paste("Parameter", i), AVALU = paste0("unit", i)
      )
    }))
    subtitle <- p_pkpg03_boxp(many_params)[[1]]$labels$subtitle
    expected_text <- paste(
      "Analyte: DrugA Plasma Specimen: SERUM PK Parameter:",
      paste(paste0("Parameter ", seq_len(12), " (unit", seq_len(12), ")"), collapse = ", ")
    )
    expect_equal(gsub("[[:space:]]+", " ", subtitle), expected_text)
    expect_true(all(nchar(strsplit(subtitle, "\n", fixed = TRUE)[[1]]) <= 80L))

    custom <- paste(rep("Keep this user subtitle exactly as entered.", 6), collapse = " ")
    expect_gt(nchar(custom), 80L)
    expect_equal(p_pkpg03_boxp(many_params, subtitle = custom)[[1]]$labels$subtitle, custom)
  })

  it("preserves custom and deliberately empty subtitles across all six entries", {
    for (id in names(cases)) {
      for (subtitle in c("User subtitle\nSecond line", "")) {
        result <- do.call(get(id), list(data = cases[[id]], subtitle = subtitle))[[1]]
        expect_equal(result$labels$subtitle, subtitle, info = id)
      }
    }
  })
})
