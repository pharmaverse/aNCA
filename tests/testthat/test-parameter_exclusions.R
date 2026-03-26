# Source the shared exclusion utilities and the parameter exclusions module
source(
  file.path(
    system.file("shiny", package = "aNCA"),
    "functions", "exclusion-utils.R"
  ),
  local = TRUE
)
source(
  file.path(
    system.file("shiny", package = "aNCA"),
    "modules", "tab_nca", "setup", "parameter_exclusions.R"
  ),
  local = TRUE
)

# --- Helper data for key-based tests ---
mock_result_df <- data.frame(
  USUBJID = c("SUBJ-01", "SUBJ-01", "SUBJ-02", "SUBJ-02"),
  PPTESTCD = c("AUCLST", "CMAX", "AUCLST", "CMAX"),
  PPORRES = c("100.5", "25.3", "98.2", "22.1"),
  PARAM = c("Analyte A", "Analyte A", "Analyte A", "Analyte A"),
  stringsAsFactors = FALSE
)

mock_res_nca <- list(
  result = mock_result_df,
  data = list(conc = list(columns = list(groups = list(
    group_subject = "USUBJID"
  ))))
)

describe(".build_exclusion_reasons", {
  it("returns empty indices and reasons for an empty list", {
    result <- .build_exclusion_reasons(list())
    expect_equal(result$indices, integer(0))
  })

  it("returns correct indices and reasons for a single exclusion", {
    lst <- list(
      list(reason = "Outlier", rows = c(1L, 3L))
    )
    result <- .build_exclusion_reasons(lst)
    expect_equal(result$indices, c(1L, 3L))
    expect_equal(result$reasons, c("Outlier", "Outlier"))
  })

  it("concatenates reasons when multiple exclusions cover the same row", {
    lst <- list(
      list(reason = "Outlier", rows = c(1L, 2L)),
      list(reason = "Protocol deviation", rows = c(2L, 4L))
    )
    result <- .build_exclusion_reasons(lst)
    expect_equal(result$indices, c(1L, 2L, 4L))
    expect_equal(result$reasons[1], "Outlier")
    expect_equal(result$reasons[2], "Outlier; Protocol deviation")
    expect_equal(result$reasons[3], "Protocol deviation")
  })
})

describe(".get_excl_key_cols", {
  it("returns grouping columns plus PPTESTCD and PPORRES", {
    cols <- .get_excl_key_cols(mock_res_nca)
    expect_true("USUBJID" %in% cols)
    expect_true("PPTESTCD" %in% cols)
    expect_true("PPORRES" %in% cols)
  })
})

describe(".extract_row_keys", {
  it("extracts key columns for given row indices", {
    key_cols <- c("USUBJID", "PPTESTCD", "PPORRES")
    keys <- .extract_row_keys(mock_result_df, c(1L, 3L), key_cols)
    expect_equal(nrow(keys), 2)
    expect_equal(keys$USUBJID, c("SUBJ-01", "SUBJ-02"))
    expect_equal(keys$PPTESTCD, c("AUCLST", "AUCLST"))
    expect_equal(keys$PPORRES, c("100.5", "98.2"))
  })

  it("ignores key columns not present in the data", {
    keys <- .extract_row_keys(mock_result_df, 1L, c("USUBJID", "NONEXISTENT"))
    expect_equal(names(keys), "USUBJID")
  })
})

describe(".match_keys_to_rows", {
  it("matches keys that exist in the result", {
    keys_df <- data.frame(
      USUBJID = "SUBJ-01", PPTESTCD = "CMAX", PPORRES = "25.3",
      stringsAsFactors = FALSE
    )
    result <- .match_keys_to_rows(keys_df, mock_result_df)
    expect_equal(result$matched, 2L)
    expect_null(result$unmatched)
  })

  it("returns unmatched keys when PPORRES differs", {
    keys_df <- data.frame(
      USUBJID = "SUBJ-01", PPTESTCD = "CMAX", PPORRES = "999.9",
      stringsAsFactors = FALSE
    )
    result <- .match_keys_to_rows(keys_df, mock_result_df)
    expect_equal(result$matched, integer(0))
    expect_equal(nrow(result$unmatched), 1)
  })

  it("handles a mix of matched and unmatched keys", {
    keys_df <- data.frame(
      USUBJID = c("SUBJ-01", "SUBJ-02"),
      PPTESTCD = c("AUCLST", "CMAX"),
      PPORRES = c("100.5", "999.9"),
      stringsAsFactors = FALSE
    )
    result <- .match_keys_to_rows(keys_df, mock_result_df)
    expect_equal(result$matched, 1L)
    expect_equal(nrow(result$unmatched), 1)
    expect_equal(result$unmatched$PPORRES, "999.9")
  })

  it("returns all unmatched when no keys match", {
    keys_df <- data.frame(
      USUBJID = "SUBJ-99", PPTESTCD = "AUCLST", PPORRES = "0",
      stringsAsFactors = FALSE
    )
    result <- .match_keys_to_rows(keys_df, mock_result_df)
    expect_equal(result$matched, integer(0))
    expect_equal(nrow(result$unmatched), 1)
  })
})

describe(".restore_exclusions_from_keys", {
  it("restores matched exclusions and sets row indices", {
    excl_list <- shiny::reactiveVal(list())
    counter <- shiny::reactiveVal(0L)
    warnings <- shiny::reactiveVal(NULL)

    overrides <- list(
      list(
        reason = "Outlier",
        keys = data.frame(
          USUBJID = c("SUBJ-01", "SUBJ-02"),
          PPTESTCD = c("AUCLST", "AUCLST"),
          PPORRES = c("100.5", "98.2"),
          stringsAsFactors = FALSE
        )
      )
    )

    shiny::isolate({
      .restore_exclusions_from_keys(
        overrides, mock_res_nca, excl_list, counter, warnings
      )
      result <- excl_list()
      expect_length(result, 1)
      expect_equal(result[[1]]$reason, "Outlier")
      expect_equal(sort(result[[1]]$rows), c(1L, 3L))
      expect_null(warnings())
    })
  })

  it("warns about unmatched keys and restores only matched ones", {
    excl_list <- shiny::reactiveVal(list())
    counter <- shiny::reactiveVal(0L)
    warnings <- shiny::reactiveVal(NULL)

    overrides <- list(
      list(
        reason = "Changed value",
        keys = data.frame(
          USUBJID = c("SUBJ-01", "SUBJ-01"),
          PPTESTCD = c("AUCLST", "CMAX"),
          PPORRES = c("100.5", "999.9"),
          stringsAsFactors = FALSE
        )
      )
    )

    shiny::isolate({
      .restore_exclusions_from_keys(
        overrides, mock_res_nca, excl_list, counter, warnings
      )
      result <- excl_list()
      expect_length(result, 1)
      expect_equal(result[[1]]$rows, 1L)
      expect_true(!is.null(warnings()))
      expect_true(grepl("999.9", warnings()))
    })
  })

  it("produces no exclusions when all keys are unmatched", {
    excl_list <- shiny::reactiveVal(list())
    counter <- shiny::reactiveVal(0L)
    warnings <- shiny::reactiveVal(NULL)

    overrides <- list(
      list(
        reason = "All gone",
        keys = data.frame(
          USUBJID = "SUBJ-99", PPTESTCD = "AUCLST", PPORRES = "0",
          stringsAsFactors = FALSE
        )
      )
    )

    shiny::isolate({
      .restore_exclusions_from_keys(
        overrides, mock_res_nca, excl_list, counter, warnings
      )
      expect_length(excl_list(), 0)
      expect_true(!is.null(warnings()))
    })
  })
})

describe("rehydrate_exclusions (general exclusions pattern)", {
  it("rehydrates overrides with fresh button IDs", {
    excl_list <- shiny::reactiveVal(list())
    counter <- shiny::reactiveVal(0L)

    overrides <- list(
      list(reason = "Outlier", rows = c(1L, 3L)),
      list(reason = "Protocol deviation", rows = c(2L))
    )

    shiny::isolate({
      rehydrate_exclusions(overrides, excl_list, counter,
                           prefix = "remove_exclusion_reason_")
      result <- excl_list()
      expect_length(result, 2)
      expect_equal(result[[1]]$xbtn_id, "remove_exclusion_reason_1")
      expect_equal(result[[2]]$xbtn_id, "remove_exclusion_reason_2")
      expect_equal(counter(), 2L)
    })
  })
})

describe("clean_exclusion_list", {
  it("strips xbtn_id and preserves other fields", {
    raw_list <- list(
      list(reason = "Outlier", rows = c(1L, 3L), xbtn_id = "x_1"),
      list(reason = "PD", rows = c(2L), xbtn_id = "x_2")
    )
    clean <- clean_exclusion_list(raw_list)

    expect_length(clean, 2)
    expect_null(clean[[1]]$xbtn_id)
    expect_equal(clean[[1]]$reason, "Outlier")
    expect_equal(clean[[1]]$rows, c(1L, 3L))
  })

  it("returns empty list for empty input", {
    expect_equal(clean_exclusion_list(list()), list())
  })
})
