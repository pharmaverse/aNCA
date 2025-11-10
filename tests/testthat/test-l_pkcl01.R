# Sample data frame for testing
adpc <- data.frame(
  PARAM = c("A", "A", "B", "B"),
  PCSPEC = c("Plasma", "Plasma", "Plasma", "Plasma"),
  ROUTE = c("Oral", "Oral", "IV", "IV"),
  TRT01A = c("Treatment1", "Treatment1", "Treatment2", "Treatment2"),
  USUBJID = c("1", "2", "3", "4"),
  ATPTREF = c("Visit1", "Visit1", "Visit2", "Visit2"),
  NFRLT = c(0, 1, 0, 1),
  AFRLT = c(0, 1.12, 0, 1.055),
  AVAL = c(0, 20.12, 30.55, NA),
  AVALU = "mg/L",
  stringsAsFactors = FALSE
)

# Define labels
attr(adpc[["AFRLT"]], "label") <- "Actual time from first dose"
attr(adpc[["NFRLT"]], "label") <- "Planned time from first dose"
attr(adpc[["AVAL"]], "label") <- "Analysis value"
attr(adpc[["PARAM"]], "label") <- "Analyte"
attr(adpc[["TRT01A"]], "label") <- "Actual treatment"
attr(adpc[["PCSPEC"]], "label") <- "Specimen"
attr(adpc[["ROUTE"]], "label") <- "Administration"
attr(adpc[["USUBJID"]], "label") <- "Unique Subject ID"
attr(adpc[["ATPTREF"]], "label") <- "Actual visit"

describe("l_pkcl01", {
  it("creates listings for each unique combination of grouping variables", {
    # For 1 case
    listings_1l <- l_pkcl01(adpc, listgroup_vars = c("PCSPEC"),
                            grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                            displaying_vars = c("NFRLT", "AFRLT", "AVAL"))
    expect_length(listings_1l, length(unique(adpc$PCSPEC)))
    expect_named(listings_1l, expected = as.character(unique(adpc$PCSPEC)))

    # For 2 case
    listings_2l <- l_pkcl01(adpc, listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
                            grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                            displaying_vars = c("NFRLT", "AFRLT", "AVAL"))
    expect_length(listings_2l, length(unique(interaction(adpc$PARAM, adpc$PCSPEC, adpc$ROUTE))))
    expect_named(listings_2l, expected = as.character(
      unique(
        interaction(adpc$PARAM, adpc$PCSPEC, adpc$ROUTE)
      )
    )
    )

    # For 3 case
    listings_3l <- l_pkcl01(adpc[1:3, ], listgroup_vars = c("PARAM", "PCSPEC", "ROUTE", "USUBJID"),
                            grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                            displaying_vars = c("NFRLT", "AFRLT", "AVAL"))

    expect_length(listings_3l, length(unique(interaction(adpc[1:3, ]$PARAM,
                                                         adpc[1:3, ]$PCSPEC,
                                                         adpc[1:3, ]$ROUTE,
                                                         adpc[1:3, ]$USUBJID))))
    expect_named(listings_3l, expected = as.character(interaction(adpc[1:3, ]$PARAM,
                                                                  adpc[1:3, ]$PCSPEC,
                                                                  adpc[1:3, ]$ROUTE,
                                                                  adpc[1:3, ]$USUBJID)))


    # For 4 case
    listings_4l <- l_pkcl01(adpc, listgroup_vars = c("PARAM", "PCSPEC", "ROUTE", "USUBJID"),
                            grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                            displaying_vars = c("NFRLT", "AFRLT", "AVAL"))

    # Include lengths and names expected
    expect_length(listings_4l, length(unique(interaction(adpc$PARAM,
                                                         adpc$PCSPEC,
                                                         adpc$ROUTE,
                                                         adpc$USUBJID))))
    expect_named(listings_4l, expected = as.character(interaction(adpc$PARAM,
                                                                  adpc$PCSPEC,
                                                                  adpc$ROUTE,
                                                                  adpc$USUBJID)))

    # All in correct format (list) and each internal element internally inherits listing_df
    purrr::walk(list(listings_1l, listings_2l, listings_4l), function(x) expect_true(class(x) == "list"))

    expect_true(all(sapply(listings_1l, inherits, "listing_df")) &&
                  all(sapply(listings_2l, inherits, "listing_df")) &&
                  all(sapply(listings_4l, inherits, "listing_df")))
  })

  it("handles missing formatting_vars_table and uses a default built:", {
    listings <- l_pkcl01(adpc,
                         listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
                         grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                         displaying_vars = c("NFRLT", "AFRLT", "AVAL"),
                         formatting_vars_table = NULL)

    # Adds labels as column names & includes units when obvious and present  (i.e, AVALU for AVAL)

    label_strings_with_aval_units <- c(TRT01A = attr(adpc$TRT01A, "label"),
                                       USUBJID = attr(adpc$USUBJID, "label"),
                                       ATPTREF = attr(adpc$ATPTREF, "label"),
                                       PARAM = attr(adpc$PARAM, "label"),
                                       PCSPEC = attr(adpc$PCSPEC, "label"),
                                       ROUTE = attr(adpc$ROUTE, "label"),
                                       NFRLT = attr(adpc$NFRLT, "label"),
                                       AFRLT = attr(adpc$AFRLT, "label"),
                                       # AVAL should include AVALU in parenthesis
                                       AVAL = paste0(attr(adpc$AVAL, "label"),
                                                     " (", unique(adpc$AVALU), ")"),
                                       AVALU = "AVALU",
                                       id_list = "id")

    expect_equal(formatters::var_labels(listings[[1]]),
                 expected = label_strings_with_aval_units)
  })

  it("handles missing subtitle and creates a default", {
    listings <- l_pkcl01(adpc, listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
                         grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                         displaying_vars = c("NFRLT", "AFRLT", "AVAL"),
                         subtitle = NULL)

    # Check if the subtitle is in the correct format
    expect_equal(attr(listings[[1]], "subtitles"),
                 expected = "Analyte: A\nSpecimen: Plasma\nAdministration: Oral")

  })

  it("handles missing footnote (no footnote)", {
    listings <- l_pkcl01(adpc, listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
                         grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                         displaying_vars = c("NFRLT", "AFRLT", "AVAL"),
                         footnote = NULL)
    expect_equal(attr(listings[[1]], "main_footer"), expected =  character())
  })

  it("handles empty data frame by providing empty list", {

    # Define the input for the function
    empty_adpc <- adpc[0, ]

    # Run the empty input and store the output
    empty_res <- l_pkcl01(empty_adpc, listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
                          grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                          displaying_vars = c("NFRLT", "AFRLT", "AVAL"))

    # Define the expected output (empty object)
    empty_list <- list()
    names(empty_list) <- character()

    # Check if the output is as expected
    expect_equal(empty_res,
                 expected = empty_list)
  })

  it("handles missing required columns", {
    incomplete_adpc <- adpc %>% select(-AFRLT)
    expect_error(l_pkcl01(incomplete_adpc, listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
                          grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                          displaying_vars = c("NFRLT", "AFRLT", "AVAL")),
                 "Missing required columns: AFRLT")
  })

  it("handles non-unique units", {
    non_unique_units_adpc <- adpc
    non_unique_units_adpc$AVALU <- c("ng/mL", "ng/mL", "ng/L", "g/L")
    expect_warning(l_pkcl01(non_unique_units_adpc, listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
                            grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                            displaying_vars = c("NFRLT", "AFRLT", "AVAL")),
                   "pkcl01, but not unique label in B.Plasma.IV for AVAL. Make sure when")
  })

  it("handles custom formatting_vars_table", {
    custom_formatting_vars_table <- data.frame(
      var_name = c("TRT01A", "USUBJID", "ATPTREF", "NFRLT", "AFRLT", "AVAL"),
      Label = c("Treatment", "Subject ID", "Visit", "Nominal Time", "Actual Time",
                "Value ($AVALU)"),
      na_str = c("missing", "missing", "missing", "missing", "missing", "missing"),
      zero_str = c("0", "0", "0", "0", "0", "BLQ"),
      align = c("center", "center", "center", "center", "center", "center"),
      format_fun = c(NA, NA, NA, NA, "round", "signif"),
      digits = c(NA, NA, NA, NA, 2, 2),
      stringsAsFactors = FALSE
    )

    listings <- l_pkcl01(adpc, listgroup_vars = c("PARAM", "PCSPEC", "ROUTE"),
                         grouping_vars = c("TRT01A", "USUBJID", "ATPTREF"),
                         displaying_vars = c("NFRLT", "AFRLT", "AVAL"),
                         formatting_vars_table = custom_formatting_vars_table)

    # Check if 0s, NAs, and units are formatted correctly
    expect_equal(as.vector(listings$`A.Plasma.Oral`$NFRLT), c("0", "1"))
    expect_equal(as.vector(listings$`A.Plasma.Oral`$AFRLT), c("0", "1.12"))
    expect_equal(as.vector(listings$`A.Plasma.Oral`$AVAL), c("BLQ", "20"))
    expect_equal(as.vector(listings$`B.Plasma.IV`$NFRLT), c("0", "1"))
    expect_equal(as.vector(listings$`B.Plasma.IV`$AFRLT), c("0", "1.05"))
    expect_equal(as.vector(listings$`B.Plasma.IV`$AVAL), c("31", NA))
    expect_equal(attr(listings$`B.Plasma.IV`$AVAL, "format_na_str"), "missing")

    # Check the structure of the listings
    expect_equal(attr(listings$`A.Plasma.Oral`, "main_title"),
                 paste0("Listing of PK Concentration by Treatment Group,",
                        "Subject and Nominal Time, PK Population"))
    expect_equal(attr(listings$`A.Plasma.Oral`, "subtitles"),
                 "Analyte: A\nSpecimen: Plasma\nAdministration: Oral")
    expect_equal(attr(listings$`A.Plasma.Oral`, "main_footer"),
                 "*: Subjects excluded from the summary table and mean plots")
    expect_equal(attr(listings$`B.Plasma.IV`, "main_title"),
                 paste0("Listing of PK Concentration by Treatment Group,",
                        "Subject and Nominal Time, PK Population"))
    expect_equal(attr(listings$`B.Plasma.IV`, "subtitles"),
                 "Analyte: B\nSpecimen: Plasma\nAdministration: IV")
    expect_equal(attr(listings$`B.Plasma.IV`, "main_footer"),
                 "*: Subjects excluded from the summary table and mean plots")

    # Check the attributes of the columns
    expect_equal(attr(listings$`A.Plasma.Oral`$TRT01A, "label"), "Treatment")
    expect_equal(attr(listings$`A.Plasma.Oral`$USUBJID, "label"), "Subject ID")
    expect_equal(attr(listings$`A.Plasma.Oral`$ATPTREF, "label"), "Visit")
    expect_equal(attr(listings$`A.Plasma.Oral`$NFRLT, "label"), "Nominal Time")
    expect_equal(attr(listings$`A.Plasma.Oral`$AFRLT, "label"), "Actual Time")
    expect_equal(attr(listings$`A.Plasma.Oral`$AVAL, "label"), "Value (mg/L)")
    expect_equal(attr(listings$`A.Plasma.Oral`$AVALU, "label"), "AVALU")

    expect_equal(attr(listings$`B.Plasma.IV`$TRT01A, "label"), "Treatment")
    expect_equal(attr(listings$`B.Plasma.IV`$USUBJID, "label"), "Subject ID")
    expect_equal(attr(listings$`B.Plasma.IV`$ATPTREF, "label"), "Visit")
    expect_equal(attr(listings$`B.Plasma.IV`$NFRLT, "label"), "Nominal Time")
    expect_equal(attr(listings$`B.Plasma.IV`$AFRLT, "label"), "Actual Time")
    expect_equal(attr(listings$`B.Plasma.IV`$AVAL, "label"), "Value (mg/L)")
    expect_equal(attr(listings$`B.Plasma.IV`$AVALU, "label"), "AVALU")

  })
})
