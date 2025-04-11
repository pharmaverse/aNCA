test_data <- data.frame(
  DOSNO = rep(1:2, each = 8),
  PPTESTCD = rep(c("A", "A", "B", "B", "C", "C", "D", "D"), 2),
  PPSTRES = c(10, 20, 5, 15, NA, 30, 0, 10, 10, 20, 5, 15, NA, 30, 0, 10),
  PPSTRESU = c("mg/L", "mg/L", "ng/mL", "ng/mL", "µg/L", "µg/L", "", "",
               "mg/L", "mg/L", "ng/mL", "ng/mL", "µg/L", "µg/L", "", ""),
  PPORRES = c(10, 20, 5, 15, NA, 30, 0, 10, 10, 20, 5, 15, NA, 30, 0, 10),
  PPORRESU = c("mg/L", "mg/L", "ng/mL", "ng/mL", "µg/L", "µg/L", "", "",
               "mg/L", "mg/L", "ng/mL", "ng/mL", "µg/L", "µg/L", "", "")
)

result <- calculate_summary_stats(test_data)

describe("calculate_summary_stats", {

  it("returns a data frame", {
    result <- calculate_summary_stats(test_data)
    expect_s3_class(result, "data.frame")
  })

  it("includes expected statistics", {
    expected_stats <- c("Geomean", "Geocv", "Mean", "SD", "Min",
                        "Max", "Median", "Count.missing", "Count.total")
    expect_true(all(expected_stats %in% result$Statistic))
  })

  it("includes column names with units", {
    expect_true("A[mg/L]" %in% colnames(result))
    expect_true("B[ng/mL]" %in% colnames(result))
    expect_true("C[µg/L]" %in% colnames(result))
    expect_true("D" %in% colnames(result))  # No units for D
  })

  it("correctly calculates summary statistics", {

    a_data <- test_data %>% filter(PPTESTCD == "A", DOSNO == 1) %>% pull(PPSTRES)

    expected_geomean <- round(exp(mean(log(a_data), na.rm = TRUE)), 3)
    expected_mean <- round(mean(a_data, na.rm = TRUE), 3)
    expected_sd <- round(sd(a_data, na.rm = TRUE), 3)

    expect_equal(
      as.numeric(result %>% filter(Statistic == "Geomean", DOSNO == 1) %>% pull(`A[mg/L]`)),
      expected_geomean
    )

    expect_equal(
      as.numeric(result %>% filter(Statistic == "Mean", DOSNO == 1) %>% pull(`A[mg/L]`)),
      expected_mean
    )

    expect_equal(
      as.numeric(result %>% filter(Statistic == "SD", DOSNO == 1) %>% pull(`A[mg/L]`)),
      expected_sd
    )
  })

  it("handles missing values correctly", {
    expect_equal(
      result %>% filter(Statistic == "Count.missing") %>% pull(`C[µg/L]`) %>% as.numeric(),
      c(1, 1)
    )
  })

  it("handles an empty dataset", {
    empty_data <- test_data[0, ]
    result <- calculate_summary_stats(empty_data)
    expect_equal(nrow(result), 0)
  })

  it("executes efficiently on a large dataset", {
    large_data <- test_data %>%
      slice(rep(seq_len(n()), each = 1000))  # Expand dataset
    result <- calculate_summary_stats(large_data)
    expect_s3_class(result, "data.frame")
  })

  it("standardizes units to the mode", {
    test_data_diff_units <- data.frame(
      DOSNO = c(1, 1, 1),
      PPTESTCD = c("A", "A", "A"),
      PPORRES = c(1, 2, 3),
      PPORRESU = c("mg/L", "mg/L", "mg/L"),
      PPSTRES = c(1, 2, 3 * (1e-6)),
      PPSTRESU = c("mg/L", "mg/L", "µg/L")
    )

    result <- calculate_summary_stats(test_data_diff_units)

    # Define the expected result
    expected_result <- tibble::tibble(
      DOSNO = rep(1, 9),
      Statistic = c(
        "Geomean", "Geocv", "Mean", "SD", "Min",
        "Max", "Median", "Count.missing", "Count.total"
      ),
      `A[mg/L]` = c(1.817, 55.032, 2, 1, 1, 3, 2, 0, 3)
    )

    # Check that the result matches the expected output
    expect_equal(result, expected_result)
  })
})
