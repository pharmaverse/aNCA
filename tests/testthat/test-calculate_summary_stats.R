test_data <- data.frame(
  DOSNO = c(1, 1, 1, 1, 1, 1),
  PPTESTCD = c("A", "A", "B", "B", "C", "C"),
  PPSTRES = c(10, 20, 5, 15, NA, 30),
  PPSTRESU = c("mg/L", "mg/L", "ng/mL", "ng/mL", "µg/L", "µg/L")
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
  })

  it("correctly calculates summary statistics", {

    a_data <- test_data %>% filter(PPTESTCD == "A") %>% pull(PPSTRES)

    expected_geomean <- round(exp(mean(log(a_data), na.rm = TRUE)), 3)
    expected_mean <- round(mean(a_data, na.rm = TRUE), 3)
    expected_sd <- round(sd(a_data, na.rm = TRUE), 3)

    expect_equal(
      as.numeric(result %>% filter(Statistic == "Geomean") %>% pull(`A[mg/L]`)),
      expected_geomean
    )

    expect_equal(
      as.numeric(result %>% filter(Statistic == "Mean") %>% pull(`A[mg/L]`)),
      expected_mean
    )

    expect_equal(
      as.numeric(result %>% filter(Statistic == "SD") %>% pull(`A[mg/L]`)),
      expected_sd
    )
  })

  it("handles missing values correctly", {
    expect_equal(
      result %>% filter(Statistic == "Count.missing") %>% pull(`C[µg/L]`) %>% as.numeric(),
      1
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

})