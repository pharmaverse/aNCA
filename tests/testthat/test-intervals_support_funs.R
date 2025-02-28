# Create sample data for testing
d_conc <- data.frame(
  conc = c(1, 0.6, 0.2, 0.1, 0.9, 0.4, 1.2, 0.8, 0.3, 0.2, 1.1, 0.5),
  time = rep(0:5, 2),
  analyte = rep(c("Analyte1", "Analyte2"), each = 6),
  include_hl = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
  ID = rep(1:2, each = 6)
)

d_dose <- data.frame(
  dose = c(100, 200),
  time = c(0, 0),
  ID = c(1, 2)
)

intervals <- data.frame(
  start = c(0, 0, 0),
  end = c(24, 48, Inf),
  half.life = c(TRUE, TRUE, TRUE),
  cmax = c(TRUE, TRUE, TRUE),
  impute = c("start_conc0,start_predose", "start_predose", "start_conc0"),
  analyte = c("Analyte1", "Analyte2", "Analyte1"),
  ID = c(1, 2, 1)
)

o_conc <- PKNCAconc(d_conc, conc ~ time | ID / analyte, include_half.life = "include_hl")
o_dose <- PKNCAdose(d_dose, dose ~ time | ID)
o_data <- PKNCAdata(o_conc, o_dose, intervals = intervals)

### Test interval_add_impute

test_that("interval_add_impute throws an error if either data or target_impute is missing", {
  expect_error(interval_add_impute(o_data), "Both 'data' and 'target_impute' must be provided.")
})

test_that("interval_add_impute throws an error for non-character target_impute", {
  expect_error(interval_add_impute(o_data, target_impute = 123),
               "'target_impute' must be a character string.")
})

test_that("interval_add_impute throws an error when input data is not a proper format object", {
  expect_error(interval_add_impute(data = o_conc,
                                   target_impute = "start_conc0"))
  expect_no_error(interval_add_impute(data = o_data,
                                      target_impute = "start_conc0"))
})

test_that("interval_add_impute throws an error for unknown target_params", {
  expect_error(
    interval_add_impute(
      o_data,
      target_impute = "start_conc0",
      target_params = "unknown_param"
    )
  )
})

test_that("interval_add_impute handles impute column with FALSE values correctly", {
  o_data_with_na_impute <- o_data
  o_data_with_na_impute$intervals$impute <- NA_character_
  result <- interval_add_impute(o_data_with_na_impute, target_impute = "new_impute")
  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                          half.life = c(TRUE, TRUE, TRUE),
                          cmax = c(TRUE, TRUE, TRUE),
                          impute = rep("new_impute", 3)))
})

test_that("interval_add_impute reports an error when the impute column is not a character", {
  o_data_not_character_impute <- o_data
  o_data_not_character_impute$intervals$impute <- 1
  expect_error(interval_add_impute(o_data_not_character_impute, target_impute = "new_impute"),
               "The 'impute' column in the intervals data.frame must be a character column.")
})

test_that("interval_add_impute creates missing impute col as NA_char & adds impute", {
  d_no_imp <- o_data
  d_no_imp$intervals$impute <- NULL
  res <- interval_add_impute(d_no_imp, target_impute = "new_impute")
  expect_equal(res$intervals, transform(d_no_imp$intervals, impute = "new_impute"))
  res <- interval_add_impute(d_no_imp$intervals, target_impute = "new_impute")
  expect_equal(res, transform(d_no_imp$intervals, impute = "new_impute"))
})

test_that("interval_add_impute with no optional parameters uses all, with new intervals below", {
  result <- interval_add_impute(o_data, target_impute = "new_impute")
  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                          half.life = c(TRUE, TRUE, TRUE),
                          cmax = c(TRUE, TRUE, TRUE),
                          impute = c("start_conc0,start_predose,new_impute",
                                     "start_predose,new_impute",
                                     "start_conc0,new_impute")))
})

test_that("interval_add_impute handles specified target_params correctly", {
  result <- interval_add_impute(o_data, target_impute = "new_impute", target_params = "half.life")
  expect_equal(result$intervals[result$intervals$half.life & !is.na(result$intervals$half.life),
                                c("analyte", "half.life", "impute")] |>
                 `rownames<-`(NULL),
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                          half.life = c(TRUE, TRUE, TRUE),
                          impute = c("start_conc0,start_predose,new_impute",
                                     "start_predose,new_impute",
                                     "start_conc0,new_impute")))
  expect_equal(result$intervals[result$intervals$cmax & !is.na(result$intervals$cmax),
                                c("analyte", "cmax", "impute")] |>
                 `rownames<-`(NULL),
               o_data$intervals[o_data$intervals$cmax, c("analyte", "cmax", "impute")] |>
                 `rownames<-`(NULL))
})

test_that("interval_add_impute handles target_groups correctly", {
  result <- interval_add_impute(o_data, target_impute = "new_impute",
                                target_groups = data.frame(analyte = "Analyte1"))
  expect_equal(result$intervals[result$intervals$analyte == "Analyte1",
                                c("analyte", "half.life", "cmax", "impute")] |>
                 `rownames<-`(NULL),
               data.frame(analyte = c("Analyte1", "Analyte1"),
                          half.life = c(TRUE, TRUE),
                          cmax = c(TRUE, TRUE),
                          impute = c("start_conc0,start_predose,new_impute",
                                     "start_conc0,new_impute")))

  expect_equal(result$intervals[result$intervals$analyte == "Analyte2",
                                c("analyte", "half.life", "cmax", "impute")] |> `rownames<-`(NULL),
               o_data$intervals[o_data$intervals$analyte == "Analyte2",
                                c("analyte", "half.life", "cmax", "impute")] |> `rownames<-`(NULL))
})

test_that("interval_add_impute handles multiple target_params correctly", {
  result <- interval_add_impute(o_data,
                                target_impute = "new_impute",
                                target_params = c("half.life", "cmax"))
  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                          half.life = c(TRUE, TRUE, TRUE),
                          cmax = c(TRUE, TRUE, TRUE),
                          impute = c("start_conc0,start_predose,new_impute",
                                     "start_predose,new_impute",
                                     "start_conc0,new_impute")))
})

test_that("interval_add_impute makes no changes and warns when no matching intervals are found", {
  result <- suppressWarnings(
    interval_remove_impute(
      o_data,
      target_impute = "start_conc0",
      target_groups = data.frame(analyte = "Analyte3")
    )
  )

  expect_equal(result, o_data)

  expect_warning(
    interval_remove_impute(o_data,
                           target_impute = "start_conc0",
                           target_groups = data.frame(analyte = "Analyte3")),
    paste0("No intervals found with the specified target parameters,",
           " groups and/or impute method. No changes made.")
  )
})

test_that("interval_add_impute handles mixed TRUE/FALSE for cmax and half.life correctly", {
  intervals_mixed <- data.frame(
    start = c(0, 0, 0, 0),
    end = c(24, 48, Inf, 72),
    half.life = c(TRUE, FALSE, TRUE, FALSE),
    cmax = c(FALSE, TRUE, FALSE, TRUE),
    impute = c("start_conc0,start_predose", "start_predose", "start_conc0", "start_predose"),
    analyte = c("Analyte1", "Analyte2", "Analyte1", "Analyte2"),
    ID = c(1, 2, 1, 2)
  )

  o_data_mixed <- PKNCAdata(o_conc, o_dose, intervals = intervals_mixed)
  result <- interval_add_impute(o_data_mixed, target_impute = "new_impute")
  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1", "Analyte2"),
                          half.life = c(TRUE, FALSE, TRUE, FALSE),
                          cmax = c(FALSE, TRUE, FALSE, TRUE),
                          impute = c("start_conc0,start_predose,new_impute",
                                     "start_predose,new_impute",
                                     "start_conc0,new_impute",
                                     "start_predose,new_impute")))
})

test_that(
  paste0(
    "interval_add_impute do not create duplicates but instead",
    " removes original ones and then adds impute method based on after"
  ),
  {
    result <- interval_add_impute(o_data,
                                  target_impute = "start_conc0",
                                  after = Inf)
    expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
                 data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                            half.life = c(TRUE, TRUE, TRUE),
                            cmax = c(TRUE, TRUE, TRUE),
                            impute = c("start_predose,start_conc0",
                                       "start_predose,start_conc0",
                                       "start_conc0")))
  }
)

test_that("interval_add_impute adds new rows with added imputations after the original ones", {
  result <- interval_add_impute(o_data, target_impute = "new_impute", target_param = "cmax")
  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte1", "Analyte2",
                                      "Analyte2", "Analyte1", "Analyte1"),
                          half.life = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                          cmax = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
                          impute = c("start_conc0,start_predose",
                                     "start_conc0,start_predose,new_impute",
                                     "start_predose",
                                     "start_predose,new_impute",
                                     "start_conc0",
                                     "start_conc0,new_impute")))
})

test_that(
  "interval_add_impute do not add new interval if non-target & target params share target impute",
  {
    intervals_mixed <- data.frame(
      start = c(0, 0),
      end = c(24, 48),
      half.life = c(TRUE, TRUE),
      cmax = c(TRUE, TRUE),
      impute = c("start_conc0,start_predose", "start_predose"),
      analyte = c("Analyte1", "Analyte2"),
      ID = 1
    )

    o_data_mixed <- PKNCAdata(o_conc, o_dose, intervals = intervals_mixed)
    result <- suppressWarnings(interval_add_impute(o_data_mixed,
                                                   target_impute = "start_predose",
                                                   target_param = "cmax",
                                                   after = Inf))
    expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
                 data.frame(analyte = c("Analyte1", "Analyte2"),
                            half.life = c(TRUE, TRUE),
                            cmax = c(TRUE, TRUE),
                            impute = c("start_conc0,start_predose", "start_predose")))
  }
)

### Test interval_remove_impute
test_that("interval_remove_impute throws an error if either data or target_impute is missing", {
  expect_error(interval_remove_impute(o_data), "Both 'data' and 'target_impute' must be provided.")
})

test_that("interval_remove_impute throws an error for non-character target_impute", {
  expect_error(interval_remove_impute(o_data, target_impute = 123),
               "'target_impute' must be a character string.")
})

test_that("interval_remove_impute throws an error when input data is not in correct format", {
  expect_error(interval_remove_impute(data = o_conc, target_impute = "start_conc0"))
  expect_no_error(interval_remove_impute(data = o_data, target_impute = "start_conc0"))
})

test_that("interval_remove_impute throws an error for unknown target_params", {
  expect_error(interval_remove_impute(o_data,
                                      target_impute = "start_conc0",
                                      target_params = "unknown_param"))
})

test_that("interval_remove_impute handles impute column with FALSE values correctly", {
  o_data_with_na_impute <- o_data
  o_data_with_na_impute$intervals$impute <- NA_character_
  result <- interval_remove_impute(o_data_with_na_impute, target_impute = "start_conc0")
  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                          half.life = c(TRUE, TRUE, TRUE),
                          cmax = c(TRUE, TRUE, TRUE),
                          impute = c(NA_character_, NA_character_, NA_character_)))
})

test_that("interval_remove_impute reports an error when impute column is not a character", {
  o_data_not_character_impute <- o_data
  o_data_not_character_impute$intervals$impute <- 1
  expect_error(interval_remove_impute(o_data_not_character_impute, target_impute = "start_conc0"),
               "The 'impute' column in the intervals data.frame must be a character column.")
})

test_that("interval_remove_impute does not modify data if global impute & column are missing", {
  d_no_imp <- o_data
  d_no_imp$intervals <- d_no_imp$intervals[, !names(d_no_imp$intervals) %in% "impute"]
  d_no_imp$impute <- NA_character_
  res <- suppressWarnings(interval_remove_impute(d_no_imp,
                                                 target_impute = "start_conc0"))
  expect_equal(res, d_no_imp)
  res <- suppressWarnings(interval_remove_impute(d_no_imp$intervals,
                                                 target_impute = "start_conc0"))
  expect_equal(res, d_no_imp$intervals)
  expect_warning(interval_remove_impute(d_no_imp, target_impute = "start_conc0"),
                 paste0("No default impute column or global method identified.",
                        " No impute methods to remove"))
  expect_warning(interval_remove_impute(d_no_imp$intervals, target_impute = "start_conc0"),
                 "No default impute column identified. No impute methods to remove")
})

test_that("interval_remove_impute.PKNCAdata handles missing impute column using global impute", {
  o_d_no_imp <- o_data
  o_d_no_imp$intervals <- o_d_no_imp$intervals[, !names(o_d_no_imp$intervals) %in% "impute"]
  o_d_no_imp$impute <- "start_conc0, start_predose"

  # When targets are all intervals, global method is changed
  res_no_target <- interval_remove_impute(o_d_no_imp, target_impute = "start_conc0")
  expect_equal(res_no_target$impute, "start_predose")

  # When targets are specific intervals, then a new column is created and the action handled
  res_target <- interval_remove_impute(o_d_no_imp,
                                       target_impute = "start_conc0",
                                       target_groups = data.frame(analyte = "Analyte1"))
  expect_equal(unique(res_target$intervals[res_target$intervals$analyte == "Analyte1", "impute"]),
               "start_predose")
  expect_equal(res_target$intervals[res_target$intervals$analyte == "Analyte2", "impute"],
               "start_conc0, start_predose")
})

test_that("interval_remove_impute with no optional parameters uses all relevant cases", {
  result <- interval_remove_impute(o_data, target_impute = "start_conc0")
  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                          half.life = c(TRUE, TRUE, TRUE),
                          cmax = c(TRUE, TRUE, TRUE),
                          impute = c("start_predose", "start_predose", NA_character_)))
})

test_that("interval_remove_impute handles specified target_params correctly", {
  result <- interval_remove_impute(o_data,
                                   target_impute = "start_conc0",
                                   target_params = "half.life")

  # Target parameter's impute was changed
  half_life_rows <- result$intervals$half.life & !is.na(result$intervals$half.life)
  expect_equal(result$intervals[half_life_rows, c("analyte", "half.life", "impute")] |>
                 `rownames<-`(NULL),
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                          half.life = c(TRUE, TRUE, TRUE),
                          impute = c("start_predose", "start_predose", NA_character_)))

  # Non-target parameter has the same impute
  cmax_rows <- result$intervals$cmax & !is.na(result$intervals$cmax)
  expect_equal(result$intervals[cmax_rows, c("analyte", "cmax", "impute")] |>
                 `rownames<-`(NULL),
               o_data$intervals[o_data$intervals$cmax, c("analyte", "cmax", "impute")] |>
                 `rownames<-`(NULL))
})

test_that("interval_remove_impute handles target_groups correctly", {
  result <- interval_remove_impute(o_data, target_impute = "start_conc0",
                                   target_groups = data.frame(analyte = "Analyte1"))
  expect_equal(result$intervals[result$intervals$analyte == "Analyte1",
                                c("analyte", "half.life", "cmax", "impute")] |>
                 `rownames<-`(NULL),
               data.frame(analyte = c("Analyte1", "Analyte1"),
                          half.life = c(TRUE, TRUE),
                          cmax = c(TRUE, TRUE),
                          impute = c("start_predose", NA_character_)))
  expect_equal(result$intervals[result$intervals$analyte == "Analyte2",
                                c("analyte", "half.life", "cmax", "impute")],
               o_data$intervals[o_data$intervals$analyte == "Analyte2",
                                c("analyte", "half.life", "cmax", "impute")])
})

test_that("interval_remove_impute handles multiple target_params correctly", {
  result <- interval_remove_impute(o_data, target_impute = "start_conc0",
                                   target_params = c("half.life", "cmax"))

  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                          half.life = c(TRUE, TRUE, TRUE),
                          cmax = c(TRUE, TRUE, TRUE),
                          impute = c("start_predose", "start_predose", NA_character_)))
})

test_that("interval_remove_impute makes no changes and warns when no matching intervals found", {
  result <- suppressWarnings(
    interval_remove_impute(
      o_data,
      target_impute = "start_conc0",
      target_groups = data.frame(analyte = "Analyte3")
    )
  )
  expect_equal(result, o_data)

  expect_warning(interval_remove_impute(o_data,
                                        target_impute = "start_conc0",
                                        target_groups = data.frame(analyte = "Analyte3")),
                 paste0("No intervals found with the specified target parameters,",
                        " groups and/or impute method. No changes made."))
})

test_that("interval_remove_impute handles properly impute character method with multiple imputes", {
  o_data_multiple_imputes <- o_data
  o_data_multiple_imputes$intervals$impute <- "start_conc0,start_predose"
  result <- interval_remove_impute(o_data_multiple_imputes, target_impute = "start_conc0")
  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                          half.life = c(TRUE, TRUE, TRUE),
                          cmax = c(TRUE, TRUE, TRUE),
                          impute = c("start_predose", "start_predose", "start_predose")))
})

test_that("interval_remove_impute handles mixed TRUE/FALSE for cmax and half.life correctly", {
  intervals_mixed <- data.frame(
    start = c(0, 0, 0, 0),
    end = c(24, 48, Inf, 72),
    half.life = c(TRUE, FALSE, TRUE, FALSE),
    cmax = c(FALSE, TRUE, FALSE, TRUE),
    impute = c("start_conc0,start_predose", "start_predose", "start_conc0", "start_predose"),
    analyte = c("Analyte1", "Analyte2", "Analyte1", "Analyte2"),
    ID = c(1, 2, 1, 2)
  )

  o_data_mixed <- PKNCAdata(o_conc, o_dose, intervals = intervals_mixed)

  result <- interval_remove_impute(o_data_mixed, target_impute = "start_conc0",
                                   target_params = c("half.life", "cmax"))

  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1", "Analyte2"),
                          half.life = c(TRUE, FALSE, TRUE, FALSE),
                          cmax = c(FALSE, TRUE, FALSE, TRUE),
                          impute = c("start_predose", "start_predose", NA_character_, "start_predose")))
})

test_that("interval_remove_impute removes properly all target_impute even if is several times", {
  o_data_multiple_imputes <- o_data
  o_data_multiple_imputes$intervals$impute <- "start_conc0,start_predose,start_conc0"
  result <- interval_remove_impute(o_data_multiple_imputes, target_impute = "start_conc0")
  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte2", "Analyte1"),
                          half.life = c(TRUE, TRUE, TRUE),
                          cmax = c(TRUE, TRUE, TRUE),
                          impute = c("start_predose", "start_predose", "start_predose")))
})

test_that("interval_remove_impute includes new rows right after the original ones", {
  result <- interval_remove_impute(o_data, target_impute = "start_conc0", target_param = "cmax")
  expect_equal(result$intervals[, c("analyte", "half.life", "cmax", "impute")],
               data.frame(analyte = c("Analyte1", "Analyte1", "Analyte2", "Analyte1", "Analyte1"),
                          half.life = c(TRUE, FALSE, TRUE, TRUE, FALSE),
                          cmax = c(FALSE, TRUE, TRUE, FALSE, TRUE),
                          impute = c("start_conc0,start_predose",
                                     "start_predose",
                                     "start_predose",
                                     "start_conc0",
                                     NA_character_)))
})

test_that("interval_add_impute and interval_remove_impute are inverses of each other", {
  result_add <- interval_add_impute(o_data, target_impute = "new_impute")
  result_remove <- interval_remove_impute(result_add, target_impute = "new_impute")
  expect_equal(result_remove, o_data)
})

# Specific tests for helper functions
test_that("add_impute_method do not crush when impute_vals is empty, returns the empty vector", {
  expect_equal(add_impute_method(character(), "new_impute"), character())
})

test_that("remove_impute_method do not crush when impute_vals is empty, returns the empty vector", {
  expect_equal(remove_impute_method(character(), "new_impute"), character())
})

