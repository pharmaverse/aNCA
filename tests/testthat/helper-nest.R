nest_adpp_fixture <- function() {
  data.frame(
    USUBJID = rep(c("1", "2", "3"), each = 2), TRT01A = "A", PPCAT = "Drug",
    PPSPEC = "SERUM", PARAM = rep(c("Cmax", "AUC"), 3), AVAL = c(1, 3, 2, 6, 4, 9),
    AVALU = rep(c("mg/L", "h*mg/L"), 3), PPSUMXF = "", stringsAsFactors = FALSE
  )
}

# Two partial-AUC intervals share PARAM "AUC"; Cmax has no interval.
nest_interval_fixture <- function() {
  d <- nest_adpp_fixture()
  d$PPSTINT <- ifelse(d$PARAM == "AUC", "PT0H", NA_character_)
  d$PPENINT <- ifelse(d$PARAM == "AUC", "PT2H", NA_character_)
  later <- d[d$PARAM == "AUC", ]
  later$PPENINT <- "PT6H"
  later$AVAL <- later$AVAL * 2
  rbind(d, later)
}
