#' Export CDISC Data
#'
#' This function processes the results from a PKNCA and exports them into CDISC compliant datasets.
#' Attention: All parameters that do no match pptest dataframe will be lost in this pipeline!
#'
#'@details Outputs are the following:
#'  * pknca_result Output from function call `pk.nca()` (formatted)
#'  * pknca_result_raw Output from function call `pk.nca()` (needs to be merged with upper later
#'                      on but now we avoid merge conflict)
#'
#' @param res_nca Object with results of the NCA analysis.
#'
#' @returns A list with two data frames:
#' \describe{
#' \item{pp}{A data frame containing the PP (Pharmacokinetic Parameters) domain data.}
#' \item{adpp}{A data frame containing the ADPP (Analysis Dataset for Pharmacokinetic Parameters)
#'            domain data.}
#' }
#'
#'
#' @import dplyr
#' @export
export_cdisc <- function(res_nca) {

  # Only select from results the requested parameters by the user
  res_nca_req <- res_nca
  res_nca_req$result <- res_nca_req$result %>%
    mutate(PPTESTCD = translate_terms(PPTESTCD, "PPTESTCD", "PKNCA"))
  res_nca_req$result <- as.data.frame(res_nca_req, filter_requested = TRUE) %>%
    mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD"))

  # Define group columns in the data
  group_conc_cols <- unique(unlist(res_nca$data$conc$columns$groups))
  group_dose_cols <- unique(unlist(res_nca$data$dose$columns$groups))
  group_diff_cols <- setdiff(group_conc_cols, group_dose_cols)
  route_col <- res_nca$data$dose$columns$route

  dose_info <- res_nca$data$dose$data %>%
    # Select only the columns that are used in the NCA results
    select(
      any_of(c(
        # Variables defined for the dose information
        group_dose_cols, "NCA_PROFILE",  route_col,
        # TODO (Gerardo): Test should use a PKNCA obj (FIXTURE) so the var is accessed via mapping
        OPTIONAL_COLUMNS,
        # Raw variables that can be directly used in PP or ADPP if present
        CDISC_COLS$PP$Variable, CDISC_COLS$ADPP$Variable
      ))
    ) %>%
    unique()

  cdisc_info <- res_nca_req$result  %>%
    left_join(dose_info,
              by = intersect(names(res_nca$result), names(dose_info)),
              suffix = c("", ".y")) %>%
    group_by(
      across(any_of(c(
        group_conc_cols, "start", "end", "PPTESTCD", "type_interval"
      )))
    )  %>%
    arrange(!!!syms(c(group_dose_cols, "start", "end", group_diff_cols, "PPTESTCD"))) %>%
    # Identify all dulicates (fromlast and fromfirst) and keep only the first one
    filter(!duplicated(paste0(USUBJID, NCA_PROFILE, PPTESTCD))) %>%
    ungroup() %>%
    #  Recode PPTESTCD PKNCA names to CDISC abbreviations
    mutate(
      PPTESTCD = translate_terms(PPTESTCD, mapping_col = "PKNCA", target_col = "PPTESTCD"),
      PPTEST = translate_terms(PPTESTCD, mapping_col = "PPTESTCD", target_col = "PPTEST"),
      DOMAIN = "PP",
      # Group ID
      PPGRPID = {
        if ("AVISIT" %in% names(.)) paste(PARAM, PCSPEC, AVISIT, sep = "-")
        else if ("VISIT" %in% names(.)) paste(PARAM, PCSPEC, VISIT, sep = "-")
        else paste(PARAM, PCSPEC, NCA_PROFILE, sep = "-")
      },
      # Parameter Category
      PPCAT = PARAM,
      PPSCAT = "NON-COMPARTMENTAL",
      PPSPEC = PCSPEC,
      # Specific ID variables
      PPSPID = if ("STUDYID" %in% names(.)) {
        paste0("/F:EDT-", STUDYID, "_PKPARAM_aNCA")
      } else {
        NA_character_
      },
      SUBJID = get_subjid(.),
      # Parameter Variables
      PPORRES = as.character(round(as.numeric(PPORRES), 12)),
      PPSTRESN = round(as.numeric(PPSTRES), 12),
      PPSTRESC = as.character(format(PPSTRESN, scientific = FALSE, trim = TRUE)),
      # SD0027: Units should be NA if there is no value
      PPORRESU = ifelse(is.na(PPORRES), NA_character_, PPORRESU),
      PPSTRESU = ifelse(is.na(PPSTRES), NA_character_, PPSTRESU),
      # Status and Reason for Exclusion
      PPSTAT = ifelse(is.na(PPSTRES), "NOT DONE",  ""),
      PPREASND = case_when(
        !is.na(exclude) & is.na(PPSTRES) ~ exclude,
        is.na(PPSTRES) ~ "NOT DERIVED",
        TRUE ~ ""
      ),
      PPREASND = substr(PPREASND, 0, 200),
      # Datetime
      PPDTC = Sys.time() %>% format("%Y-%m-%dT%H:%M"),
      PPRFTDTC = {
        if ("PCRFTDTC" %in% names(.)) {
          PCRFTDTC
        } else if ("PCRFTDTM" %in% names(.)) {
          strptime(PCRFTDTM, format = "%Y-%m-%d %H:%M:%S") %>% format("%Y-%m-%dT%H:%M:%S")
        } else {
          NA_character_
        }
      },
      PPDY = as.numeric(difftime(PPDTC, PPRFTDTC, units = "days")),
      EPOCH = if ("EPOCH" %in% names(.)) {
        EPOCH
      } else {
        NA_character_
      },
      # Matrix
      PPSPEC = PCSPEC,
      # TODO start and end intervals in case of partial aucs -> see oak file in templates
      PPSTINT = ifelse(
        startsWith(PPTESTCD, "AUCINT"),
        convert_to_iso8601_duration(start, RRLTU),
        NA_character_
      ),
      PPENINT = ifelse(
        startsWith(PPTESTCD, "AUCINT"),
        convert_to_iso8601_duration(end, RRLTU),
        NA_character_
      ),
      PPFAST = {
        if ("EXFAST" %in% names(.)) {
          EXFAST
        } else if ("PCFAST" %in% names(.)) {
          PCFAST
        } else if ("FEDSTATE" %in% names(.)) {
          FEDSTATE
        } else {
          NULL
        }
      },
      NCA_PROFILE = NCA_PROFILE
    ) %>%
    # Map PPTEST CDISC descriptions using PPTESTCD CDISC names
    group_by(USUBJID)  %>%
    mutate(PPSEQ = if ("PCSEQ" %in% names(.)) PCSEQ else row_number())  %>%
    ungroup() %>%
    # Parameters with a one-to-many mapping in PKNCA / CDISC
    mutate(
      # one-to-many based on route
      PPTEST = case_when(
        grepl("MRT(LST|IFO|IFP)", PPTESTCD) & !!sym(route_col) == "intravascular" ~ 
          gsub("(MRT )(.*)", "\\1IV Cont Inf \\2", PPTEST),
        grepl("MRT(LST|IFO|IFP)", PPTESTCD) & !!sym(route_col) == "extravascular" ~ 
          gsub("(MRT )(.*)", "\\1Extravasc \\2", PPTEST),
        TRUE ~ PPTEST
      ),
      PPTESTCD = case_when(
        grepl("MRT(LST|IFO|IFP)", PPTESTCD) & !!sym(route_col) == "intravascular" ~ 
          gsub("(MRT)(LST|IFO|IFP)", "\\1IC\\2", PPTESTCD),
        grepl("MRT(LST|IFO|IFP)", PPTESTCD) & !!sym(route_col) == "extravascular" ~ 
          gsub("(MRT)(LST|IFO|IFP)", "\\1EV\\2", PPTESTCD),
        TRUE ~ PPTESTCD
      )
    ) %>%
    # Make NA expected columns if not present in the data (character)
    mutate(
      across(
        .cols = setdiff(
          bind_rows(CDISC_COLS) %>%
            filter(Core == "Exp" & (Type == "Char" | Type == "Text")) %>%
            pull(Variable),
          names(.)
          ),
        .fns = ~ NA_character_,
        .names = "{.col}"
      )
    ) %>%
    # Make NA expected columns if not present in the data (numeric)
    mutate(
      across(
        .cols = setdiff(
          bind_rows(CDISC_COLS) %>%
            filter(Core == "Exp" & (Type != "Char" & Type != "Text")) %>%
            pull(Variable),
          names(.)
        ),
        .fns = ~ NA,
        .names = "{.col}"
      )
    )

  # select pp columns
  pp <- cdisc_info %>%
    select(any_of(c(CDISC_COLS$PP$Variable, "PPFAST"))) %>%
    # Deselect permitted columns with only NAs
    select(
      -which(
        names(.) %in% CDISC_COLS$PP$Variable[CDISC_COLS$PP$Core == "Perm"] &
        sapply(., function(x) all(is.na(x))) &
        !names(.) %in% c("EPOCH") # here are exceptions not justified by CDISC
      )
    )
  browser()

  adpp <- cdisc_info %>%
    # Rename/mutate variables from PP
    mutate(AVAL = PPSTRESN, AVALC = PPSTRESC, AVALU = PPSTRESU,
           PARAMCD = PPTESTCD, PARAM = PPTEST) %>%
    select(any_of(c(CDISC_COLS$ADPP$Variable, "PPFAST")))

  adpc <- res_nca$data$conc$data %>%
    mutate(
      ANL01FL = ifelse(is.excluded.hl, NA_character_, "Y"),
      SUBJID = get_subjid(.),
      ATPT = {
        if ("PCTPT" %in% names(.)) PCTPT
        else NA_character_
      },
      ATPTN = {
        if ("PCTPTNUM" %in% names(.)) PCTPTNUM
        else NA
      },
      ATPTREF = {
        if ("PCTPTREF" %in% names(.)) PCTPTREF
        else NA_character_
      },
      PCSTRESU = AVALU
    ) %>%
    # Order columns using a standard, and then put the rest of the columns
    select(any_of(CDISC_COLS$ADPC$Variable), everything())  %>%
    # Deselect columns that are only used internally in the App
    select(-any_of(INTERNAL_ANCA_COLS))

  # Keep StudyID value to use for file naming
  studyid <- if ("STUDYID" %in% names(cdisc_info)) unique(cdisc_info$STUDYID)[1] else ""

  list(pp = pp, adpp = adpp, adpc = adpc, studyid = studyid)
}

#' Function to identify the common prefix in a character vector.
#' @details
#' Checks the common prefix for all provided strings. If no
#' common prefix is detected, returns empty string.
#'get
#' @param strings Character vector with strings to check.
#' @returns A character string with common prefix.
#'
#' @examples
#' find_common_prefix(c("abc-100", "abc-102", "abc-103")) # "abc-10"
#' @noRd
#' @keywords internal
find_common_prefix <- function(strings) {
  # Get the strings with the greatest prefix mismatch
  letters <- sort(strings) %>%
    .[c(1, length(.))] %>%
    # For the comparison make all have same number of letters
    sapply(\(x) substr(x, 0, min(nchar(.)))) %>%
    strsplit("")

  mismatch <- letters[[1]] != letters[[2]]

  substr(strings[[1]], 0, which(mismatch)[1] - 1)
}

#' Helper function to extract SUBJID from data
#'
#' This function extracts the `SUBJID` from the provided dataset. If `SUBJID` is not available,
#' it attempts to derive it from `USUBJID` by removing the `STUDYID` prefix or the common prefix
#' shared by all `USUBJID` values.
#'
#' @details
#' The function first checks if `SUBJID` exists in the dataset. If not, it derives `SUBJID` from
#' `USUBJID` by:
#' \itemize{
#'   \item Removing the `STUDYID` prefix if it exists.
#'   \item Removing the longest common prefix shared by all `USUBJID` values.
#' }
#' If neither `SUBJID` nor `USUBJID` is available, the function returns `NA`.
#'
#' @param data A data frame containing `SUBJID`, `USUBJID`, and optionally `STUDYID`.
#'
#' @returns A vector of `SUBJID` values.
#'
#' @examples
#' data <- data.frame(
#'   STUDYID = c("STUDY1", "STUDY1"),
#'   USUBJID = c("STUDY1-001", "STUDY1-002")
#' )
#' get_subjid(data)
#'
#' @noRd
#' @keywords internal
get_subjid <- function(data) {
  if ("SUBJID" %in% names(data)) {
    data$SUBJID
  } else if ("USUBJID" %in% names(data)) {
    if ("STUDYID" %in% names(data)) {
      stringr::str_remove(as.character(data$USUBJID), paste0(as.character(data$STUDYID), "\\W?"))
    } else {
      gsub(find_common_prefix(data$USUBJID), "", data$USUBJID)
    }
  } else {
    NA
  }
}

CDISC_COLS <- list(
  ADPC = metadata_variables %>%
    filter(Dataset == "ADPC") %>%
    arrange(Order) %>%
    select(Variable, Label, Type, Role, Core, Length),

  ADPP = metadata_variables %>%
    filter(Dataset == "ADPP") %>%
    arrange(Order) %>%
    select(Variable, Label, Type, Role, Core, Length),

  PP = metadata_variables %>%
    filter(Dataset == "PP") %>%
    arrange(Order) %>%
    select(Variable, Label, Type, Role, Core, Length)
)

INTERNAL_ANCA_COLS <- c(
  "exclude", "is.excluded.hl", "volume", "std_route",
  "duration", "TIME", "IX", "exclude_half.life", "is.included.hl",
  "conc_groups", "REASON"
)

# They will be used if present. We assume they follow CDISC standard names
OPTIONAL_COLUMNS <- c("RRLTU", "PCRFTDTC", "PCRFTDTM", "EXFAST", "PCFAST", "FEDSTATE", "PCSEQ")
