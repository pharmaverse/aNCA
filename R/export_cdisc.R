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

  # Define the CDISC columns we need and its rules using the metadata_variables object
  CDISC_COLS <- metadata_variables %>%
    filter(Dataset %in% c("ADPC", "ADPP", "PP")) %>%
    arrange(Order) %>%
    group_by(Dataset) %>%
    group_split() %>%
    setNames(c("ADPC", "ADPP", "PP"))

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
        # Raw variables that can be directly used in PP or ADPP if present
        CDISC_COLS$PP$Variable, CDISC_COLS$ADPP$Variable,
        # Variables that can be used to guess other missing variables
        "PCRFTDTM"
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
      PPRFTDTC = {
        if ("PCRFTDTC" %in% names(.)) {
          PCRFTDTC
        } else if ("PCRFTDTM" %in% names(.)) {
          strptime(PCRFTDTM, format = "%d-%m-%Y %H:%M") %>% format("%Y-%m-%dT%H:%M:%S")
        } else {
          NA_character_
        }
      },
      EPOCH = if ("EPOCH" %in% names(.)) {
        EPOCH
      } else {
        NA_character_
      },
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
      # ADPP Specific
      AVAL = PPSTRESN,
      AVALC = PPSTRESC,
      AVALU = PPSTRESU,
      PARAMCD = PPTESTCD,
      PARAM = PPTEST,
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
    # Select only columns needed for PP, ADPP, ADPC
    select(any_of(metadata_variables[["Variable"]])) %>%
    # Make character expected columns NA_character_ if missing
    mutate(
      across(
        .cols = setdiff(
          metadata_variables %>%
            filter(Core == "Exp" & (Type == "Char" | Type == "Text")) %>%
            pull(Variable),
          names(.)
        ),
        .fns = ~ NA_character_,
        .names = "{.col}"
      )
    ) %>%
    # Make numeric expected columns NA if missing
    mutate(
      across(
        .cols = setdiff(
          metadata_variables %>%
            filter(Core == "Exp" & (Type != "Char" & Type != "Text")) %>%
            pull(Variable),
          names(.)
        ),
        .fns = ~ NA,
        .names = "{.col}"
      )
    )

  # Adjust class and length to the standards
  cdisc_info <- adjust_class_and_length(cdisc_info, metadata_variables)

  # Add labels to the columns
  labels_map <- metadata_variables %>%
    filter(!duplicated(Variable)) %>%
    pull(Label, Variable)
  var_labels(cdisc_info) <- labels_map[names(cdisc_info)]

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

  adpp <- cdisc_info %>%
    select(any_of(c(CDISC_COLS$ADPP$Variable)))

  adpc <- res_nca$data$conc$data %>%
    left_join(dose_info,
              by = intersect(names(res_nca$data$conc$data), names(dose_info)),
              suffix = c("", ".y")) %>%
    mutate(
      PARAMCD = if ("PARAMCD" %in% names(.)) {
        PARAMCD
      } else if ("PCTESTCD" %in% names(.)) {
        PCTESTCD
      } else {
        NA_character_
      },
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
      PCSTRESU = AVALU,
      PCRFTDTM = if ("PCRFTDTM" %in% names(.)) {
        as.POSIXct(strptime(PCRFTDTM, format = "%d-%m-%Y %H:%M"))
      } else {
        NA
      }
    ) %>%
    # Order columns using a standard, and then put the rest of the columns
    select(any_of(CDISC_COLS$ADPC$Variable))

  adpc <- adjust_class_and_length(adpc, metadata_variables)
  var_labels(adpc) <- labels_map[names(adpc)]

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

# Helper: adjust class and length for a data.frame based on metadata_variables
adjust_class_and_length <- function(df, metadata) {
  for (var in names(df)) {
    var_specs <- metadata %>% filter(Variable == var, !duplicated(Variable))
    if (nrow(var_specs) == 0) next
    if (var_specs$Type %in% c("Char", "text")) {
      df[[var]] <- substr(as.character(df[[var]]), 1, var_specs$Length)
    } else if (var_specs$Type %in% c("Num", "integer", "float", "duration") &&
               !endsWith(var, "DTM")) {
      df[[var]] <- round(as.numeric(df[[var]]), var_specs$Length)
    }
  }
  df
}
