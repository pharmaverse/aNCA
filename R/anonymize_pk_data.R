#' This function anonymizes RO numbers and Study IDs of pk data files.
#'
#' @import haven
#' @import dplyr
#'
#' @param path path to dataframe in xpt, sas or csv format
#' @param overwrite decides whether file at input location `path` is overwritten. default is TRUE
#' @param drug_map set to NULL
#' @return anonymized dataframe file
#'
#' @export
#'
#' @author Pascal Bärtschi
#'
anonymize_pk_data <- function(path, drug_map = NULL, overwrite = TRUE) {
  # assert file
  checkmate::assert_file_exists(path)

  # file extension
  extension <- stringr::str_split_i(path, "\\.", i = -1)

  # extension decision read-in
  if (extension == "csv") {
    data <- read.csv(path)
  } else if (extension == "sas7bdat") {
    data <- read_sas(path)
  } else if (extension == "xpt") {
    data <- read_xpt(path)
  } else {
    stop("This file extension is not supported yet.")
  }

  #assert data loading
  checkmate::assert(checkDataFrame(data))

  # find all ronumbers
  all_ronum <- lapply(
    X = data,
    FUN = function(x) stringr::str_extract(x, "\\bRO\\d{7}\\b")
  ) %>%
    unlist() %>%
    unique() %>%
    na.omit()

  # find all tracknumbers
  all_tracknum <- lapply(
    X = data,
    FUN = function(x) stringr::str_extract(x, "\\bTrack\\d{2}\\b")
  ) %>%
    unlist() %>%
    unique() %>%
    na.omit()

  all_studyid <- lapply(
    X = data,
    FUN = function(x) stringr::str_extract(x, "\\b..\\d{5}\\b")
  ) %>%
    unlist() %>%
    unique() %>%
    na.omit()

  if (length(all_ronum) > 0) {
    # instead of seq along, generate random numbers with 7 digits
    ronum_map <- setNames(paste0("Analyte", sprintf("%02d", seq_along(all_ronum))), all_ronum)
  } else {
    ronum_map <- character(0)
  }

  if (length(all_tracknum) > 0) {
    # instead of seq along, generate random numbers with 7 digits
    tracknum_map <- setNames(
      paste0("Analyte",  sprintf("%02d", seq_along(all_tracknum))), all_tracknum
    )
  } else {
    tracknum_map <- character(0)
  }

  if (length(all_studyid) > 0) {
    studyid_map <- setNames(paste0("XX",  sprintf("%02d", seq_along(all_studyid))), all_studyid)
  } else {
    studyid_map <- character(0)
  }

  # concatenate the maps
  concat_map <- c(ronum_map, studyid_map, tracknum_map, drug_map) # nolint

  # columns required for NCA
  req4nca <- c(
    "STUDYID", "USUBJID", "PCSPEC", "DOSEFRQ", "DOSNO", "AFRLT", "ARRLT", "NRRLT",
    "NFRLT", "PCSTRESC", "PARAM", "TAU", "PCSTRESU", "ROUTE", "DOSEA", "AGE", "SEX", "RACE",
    "ADOSEDUR", "DOSEDURU", "NDOSEDUR", "RRLTU", "DOSEA", "DOSEU", "PCLLOQ", "DRUG",
    "DOSETRT", "AVISIT", "AVAL", "AVALU", "DOSEU", "EVID", "ATPTREF", "SITEID", "TRT01A",
    "TRT01P", "PCRFTDTM", "WTBL", "WTBLU", "HTBL", "HTBLU"
  )

  # mutate and rename
  data <- data %>%
    # apply the map to all columns that hold RO and STUDYNUMBERS
    mutate_all(~replace_str_with_map(., concat_map)) %>%
    rename_with(~replace_str_with_map(., concat_map)) %>%
    # select only columns that are needed to perform NCA
    select(any_of(req4nca))

  # find the req4nca columns that are not in data
  if (setdiff(req4nca, colnames(data)) %>% length > 0) {
    message(paste(
      "The dataframe with path", path, "is missing the following colnames: ",
      paste(setdiff(req4nca, colnames(data)), collapse = ", ")
    ))
  }

  # compose a message warning that the unique values of start_with("TRT")
  # are ... and that DRUG names have to anonymized by hand
  if (any(grepl("TRT", colnames(data)))) {
    message(
      paste(
        "The unique values of the columns starting with TRT are: ",
        data %>%
          select(all_of(
            data %>%
              select(starts_with("TRT")) %>%
              colnames()
          )) %>%
          unique() %>%
          paste(collapse = ", "),
        toupper(". Please make sure to replace drug names!")
      )
    )

  }

  # save the file
  if (extension == "csv") {
    if (overwrite) {
      write.csv(data, path, row.names = FALSE)
    } else {
      write.csv(data, paste0(gsub(".csv", "", path), "_anonymized.csv"), row.names = FALSE)
    }

  } else if (extension == "sas7bdat") {
    if (overwrite) {
      write_xpt(data, gsub(".sas7bdat", ".xpt", path))
      file.remove(path)
      message(
        "The sas7bdat file was converted to xpt and the original file was removed, ",
        "as the sas7bdat format was deprecated in haven."
      )
    } else {
      write_xpt(data, paste0(gsub(".sas7bdat", "", path), "_anonymized.xpt"))
      file.remove(path)
      message(
        "The sas7bdat file was converted to xpt and the original file was removed, ",
        "as the sas7bdat format was deprecated in haven."
      )
    }
  } else if (extension == "xpt") {
    if (overwrite) {
      write_xpt(data, path)
    } else {
      write_xpt(data, paste0(gsub(".xpt", "", path), "_anonymized.xpt"))
    }
  }

}

# function to replace according to the map
replace_str_with_map <- function(x, map) {
  for (str_ in names(map)) {
    x <- gsub(str_, map[[str_]], x)
  }
  return(x)
}
