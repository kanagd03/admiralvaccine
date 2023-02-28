#' `derive_vars_params.R`
#'
#' Creating `PARAMCD` from lookup file and assigning `PARAM`,`PARAMN`,`PARCAT1`,
#' `PARCAT2` variables
#'
#' @param dataset - Input dataset
#'        Input dataset is expected to have variables `USUBJID`,`FAOBJ`,
#'        `FACAT`, `FATESTCD` and `FATEST`
#'
#' @param lookup_dataset - lookup file containing `PARAMCD` values for every
#'        unique `FATESTCD` and `FAOBJ`
#'        lookup file is expected to have variables `FATEST`, `PARAMCD`,
#'        `FATESTCD`, `FAOBJ` and one entry for every unique
#'        `FATESTCD` and `FAOBJ`
#'
#' @return The output dataset contains all observations and variables of the
#'   input dataset along with `PARAM`,`PARAMCD`,`PARCAT1`,`PARCAT2`,`PARAMN`
#'
#' @author Dhivya Kanagaraj
#'
#' @details A lookup file is required with PARAMCD values for every combination
#'      of `FATEST` & `FAOBJ`.
#'      `PARAMCD` values comes from lookup file.
#'      `PARAMN` is assigned with a unique number for every unique PARAM value.
#'      `PARAM` value is a combination of `FAOBJ` `FATEST` `FASTRESU` `FALOC`
#'      `FADIR` `FALAT`
#'
#' @export
#'
#' @family der_adxx
#'
#' @keywords der_adxx
#'
#' @examples
#'
#' library(tibble)
#' lookup_dataset <- tribble(
#'   ~FATESTCD,    ~PARAMCD,    ~FATEST,                ~FAOBJ,
#'   "SEV",        "SEVREDN",   "Severity",             "Redness",
#'   "DIAMETER",   "DIARE",     "Diameter",             "Redness",
#'   "MAXDIAM",    "MDIRE",     "Maximum Diameter cm",  "Redness",
#'   "MAXTEMP",    "MAXTEMP",   "Maximum Temperature",  "Fever",
#'   "OCCUR",      "OCFEVER",   "Occurrence Indicator", "Fever",
#'   "OCCUR",      "OCERYTH",   "Occurrence Indicator", "Erythema",
#'   "SEV",        "SEVPAIN",   "Severity",             "Pain at Injection site",
#'   "OCCUR",      "OCPAIN",    "Occurrence Indicator", "Pain at Injection site",
#'   "OCCUR",      "OCSWEL",    "Occurrence Indicator", "Swelling"
#' )
#'
#' input <- tribble(
#'   ~USUBJID, ~FACAT, ~FASCAT, ~FATESTCD, ~FAOBJ, ~FATEST,
#'   "ABC101", "REACTOGENICITY", "ADMINISTRATIVE SITE", "SEV", "Redness",
#'   "Severity",
#'   "ABC101", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DIAMETER", "Redness",
#'   "Diameter",
#'   "ABC101", "REACTOGENICITY", "ADMINISTRATIVE SITE", "MAXDIAM", "Redness",
#'   "Maximum Diameter",
#'   "ABC101", "REACTOGENICITY", "SYSTEMIC", "MAXTEMP", "Fever",
#'   "Maximum Temp",
#'   "ABC101", "REACTOGENICITY", "SYSTEMIC", "OCCUR", "Fever",
#'   "Occurrence",
#'   "ABC101", "REACTOGENICITY", "ADMINISTRATIVE SITE", "OCCUR", "Erythema",
#'   "Occurrence",
#'   "ABC101", "REACTOGENICITY", "ADMINISTRATIVE SITE", "SEV", "Swelling",
#'   "Severity",
#'   "ABC101", "REACTOGENICITY", "ADMINISTRATIVE SITE", "OCCUR", "Swelling",
#'   "Occurrence",
#'   "ABC101", "REACTOGENICITY", "ADMINISTRATIVE SITE", "OCCUR", "Swelling",
#'   "Occurrence"
#' )
#' derive_vars_params(
#'   dataset = input,
#'   lookup_dataset = lookup_dataset
#' )
#'
derive_vars_params <- function(dataset,
                               lookup_dataset) {
  assert_data_frame(dataset,
    required_vars = vars(USUBJID, FAOBJ)
  )
  # Merging lookup file to get PARAMCD values
  adface <- derive_vars_merged(
    dataset,
    dataset_add = lookup_dataset,
    new_vars = vars(PARAMCD),
    by_vars = vars(FATESTCD, FAOBJ)
  )

  # Checking if permissible variable exisits in dataset
  eFASTRESU <- "FASTRESU" %in% colnames(adface)
  eFALOC <- "FALOC" %in% colnames(adface)
  eFADIR <- "FADIR" %in% colnames(adface)
  eFALAT <- "FALAT" %in% colnames(adface)

  # Assigning PARCAT1 PARCAT2 & PARAM
  adface <- adface %>%
    convert_na_to_blanks() %>%
    mutate(
      PARCAT1 = FACAT,
      PARCAT2 = FASCAT,
      PARAM = str_to_sentence(paste(FAOBJ, FATEST,
        if (eFASTRESU == "TRUE") {
          FASTRESU
        },
        if (eFALOC == "TRUE") {
          FALOC
        },
        if (eFADIR == "TRUE") {
          FADIR
        },
        if (eFALAT == "TRUE") {
          FALAT
        },
        sep = " "
      ))
    )

  # Assigning PARAMN
  paramn <- adface %>%
    distinct(PARAM, .keep_all = FALSE) %>%
    mutate(PARAMN = 1:n())

  adface <- merge(
    x = adface,
    y = paramn,
    by = c("PARAM"),
    all.x = TRUE
  )
  return(adface)
}

# ________________________END OF THE FUNCTION___________________________________
