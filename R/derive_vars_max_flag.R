#' `derive_vars_max_anlfl.R`
#'
#' @param `dataset` Input dataset
#' 
#' @param `flag1` - Flags the maximum record per subject per event per 
#'                  Vaccination
#' *Default: "ANL01FL"*
#' *Permitted value: Any variable name or NULL*
#' `NULL` denotes not to create the flag
#' 
#' @param `flag2` - Flags the maximum record per subject per event for Overall
#' *Default: "ANL02FL"*
#' *Permitted value: Any variable name or NULL*
#' `NULL` denotes not to create the flag
#' 
#' @return The output dataset creates `ANLxxFL` flags 
#'   
#' @author Dhivya Kanagaraj
#' 
#' @details This utility flags the maximum record per subject per event 
#'          per vaccination/Overall
#' 
#' @export
#' 
#' @family der_adxx
#' 
#' @keywords der_adxx
#'
#' @examples
#' library(dplyr)
#' library(tidyverse)
#' library(admiraldev)
#' library(admiral)
#' library(tibble)
#' input <- tribble( 
#'   ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FADTC, ~PARAMCD,
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10,  "2015-01-10", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "2015-01-11", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "2015-02-10", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "2015-02-11", "DIARE",
#'   "ABC101", "FATIQUE", "SEV", "VACC 1", 1,  "2015-01-10", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "2015-01-11", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "2015-02-10", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "2015-02-11", "SEVFAT")
#' 
#' derive_vars_max_flag(dataset = input,
#'                      flag1 = "ANL01FL",
#'                      flag2 = "ANL02FL")
#' derive_vars_max_flag(dataset = input,
#'                      flag1 = NULL,
#'                      flag2 = "ANL02FL")
#' derive_vars_max_flag(dataset = input,
#'                      flag1 = "ANL01FL",
#'                      flag2 = NULL)                             

derive_vars_max_flag <- function(dataset,
                                    flag1,
                                    flag2){
  
  assert_data_frame(dataset,
                    required_vars = vars(USUBJID, FAOBJ))
  
  # Getting distinct PARAMCD values except for "OCCUR"
  param <-dataset %>% 
          filter(!is.na(PARAMCD) & FATESTCD != "OCCUR") %>% 
          distinct(PARAMCD)
  
  paramcd <- as.list(param)
  
 # Flagging maximum record per subject per event per Vaccination
  if (!is.null(flag1)){
    for (value in paramcd){
      dataset <- restrict_derivation(
        dataset,
      derivation = derive_var_worst_flag,
      args = params(
        by_vars = vars(USUBJID,FAOBJ,FATPTREF),
        order = vars(FADTC),
        new_var = new_var_1,
        param_var = PARAMCD,
        analysis_var = AVAL,
        worst_high = value,
        worst_low = character(0)
        ),
      filter = !is.na(AVAL)
      )
    }
    }

  # Flagging maximum record per subject per event for Overall
  if (!is.null(flag2)){
     for (value in paramcd){
       dataset <- restrict_derivation(
         dataset,
        derivation = derive_var_worst_flag,
        args = params(
          by_vars = vars(USUBJID,FAOBJ),
          order = vars(FADTC),
          new_var = new_var_2,
          param_var = PARAMCD,
          analysis_var = AVAL,
          worst_high = value,
          worst_low = character(0)
          ),
        filter = !is.na(AVAL)
        )
     }
  }
  
  lookup <- c(flag1 = "new_var_1", 
              flag2 = "new_var_2")
  
  adface <- dataset %>% rename(any_of(lookup))
  
  return(adface)
}

# ________________________END OF THE FUNCTION___________________________________

