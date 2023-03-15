#' Adds vaccination date variables to the output dataset.
#'
#' Creates vaccination date variables from ex domain.A date variable will be
#' created for each vaccination taking values from the variable `EXSTDTC`.
#'
#' @param dataset Input dataset
#'
#'   The variables specified by the `by_vars` argument are expected.
#'
#' @param by_vars Grouping variables.
#'
#' *Default: exprs(USUBJID, VISITNUM)*
#'
#' The variables to be grouped to filter the first observation within each
#' by group.
#'
#' @param order Sorting variables.
#'
#' *Default: exprs(USUBJID,VISITNUM,VISIT,EXSTDTC)*
#'
#'  The variables order to be specified either in ascending or descending order.
#'  By default ascending order will be applicable.
#'
#' @return the dataset with vaccination date variables which is created
#' using `EXSTDTC`.
#'
#' @author Vikram S
#'
#' @details
#'
#' If there are multiple vaccinations for a visit per subject,only first
#' observation will be filtered based on the variable order specified
#' on the `order` argument.
#'
#' The number of variables created will be based on the number of vaccinations
#' per subject per visit.
#'
#' The output dataset will have one record per subject.
#'
#' @export
#'
#' @keywords der_adxx
#'
#' @family der_adxx
#'
#' @examples
#' derive_vars_vaxdt(
#'   dataset = ex,
#'   by_vars = exprs(USUBJID,VISITNUM),
#'   order = exprs(USUBJID,VISITNUM,VISIT,EXSTDTC)
#' )

derive_vars_vaxdt <- function(dataset,
                              by_vars,
                              order) {
  # assertion checks
  assert_vars(by_vars)
  assert_order_vars(order, optional = TRUE)
  assert_data_frame(dataset, required_vars = by_vars)

  # derive vaccination date variables
  dt_var <- dataset %>%
    group_by(!!!by_vars) %>%
    arrange(!!!order) %>%
    filter(row_number()==1) %>%
    pivot_wider(names_from = VISITNUM,values_from = EXSTDTC,names_prefix = "vaxdt",
                id_cols = USUBJID) %>% ungroup()

  # select only vaccination date variables
  s_name <- dt_var %>% select(starts_with("vaxdt"))

  # rename the derived vaccination date variables
  for (i in seq_along(s_name)){
    names(s_name)[i] <- paste0("VAX0",i,"DT")
    s_name[[i]] <- as.Date(s_name[[i]])
  }

 # Keep only `USUBJID` and vaccination date variables in the output dataset
 dt_var %>%
  select(USUBJID) %>%
  bind_cols(s_name)

}




