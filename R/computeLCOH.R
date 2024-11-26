#' Compute LCOH from LCC
#'
#' @author Ricarda Rosemann
#'
#' @param dfLcc data frame, lifecycle costs
#' @param dfLt data frame, lifetime estimate
#' @param dfUe data frame, useful energy demand
#' @param dfDt data frame, lengths of time periods
#' @param dfDiscount data frame, discount rates
#' @param dims character, dimensions of all data, excluding time dimension
#'
#' @importFrom dplyr %>% .data group_by left_join mutate rename select summarise
#'
computeLCOH <- function(dfLcc, dfLt, dfUe, dfDt, dfDiscount, dims) {

  dfUe <- computeDiscountSum(dfUe, dfDt, dfDiscount, unique(dfLt[["ttot"]]), unique(dfLt[["ttot2"]])) %>%
    select(-"value") # Handle this in computeDiscountSum instead?

  expUe <- dfLt %>%
    rename(ltProb = relVal) %>%
    select(-"absVal") %>%
    left_join(dfUe, by = c(intersect(dims, colnames(dfUe)), "ttot", "ttot2")) %>%
    group_by(across(all_of(c(dims, "ttot")))) %>%
    summarise(ue = sum(.data[["ltProb"]] * .data[["cumVal"]]), .groups = "drop")

  dfLcc %>%
    rename(lcc = "value") %>%
    left_join(expUe, by = c(dims, "ttot")) %>%
    mutate(value = .data[["lcc"]] / .data[["ue"]]) %>%
    select(-"ue", -"lcc")
}
