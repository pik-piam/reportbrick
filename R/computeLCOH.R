#' Compute LCOH from LCC
#'
#' Compute the levelized costs of heat (LCOH) by scaling the LCC with discounted UE demand
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

  # Compute the sum of discounted UE demand
  dfUe <- computeDiscountSum(dfUe, dfDt, dfDiscount, unique(dfLt[["ttotIn"]]), unique(dfLt[["ttotOut"]])) %>%
    rename(discountSum = "value")

  # Compute the (discounted) expected UE demand based on the heating system lifetime
  expUe <- dfLt %>%
    rename(ltProb = "relVal") %>%
    select(-"absVal") %>%
    left_join(dfUe, by = c(intersect(dims, colnames(dfUe)), "ttotIn", "ttotOut")) %>%
    group_by(across(all_of(c(dims, "ttotIn")))) %>%
    summarise(ue = sum(.data[["ltProb"]] * .data[["discountSum"]]), .groups = "drop")

  joinDims <- if ("hsr" %in% colnames(dfLcc)) c(setdiff(dims, "hs"), hsr = "hs", "ttotIn") else c(dims, "ttotIn")

  # Scale LCC by expected UE demand to obtain LCOH
  dfLcc %>%
    rename(lcc = "value") %>%
    left_join(expUe, by = joinDims) %>%
    mutate(value = .data[["lcc"]] / .data[["ue"]],
           value = .data$value * 100) %>% # Convert USD to US cents
    select(-"ue", -"lcc")
}
