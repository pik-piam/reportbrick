#' Compute the discounted sum of a given variable between two time periods
#'
#' Compute the discount rate between two time periods
#' and determine the discounted sum of a given variable.
#' This can be used to compute the expected value of a time dependent variable,
#' where the end time is randomly distributed (e.g. for life cycle operational costs)
#'
#' @author Ricarda Rosemann
#'
#' @param df data frame, should contain one time vector
#' @param dfDt data frame, contains lengths of time periods
#' @param dfDiscount data frame, contains discount rate
#' @param ttot numeric, first time vector
#' @param ttot2 numeric, second time vector
#'
#' @importFrom dplyr %>% .data filter group_by left_join mutate rename select ungroup
#' @importFrom tidyr crossing
#'
computeDiscountSum <- function(df, dfDt, dfDiscount, ttotNum, ttot2Num) {

  if (!"ttot" %in% colnames(df)) df <- tidyr::crossing(df, ttot = ttotNum)

  df <- df %>%
    select(-"bs") %>%
    filter(.data[["ttot"]] %in% ttotNum) %>%
    left_join(dfDt, by = "ttot") %>%
    left_join(dfDiscount %>%
                rename(discountIn = "value"),
              by = c("ttot", "typ")) %>%
    tidyr::crossing(ttot2 = ttot2Num) %>%
    filter(.data[["ttot"]] <= .data[["ttot2"]]) %>%
    left_join(dfDiscount %>%
                rename(discountOut = "value"),
              by = c("ttot2" = "ttot", "typ")) %>%
    mutate(discount = .data[["discountOut"]] / .data[["discountIn"]]) %>%
    group_by(across(any_of(c("hs", "vin", "reg", "loc", "typ", "ttot")))) %>%
    mutate(cumVal = cumsum(.data[["value"]] * .data[["discount"]] * .data[["dt"]])) %>%
    ungroup() %>%
    select(-"dt", -"discountIn", -"discountOut", -"discount")

}
