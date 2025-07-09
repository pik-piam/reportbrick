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
computeDiscountSum <- function(df, dfDt, dfDiscount, ttotInNum, ttotOutNum) {

  if (!"ttot" %in% colnames(df)) df <- tidyr::crossing(df, ttot = ttotInNum)

  df %>%
    select(-"bs") %>%
    filter(.data[["ttot"]] %in% ttotInNum) %>%
    left_join(dfDt, by = "ttot") %>%
    left_join(dfDiscount %>%
                rename(discountIn = "value"),
              by = c("ttot", "typ")) %>%
    rename(ttotIn = "ttot") %>%
    tidyr::crossing(ttotOut = ttotOutNum) %>%
    filter(.data[["ttotIn"]] <= .data[["ttotOut"]]) %>%
    
    # Compute discounting between ttotIn and ttotOut
    left_join(dfDiscount %>%
                rename(discountOut = "value"),
              by = c("ttotOut" = "ttot", "typ")) %>%
    mutate(discount = .data[["discountOut"]] / .data[["discountIn"]]) %>%
    select(-"discountIn", -"discountOut") %>%
    
    # Compute the cumulative discounted sum along ttotOut
    group_by(across(-any_of(c("ttotOut", "discount", "dt", "value")))) %>%
    mutate(value = cumsum(.data[["value"]] * .data[["discount"]] * .data[["dt"]])) %>%
    ungroup() %>%
    select(-"dt", -"discount")

}
