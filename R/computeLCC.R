#' Compute LCC
#'
#' Compute the lifecycle costs (LCC)
#'
#' @author Ricarda Rosemann
#'
#' @param dfLt data frame, lifetime estimate
#' @param dfCostsOpe data frame, operational costs
#' @param dfCostsRen data frame, installation/renovation costs
#' @param dfDt data frame, lengths of time periods
#' @param dfDiscount data frame, discount factors
#'
#' @importFrom dplyr all_of left_join mutate rename rename_with
#' @importFrom tidyr pivot_longer
#'
computeLCC <- function(dfLt, dfCostsOpe, dfCostsRen, dfDt, dfDiscount) {

  hsName <- if ("hsr" %in% colnames(dfCostsRen)) "hsr" else "hs"

  .computeLCCOpe(dfLt, dfCostsOpe, dfDt, dfDiscount) %>%
    rename(lccOpe = "value") %>%
    rename_with(~ if ("hsr" %in% colnames(dfCostsRen)) paste0(.x, "r") else .x, .cols = "hs") %>%
    left_join(dfCostsRen,
              by = c(hsName, "vin", "reg", "loc", "typ", "ttot")) %>%
    pivot_longer(cols = all_of(c("tangible", "intangible", "lccOpe")),
                 names_to = "costType", values_to = "value") %>%
    mutate(bs = "low")
}

#' Compute lifecycle operational costs
#'
#' @param dfLt data frame, lifetime estimate
#' @param dfCosts data frame, costs
#' @param dfDiscount data frame, discount factor
#'
#' @importFrom dplyr %>% across all_of .data group_by left_join mutate summarise
#' @importFrom tidyr crossing
#'
.computeLCCOpe <- function(dfLt, dfCosts, dfDt, dfDiscount) {

  if (!is.data.frame(dfDiscount)) {
    dfDiscount <- data.frame(
      value = dfDiscount
    ) %>%
      crossing(ttot = unique(dfLt[["ttot"]]), typ = unique(dfLt[["typ"]])) %>%
      mutate(value = 1 / (1 + .data[["value"]])^.data[["ttot"]])
  }

  dfCosts <- computeDiscountSum(dfCosts, dfDt, dfDiscount, unique(dfLt[["ttot"]]), unique(dfLt[["ttot2"]]))

  # Compute Lifetime operational costs based on lifetime data
  dfLt%>%
    left_join(dfCosts, by = c("hs", "vin", "reg", "loc", "typ", "ttot", "ttot2")) %>%
    group_by(across(all_of(c("qty", "hs", "vin", "reg", "loc", "typ", "ttot")))) %>%
    summarise(value = sum(.data[["relVal"]] * .data[["cumVal"]]), .groups = "drop")
}
