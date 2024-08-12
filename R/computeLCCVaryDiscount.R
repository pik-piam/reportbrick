#' Compute LCC for varying discount rate
#'
#' @author Ricarda Rosemann
#'
#' @param dfLt data frame, lifetime estimate
#' @param dfCostsOpe data frame, operational costs
#' @param dfCostsRen data frame, installation/renovation costs
#' @param dfDt data frame, lengths of time periods
#' @param dfDiscount data frame, discount factors
#'
#' @importFrom dplyr %>% mutate
#'
computeLCCVaryDiscount <- function(dfLt, dfCostsOpe, dfCostsRen, dfDt, dfDiscount) {

  lccRes <- rbind(
    computeLCC(dfLt, dfCostsOpe, dfCostsRen, dfDt, dfDiscount) %>%
      mutate(r = 0.21),
    computeLCC(dfLt, dfCostsOpe, dfCostsRen, dfDt, 0.05) %>%
      mutate(r = 0.05),
    computeLCC(dfLt, dfCostsOpe, dfCostsRen, dfDt, 0.3) %>%
      mutate(r = 0.3)
  )
}
