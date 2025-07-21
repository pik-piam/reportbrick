#' Rescale the remaining share to a given Weibull inflow
#'
#' @author Ricarda Rosemann
#'
#' @param remainingShare data frame with share of remaining installations
#' @param wbRescale data frame with rescaling factor from weibull distribution
#' @param ttotRescale numeric, time period to rescale on
#'
#' @importFrom dplyr %>% .data filter left_join mutate rename select
#' @importFrom tidyr complete nesting
#'
rescaleRemainingShare <- function(remainingShare, wbRescale, ttotRescale, dims) {

  wbRescale <- wbRescale %>%
    filter(.data$ttotOut == ttotRescale) %>%
    select(-"ttotIn", -"ttotOut") %>%
    rename(scale = "relVal")
  
  remainingShare %>%
    complete(
      nesting(qty, bs, hs, vin, region, loc, typ, inc, ttotIn),
      ttotOut = c(ttotRescale, .data$ttotOut),
      fill = list(relVal = 1)
    ) %>%
    left_join(wbRescale, by = c("hs", "region", "typ")) %>%
    mutate(relVal = .data$relVal * .data$scale) %>%
    select(-"scale")
}