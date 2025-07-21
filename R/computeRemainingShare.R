#' Compute the remaining share of an inflow
#'
#' @author Ricarda Rosemann
#'
#' @param dfLt data frame with life time data
#' @param asDensity logical, whether the data is given as a density
#'
#' @importFrom dplyr %>% across all_of .data group_by mutate select
#'
computeRemainingShare <- function(dfLt, asDensity = TRUE, valueName = "relVal") {
  
  dfLt %>%
    rename(relVal = valueName) %>%
    select(-any_of("absVal")) %>%
    group_by(across(-all_of(c("ttotOut", "relVal")))) %>%
    mutate(relVal = if (isTRUE(asDensity)) 1 - cumsum(.data$relVal) else 1 - .data$relVal) %>%
    ungroup()
}