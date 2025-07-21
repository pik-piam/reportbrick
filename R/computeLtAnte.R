#' Compute heating system lifetimes ex-ante
#'
#' Determine the lifetime of the heating system according to the respective Weibull distribution.
#' No early retirement is reflected
#'
#' @author Ricarda Rosemann
#'
#' @param variable character, variable for which the lifetime is computed
#' @param data data frame, dimensions of desired lifetimes,
#'   usually also contains values to compute the absolute removed quantities
#' @param ttotNum numeric, all time periods to be considered
#' @param lifeTimeHs data frame, Weibull parameters
#' @param dims character, dimensions of the data without time dimensions
#' @param runSimple logical, whether to use the simplified formula
#' @param dataValue data frame, optionally provide the values to compute the
#'   absolute removed quantities separately
#' @param p_dt data frame, lengths of the time periods
#'   (only required for simple formula)
#' @param returnDistr logical, whether to return the distribution rather than the density function
#'
#' @importFrom dplyr %>% across all_of .data filter group_by left_join mutate
#'   rename select summarise ungroup
#'
computeLtAnte <- function(data, shareRen, t0, isFlow = TRUE) {
  
  shareRen <- shareRen %>%
    group_by(across(-all_of(c("ttotOut", "value")))) %>%
    mutate(value = c(.data$value[1], diff(.data$value))) %>%
    ungroup() %>%
    rename(relVal = "value")

  # Compute absolute floorspace being removed from the stock from relative share
  ltAnte <- data %>%
    left_join(shareRen,
              by = c("hs", "region", "typ", "ttotIn")) %>%
    filter(.data$ttotIn <= .data$ttotOut, xor(isTRUE(isFlow), .data$ttotIn %in% t0),
           !.data$ttotOut %in% t0) %>%
    mutate(absVal = .data[["value"]] * .data[["relVal"]]) %>%
    select(-"value")

  return(ltAnte)
}
