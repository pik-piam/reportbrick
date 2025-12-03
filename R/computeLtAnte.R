#' Compute heating system lifetimes ex-ante
#'
#' Determine the lifetime of the heating system according to the respective Weibull distribution.
#' No early retirement is reflected
#'
#' @author Ricarda Rosemann
#'
#' @param data data frame, dimensions of desired lifetimes,
#'   usually also contains values to compute the absolute removed quantities
#' @param shareRen data frame with the data on which share of inflows has to be replaced when
#' @param t0 numeric, initial time period (time period before the model starts)
#' @param isFlow logical, is \code{data} flow data?
#'
#' @importFrom dplyr %>% across all_of .data filter group_by left_join mutate
#'   rename select ungroup
#'
computeLtAnte <- function(data, shareRen, t0, isFlow = TRUE) {

  joinCols <- c("hs", "region", "typ", "ttotIn")
  if (isFALSE(isFlow)) joinCols <- c(joinCols, "vin")

  shareRen <- shareRen %>%
    group_by(across(-all_of(c("ttotOut", "value")))) %>%
    mutate(value = c(.data$value[1], diff(.data$value))) %>%
    ungroup() %>%
    rename(relVal = "value")

  # Compute absolute floorspace being removed from the stock from relative share
  ltAnte <- data %>%
    left_join(shareRen,
              by = joinCols) %>%
    filter(.data$ttotIn <= .data$ttotOut, xor(isTRUE(isFlow), .data$ttotIn %in% t0),
           !.data$ttotOut %in% t0) %>%
    mutate(absVal = .data[["value"]] * .data[["relVal"]]) %>%
    select(-"value")

  return(ltAnte)
}
