#' Normalize LCC
#'
#' Normalize the life cycle costs by rescaling to the average lifetime across
#' all heating systems
#'
#' @author Ricarda Rosemann
#'
#' @param dfLcc data frame, lifecycle costs
#' @param dfLt data frame, lifetime estimate
#' @param dfDt data frame, lengths of time periods
#' @param flow character, which flow data is contained in \code{dfLcc} and \code{dfLt}?
#'
#' @importFrom dplyr all_of left_join mutate rename rename_with
#' @importFrom tidyr pivot_longer
#'
normalizeLCC <- function(dfLcc, dfLt, dfDt, flow) {

  if (flow == "construction") {
    avgGroup <- c("hs", "bs", "expLt")
    hsName <- "hs"
    toRename <- c()
  } else {
    avgGroup <- c("hs", "expLt")
    toRename <- c(hsr = "hs")
    hsName <- "hsr"
  }

  # Compute expected lifetime of each heating system and average lifetime across all heating systems
  expLt <- dfLt %>%
    left_join(dfDt, by = c(ttotIn = "ttot")) %>%
    left_join(dfDt, by = c(ttotOut = "ttot"), suffix = c("In", "Out")) %>%
    mutate(lt = .data$ttotOut - .data$dtOut / 2 - (.data$ttotIn - .data$dtIn / 2)) %>%
    group_by(across(any_of(c("qty", "bs", "hs", "hsr", "vin", "region", "loc", "typ",
                             "inc", "costType", "ttotIn")))) %>%
    summarise(expLt = sum(.data$lt * .data$relVal), .groups = "drop") %>%
    group_by(across(-all_of(avgGroup))) %>%
    mutate(avgLt = mean(.data$expLt),
           scale = ifelse(.data$expLt == 0, 1, .data$avgLt / .data$expLt)) %>%
    ungroup() %>%
    select(-"expLt", -"avgLt") %>%
    rename(toRename)

  # Scale LCC
  dfLcc %>%
    mutate(across(any_of("bsr"), ~ "low")) %>%
    left_join(expLt, by = c("qty", "bs", hsName, "vin", "region", "loc", "typ", "inc", "ttotIn")) %>%
    mutate(value = .data$value * .data$scale) %>%
    select(-"scale")
}
