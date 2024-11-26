#' Estimate Brick model heating system shares
#'
#' @author Ricarda Rosemann
#'
#' @param variable character, Brick variable that is evaluated
#' @param data data frame, Brick stock and flow data
#' @param energyLadder data frame, mapping of heating systems and energy ladder position
#' @param energyLadderNo numeric, energy ladder position to be considered in the share
#'
#' @importFrom dplyr %>% across all_of .data filter group_by left_join mutate
#'   rename select summarise ungroup
#'
computeBrickShare <- function(variable, data) {

  # If this is renovation data: Need to sum over hsr and remove zero renovation
  if (variable == "renovation") {
    hsName <- "hsr"
    data <- filter(data, .data[["hsr"]] != "0")
  } else {
    hsName <- "hs"
  }

  data %>%
    group_by(across(-all_of(c(hsName, "value")))) %>%
    mutate(totVal = sum(.data[["value"]]),
           shareVal = .data[["value"]] / .data[["totVal"]]) %>%
    ungroup() %>%
    select(-"totVal", -"value") %>%
    rename(value = "shareVal")
}
