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
computeBrickShare <- function(variable, data, energyLadder = NULL, energyLadderNo = NULL) {

  # If this is renovation data: Filter for previous heating systems that match the energy ladder
  # and summarise across the previous heating system dimension
  if (variable == "renovation") {
    if (is.null(energyLadder) || is.null(energyLadderNo)) {
      warning("Energy ladder needs to be provided to compute renovation brick share.",
              "The corresponding variable is skipped")
      return(mutate(data, value = NA))
    }
    data <- data %>%
      left_join(energyLadder, by = "hs") %>%
      filter(.data[["energyLadder"]] == energyLadderNo, .data[["hsr"]] != "0") %>%
      group_by(across(-all_of(c("hs", "value")))) %>%
      summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
      select(-"bs") %>%
      rename(bs = "bsr", hs = "hsr")
  }

  brickShare <- data %>%
    group_by(across(-all_of(c("hs", "value")))) %>%
    mutate(totVal = sum(.data[["value"]]),
           shareVal = .data[["value"]] / .data[["totVal"]]) %>%
    ungroup()

  brickShare <- brickShare %>%
    select(-"totVal", -"value") %>%
    rename(value = "shareVal")

}
