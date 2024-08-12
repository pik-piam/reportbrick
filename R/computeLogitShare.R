#' Compute heating system shares from logit model
#'
#' Based on the lifecycle costs, compute the shares we would expect for each heating system
#' if this was simply a logit model
#'
#' @author Ricarda Rosemann
#'
#' @param variable character, flow variable for which the share is computed
#' @param lcc data frame, lifecycle costs
#' @param dims character, dimensions of the lcc data, without time peridos
#' @param lambda numeric, price sensitivity
#' @param energyLadder data frame, mapping between heating systems and energy ladder position
#'   (only required for renovation flows)
#' @param energyLadderNo numeric, maximum energy ladder position to be included in the share
#'   (only required for renovation flows)
#'
#' @importFrom dplyr %>% across all_of .data filter group_by left_join mutate
#'   select summarise ungroup
#'
computeLogitShare <- function(variable, lcc, dims, lambda,
                              energyLadder = NULL, energyLadderNo = NULL) {

  hsName <- "hs"

  # For renovation flows: Filter for desired energy ladder positions
  if (variable == "renovation") {
    if (is.null(energyLadder) || is.null(energyLadderNo)) {
      warning("Energy ladder needs to be provided to compute renovation logit share.",
              "The corresponding variable is skipped")
      return(mutate(lcc, value = NA)) #TODO: Null might be better here to not overload the output with meaningless data
    }
    if ("hsr" %in% colnames(lcc)) {
      hsName <- "hsr"
      lcc <- lcc %>%
        left_join(energyLadder, by = "hs") %>%
        filter(.data[["energyLadder"]] == energyLadderNo)
    } else {
      lcc <- lcc %>%
        left_join(energyLadder, by = hsName) %>%
        filter(.data[["energyLadder"]] <= energyLadderNo)
    }
  }

  # Compute total lcc
  lccSum <- lcc  %>%
    group_by(across(all_of(c(dims, "ttot")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop")

  logitShare <- lccSum %>%
    mutate(expVal = exp(-lambda * .data[["value"]])) %>%
    group_by(across(-all_of(c(hsName, "expVal", "value")))) %>%
    mutate(totVal = sum(.data[["expVal"]])) %>%
    ungroup() %>%
    mutate(value = .data[["expVal"]] / .data[["totVal"]])

  logitShare <- logitShare %>%
    select(-"expVal", -"totVal")

  return(logitShare)

}
