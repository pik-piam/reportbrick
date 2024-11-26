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
computeLogitShare <- function(variable, lcc, dims, lambda) {

  # For renovation data with full resolution will need to sum over hsr
  hsName <- "hs"
  if (variable == "renovation" && "hsr" %in% colnames(lcc)) hsName <- "hsr"

  # Compute total lcc
  lcc  %>%
    group_by(across(all_of(c(dims, "ttot")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    mutate(expVal = exp(-lambda * .data[["value"]])) %>%
    group_by(across(-all_of(c(hsName, "expVal", "value")))) %>%
    mutate(totVal = sum(.data[["expVal"]])) %>%
    ungroup() %>%
    mutate(value = .data[["expVal"]] / .data[["totVal"]]) %>%
    select(-"expVal", -"totVal")
}
