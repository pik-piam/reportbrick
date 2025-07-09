#' Compute heating system shares from logit model
#'
#' Based on the lifecycle costs, compute the shares we would expect for each heating system
#' if this was simply a logit model
#'
#' @author Ricarda Rosemann
#'
#' @param variable character, flow variable for which the share is computed
#' @param lcc data frame, lifecycle costs
#' @param lambda numeric, price sensitivity
#'
#' @importFrom dplyr %>% across all_of .data filter group_by left_join mutate
#'   select summarise ungroup
#'
computeLogitShare <- function(variable, lcc, lambda) {

  # For renovation data with full resolution we will need to sum over hsr
  hsName <- if (variable == "renovation" && "hsr" %in% colnames(lcc)) "hsr" else "hs"

  # Compute logit shares from LCC and the price sensitivity lambda
  lcc  %>%
    
    # Sum over all cost types to obtain total LCC
    group_by(across(-all_of(c("costType", "value")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    
    # Compute the logit share
    mutate(expoVal = exp(-lambda * .data[["value"]])) %>%
    group_by(across(-all_of(c(hsName, "expoVal", "value")))) %>%
    mutate(totVal = sum(.data[["expoVal"]])) %>%
    ungroup() %>%
    mutate(value = .data[["expoVal"]] / .data[["totVal"]]) %>%
    select(-"expoVal", -"totVal")
}
