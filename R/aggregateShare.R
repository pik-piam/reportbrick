#' Aggregate the logit/brick shares across hs
#'
#' Aggregate hsr shares by applying a weight (typically the total renovation outflow
#' to reconstruct the renovation flow by hsr), summing over hs and then recalculating
#' the share.
#'
#' @param share data frame with heating system shares
#' @param dimAgg character, dimension to aggregate over
#' @param dimShare character, dimension to compute the share over
#' @param weight data frame with weights to scale
#' @param ttotWeight character, name of the ttot column in the weight
#' @param energyLadder data frame containing the map between heating system and energy ladder
#' @param energyLadderNo numeric, energy ladder step to filter for
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% across all_of .data filter group_by left_join mutate rename
#'   select summarise ungroup
#' @importFrom tidyr replace_na
#'
aggregateShare <- function(share, dimAgg, dimShare = "hsr",
                           weight = NULL, ttotWeight = "ttotOut",
                           energyLadder = NULL, energyLadderNo = NULL) {

  # Check if the data also contains absolute values
  absInData <- all(c("absVal", "relVal") %in% colnames(share))

  # Filter for the desired energy ladder steps
  if (!is.null(energyLadder) && !is.null(energyLadderNo)) {
    share <- share %>%
      left_join(energyLadder, by = "hs") %>%
      filter(.data[["energyLadder"]] == energyLadderNo)
  }

  # aggregate absolute value if it exists and remove the absVal column from data
  if (isTRUE(absInData)) {
    aggAbs <- share %>%
      group_by(across(-any_of(c(dimAgg, "absVal", "relVal", "value")))) %>%
      summarise(absVal = sum(.data$absVal))

    share <- share %>%
      select(-"absVal") %>%
      rename(value = "relVal")
  }

  # Remove zero transition and set missing values to zero
  if ("hsr" %in% colnames(share)) {
    share <- share %>%
      filter(.data[["hsr"]] != "0") %>%
      replace_na(list(value = 0))
  }

  # Apply given weights to the share
  if (!is.null(weight)) {
    weight <- rename(weight, weightVal = "value")

    share <- share %>%
      # hs in share is the heating system going out of the stock at time ttotIn
      left_join(weight, by = c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", ttotIn = ttotWeight)) %>%
      mutate(value = .data[["value"]] * .data[["weightVal"]]) %>%
      select(-"weightVal")
  }

  share <- share %>%

    # Sum the shares over dimension dimAgg
    group_by(across(-all_of(c(dimAgg, "value")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%

    # Compute total as sum over dimShare and recalculate the share
    group_by(across(-all_of(c(dimShare, "value")))) %>%
    mutate(totVal = sum(.data[["value"]]),
           shareVal = .data[["value"]] / .data[["totVal"]]) %>%
    ungroup() %>%
    select(-"totVal", -"value") %>%
    rename(value = "shareVal")

  if (isTRUE(absInData)) {
    share <- share %>%
      rename(relVal = "value") %>%
      full_join(aggAbs, by = intersect(colnames(aggAbs), colnames(share)))
  }

  share
}
