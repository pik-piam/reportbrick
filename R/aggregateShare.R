#' Aggregate the logit/brick shares across hs
#'
#' Aggregate hsr shares by applying a weight (typically the total renovation flow
#' to reconstruct the renovation flow by hsr), summing over hs and then recalculating
#' the share.
#'
#' @param share data frame with heating system shares
#' @param weight data frame with weights to scale
#' @param energyLadder data frame containing the map between heating system and energy ladder
#' @param ernegyLadderNo numeric, energy ladder step to filter for
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% across all_of .data filter group_by left_join mutate rename
#'   replace_na select summarise ungroup
#'
aggregateShare <- function(share, weight = NULL, energyLadder = NULL, energyLadderNo = NULL) {

  baseDims <- setdiff(colnames(share), c("bs", "hs", "bsr", "hsr", "ttotIn", "value"))

  # Filter for the desired energy ladder steps
  if (!is.null(energyLadder) && !is.null(energyLadderNo)) {
    share <- share %>%
      left_join(energyLadder, by = "hs") %>%
      filter(.data[["energyLadder"]] == energyLadderNo)
  }

  # Remove zero transition and set missing values to zero
  share <- share %>%
    filter(.data[["hsr"]] != "0") %>%
    replace_na(list(value = 0))

  # Apply given weights to the share
  if (!is.null(weight)) {
    weight <- rename(weight, weightVal = "value")

    share <- share %>%
      left_join(weight, by = c("bs", "hs", baseDims, "ttotIn")) %>%
      mutate(value = .data[["value"]] * .data[["weightVal"]]) %>%
      select(-"weightVal")
  }

  share %>%
    
    # Sum the shares over hs
    group_by(across(all_of(c("bs", baseDims, "ttotIn", "bsr", "hsr")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    select(-"bs") %>%
    rename(bs = "bsr", hs = "hsr") %>%
    
    # Compute total as sum over (bsr, hsr) and recalculate the share
    group_by(across(all_of(c("bs", baseDims, "ttotIn")))) %>%
    mutate(totVal = sum(.data[["value"]]),
           shareVal = .data[["value"]] / .data[["totVal"]]) %>%
    ungroup() %>%
    select(-"totVal", -"value") %>%
    rename(value = "shareVal")
}
