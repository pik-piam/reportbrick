#' Aggregate the logit shares across hs
#'
#' @param name description
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% across all_of .data group_by left_join mutate rename
#'   select summarise ungroup
#'
aggregateShare <- function(share, weight = NULL, energyLadder = NULL, energyLadderNo = NULL) {

  baseDims <- setdiff(colnames(share), c("bs", "hs", "bsr", "hsr", "ttot", "value"))

  if (!is.null(energyLadder) && !is.null(energyLadderNo)) {
    share <- share %>%
      left_join(energyLadder, by = "hs") %>%
      filter(.data[["energyLadder"]] == energyLadderNo)
  }

  share <- share %>%
    filter(.data[["hsr"]] != "0") %>%
    replace_na(list(value = 0))

  if (!is.null(weight)) {
    weight <- rename(weight, weightVal = "value")

    share <- share %>%
      left_join(weight, by = c("bs", "hs", baseDims, "ttot")) %>%
      mutate(value = .data[["value"]] * .data[["weightVal"]]) %>%
      select(-"weightVal")
  }

  share %>%
    group_by(across(all_of(c("bs", baseDims, "ttot", "bsr", "hsr")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    select(-"bs") %>%
    rename(bs = "bsr", hs = "hsr") %>%
    group_by(across(all_of(c("bs", baseDims, "ttot")))) %>%
    mutate(totVal = sum(.data[["value"]]),
           shareVal = .data[["value"]] / .data[["totVal"]]) %>%
    ungroup() %>%
    select(-"totVal", -"value") %>%
    rename(value = "shareVal")
}
