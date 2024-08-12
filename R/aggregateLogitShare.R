#' Aggregate the logit shares across hs
#'
#' @param name description
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% across all_of .data group_by left_join mutate rename
#'   select summarise ungroup
#'
aggregateLogitShare <- function(logitShare, renovationIn) {

  baseDims <- setdiff(intersect(colnames(logitShare), colnames(renovationIn)),
                      c("bs", "hs", "bsr", "hsr", "ttot", "value"))

  renovationIn <- rename(renovationIn, renVal = "value")

  logitShare <- logitShare %>%
    left_join(renovationIn, by = c("bs", "hs", baseDims, "ttot")) %>%
    mutate(logitRen = .data[["value"]] * .data[["renVal"]])
  logitShare <- logitShare %>%
    group_by(across(all_of(c("bs", baseDims, "ttot", "bsr", "hsr")))) %>%
    summarise(value = sum(.data[["logitRen"]]), .groups = "drop")
  logitShare <- logitShare %>%
    select(-"bs") %>%
    rename(bs = "bsr", hs = "hsr") %>%
    group_by(across(all_of(c("bs", baseDims, "ttot")))) %>%
    mutate(totVal = sum(.data[["value"]]),
           shareVal = .data[["value"]] / .data[["totVal"]]) %>%
    ungroup()

  logitShare <- logitShare %>%
    select(-"totVal", -"value") %>%
    rename(value = "shareVal")

}
