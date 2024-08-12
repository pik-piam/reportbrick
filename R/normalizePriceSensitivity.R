#' Compute a normalized price sensitivity from life-cycle costs
#'
#' @param
#'
#' @author Ricarda Rosemann
#'
normalizePriceSensitivity <- function(dfLcc, brickRes, lambda, dims, groupCols = NULL) {

  # browser()

  brickWeights <- brickRes %>%
    group_by(across(any_of(groupCols))) %>%
    mutate(total = sum(.data[["value"]]),
           weight = .data[["value"]] / .data[["total"]]) %>%
    ungroup()
  brickWeights <- brickWeights %>%
    select(-"value", -"total")

  weightedAvg <- dfLcc %>%
    group_by(across(-all_of(c("costType", "value")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop")
  weightedAvg <- weightedAvg %>%
    left_join(brickWeights,
              by = c(dims, "ttot"))
  weightedAvg <- weightedAvg %>%
    group_by(across(any_of(groupCols))) %>%
    summarise(value = sum(.data[["value"]] * .data[["weight"]] * lambda), .groups = "drop")

  return(weightedAvg)

}
