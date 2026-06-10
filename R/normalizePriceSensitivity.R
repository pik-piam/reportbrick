#' Compute a normalized price sensitivity from life-cycle costs
#'
#' @param dfLcc data frame containing LCC data
#' @param brickRes data frame with brick results corresponding to the LCC data
#' @param lambda numeric, price sensitivity used for the brick run from which the data comes
#' @param timeDimWeight character, time dimension of the weight \code{brickRes}
#' @param groupCols character, columns to group by when normalizing
#'
#' @author Ricarda Rosemann
#'
normalizePriceSensitivity <- function(dfLcc, brickRes, lambda, timeDimWeight = "ttotIn", groupCols = NULL) {

  brickWeights <- brickRes %>%
    group_by(across(any_of(groupCols))) %>%
    mutate(total = sum(.data[["value"]]),
           weight = .data[["value"]] / .data[["total"]]) %>%
    ungroup() %>%
    rename(ttotIn = timeDimWeight) %>%
    select(-"value", -"total")

  dfLcc %>%
    group_by(across(-all_of(c("costType", "value")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    left_join(brickWeights,
              by = intersect(colnames(dfLcc), colnames(brickWeights))) %>%
    group_by(across(any_of(groupCols))) %>%
    summarise(value = sum(.data[["value"]] * .data[["weight"]] * lambda), .groups = "drop")
}
