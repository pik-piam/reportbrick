#' Verify the lifetime constraint of heating systems
#'
#' @author Ricarda Rosemann
#'
#' @param inflow data frame, all inflows
#' @param v_stockInit data frame, brick results on initial stock
#' @param outflow data frame, all outflows
#' @param p_shareRenHSinit data frame with shares to be renovated for the initial stock
#' @param p_shareRenHS data frame with shares to be renovated for the inflow
#' @param dims character, dimensions of the data without time periods
#'
#' @importFrom dplyr %>% across all_of .data filter group_by left_join mutate
#'   rename select summarise
#' @importFrom tidyr crossing pivot_longer replace_na
#'
verifyLtHs <- function(inflow, v_stockInit, outflow, p_shareRenHSinit, p_shareRenHS, dims) {

  # Compute the initial stock that needs to be demolished
  stockInit <- v_stockInit %>%
    rename(stockVal = "value") %>%
    left_join(p_shareRenHSinit %>%
                rename(shareVal = .data[["value"]]),
              by = c("hs", "region", "typ", "ttotIn")) %>%
    mutate(initVal = .data[["stockVal"]] * .data[["shareVal"]]) %>%
    select(-"ttotIn", -"shareVal", -"stockVal")


  # Compute the left-hand side of the inequality as the sum over ttot of the outflows
  # Compute the sum of over ttot of the inflows weighted by the shares
  ltIneq <- outflow %>%
    rename(outVal = "value", ttotSum = "ttotOut") %>%
    left_join(inflow %>%
                rename(inVal = "value"),
              by = c(dims, ttotSum = "ttotIn")) %>%
    crossing(ttotOut = unique(.data[["ttotSum"]])[-1]) %>%
    filter(.data[["ttotOut"]] >= .data[["ttotSum"]]) %>%
    left_join(p_shareRenHS %>%
                rename(shareVal = "value"),
              by = c(intersect(dims, colnames(p_shareRenHS)), ttotSum = "ttotIn", "ttotOut"))
  ltIneq <- ltIneq %>%
    group_by(across(all_of(c(dims, "ttotOut")))) %>%
    summarise(inSum = sum(.data[["inVal"]] * .data[["shareVal"]]),
              lhsLtIneq = sum(.data$outVal),
              .groups = "drop") %>%
    left_join(stockInit, by = c(dims, "ttotOut")) %>%
    replace_na(list(initVal = 0)) %>%
    mutate(rhsLtIneq = .data[["inSum"]] + .data[["initVal"]])

  # Compute the right-hand side from the weighted sum of the inflows and the weighter initial stock
  # Check if the inequality holds and is binding.
  ltIneq <- ltIneq %>%
    mutate(ineqHolds = .data[["lhsLtIneq"]] + 1E-6 >= .data[["rhsLtIneq"]],
           ineqBinding = abs(.data[["lhsLtIneq"]] - .data[["rhsLtIneq"]]) <= 1E-6) %>%
    select(-"initVal", -"inSum")

  if (!all(ltIneq[["ineqHolds"]])) {
    message("The lifetime inequality is not satisfied for at least one instance.")
  }

  ltIneq <- ltIneq %>%
    select(-"ineqHolds", -"ineqBinding") %>%
    pivot_longer(cols = all_of(c("lhsLtIneq", "rhsLtIneq")),
                 names_to = "variable", values_to = "value")

  stats::setNames(
    lapply(c("lhsLtIneq", "rhsLtIneq"), function(v) {
      ltIneq %>%
        filter(.data[["variable"]] == v) %>%
        select(-"variable")
    }),
    c("lhsLtIneq", "rhsLtIneq")
  )


}
