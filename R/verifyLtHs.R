#' Verify the lifetime constraint of heating systems
#'
#' @author Ricarda Rosemann
#'
#' @param gdx character, path to input gdx file
#' @param inflow data frame, all inflows
#' @param v_stockInit data frame, brick results on initial stock
#' @param outflow data frame, all outflows
#' @param dims character, dimensions of the data without time periods
#' @param standingLtStock numeric, assumed prior standing lifetime of the initial stock
#'
#' @importFrom dplyr %>% across all_of .data filter group_by left_join mutate
#'   rename select summarise
#' @importFrom tidyr crossing pivot_longer replace_na
#'
verifyLtHs <- function(gdx, inflow, v_stockInit, outflow, lifeTimeHs, dims, standingLtStock = 6) {

  p_shareRenHSinit <- .prepareShare(readGdxSymbol(gdx, "p_shareRenHSinit", asMagpie = FALSE))
  p_shareRenHS <- .prepareShare(readGdxSymbol(gdx, "p_shareRenHs", asMagpie = FALSE))

  # Compute the initial stock that needs to be demolished
  stockInit <- v_stockInit %>%
    rename(stockVal = "value") %>%
    left_join(p_shareRenHSinit %>%
                rename(shareVal = .data[["value"]]),
              by = c("hs", "reg", "typ", "ttot")) %>%
    mutate(initVal = .data[["stockVal"]] * .data[["shareVal"]]) %>%
    select(-"ttot", -"shareVal", -"stockVal")

  # Compute the left-hand side of the inequality as the sum over ttot of the outflows
  # Compute the sum of over ttot of the inflows weighted by the shares
  ltIneq <- outflow %>%
    rename(outVal = "value") %>%
    left_join(inflow %>%
                rename(inVal = "value"),
              by = c(dims, "ttot", "dt")) %>%
    crossing(ttot2 = unique(.data[["ttot"]])[-1]) %>%
    filter(.data[["ttot2"]] >= .data[["ttot"]]) %>%
    left_join(p_shareRenHS %>%
                rename(shareVal = "value"),
              by = c(intersect(dims, colnames(p_shareRenHS)), "ttot", "ttot2"))
  ltIneq <- ltIneq %>%
    group_by(across(all_of(c(dims, "ttot2")))) %>%
    summarise(lhsLtIneq = sum(.data[["outVal"]]),
              inSum = sum(.data[["inVal"]] * .data[["shareVal"]]), .groups = "drop")

  # Compute the right-hand side from the weighted sum of the inflows and the weighter initial stock
  # Check if the inequality holds and is binding.
  ltIneq <- ltIneq %>%
    left_join(stockInit, by = c(dims, "ttot2")) %>%
    replace_na(list(initVal = 0)) %>%
    mutate(rhsLtIneq = .data[["inSum"]] + .data[["initVal"]]) %>%
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

  out <- stats::setNames(
    lapply(c("lhsLtIneq", "rhsLtIneq"), function(v) {
      ltIneq %>%
        filter(.data[["variable"]] == v) %>%
        select(-"variable")
    }),
    c("lhsLtIneq", "rhsLtIneq")
  )


}

#' Adjust the column names to obtain proper numbering of duplicates
#' and convert time period columns to factors
#'
#' This is targeted at the shares read from gdx
#'
#' @param df data frame for which the adjustment of column names and time periods should be done
#'
#' @importFrom dplyr %>% .data mutate
#'
.prepareShare <- function(df) {

  .removeColNumbering(df, offset = 1) %>%
    mutate(ttot = as.numeric(levels(.data[["ttot"]]))[.data[["ttot"]]],
           ttot2 = as.numeric(levels(.data[["ttot2"]]))[.data[["ttot2"]]])

}

#' Remove column numbering and renumber duplicates
#'
#' Converts data with duplicates, for which all columns are numbered when reading from gdx,
#' to a state where only the duplicates are numbered.
#'
#' @param df data frame for which the columns should be renamed
#' @param offset numeric, number by which the renumbering is shifted
#' @param suffix character that is added the renumbered column name before the number
#'
.removeColNumbering <- function(df, offset = 0, suffix = "") {

  colnamesUnnmbd <- sub("\\_\\d{1-3}$", "", colnames(df))

  duplicates <- which(duplicated(colnamesUnnmbd))
  for (i in seq_along(duplicates)) {
    colnamesUnnmbd[duplicates[i]] <- paste0(colnamesUnnmbd[duplicates[i]], suffix, offset + i)
  }

  colnames(df) <- colnamesUnnmbd

  return(df)
}
