#' Compute mixed lifetimes of heating systems
#'
#' Determine heating system lifetimes such that they roughly follow a Weibull distribution,
#' but match Brick outputs.
#'
#' @author Ricarda Rosemann
#'
#' @param ltAnte data frame, ex-ante lifetimes
#' @param outflow data frame, brick outflows
#' @param brickRes named list of data frames, brick output data on stocks and flows
#' @param conShare data frame, share of constructions in total inflows
#' @param ttotNum numeric, all time periods
#' @param dims character, dimensions of the data without time periods
#'
#' @importFrom dplyr %>% across all_of filter full_join group_by left_join mutate
#'   rename select summarise
#' @importFrom tidyr replace_na
#'
computeLtMixed <- function(ltAnte, outflow, brickRes, conShare, ttotNum, dims) {

  t0 <- ttotNum[1]
  tRun <- setdiff(ttotNum, t0)

  renShare <- mutate(conShare, share = 1 - .data[["share"]])

  outAnte <- .prepareLtAnte(ltAnte[["stock"]], c(stock = "absVal")) %>%
    full_join(.prepareLtAnte(ltAnte[["construction"]], c(con = "absVal"), sumTtot = TRUE), by = c(dims, "ttot2")) %>%
    full_join(.prepareLtAnte(ltAnte[["renovation"]], c(ren = "absVal"), sumTtot = TRUE), by = c(dims, "ttot2")) %>%
    replace_na(list(stock = 0, con = 0)) %>%
    mutate(value = .data[["stock"]] + .data[["con"]] + .data[["ren"]]) %>%
    select(-"stock", -"con", -"ren")

  outAnteSum <- outAnte %>%
    group_by(across(all_of(dims))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop")

  outModelSum <- outflow %>%
    group_by(across(all_of(dims))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop")

  outDiff <- outAnte %>%
    rename(valAnte = "value") %>%
    left_join(outflow %>%
                rename(valModel = "value"),
              by = c(dims, ttot2 = "ttot")) %>%
    mutate(value = .data[["valAnte"]] - .data[["valModel"]])

  outDiffSum <- outAnteSum %>%
    rename(valAnte = "value") %>%
    left_join(outModelSum %>%
                rename(valModel = "value"),
              by = c(dims)) %>%
    mutate(value = .data[["valAnte"]] - .data[["valModel"]])

  stockInitLtDirect <- .computeLtDirect(ltAnte[["stock"]], outAnte, outflow, dims)
  conLtDirect <- .computeLtDirect(ltAnte[["construction"]], outAnte, outflow, dims)
  renLtDirect <- .computeLtDirect(ltAnte[["renovation"]], outAnte, outflow, dims)

  # #Test (temporary)
  # conTest <- conLtDirect %>%
  #   rename(directVal = "value") %>%
  #   left_join(brickRes[["construction"]] %>%
  #               rename(totVal = "value")) %>%
  #   left_join(ltAnte[["construction"]] %>%
  #               rename(anteVal = "absVal")) %>%
  #   mutate(totShare = .data[["directVal"]] / .data[["totVal"]])

  # Compute leave times of initial stock and inflows
  # add results column to all DFs and fill with zero
  stockInitLtMixed <- .prepareLtMixed(stockInitLtDirect)
  conLtMixed <- .prepareLtMixed(conLtDirect)
  renLtMixed <- .prepareLtMixed(renLtDirect)

  for (t2 in tRun) {
    # Compute unattributed outflow
    stockInitLtMixed <- .computeUnattrOut(stockInitLtMixed, brickRes[["stock"]], t2, dims)
    conLtMixed <- .computeUnattrOut(conLtMixed, brickRes[["construction"]], t2, setdiff(dims, "vin"))
    renLtMixed <- .computeUnattrOut(renLtMixed, select(brickRes[["renovation"]], -"dt"), t2, dims)

    unattrTot <- rbind(
      stockInitLtMixed %>%
        mutate(variable = "stock"),
      conLtMixed %>%
        mutate(variable = "con"),
      renLtMixed %>%
        mutate(variable = "ren")
    ) %>%
      select("variable", dims, "ttot", "ttot2", "remInflow", "unattr") %>%
      filter(.data[["ttot2"]] == t2)
    unattrTot <- unattrTot %>%
      group_by(across(any_of(c(dims, "ttot2")))) %>%
      summarise(sumUnattr = sum(.data[["unattr"]]))

    # compute initial stock lt and save to new column
    stockThis <- .computeLtMixed(stockInitLtMixed, unattrTot, t0, t2, dims)
    stockInitLtMixed <- .addResultsMixed(stockInitLtMixed, stockThis, t0, t2, dims, addDims = c("directVal", "remInflow", "unattr"))

    for (t1 in tRun[tRun <= t2]) {
      # Compute lifetimes for construction and renovation flow
      conThis <- .computeLtMixed(conLtMixed, unattrTot, t1, t2, dims, dfStock = stockThis, dfShare = conShare)
      renThis <- .computeLtMixed(renLtMixed, unattrTot, t1, t2, dims, dfStock = stockThis, dfShare = renShare)

      conLtMixed <- .addResultsMixed(conLtMixed, .combineAddAttr(conThis, renThis, dims), t1, t2, dims,
                                     addDims = c("directVal", "remInflow", "unattr"))
      renLtMixed <- .addResultsMixed(renLtMixed, .combineAddAttr(renThis, conThis, dims), t1, t2, dims,
                                     addDims = c("directVal", "remInflow", "unattr"))
    }
  }

  out <- list()
  out[["stockInitLtMixed"]] <- .processLtMixed(stockInitLtMixed, brickRes[["stock"]], ltAnte[["stock"]], dims)
  out[["conLtMixed"]] <- .processLtMixed(conLtMixed, brickRes[["construction"]], ltAnte[["construction"]], dims, removeVin = TRUE)
  out[["renLtMixed"]] <- .processLtMixed(renLtMixed, brickRes[["renovation"]], ltAnte[["renovation"]], dims)

  return(out)
}

#' Prepare ex-ante lifetime data to be used in mixed lifetime computation
#'
#' Remove not required columns, rename columns and sum over ttot as indicated
#'
#' @param df data frame, lifetime data to manipulate
#' @param toRename named character, old (value) and new name (name) of columns to be renamed
#' @param sumTtot logical, whether to sum over ttot
#'
#' @importFrom dplyr %>% across all_of any_of .data group_by rename select summarise
#'
.prepareLtAnte <- function(df, toRename, sumTtot = FALSE) {

  df <- df %>%
    select(-"relVal")

  if (isTRUE(sumTtot)) {
    df <- df %>%
      group_by(across(-all_of(c("ttot", "absVal")))) %>%
      summarise(absVal = sum(.data[["absVal"]]), .groups = "drop")
  }

  df <- df %>%
    select(-any_of("ttot")) %>%
    rename(toRename)
}

#' Compute direct lifetime estimate
#'
#' Compute the lifetime as the ex-ante estimate scaled by the ratio of ex-ante
#' and actual total outflows
#'
#' @param dfLtAnte data frame, ex-ante lifetimes
#' @param outAnte data frame, total ex-ante outflows
#' @param outflow data frame, total actual brick outflows
#' @param dims character, dimensions of the data without time periods
#'
#' @importFrom dplyr %>% .data left_join mutate rename select
#'
.computeLtDirect <- function(dfLtAnte, outAnte, outflow, dims) {

  dfLtAnte %>%
    select(-"relVal") %>%
    left_join(outAnte %>%
                rename(outAnte = "value"),
              by = c(dims, "ttot2")) %>%
    left_join(outflow %>%
                select(-"dt") %>%
                rename(outflow = "value", ttot2 = "ttot"),
              by = c(dims, "ttot2")) %>%
    mutate(value = ifelse(.data[["absVal"]] == 0, #Only then should outAnte be zero
                          0,
                          .data[["absVal"]] * .data[["outflow"]] / .data[["outAnte"]])) %>%
    select(-"absVal", -"outflow", -"outAnte")


}

#' Rename direct lifetime estimate and add zero columns
#'
#' Zero columns are required to be included in summations later
#'
#' @param dfLtDirect data frame, direct lifetime estimates
#'
#' @importFrom dplyr %>% mutate rename
.prepareLtMixed <- function(dfLtDirect) {

  dfLtDirect %>%
    rename(directVal = "value") %>%
    mutate(addAttr = 0, value = 0)
}

#' Compute the unattributed outflow
#'
#' Compute the amount of the brick outflow that has not been attributed to an
#' inflow by the direct lifetime estimate when also reflecting what remains of
#' the inflow at the current time period.
#'
#' @param dfLtMixed data frame, direct lifetime estimates of current time period and
#'   mixed lifetime estimates of previous time periods
#' @param dfTotVal data frame, inflows in brick results
#' @param tOut numeric, time period of removal for which we are computing the
#'   unattributed outflows
#' @param dims character, dimensions of the data without time periods
#'
#' @importFrom dplyr %>% across any_of .data filter group_by left_join mutate
#'   rename right_join select summarise
#'
.computeUnattrOut <- function(dfLtMixed, dfTotVal, tOut, dims) {

  # Compute the sum of previous outflows by the mixed estimate
  dfLtMixedTmp <- dfLtMixed %>%
    filter(.data[["ttot2"]] <= tOut) %>%
    group_by(across(any_of(c(dims, "ttot")))) %>%
    summarise(sumVal = sum(.data[["value"]]), .groups = "drop")
  # Compute the remaining inflow from the total inflow and the previous outflows
  # attributed to this inflow.
  # Compute the unattributed outflow as the portion of the direct estimate that
  # cannot be covered by the remaining inflow.
  dfLtMixed <- dfLtMixedTmp %>%
    dplyr::right_join(dfLtMixed, by = c(dims, "ttot")) %>%
    left_join(dfTotVal %>%
                rename(totVal = "value"),
              by = c(dims, "ttot")) %>%
    mutate(remInflow =  .data[["totVal"]] - .data[["sumVal"]],
           unattr = pmax(.data[["directVal"]] - .data[["remInflow"]], 0))
  dfLtMixed <- dfLtMixed %>%
    select(-"totVal", -"sumVal")

}

#' Compute the mixed lifetime estimate
#'
#' The mixed estimate is computed as the sum of the direct estimate and the total
#' unattributed outflow of the current time period, unless this surpasses the
#' remaining inflow. In the latter case the mixed lifetime estimate is given by
#' the total remaining inflow.
#'
#' @param dfLtMixed data frame, mixed lifetime estimates of previous time steps,
#'   direct estimates and remaining inflows of the current time step
#' @param unattrTot data frame, total unattributed outflow
#' @param tIn numeric, current time period of installation / inflow
#' @param tOut numeric, current time period of removal / outflow
#' @param dims, character, dimensions of the data without time periods
#' @param dfStock data frame, mixed estimates and additional attribution of the initial stock
#'   (only required for flow computations)
#' @param dfShare data frame, share of the current flow in total inflows
#'   (only required for flow computations)
#'
#' @importFrom dplyr %>% across all_of any_of .data filter group_by left_join mutate
#'   rename select summarise
#' @importFrom tidyr replace_na
#'
.computeLtMixed <- function(dfLtMixed, unattrTot, tIn, tOut, dims, dfStock = NULL, dfShare = NULL) {

  dfLtMixed <- filter(dfLtMixed, .data[["ttot2"]] == tOut)

  if (!is.null(dfStock)) {

    # Isolate additional attribution in stock data frame
    dfStock <- dfStock %>%
      rename(stockAttr = "addAttr") %>%
      select(all_of(c(dims, "ttot2", "stockAttr")))

    # Sum over additional attributions to previous inflows
    dfLtMixed <- dfLtMixed %>%
      filter(.data[["ttot"]] <= tIn) %>%
      group_by(across(any_of(c(dims, "ttot2")))) %>%
      summarise(addAttrSum = sum(.data[["addAttr"]]), .groups = "drop") %>%
      mutate(ttot = tIn) %>%
      left_join(dfLtMixed, by = c(dims, "ttot", "ttot2")) %>%
      left_join(dfStock, by = c(dims, "ttot2")) %>%
      replace_na(list(stockAttr = 0)) %>%
      left_join(dfShare, by = c(dims, "ttot"))

  }

  # Compute the maximum possible outflow to be attributed to the current in- and out time periods
  # as the sum of the direct estimate and the appropriate share of the still unattributed outflow.
  # Set the final mixed estimate to this maximum possible outflow, unless this surpasses the
  # remaining inflow. In that case, set the mixed estimate to the remaining inflow.
  dfLtMixed <- dfLtMixed %>%
    left_join(unattrTot, by = c(dims, "ttot2")) %>%
    mutate(maxOutflow = if (is.null(dfStock)) .data[["directVal"]] + .data[["sumUnattr"]]
           else .data[["directVal"]] + .data[["share"]] * (.data[["sumUnattr"]] - .data[["stockAttr"]] - .data[["addAttrSum"]]),
           value = pmin(.data[["maxOutflow"]], .data[["remInflow"]]),
           addAttr = pmax(.data[["value"]] - .data[["directVal"]], 0)) %>%
    select(-any_of(c("maxOutflow", "sumUnattr", "addAttrSum", "stockAttr", "share")))

  return(dfLtMixed)

}

#' Add the mixed estimate results of a specific in and a specific out time period
#' to the data frame storing all mixed estimates
#'
#' @param dfOrig data frame, contains all previous mixed estimates
#' @param dfNew data frame, newly computed mixed estimates
#' @param tIn numeric, time period of installation / inflow of the newly computed mixed estimates
#' @param tOut numeric, time period of removal / outflow of the newly computed mixed estimates
#' @param dims character, dimensions of the data without time periods
#' @param addDims character, additional dimensions present in the data
#'
#' @importFrom dplyr %>% .data left_join mutate select
#'
.addResultsMixed <- function(dfOrig, dfNew, tIn, tOut, dims, addDims = NULL) {

  dfOrig %>%
    left_join(dfNew, by = c(dims, "ttot", "ttot2", addDims)) %>%
    mutate(value = ifelse(
      .data[["ttot"]] == tIn & .data[["ttot2"]] == tOut,
      .data[["value.y"]],
      .data[["value.x"]]
    ),
    addAttr = ifelse(
      .data[["ttot"]] == tIn & .data[["ttot2"]] == tOut,
      .data[["addAttr.y"]],
      .data[["addAttr.x"]]
    )) %>%
    select(-"value.x", -"value.y", -"addAttr.x", -"addAttr.y")

}

#' Add the additional attributions of two data frames
#'
#' This is required to join the additional attributions of construction and
#' renovation flows.
#'
#' @param dfThis data frame, contains a column \code{addAttr} with the additional attributions
#' @param dfOther data frame, contains a column \code{addAttr} with the additional attributions
#' @param dims character, dimensions of the data without the time periods
#'
#' @importFrom dplyr %>% any_of .data left_join mutate rename select
#' @importFrom tidyr replace_na
#'
.combineAddAttr <- function(dfThis, dfOther, dims) {

  #TODO: I get NAs if adding con values to ren, and I omit other vintages if adding ren values to con -> but this is not a problem as I don't compute these vintages
  # Rename addAttr column
  dfOther <- dfOther %>%
    rename(addAttrOther = "addAttr") %>%
    select(any_of(c(dims, "ttot", "ttot2", "addAttrOther")))

  dfThis %>%
    left_join(dfOther, by = c(dims, "ttot", "ttot2")) %>%
    replace_na(list(addAttrOther = 0)) %>%
    mutate(addAttr = .data[["addAttr"]] + .data[["addAttrOther"]]) %>%
    select(-"addAttrOther")

}

#' Compute the relative value of the lifetime estimates
#'
#' By setting the mixed estimates in relation to the total inflows, compute the
#' probabilities of removal in a given time period.
#' If both the total inflow and the absolute lifetime estimate are close to zero,
#' set this to the ex-ante value.
#'
#' @param dfLt data frame, mixed lifetime estimates
#' @param dfVal data frame, total inflows as given by brick results
#' @param dfAnte data frame, ex-ante lifetime estimates
#' @param dims character, dimensions of the data without time periods
#' @param removeVin logical, whether to remove the vintage column from the dimensions
#'
#' @importFrom dplyr %>% any_of .data left_join mutate rename select
#'
.processLtMixed <- function(dfLt, dfVal, dfAnte, dims, removeVin = FALSE) {

  if (isTRUE(removeVin)) dims <- setdiff(dims, "vin")

  dfLt <- dfLt %>%
    select(-any_of(c("directVal", "remInflow", "unattr", "addAttr"))) %>%
    rename(absVal = "value") %>%
    left_join(dfVal, by = c(dims, "ttot")) %>%
    left_join(dfAnte %>%
                select(-"absVal") %>%
                rename(anteVal = "relVal"),
              by = c(union(dims, "vin"), "ttot", "ttot2")) %>%
    mutate(relVal = ifelse(.data[["absVal"]] <= 1E-9 & .data[["value"]] <= 1E-9, # TODO: Check on this heuristic, find a meaningful way to handle this!
                           .data[["anteVal"]],
                           .data[["absVal"]] / .data[["value"]]))
  dfLt <- dfLt %>%
    select(-any_of(c("dt", "value", "anteVal")))

  return(dfLt)

}
