#' Compute estimated lifetimes of heating systems
#'
#' Determine heating system lifetimes such that they roughly follow a Weibull distribution,
#' but match Brick outputs.
#'
#' @author Ricarda Rosemann
#'
#' @param ltMax data frame, maximum lifetimes computed ex-ante from Weibull distribution
#' @param outflow data frame, brick outflows
#' @param brickRes named list of data frames, brick output data on stocks and flows
#' @param conShare data frame, share of constructions in total inflows
#' @param ttotNum numeric, all time periods
#'
#' @importFrom dplyr %>% across all_of filter full_join group_by left_join mutate
#'   rename select summarise
#' @importFrom tidyr pivot_wider replace_na
#'
computeLt <- function(ltMax, outflow, brickRes, conShare, ttotNum) {

  subs <- c("region", "loc", "typ", "inc")

  t0 <- ttotNum[1]
  tRun <- setdiff(ttotNum, t0)

  renShare <- mutate(conShare, share = 1 - .data[["share"]])

  # Compute maximum outflows, i.e. according to the Weibull distribution
  outMax <- do.call(rbind, lapply(names(ltMax), function(nm) {
    .prepareLtMax(ltMax[[nm]], sumTtot = nm %in% c("construction", "renovation")) %>%
      mutate(variable = nm)
  })) %>%
    pivot_wider(names_from = "variable", values_from = "absVal") %>%
    replace_na(list(stock = 0, construction = 0)) %>%
    mutate(value = .data[["stock"]] + .data[["construction"]] + .data[["renovation"]]) %>%
    select(-"stock", -"construction", -"renovation")

  # Compute direct lifetime estimate as the ex-ante lifetime,
  # scaled by the ratio between ex-ante and model outflow
  ltDirect <- lapply(ltMax, function(dfLt) .computeLtDirect(dfLt, outMax, outflow))

  # add results columns "addAttr" and "value" to all DFs and fill with zero
  ltFinal <- lapply(ltDirect, .adjustLtDirect)

  for (tOut in tRun) {
    # Compute inflow that remains after previous time periods, compute unattributed outflow
    unattrOut <- stats::setNames(lapply(names(ltFinal), function(nm) {
      .computeUnattrOut(ltFinal[[nm]], brickRes[[nm]], tOut)
    }), names(ltFinal))

    # Compute total of unattributed outflow of stock and flows
    unattrTot <- do.call(rbind, lapply(names(unattrOut), function(nm) {
      mutate(unattrOut[[nm]], variable = nm)
    })) %>%
      select("variable", "qty", "bs", "hs", "vin", subs, "ttotIn", "ttotOut", "remInflow", "unattr") %>%
      filter(.data[["ttotOut"]] == tOut) %>%
      group_by(across(-any_of(c("variable", "ttotIn", "remInflow", "unattr")))) %>%
      summarise(sumUnattr = sum(.data[["unattr"]]), .groups = "drop")

    # compute initial stock lt and save to new column
    stockThis <- .computeLtFinal(ltFinal[["stock"]], unattrOut[["stock"]], unattrTot, t0, tOut)
    ltFinal[["stock"]] <- .addResults(ltFinal[["stock"]], stockThis, t0, tOut)

    for (tIn in tRun[tRun <= tOut]) {
      # Compute lifetimes for construction and renovation flow
      conThis <- .computeLtFinal(ltFinal[["construction"]], unattrOut[["construction"]],
                                 unattrTot, tIn, tOut, dfStock = stockThis, dfShare = conShare)
      renThis <- .computeLtFinal(ltFinal[["renovation"]], unattrOut[["renovation"]],
                                 unattrTot, tIn, tOut, dfStock = stockThis, dfShare = renShare)

      # Save results in lt data frame, save combined additional attributions of construction and renovation
      ltFinal[["construction"]] <- .addResults(ltFinal[["construction"]],
                                               .combineAddAttr(conThis, renThis),
                                               tIn, tOut)
      ltFinal[["renovation"]] <- .addResults(ltFinal[["renovation"]],
                                             .combineAddAttr(renThis, conThis),
                                             tIn, tOut)
    }
  }

  stats::setNames(lapply(names(ltFinal), function(nm) {
    .computeRelativeLt(ltFinal[[nm]], brickRes[[nm]], ltMax[[nm]])
  }), names(ltFinal))
}

#' Prepare maximum lifetime data to be used in estimated lifetime computation
#'
#' Remove not required columns, rename columns and sum over ttotIn as indicated
#'
#' @param df data frame, lifetime data to manipulate
#' @param sumTtot logical, whether to sum over ttotIn
#'
#' @importFrom dplyr %>% across all_of any_of .data group_by rename select summarise
#'
.prepareLtMax <- function(df, sumTtot = FALSE) {

  df <- df %>%
    select(-"relVal")

  if (isTRUE(sumTtot)) {
    df <- df %>%
      group_by(across(-all_of(c("ttotIn", "absVal")))) %>%
      summarise(absVal = sum(.data[["absVal"]]), .groups = "drop")
  }

  df %>%
    select(-any_of("ttotIn"))
}

#' Compute direct lifetime estimate
#'
#' Compute the lifetime as the maximum estimate scaled by the ratio of maximum
#' and actual total outflows
#'
#' @param dfLtMax data frame, ex-ante lifetimes
#' @param outMax data frame, total ex-ante outflows
#' @param outflow data frame, total actual brick outflows
#'
#' @importFrom dplyr %>% .data left_join mutate rename select
#'
.computeLtDirect <- function(dfLtMax, outMax, outflow) {
  dfLtMax %>%
    select(-"relVal") %>%
    left_join(outMax %>%
                rename(outMax = "value"),
              by = c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", "ttotOut")) %>%
    left_join(outflow %>%
                rename(outflow = "value"),
              by = c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", "ttotOut")) %>%
    mutate(value = ifelse(.data[["absVal"]] == 0, #Only then should outMax be zero
                          0,
                          .data[["absVal"]] * .data[["outflow"]] / .data[["outMax"]])) %>%
    select(-"absVal", -"outflow", -"outMax")
}

#' Rename direct lifetime estimate and add zero columns
#'
#' Zero columns are required to be included in summations later
#'
#' @param dfLtDirect data frame, direct lifetime estimates
#'
#' @importFrom dplyr %>% mutate rename
#'
.adjustLtDirect <- function(dfLtDirect) {

  dfLtDirect %>%
    rename(directVal = "value") %>%
    mutate(addAttr = 0, value = 0)
}

#' Compute the unattributed outflow
#'
#' Compute the remaining inflow that has not yet been attributed to any outflows
#' in the time periods before \code{tOut}.
#' If the direct estimate surpasses this amount, then the difference is the un-
#' attributed outflow.
#'
#' @param dfLt data frame, direct lifetime estimates of current time period and
#'   final lifetime estimates of previous time periods.
#'   Required (value) columns: \code{directVal}, \code{addAttr}, \code{value}.
#' @param dfBrickIn data frame, inflows in brick results
#' @param tOut numeric, time period of removal for which we are computing the
#'   unattributed outflows
#'
#' @importFrom dplyr %>% across any_of .data filter group_by left_join mutate
#'   rename right_join select summarise
#'
.computeUnattrOut <- function(dfLt, dfBrickIn, tOut) {

  # Compute the sum of previous outflows by the mixed estimate
  dfLt %>%
    filter(.data[["ttotOut"]] <= tOut) %>%
    group_by(across(-any_of(c("ttotOut", "directVal", "addAttr", "value")))) %>%
    summarise(sumVal = sum(.data[["value"]]), .groups = "drop") %>%
    dplyr::right_join(dfLt,
                      by = c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", "ttotIn")) %>%
    left_join(dfBrickIn %>%
                rename(inVal = "value"),
              by = c("qty", "bs", "hs", if ("vin" %in% colnames(dfBrickIn)) "vin",
                     "region", "loc", "typ", "inc", "ttotIn")) %>%
    # Compute the remaining inflow from the total inflow and the previous outflows
    # attributed to this inflow.
    mutate(remInflow =  .data[["inVal"]] - .data[["sumVal"]],
           # Compute the unattributed outflow as the portion of the direct estimate that
           # cannot be covered by the remaining inflow.
           unattr = pmax(.data[["directVal"]] - .data[["remInflow"]], 0)) %>%
    select(-"inVal", -"sumVal", -"directVal", -"addAttr", -"value")

}

#' Compute the final lifetime estimate
#'
#' The final estimate is computed as the sum of the direct estimate and the total
#' unattributed outflow of the current time period, unless this surpasses the
#' remaining inflow. In the latter case the final lifetime estimate is given by
#' the total remaining inflow.
#'
#' @param dfLt data frame, final lifetime estimates of previous time steps,
#'   direct estimates and remaining inflows of the current time step
#' @param unattrOut data frame with outflow that has not been attributed to any inflow
#' @param unattrTot data frame, total unattributed outflow
#' @param tIn numeric, current time period of installation / inflow
#' @param tOut numeric, current time period of removal / outflow
#' @param dfStock data frame, mixed estimates and additional attribution of the initial stock
#'   (only required for flow computations)
#' @param dfShare data frame, share of the current flow in total inflows
#'   (only required for flow computations)
#'
#' @importFrom dplyr %>% across all_of any_of .data filter group_by left_join mutate
#'   rename select summarise
#' @importFrom tidyr replace_na
#'
.computeLtFinal <- function(dfLt, unattrOut, unattrTot, tIn, tOut, dfStock = NULL, dfShare = NULL) {

  subs <- c("region", "loc", "typ", "inc")

  dfLt <- filter(dfLt, .data[["ttotOut"]] == tOut)

  if (!is.null(dfStock)) {

    # Isolate additional attribution in stock data frame
    dfStock <- dfStock %>%
      rename(stockAttr = "addAttr") %>%
      select(all_of(c("qty", "bs", "hs", "vin", subs, "ttotOut", "stockAttr")))

    # Sum over additional attributions to previous inflows
    dfLt <- dfLt %>%
      filter(.data[["ttotIn"]] <= tIn) %>%
      group_by(across(any_of(c("qty", "bs", "hs", "vin", subs, "ttotOut")))) %>%
      summarise(addAttrSum = sum(.data[["addAttr"]]), .groups = "drop") %>%
      mutate(ttotIn = tIn) %>%
      left_join(dfLt, by = c("qty", "bs", "hs", "vin", subs, "ttotIn", "ttotOut")) %>%
      left_join(dfStock, by = c("qty", "bs", "hs", "vin", subs, "ttotOut")) %>%
      replace_na(list(stockAttr = 0)) %>%
      left_join(dfShare, by = c("qty", "bs", "hs", "vin", subs, "ttotIn"))

  }

  # Compute the maximum possible outflow to be attributed to the current in- and out time periods
  # as the sum of the direct estimate and the appropriate share of the still unattributed outflow.
  # Set the final estimate to this maximum possible outflow, unless this surpasses the
  # remaining inflow. In that case, set the mixed estimate to the remaining inflow.
  dfLt %>%
    left_join(unattrOut, by = c("qty", "bs", "hs", "vin", subs, "ttotIn", "ttotOut")) %>%
    left_join(unattrTot, by = c("qty", "bs", "hs", "vin", subs, "ttotOut")) %>%
    mutate(
      maxOutflow = if (is.null(dfStock)) .data[["directVal"]] + .data[["sumUnattr"]]
      else .data[["directVal"]]
      + .data[["share"]] * (.data[["sumUnattr"]] - .data[["stockAttr"]] - .data[["addAttrSum"]]),
      value = pmin(.data[["maxOutflow"]], .data[["remInflow"]]),
      addAttr = pmax(.data[["value"]] - .data[["directVal"]], 0)
    ) %>%
    select(-any_of(c("maxOutflow", "sumUnattr", "addAttrSum", "stockAttr", "share", "remInflow", "unattr")))

}

#' Add the final estimate results of a specific in and a specific out time period
#' to the data frame storing all final estimates
#'
#' @param dfOrig data frame, contains all previous final estimates
#' @param dfNew data frame, newly computed final estimates
#' @param tIn numeric, time period of installation / inflow of the newly computed mixed estimates
#' @param tOut numeric, time period of removal / outflow of the newly computed mixed estimates
#'
#' @importFrom dplyr %>% .data left_join mutate select
#'
.addResults <- function(dfOrig, dfNew, tIn, tOut) {

  dfOrig %>%
    left_join(
      dfNew,
      by = c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", "directVal", "ttotIn", "ttotOut")
    ) %>%
    mutate(value = ifelse(
      .data[["ttotIn"]] == tIn & .data[["ttotOut"]] == tOut,
      .data[["value.y"]],
      .data[["value.x"]]
    ),
    addAttr = ifelse(
      .data[["ttotIn"]] == tIn & .data[["ttotOut"]] == tOut,
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
#'
#' @importFrom dplyr %>% any_of .data left_join mutate rename select
#' @importFrom tidyr replace_na
#'
.combineAddAttr <- function(dfThis, dfOther) {

  #TODO: I get NAs if adding con values to ren #nolint: todo_comment_linter.
  # Rename addAttr column
  dfOther <- dfOther %>%
    rename(addAttrOther = "addAttr") %>%
    select(any_of(c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", "ttotIn", "ttotOut", "addAttrOther")))

  dfThis %>%
    left_join(dfOther, by = c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", "ttotIn", "ttotOut")) %>%
    replace_na(list(addAttrOther = 0)) %>%
    mutate(addAttr = .data[["addAttr"]] + .data[["addAttrOther"]]) %>%
    select(-"addAttrOther")

}

#' Compute the relative value of the lifetime estimates
#'
#' By setting the final estimates in relation to the total inflows, compute the
#' probabilities of removal in a given time period.
#' If both the total inflow and the absolute lifetime estimate are close to zero,
#' set this to the maximum value.
#'
#' @param dfLt data frame, mixed lifetime estimates
#' @param dfBrickIn data frame, total inflows as given by brick results
#' @param dfMax data frame, ex-ante lifetime estimates
#'
#' @importFrom dplyr %>% any_of .data left_join mutate rename select
#'
.computeRelativeLt <- function(dfLt, dfBrickIn, dfMax) {

  dfLt %>%
    select(-any_of(c("directVal", "addAttr"))) %>%
    rename(absVal = "value") %>%
    left_join(dfBrickIn %>%
                rename(inVal = "value"),
              by = c("qty", "bs", "hs", if ("vin" %in% colnames(dfBrickIn)) "vin",
                     "region", "loc", "typ", "inc", "ttotIn")) %>%
    left_join(dfMax %>%
                select(-"absVal") %>%
                rename(maxVal = "relVal"),
              by = c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", "ttotIn", "ttotOut")) %>%
    # TODO: Check on this heuristic, find a meaningful way to handle this! #nolint: todo_comment_linter.
    mutate(relVal = ifelse(.data[["absVal"]] <= 1E-9 & .data[["inVal"]] <= 1E-9,
                           .data[["maxVal"]],
                           .data[["absVal"]] / .data[["inVal"]])) %>%
    select(-any_of(c("inVal", "maxVal")))

}
