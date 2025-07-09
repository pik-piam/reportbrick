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

  # Compute ex-ante outflows, i.e. according to the Weibull distribution
  outAnte <- .prepareLtAnte(ltAnte[["stock"]], c(stock = "absVal")) %>%
    full_join(.prepareLtAnte(ltAnte[["construction"]], c(con = "absVal"), sumTtot = TRUE), by = c(dims, "ttotOut")) %>%
    full_join(.prepareLtAnte(ltAnte[["renovation"]], c(ren = "absVal"), sumTtot = TRUE), by = c(dims, "ttotOut")) %>%
    replace_na(list(stock = 0, con = 0)) %>%
    mutate(value = .data[["stock"]] + .data[["con"]] + .data[["ren"]]) %>%
    select(-"stock", -"con", -"ren")

  # Compare ex-ante and model outflows (temporary, for diagnostics)
  outAnteSum <- outAnte %>%
    group_by(across(-all_of(c("ttotOut", "value")))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop")

  outModelSum <- outflow %>%
    group_by(across(-all_of(c("ttotOut", "value")))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop")

  outDiff <- outAnte %>%
    rename(valAnte = "value") %>%
    left_join(outflow %>%
                rename(valModel = "value"),
              by = c(dims, "ttotOut")) %>%
    mutate(value = .data[["valAnte"]] - .data[["valModel"]])

  outDiffSum <- outAnteSum %>%
    rename(valAnte = "value") %>%
    left_join(outModelSum %>%
                rename(valModel = "value"),
              by = c(dims)) %>%
    mutate(value = .data[["valAnte"]] - .data[["valModel"]])

  # Compute direct lifetime estimate as the ex-ante lifetime,
  # scaled by the ratio between ex-ante and model outflow
  ltDirect <- lapply(ltAnte, function(dfLt) .computeLtDirect(dfLt, outAnte, outflow, dims))

  # add results columns "addAttr" and "value" to all DFs and fill with zero
  ltMixed <- lapply(ltDirect, .prepareLtMixed)

  for (tOut in tRun) {
    # Compute inflow that remains after previous time periods, compute unattributed outflow
    unattrOut <- stats::setNames(lapply(names(ltMixed), function(nm) {
      dimsBrick <- if ("vin" %in% colnames(brickRes[[nm]])) dims else setdiff(dims, "vin")
      .computeUnattrOut(ltMixed[[nm]], brickRes[[nm]], tOut, dims, dimsBrick = dimsBrick)
    }), names(ltMixed))

    # Compute total of unattributed outflow of stock and flows
    unattrTot <- do.call(rbind, lapply(names(unattrOut), function(nm) {
      mutate(unattrOut[[nm]], variable = nm)
    })) %>%
      select("variable", dims, "ttotIn", "ttotOut", "remInflow", "unattr") %>%
      filter(.data[["ttotOut"]] == tOut) %>%
      group_by(across(-any_of(c("variable", "ttotIn", "remInflow", "unattr")))) %>%
      summarise(sumUnattr = sum(.data[["unattr"]]), .groups = "drop")

    # compute initial stock lt and save to new column
    stockThis <- .computeLtMixed(ltMixed[["stock"]], unattrOut[["stock"]], unattrTot, t0, tOut, dims)
    ltMixed[["stock"]] <- .addResultsMixed(ltMixed[["stock"]], stockThis, t0, tOut, c(dims, "directVal"))

    for (tIn in tRun[tRun <= tOut]) {
      # Compute lifetimes for construction and renovation flow
      conThis <- .computeLtMixed(ltMixed[["construction"]], unattrOut[["construction"]], unattrTot, tIn, tOut, dims, dfStock = stockThis, dfShare = conShare)
      renThis <- .computeLtMixed(ltMixed[["renovation"]], unattrOut[["renovation"]], unattrTot, tIn, tOut, dims, dfStock = stockThis, dfShare = renShare)

      # Save results in lt data frame, save combined additional attributions of construction and renovation
      ltMixed[["construction"]] <- .addResultsMixed(ltMixed[["construction"]], .combineAddAttr(conThis, renThis, dims), tIn, tOut, c(dims, "directVal"))
      ltMixed[["renovation"]] <- .addResultsMixed(ltMixed[["renovation"]], .combineAddAttr(renThis, conThis, dims), tIn, tOut, c(dims, "directVal"))
    }
  }

  stats::setNames(lapply(names(ltMixed), function(nm) {
    dimsBrick <- if ("vin" %in% colnames(brickRes[[nm]])) dims else setdiff(dims, "vin")
    .processLtMixed(ltMixed[[nm]], brickRes[[nm]], ltAnte[[nm]], dims, dimsBrick = dimsBrick)
  }), c("stockInitLtMixed", "conLtMixed", "renLtMixed"))
}

#' Prepare ex-ante lifetime data to be used in mixed lifetime computation
#'
#' Remove not required columns, rename columns and sum over ttotIn as indicated
#'
#' @param df data frame, lifetime data to manipulate
#' @param toRename named character, old (value) and new name (name) of columns to be renamed
#' @param sumTtot logical, whether to sum over ttotIn
#'
#' @importFrom dplyr %>% across all_of any_of .data group_by rename select summarise
#'
.prepareLtAnte <- function(df, toRename, sumTtot = FALSE) {

  df <- df %>%
    select(-"relVal")

  if (isTRUE(sumTtot)) {
    df <- df %>%
      group_by(across(-all_of(c("ttotIn", "absVal")))) %>%
      summarise(absVal = sum(.data[["absVal"]]), .groups = "drop")
  }

  df %>%
    select(-any_of("ttotIn")) %>%
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
              by = c(dims, "ttotOut")) %>%
    left_join(outflow %>%
                rename(outflow = "value"),
              by = c(dims, "ttotOut")) %>%
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
#'
.prepareLtMixed <- function(dfLtDirect) {

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
#' @param dfLtMixed data frame, direct lifetime estimates of current time period and
#'   mixed lifetime estimates of previous time periods.
#'   Required (value) columns: \code{directVal}, \code{addAttr}, \code{value}.
#' @param dfBrickIn data frame, inflows in brick results
#' @param tOut numeric, time period of removal for which we are computing the
#'   unattributed outflows
#' @param dims character, dimensions of the data without time periods
#' @param dimsBrick character, dimensions of the brick object without time periods
#'
#' @importFrom dplyr %>% across any_of .data filter group_by left_join mutate
#'   rename right_join select summarise
#'
.computeUnattrOut <- function(dfLtMixed, dfBrickIn, tOut, dims, dimsBrick = dims) {

  # Compute the sum of previous outflows by the mixed estimate
  dfLtMixed %>%
    filter(.data[["ttotOut"]] <= tOut) %>%
    group_by(across(-any_of(c("ttotOut", "directVal", "addAttr", "value")))) %>%
    summarise(sumVal = sum(.data[["value"]]), .groups = "drop") %>%
    dplyr::right_join(dfLtMixed, by = c(dims, "ttotIn")) %>%
    left_join(dfBrickIn %>%
                rename(inVal = "value"),
              by = c(dimsBrick, "ttotIn")) %>%
    # Compute the remaining inflow from the total inflow and the previous outflows
    # attributed to this inflow.
    mutate(remInflow =  .data[["inVal"]] - .data[["sumVal"]],
           # Compute the unattributed outflow as the portion of the direct estimate that
           # cannot be covered by the remaining inflow.
           unattr = pmax(.data[["directVal"]] - .data[["remInflow"]], 0)) %>%
    select(-"inVal", -"sumVal", -"directVal", -"addAttr", -"value")

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
.computeLtMixed <- function(dfLtMixed, unattrOut, unattrTot, tIn, tOut, dims, dfStock = NULL, dfShare = NULL) {

  dfLtMixed <- filter(dfLtMixed, .data[["ttotOut"]] == tOut)

  if (!is.null(dfStock)) {

    # Isolate additional attribution in stock data frame
    dfStock <- dfStock %>%
      rename(stockAttr = "addAttr") %>%
      select(all_of(c(dims, "ttotOut", "stockAttr")))
    
    # Sum over additional attributions to previous inflows
    dfLtMixed <- dfLtMixed %>%
      filter(.data[["ttotIn"]] <= tIn) %>%
      group_by(across(any_of(c(dims, "ttotOut")))) %>%
      summarise(addAttrSum = sum(.data[["addAttr"]]), .groups = "drop") %>%
      mutate(ttotIn = tIn) %>%
      left_join(dfLtMixed, by = c(dims, "ttotIn", "ttotOut")) %>%
      left_join(dfStock, by = c(dims, "ttotOut")) %>%
      replace_na(list(stockAttr = 0)) %>%
      left_join(dfShare, by = c(dims, "ttotIn"))

  }

  # Compute the maximum possible outflow to be attributed to the current in- and out time periods
  # as the sum of the direct estimate and the appropriate share of the still unattributed outflow.
  # Set the final mixed estimate to this maximum possible outflow, unless this surpasses the
  # remaining inflow. In that case, set the mixed estimate to the remaining inflow.
  dfLtMixed %>%
    left_join(unattrOut, by = c(dims, "ttotIn", "ttotOut")) %>%
    left_join(unattrTot, by = c(dims, "ttotOut")) %>%
    mutate(
      maxOutflow = if (is.null(dfStock)) .data[["directVal"]] + .data[["sumUnattr"]]
      else .data[["directVal"]] + .data[["share"]] * (.data[["sumUnattr"]] - .data[["stockAttr"]] - .data[["addAttrSum"]]),
      value = pmin(.data[["maxOutflow"]], .data[["remInflow"]]),
      addAttr = pmax(.data[["value"]] - .data[["directVal"]], 0)
    ) %>%
    select(-any_of(c("maxOutflow", "sumUnattr", "addAttrSum", "stockAttr", "share", "remInflow", "unattr")))

}

#' Add the mixed estimate results of a specific in and a specific out time period
#' to the data frame storing all mixed estimates
#'
#' @param dfOrig data frame, contains all previous mixed estimates
#' @param dfNew data frame, newly computed mixed estimates
#' @param tIn numeric, time period of installation / inflow of the newly computed mixed estimates
#' @param tOut numeric, time period of removal / outflow of the newly computed mixed estimates
#' @param dims character, dimensions of the data without time periods
#'
#' @importFrom dplyr %>% .data left_join mutate select
#'
.addResultsMixed <- function(dfOrig, dfNew, tIn, tOut, dims) {

  dfOrig %>%
    left_join(dfNew, by = c(dims, "ttotIn", "ttotOut")) %>%
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
    select(any_of(c(dims, "ttotIn", "ttotOut", "addAttrOther")))

  dfThis %>%
    left_join(dfOther, by = c(dims, "ttotIn", "ttotOut")) %>%
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
#' @param dfBrickIn data frame, total inflows as given by brick results
#' @param dfAnte data frame, ex-ante lifetime estimates
#' @param dims character, dimensions of the data without time periods
#' @param dimsBrick character, dimensions of the brick results
#'
#' @importFrom dplyr %>% any_of .data left_join mutate rename select
#'
.processLtMixed <- function(dfLt, dfBrickIn, dfAnte, dims, dimsBrick = dims) {

  dfLt %>%
    select(-any_of(c("directVal", "addAttr"))) %>%
    rename(absVal = "value") %>%
    left_join(dfBrickIn %>%
                rename(inVal = "value"),
              by = c(dimsBrick, "ttotIn")) %>%
    left_join(dfAnte %>%
                select(-"absVal") %>%
                rename(anteVal = "relVal"),
              by = c(union(dims, "vin"), "ttotIn", "ttotOut")) %>%
    mutate(relVal = ifelse(.data[["absVal"]] <= 1E-9 & .data[["inVal"]] <= 1E-9, # TODO: Check on this heuristic, find a meaningful way to handle this!
                           .data[["anteVal"]],
                           .data[["absVal"]] / .data[["inVal"]])) %>%
    select(-any_of(c("inVal", "anteVal")))

}
