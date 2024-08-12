#' Compute ex-post lifetimes
#'
#' @author Ricarda Rosemann
#'
#' @param inflow data frame, total inflows
#' @param outflow data frame, total outflows
#' @param data named list, stock and flow data from brick results
#' @param conShare data frame, share of constructions in total inflows
#' @param p_ttotVin data frame, mapping between time periods and vintages
#' @param ttotNum numeric, all time periods
#' @param dims character, dimensions of the data without time periods
#'
#' @importFrom dplyr %>% .data filter left_join mutate right_join select
#' @importFrom tidyr crossing
#'
computeLtPost <- function(inflow, outflow, data, conShare, p_ttotVin, ttotNum, dims) {

  t0 <- ttotNum[1]
  tRun <- setdiff(ttotNum, t0)

  # Initialize data frames to collect leave times of initial stock and inflows
  inAll <- inflow %>%
    crossing(ttot2 = ttotNum[-1]) %>%
    filter(.data[["ttot2"]] >= .data[["ttot"]], .data[["ttot"]] != t0) %>%
    mutate(value = 0)

  stockAll <- data[["stock"]] %>%
    select(-"value") %>%
    crossing(ttot2 = ttotNum) %>%
    mutate(value = 0)

  # Compute leave times of initial stock and inflows
  for (t2 in tRun) {
    stockThis <- .computeLeaveInitStock(t2, data[["stock"]], stockAll, outflow, dims)
    stockAll <- .addResults(stockAll, stockThis, t0, t2, dims)

    for (t1 in tRun[tRun <= t2]) {
      inThis <- .computeLeaveInFlow(t1, t2, inAll, stockAll, inflow, outflow, dims)
      inAll <- .addResults(inAll, inThis, t1, t2, dims, addDims = "dt")
    }
  }

  # Process leave time results and separate construction and renovation
  stockAll <- stockAll %>%
    filter(.data[["ttot2"]] != t0) %>%
    mutate(ttot = t0, .before = "ttot2")

  conAll <- inAll %>%
    select(-"dt") %>%
    left_join(conShare, by = c(dims, "ttot")) %>%
    mutate(value = .data[["value"]] * .data[["share"]]) %>%
    dplyr::right_join(p_ttotVin %>%
                        filter(.data[["ttot"]] != t0),
                      by = c("ttot", "vin")) %>%
    select(-"share")

  renAll <- inAll %>%
    select(-"dt") %>%
    left_join(conShare, by = c(dims, "ttot")) %>%
    mutate(value = .data[["value"]] * (1 - .data[["share"]])) %>%
    select(-"share")

  # Further processing: Compute relative value
  stockInitLtPost <- .processLtPost(stockAll, data[["stock"]], dims)
  conLtPost <- .processLtPost(conAll, data[["construction"]], setdiff(dims, "vin"))
  renLtPost <- .processLtPost(
    renAll,
    data[["renovation"]] %>%
      select(-"dt"),
    dims
  )

  # Test that for construction all entries with non-matching vintage are zero
  conTest <- conAll %>%
    dplyr::anti_join(p_ttotVin, by = c("ttot", "vin"))
  if (any(conTest[["value"]] > 0)) {
    message("Ex-post lifetime probabilites of construction are implausible: ",
            "Non-zero entries for vintages that do not match the given time period.")
  }

  return(list(stockInitLtPost = stockInitLtPost, conLtPost = conLtPost, renLtPost = renLtPost))

}

#' Determine leave time of initial stock
#'
#' @param tOut numeric, time period of removal / outflow
#' @param dfStockInit data frame, brick result data on initial stock
#' @param dfStock data frame, contains ex-post life time estimates of previous time periods
#' @param dfOut data frame, total outflow
#' @param dims character, dimensions of the data without time periods
#'
#' @importFrom dplyr %>% across any_of .data filter group_by left_join mutate
#'   rename select summarise
#'
.computeLeaveInitStock <- function(tOut, dfStockInit, dfStock, dfOut, dims) {

  dfStock <- dfStock %>%
    filter(.data[["ttot2"]] < tOut) %>%
    group_by(across(any_of(dims))) %>%
    summarise(valueCumSum = sum(value), .groups = "drop") %>%
    left_join(dfOut %>%
                rename(valueOut = "value", dtOut = "dt", ttot2 = "ttot") %>%
                filter(.data[["ttot2"]] == tOut),
              by = dims) %>%
    left_join(dfStockInit %>%
                rename(valueTot = "value"),
              by = dims) %>%
    mutate(value = pmin(.data[["dtOut"]] * .data[["valueOut"]], .data[["valueTot"]] - .data[["valueCumSum"]]))
  dfStock <- dfStock %>%
    select(-"valueOut", -"valueTot", -"valueCumSum", -"dtOut")
}

#' Determine leave time of inflows
#'
#' @param tIn numeric, time period of installation / inflow
#' @param tOut numeric, time period of removal / outflow
#' @param dfInAll data frame, contains ex-post estimates of all previous time periods
#' @param dfStock data frame, ex-post lifetime estimates of the initial stock
#' @param dfIn data frame, total inflows
#' @param dfOut data frame, total outflows
#' @param dims character, dimensions of the data without time periods
#'
#' @importFrom dplyr %>% across any_of .data filter group_by left_join mutate
#'   rename select summarise
#'
.computeLeaveInFlow <- function(tIn, tOut, dfInAll, dfStock, dfIn, dfOut, dims) {

  dfInPrev <- dfInAll %>%
    filter(.data[["ttot"]] == tIn, .data[["ttot2"]] >= tIn, .data[["ttot2"]] <= tOut) %>%
    group_by(across(any_of(dims))) %>%
    summarise(prevCumSum = sum(.data[["dt"]] * .data[["value"]]), .groups = "drop")

  dfInAll <- dfInAll %>%
    filter(.data[["ttot"]] <= tIn, .data[["ttot2"]] == tOut)
  dfInAll <- dfInAll %>%
    group_by(across(any_of(dims))) %>%
    summarise(valueCumSum = sum(.data[["dt"]] * .data[["value"]]), .groups = "drop")
  dfInAll <- dfInAll %>%
    left_join(dfInPrev, by = dims)
  dfInAll <- dfInAll %>%
    left_join(dfOut %>%
                rename(valueOut = "value", ttot2 = "ttot", dtOut = "dt") %>%
                filter(.data[["ttot2"]] == tOut),
              by = dims)
  dfInAll <- dfInAll %>%
    dplyr::right_join(dfIn %>%
                        rename(valueIn = "value") %>%
                        filter(.data[["ttot"]] == tIn),
                      by = dims)
  dfInAll <- dfInAll %>%
    left_join(dfStock %>%
                rename(valueStock = "value") %>%
                filter(.data[["ttot2"]] == tOut),
              by = c(dims, "ttot", "ttot2")) %>%
    replace_na(list(valueStock = 0))
  dfInAll <- dfInAll %>%
    mutate(value = pmin(.data[["dtOut"]] * .data[["valueOut"]] - .data[["valueStock"]] - .data[["valueCumSum"]],
                        .data[["dt"]] * .data[["valueIn"]] - .data[["prevCumSum"]]) / .data[["dt"]])
  dfInAll <- dfInAll %>%
    select(-"valueOut", -"valueIn", -"valueStock", -"valueCumSum", -"prevCumSum", -"dtOut")
}

#' Add the ex-post estimate results of a specific in and a specific out time period
#' to the data frame storing all ex-post estimates
#'
#' @param dfOrig data frame, contains all previous ex-post estimates
#' @param dfNew data frame, newly computed ex-post estimates
#' @param tIn numeric, time period of installation / inflow of the newly computed ex-post estimates
#' @param tOut numeric, time period of removal / outflow of the newly computed ex-post estimates
#' @param dims character, dimensions of the data without time periods
#' @param addDims character, additional dimensions present in the data
#'
#' @importFrom dplyr %>% .data left_join mutate select
#'
.addResults <- function(dfOrig, dfNew, tIn, tOut, dims, addDims = NULL) {

  dfOrig %>%
    left_join(dfNew, by = c(dims, "ttot", "ttot2", addDims)) %>%
    mutate(value = ifelse(
      .data[["ttot"]] == tIn & .data[["ttot2"]] == tOut,
      .data[["value.y"]],
      .data[["value.x"]]
    )) %>%
    select(-"value.x", -"value.y")

}

#' Compute the relative value of the lifetime estimates
#'
#' By setting the ex-post estimates in relation to the total inflows, compute the
#' probabilities of removal in a given time period.
#' If the total inflow is close to zero, this will be NA.
#'
#' @param ltPost data frame, ex-post lifetime estimates
#' @param data data frame, total inflows as given by brick results
#' @param dims character, dimensions of the data without time periods
#'
#' @importFrom dplyr %>% any_of .data left_join mutate rename select
#'
.processLtPost <- function(ltPost, data, dims) {

  ltPost <- ltPost %>%
    rename(absVal = "value") %>%
    left_join(data, by = c(dims, "ttot")) %>%
    mutate(relVal = .data[["absVal"]] / .data[["value"]]) %>%
    select(-"value")

}
