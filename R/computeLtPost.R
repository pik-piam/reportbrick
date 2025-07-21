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
    crossing(ttotOut = ttotNum[-1]) %>%
    filter(.data[["ttotOut"]] >= .data[["ttotIn"]], .data[["ttotIn"]] != t0) %>%
    mutate(value = 0)

  stockAll <- data[["stock"]] %>%
    select(-"value") %>%
    crossing(ttotOut = ttotNum) %>%
    mutate(value = 0)

  # Compute leave times of initial stock and inflows
  for (tOut in tRun) {
    stockThis <- .computeLeaveInitStock(tOut, data[["stock"]], stockAll, outflow, dims)
    stockAll <- .addResults(stockAll, stockThis, t0, tOut, dims)

    for (tIn in tRun[tRun <= tOut]) {
      inThis <- .computeLeaveInFlow(tIn, tOut, inAll, stockAll, inflow, outflow, dims)
      inAll <- .addResults(inAll, inThis, tIn, tOut, dims)
    }
  }

  # Process leave time results and separate construction and renovation
  stockAll <- stockAll %>%
    filter(.data[["ttotOut"]] != t0) %>%
    mutate(ttotIn = t0, .before = "ttotOut")

  conAll <- inAll %>%
    left_join(conShare, by = c(dims, "ttotIn")) %>%
    mutate(value = .data[["value"]] * .data[["share"]]) %>%
    dplyr::right_join(p_ttotVin %>%
                        filter(.data[["ttot"]] != t0),
                      by = c(ttotIn = "ttot", "vin")) %>%
    select(-"share")

  renAll <- inAll %>%
    left_join(conShare, by = c(dims, "ttotIn")) %>%
    mutate(value = .data[["value"]] * (1 - .data[["share"]])) %>%
    select(-"share")

  # Further processing: Compute relative value
  stockInitLtPost <- .processLtPost(stockAll, data[["stock"]], dims)
  conLtPost <- .processLtPost(conAll, data[["construction"]], dims)
  renLtPost <- .processLtPost(
    renAll,
    data[["renovation"]],
    dims
  )

  # Test that for construction all entries with non-matching vintage are zero
  conTest <- conAll %>%
    dplyr::anti_join(p_ttotVin, by = c(ttotIn = "ttot", "vin"))
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
    filter(.data[["ttotOut"]] < tOut) %>%
    group_by(across(any_of(dims))) %>%
    summarise(valueCumSum = sum(value), .groups = "drop") %>%
    left_join(dfOut %>%
                rename(valueOut = "value") %>%
                filter(.data[["ttotOut"]] == tOut),
              by = dims) %>%
    left_join(dfStockInit %>%
                rename(valueTot = "value"),
              by = dims) %>%
    mutate(value = pmin(.data[["valueOut"]], .data[["valueTot"]] - .data[["valueCumSum"]])) %>%
    select(-"valueOut", -"valueTot", -"valueCumSum")
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
    filter(.data[["ttotIn"]] == tIn, .data[["ttotOut"]] >= tIn, .data[["ttotOut"]] <= tOut) %>%
    group_by(across(any_of(dims))) %>%
    summarise(prevCumSum = sum(.data[["value"]]), .groups = "drop")

  dfInAll %>%
    filter(.data[["ttotIn"]] <= tIn, .data[["ttotOut"]] == tOut) %>%
    group_by(across(any_of(dims))) %>%
    summarise(valueCumSum = sum(.data[["value"]]), .groups = "drop") %>%
    left_join(dfInPrev, by = dims) %>%
    left_join(dfOut %>%
                rename(valueOut = "value") %>%
                filter(.data[["ttotOut"]] == tOut),
              by = dims) %>%
    dplyr::right_join(dfIn %>%
                        rename(valueIn = "value") %>%
                        filter(.data[["ttotIn"]] == tIn),
                      by = dims) %>%
    left_join(dfStock %>%
                rename(valueStock = "value") %>%
                filter(.data[["ttotOut"]] == tOut) %>%
                select(-"ttotIn"),
              by = c(dims, "ttotOut")) %>%
    mutate(value = pmin(.data[["valueOut"]] - .data[["valueStock"]] - .data[["valueCumSum"]],
                        .data[["valueIn"]] - .data[["prevCumSum"]])) %>%
    select(-"valueOut", -"valueIn", -"valueStock", -"valueCumSum", -"prevCumSum")
}

#' Add the ex-post estimate results of a specific in and a specific out time period
#' to the data frame storing all ex-post estimates
#'
#' @param dfOrig data frame, contains all previous ex-post estimates
#' @param dfNew data frame, newly computed ex-post estimates
#' @param tIn numeric, time period of installation / inflow of the newly computed ex-post estimates
#' @param tOut numeric, time period of removal / outflow of the newly computed ex-post estimates
#' @param dims character, dimensions of the data without time periods
#'
#' @importFrom dplyr %>% .data left_join mutate select
#'
.addResults <- function(dfOrig, dfNew, tIn, tOut, dims) {

  dfOrig %>%
    left_join(dfNew, by = c(dims, "ttotIn", "ttotOut")) %>%
    mutate(value = ifelse(
      .data[["ttotIn"]] == tIn & .data[["ttotOut"]] == tOut,
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
    left_join(data, by = c(dims, "ttotIn")) %>%
    mutate(relVal = .data[["absVal"]] / .data[["value"]]) %>%
    select(-"value")

}
