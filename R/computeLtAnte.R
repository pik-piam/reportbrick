#' Compute heating system lifetimes ex-ante
#'
#' Determine the lifetime of the heating system according to the respective Weibull distribution.
#' No early retirement is reflected
#'
#' @author Ricarda Rosemann
#'
#' @param variable character, variable for which the lifetime is computed
#' @param data data frame, dimensions of desired lifetimes,
#'   usually also contains values to compute the absolute removed quantities
#' @param ttotNum numeric, all time periods to be considered
#' @param lifeTimeHs data frame, Weibull parameters
#' @param dims character, dimensions of the data without time dimensions
#' @param runSimple logical, whether to use the simplified formula
#' @param dataValue data frame, optionally provide the values to compute the
#'   absolute removed quantities separately
#' @param p_dt data frame, lengths of the time periods
#'   (only required for simple formula)
#'
#' @importFrom dplyr %>% across all_of .data filter group_by left_join mutate
#'   rename select summarise ungroup
#'
computeLtAnte <- function(variable, data, ttotNum, lifeTimeHs, dims,
                          runSimple = FALSE, dataValue = NULL, p_dt = NULL) {

  if (is.null(dataValue)) dataValue <- data
  hsCol <- "hs"

  # dtTtotNum <- ttotNum[[length(ttotNum)]] - ttotNum[[length(ttotNum) - 1]]
  # ttotExtended <- c(ttotNum, ttotNum[[length(ttotNum)]] + seq(dtTtotNum, 50, by = dtTtotNum))

  # Assemble in and out time periods and compute starting and end point
  times <- expand.grid(ttot = ttotNum, ttot2 = ttotNum) %>%
    filter(.data[["ttot2"]] >= .data[["ttot"]]) %>%
    group_by(ttot2) %>%
    mutate(tIn0 = .extractInitTime(.data[["ttot"]]),
           tIn1 = .data[["ttot"]]) %>%
    ungroup() %>%
    group_by(ttot) %>%
    mutate(tOut0 = .extractInitTime(.data[["ttot2"]], ttotNum = ttotNum),
           tOut1 = .data[["ttot2"]])

  # Specific treatment of construction and renovation input data:
  # Filter time vector, rename or remove columns
  if (variable %in% c("construction", "renovation")) {
    data <- filter(data, .data[["ttot"]] != ttotNum[1])
    dataValue <- select(dataValue, -"dt")
    if (variable == "renovation") {
      data <- filter(data, .data[["hsr"]] != "0")
      lifeTimeHs <- rename(lifeTimeHs, hsr = "hs")
      hsCol <- "hsr"
    } else if (variable == "construction") {
      data <- select(data, -"dt")
    }
  }

  # Create data frame for lifetime computations from input data, the correct in and out times,
  # and the parameters of the Weibull distribution
  ltAnte <- data %>%
    select(-"value") %>%
    left_join(times, by = "ttot") %>%
    left_join(lifeTimeHs, by = c("reg", "typ", hsCol))

  # Call the respective functions to compute the lifetime
  if(isFALSE(runSimple)) {
    if (variable == "stock") {
      ltAnte <- .computeLtStockAnte(select(ltAnte, -"tIn0", -"tIn1"))
    } else if (variable == "construction") {
      ltAnte <- .computeLtAnte(ltAnte, 4)
    } else if (variable == "renovation") {
      ltAnte <- .computeLtAnte(ltAnte, 4) %>%
        group_by(across(-all_of(c("hs", "value")))) %>%
        summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
        select(-"bs") %>%
        rename(bs = "bsr", hs = "hsr")
    }
  } else {
    if (!"dt" %in% colnames(ltAnte)) ltAnte <- left_join(ltAnte, p_dt, by = "ttot")
    if (variable == "stock") {
      ltAnte <- .computeLtAnteSimple(ltAnte, dims, ttotNum[1], standingLifetime = 6)
    } else if (variable == "construction") {
      ltAnte <- .computeLtAnteSimple(ltAnte, dims, ttotNum[1]) %>%
        relocate("vin", .before = "reg")
    } else if (variable == "renovation") {
        ltAnte <- .computeLtAnteSimple(ltAnte, c(dims, "hsr", "bsr"), ttotNum[1]) %>%
          group_by(across(-all_of(c("hs", "value")))) %>%
          summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
          select(-"bs") %>%
          rename(bs = "bsr", hs = "hsr")
    }
    ltAnte <- select(ltAnte, -"dt")
  }


  ltAnte <- ltAnte %>%
    rename(relVal = "value") %>%
    left_join(dataValue, by = c(dims, "ttot")) %>%
    mutate(absVal = .data[["value"]] * .data[["relVal"]]) %>%
    select(-"value")

  return(ltAnte)
}

#' Determine the starting year of each time period
#'
#' @param ttot numeric, time periods
#' @param ttotNum numeric, additional vector of time periods that needs to start
#'   earlier than \code{ttot}
#'
.extractInitTime <- function(ttot, ttotNum = NULL) {
  if (!is.null(ttotNum) && ttot[1] != ttotNum[1]) {
    firstVal <- ttotNum[which(ttotNum == ttot[1]) - 1]
  } else {
    firstVal <- ttot[1]
  }
  t <- c(firstVal, ttot[1:length(ttot)-1])
  return(t)
}

#' Compute the ex-ante lifetime of the initial stock
#'
#' @param dfLt data frame, needs to contain input dimensions, each with intervals
#'   for all time periods and with the Weibull paramters
#' @param standingLifetime numeric, assumed standing lifetime of the initial stock
#'
#' @importFrom dplyr %>% .data mutate select
#' @importFrom stats pweibull
#'
.computeLtStockAnte <- function(dfLt, standingLifetime = 6) {
  dfLt %>%
    mutate(value = (pweibull(.data[["tOut1"]] - .data[["ttot"]] + standingLifetime, .data[["shape"]], .data[["scale"]])
                    - pweibull(.data[["tOut0"]] - .data[["ttot"]] + standingLifetime, .data[["shape"]], .data[["scale"]]))) %>%
    mutate(value = .data[["value"]] / (1 - pweibull(standingLifetime, .data[["shape"]], .data[["scale"]]))) %>%
    select(-"tOut0", -"tOut1", -"shape", -"scale")
}

#' Compute the ex-ante lifetime of flows by numerical integration
#'
#' @param dfLt data frame, needs to contain input dimensions, each with intervals
#'   for all time periods and with the Weibull paramters
#' @param n numeric, number of steps in the numerical integration
#'
#' @importFrom dplyr %>% .data everything group_by left_join mutate select
#'   summarise ungroup
#' @importFrom tidyr crossing
#' @importFrom pracma integral
#'
.computeLtAnte <- function(dfLt, n = 4, selfImplementation = FALSE) {

  if (isTRUE(selfImplementation)) {
    # Compute the sum of the function evaluation in the inner discretization points
    dfLtK <- dfLt %>%
      mutate(step = (.data[["tIn1"]] - .data[["tIn0"]]) / n) %>%
      crossing(k = seq(1, n-1)) %>%
      group_by(across(-all_of("k"))) %>%
      summarise(sumFunc = sum(
          .weibullIntegrand(
            .data[[a]] + .data[["k"]] * .data[["step"]],
            .data[["shape"]],
            .data[["scale"]],
            .data[["tOut0"]],
            .data[["tOut1"]])
          ),
          .groups = "drop"
        )

    # Compute the function evaluation at the start and end point and combine with inner results
    dfLt <- dfLt %>%
      left_join(dfLtK, by = intersect(colnames(dfLt), colnames(dfLtK))) %>%
      mutate(value = (1 / (.data[["tIn1"]] - .data[["tIn0"]]) * .data[["step"]]
                      * (.weibullIntegrand(.data[["tIn0"]], .data[["shape"]], .data[["scale"]], .data[["tOut0"]], .data[["tOut1"]]) / 2
                         + .data[["sumFunc"]]
                         + .weibullIntegrand(.data[["tIn1"]], .data[["shape"]], .data[["scale"]], .data[["tOut0"]], .data[["tOut1"]]) / 2))) %>%
      select(-"tIn0", -"tIn1", -"tOut0", -"tOut1", -"step", -"shape", -"scale", -"sumFunc")
  } else {
    dfLt <- dfLt %>%
      group_by(across(everything())) %>%
      mutate(value = 1 / (.data[["tIn1"]] - .data[["tIn0"]])
                     * integral(.weibullIntegrand, unique(.data[["tIn0"]]), unique(.data[["tIn1"]]),
                              shape = .data[["shape"]], scale = .data[["scale"]],
                              tOut0 = .data[["tOut0"]], tOut1 = .data[["tOut1"]])) %>%
      ungroup() %>%
      select(-"tIn0", -"tIn1", -"tOut0", -"tOut1", -"shape", -"scale")
  }

  return(dfLt)
}

.weibullIntegrand <- function(x, shape, scale, tOut0, tOut1) {
  pweibull(tOut1 - x, shape, scale) - pweibull(tOut0 - x, shape, scale)
}

#' Compute the ex-ante lifetime by a simple approximation formula
#'
#' @param dfLt data frame, needs to contain input dimensions, each with intervals
#'   for all time periods and with the Weibull paramters
#' @param dims character, dimensions of the data, without time steps
#' @param t0 numeric, initial time period
#' @param standingLifetime numeric, assumed prior standing lifetime
#' @param cutOffShare numeric, share at which the distribution function jumps to 1
#'
#' @importFrom dplyr %>% across all_of .data group_by mutate select ungroup
#' @importFrom stats pweibull
#'
.computeLtAnteSimple <- function(dfLt, dims, t0, standingLifetime = 0, cutOffShare = 0.95) {

  dfLt <- dfLt %>%
    select(-"tIn0", -"tIn1", -"tOut0", -"tOut1") %>%
    mutate(lt = .data[["ttot2"]] - .data[["ttot"]] + .data[["dt"]] / 2
           + standingLifetime,
           value = pweibull(.data[["lt"]], .data[["shape"]], .data[["scale"]])) %>%
    mutate(value = ifelse(.data[["value"]] > cutOffShare, 1, .data[["value"]]))
  dfLt <- dfLt %>%
    group_by(across(all_of(c(dims, "ttot")))) %>%
    mutate(value = c(.data[["value"]][[1]], diff(.data[["value"]])))
  dfLt <- dfLt %>%
    mutate(value = ifelse(.data[["ttot2"]] == t0,
                          .data[["value"]] - pweibull(standingLifetime, .data[["shape"]], .data[["scale"]]),
                          .data[["value"]])) %>%
    ungroup() %>%
    select(-"shape", -"scale", -"lt")

  return(dfLt)

}
