#' Compute life cycle costs and logit share
#'
#' @author Ricarda Rosemann
#' @importFrom dplyr %>% filter full_join group_by last left_join mutate right_join select summarise
#' @importFrom stats pweibull
#' @importFrom tidyr crossing replace_na

library(dplyr)
library(tidyr)

#TODO: Proper handling of hs vs hsr
#TODO: Proper handling of building shell!!! (Ignored for the time being)
reportLCCShare <- function(gdx, pathLt = NULL) {

  if (is.null(pathLt)) pathLt <- "C:/Users/ricardar/Documents/PIAM/brick/inst/input/f_lifetimeHeatingSystem.cs4r"
  path <- dirname(gdx)

  # READ IN DATA ---------------------------------------------------------------

  # Load time periods and model resolution
  ttot <- readGdxSymbol(gdx, "ttot", asMagpie = FALSE)[["tall"]]
  ttot <- as.numeric(levels(ttot))[ttot]
  t0 <- min(ttot)
  tRun <- setdiff(ttot, t0)
  ttotNum <- ttot

  # Load info on time period and vintage
  p_dtVin <- readGdxSymbol(gdx, "p_dtVin", asMagpie = FALSE)

  p_dtVin <- p_dtVin %>%
    group_by(ttot) %>%
    summarise(vin = dplyr::last(.data[["vin"]]), dt = dplyr::last(.data[["value"]]),
              .groups = "drop") %>%
    mutate(ttot = as.numeric(levels(.data[["ttot"]]))[.data[["ttot"]]])
  p_dt <- select(p_dtVin, -"vin")
  p_ttotVin <- select(p_dtVin, -"dt")
  vinExists <- readGdxSymbol(gdx, "vinExists", asMagpie = FALSE)
  renAllowed <- readGdxSymbol(gdx, "renAllowed", asMagpie = FALSE)

  # Load stocks and flows
  v_stock <- .cleanReadGdx(readGdxSymbol(gdx, "v_stock", asMagpie = FALSE), vinExists)
  v_construction <- .cleanReadGdx(readGdxSymbol(gdx, "v_construction", asMagpie = FALSE), vinExists)
  v_renovation <- .cleanReadGdx(readGdxSymbol(gdx, "v_renovation", asMagpie = FALSE), vinExists, renAllowed)
  v_demolition <- .cleanReadGdx(readGdxSymbol(gdx, "v_demolition", asMagpie = FALSE), vinExists)

  dims <- setdiff(colnames(v_stock), c("ttot", "value"))

  # Load cost components
  p_specCostOpe <- readGdxSymbol(gdx, "p_specCostOpe", asMagpie = FALSE)
  p_specCostCon <- readGdxSymbol(gdx, "p_specCostCon", asMagpie = FALSE)
  p_specCostRen <- readGdxSymbol(gdx, "p_specCostRen", asMagpie = FALSE)
  p_specCostDem <- readGdxSymbol(gdx, "p_specCostDem", asMagpie = FALSE)

  # Load parameters (Discount rate, price sensitivity, Weibull parameters)
  p_discountFac <- readGdxSymbol(gdx, "p_discountFac", asMagpie = FALSE)
  priceSensBs <- readGdxSymbol(gdx, "priceSensBs", asMagpie = FALSE)
  priceSensHs <- readGdxSymbol(gdx, "priceSensHs", asMagpie = FALSE)
  # Weibull parameters have to be read from cs4r!
  lifeTimeHs <- read.csv(pathLt, header = FALSE, comment.char = "*")
  colnames(lifeTimeHs) <- c("reg", "typ", "hs", "variable", "value")

  # TODO: Write the filtering properly
  lifeTimeHs <- pivot_wider(lifeTimeHs, names_from = "variable", values_from = "value") %>%
    filter(.data[["reg"]] %in% levels(v_construction[["reg"]]),
           .data[["typ"]] %in% levels(v_construction[["typ"]])) %>%
    mutate(across(where(is.character), as.factor))

  # Initialize output
  out <- list()

  # IN- AND OUTFLOW COMPUTATION ------------------------------------------------

  # Attribute construction to vintages
  conVin <- v_construction %>%
    left_join(p_dtVin,
              by = intersect(colnames(v_construction), colnames(p_dtVin)))


  #TODO: Temporary, should handle bsr properly eventually
  # Compute renovation in- and outflow
  v_renovationOut <- v_renovation %>%
    filter(hsr != "0") %>%
    group_by(across(-all_of(c("hsr", "bsr", "value")))) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    left_join(p_dt, by = "ttot")

  v_renovationIn <- v_renovation %>%
    filter(hsr != "0") %>%
    group_by(across(-all_of(c("hs", "bs", "value")))) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    rename(hs = "hsr", bs = "bsr") %>%
    left_join(p_dt, by = "ttot")

  v_demolitionOut <- left_join(v_demolition, p_dt, by = "ttot")

  # Compute total in- and outflow
  outflow <- v_renovationOut %>%
    rename(ren = "value") %>%
    full_join(v_demolitionOut %>%
                rename(dem = "value"),
              by = c(dims, "ttot", "dt")) %>%
    mutate(value = ren + dem) %>%
    select(-"ren", -"dem")

  inflow <- v_renovationIn %>%
    rename(ren = "value") %>%
    full_join(conVin %>%
                rename(con = "value"),
              by = c(dims, "ttot", "dt")) %>%
    replace_na(list(con = 0)) %>%
    mutate(value = .data[["con"]] + .data[["ren"]],
           share = ifelse(.data[["value"]] != 0, .data[["con"]] / .data[["value"]], 0)) %>%
    select(-"con", -"ren")

  conShare <- select(inflow, -"value", -"dt")
  renShare <- mutate(conShare, share = 1 - .data[["share"]])
  inflow <- select(inflow, -"share")

  # Compute initial stock
  v_stockInit <- v_stock %>%
    filter(ttot == t0)


  # COMPUTE EX-ANTE LIFETIME PROBABILITIES -------------------------------------

  # Assemble in and out time periods and compute starting and end point
  times <- expand.grid(ttot = ttot, ttot2 = ttot) %>%
    filter(.data[["ttot2"]] >= .data[["ttot"]]) %>%
    group_by(ttot2) %>%
    mutate(tIn0 = .extractInitTime(.data[["ttot"]]),
           tIn1 = .data[["ttot"]]) %>%
    ungroup() %>%
    group_by(ttot) %>%
    mutate(tOut0 = .extractInitTime(.data[["ttot2"]], ttotNum = ttotNum),
           tOut1 = .data[["ttot2"]])

  # Compute life time probabilities of initial stock
  stockInitLtAnte <- v_stockInit %>%
    select(-"value") %>%
    crossing(ttot2 = ttotNum[-1]) %>%
    left_join(times, by = c("ttot", "ttot2")) %>%
    select(-"tIn0", -"tIn1") %>%
    left_join(lifeTimeHs, by = c("reg", "typ", "hs"))

  stockInitLtAnte <- .computeLtStock(stockInitLtAnte) %>%
    rename(relVal = "value") %>%
    left_join(v_stock, by = c(dims, "ttot")) %>%
    mutate(absVal = .data[["value"]] * .data[["relVal"]]) %>%
    select(-"value")

  out[["stockInitLtAnte"]] <- stockInitLtAnte

  # Compute lifetime probabilities in newly constructed buildings
  conLtAnte <- v_construction %>%
    select(-"value") %>%
    filter(.data[["ttot"]] != ttotNum[1]) %>%
    left_join(times, by = "ttot") %>%
    left_join(lifeTimeHs, by = c("reg", "typ", "hs"))

  conLtAnte <- .computeLtAnte(conLtAnte, .weibullIntegrand, 4) %>%
    left_join(p_ttotVin, by = "ttot") %>%
    relocate("vin", .before = "reg") %>%
    rename(relVal = "value") %>%
    left_join(v_construction, by = c(setdiff(dims, "vin"), "ttot")) %>%
    mutate(absVal = .data[["value"]] * .data[["relVal"]]) %>%
    select(-"value")

  out[["conLtAnte"]] <- conLtAnte

  # Compute lifetime probabilities of renovations
  renLtAnte <- v_renovation %>%
    select(-"value") %>%
    filter(.data[["hsr"]] != "0", .data[["ttot"]] != ttotNum[1]) %>%
    left_join(times, by = "ttot") %>%
    left_join(lifeTimeHs %>% rename(hsr = "hs"), by = c("reg", "typ", "hsr"))

  renLtAnte <- .computeLtAnte(renLtAnte, .weibullIntegrand, 4) %>%
    group_by(across(-all_of(c("hs", "value")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    select(-"bs") %>%
    rename(bs = "bsr", hs = "hsr", relVal = "value") %>%
    left_join(v_renovationIn %>%
                select(-"dt"),
              by = c(dims, "ttot")) %>%
    mutate(absVal = .data[["value"]] * .data[["relVal"]]) %>%
    select(-"value")

  out[["renLtAnte"]] <- renLtAnte


  # COMPUTE EX-POST LIFETIME PROBABILITIES -------------------------------------

  # Initialize data frames to collect leave times of initial stock and inflows
  inAll <- inflow %>%
    crossing(ttot2 = ttotNum[-1]) %>%
    filter(.data[["ttot2"]] >= .data[["ttot"]], .data[["ttot"]] != t0) %>%
    mutate(value = 0)

  stockAll <- v_stockInit %>%
    select(-"value") %>%
    crossing(ttot2 = ttotNum) %>%
    mutate(value = 0)

  # Compute leave times of initial stock and inflows
  for (t2 in tRun) {
    stockThis <- .computeLeaveInitStock(t2, v_stockInit, stockAll, outflow, rprt = dims)
    stockAll <- stockAll %>%
      left_join(stockThis, by = c(dims, "ttot", "ttot2")) %>%
      mutate(value = ifelse(.data[["ttot2"]] == t2, .data[["value.y"]], .data[["value.x"]])) %>%
      select(-"value.x", -"value.y")

    for (t1 in tRun[tRun <= t2]) {
      inThis <- .computeLeaveInFlow(t1, t2, inAll, stockAll, inflow, outflow, rprt = dims)
      inAll <- inAll %>%
        left_join(inThis, by = c(dims, "ttot", "ttot2", "dt")) %>%
        mutate(value = ifelse(.data[["ttot"]] == t1 & .data[["ttot2"]] == t2,
                              .data[["value.y"]], .data[["value.x"]])) %>%
        select(-"value.x", -"value.y")
    }
  }

  # Process leave time results and separate construction and renovation
  stockAll <- stockAll %>%
    filter(.data[["ttot2"]] != t0) %>%
    mutate(ttot = t0, .before = "ttot2")

  conAll <- inAll %>%
    select(-"dt") %>%
    left_join(conShare, by = c(dims, "ttot")) %>%
    mutate(value = .data[["value"]] * .data[["share"]])

  renAll <- inAll %>%
    select(-"dt") %>%
    left_join(conShare, by = c(dims, "ttot")) %>%
    mutate(value = .data[["value"]] * (1 - .data[["share"]]))

  # Ex-post life time of initial stock
  out[["stockInitLtPost"]] <- stockAll %>%
    rename(absVal = "value") %>%
    left_join(v_stock, by = c(dims, "ttot")) %>%
    mutate(relVal = .data[["absVal"]] / .data[["value"]]) %>%
    select(-"value")

  # Ex-post lifetime in newly constructed buildings
  out[["conLtPost"]] <- conAll %>%
    dplyr::right_join(p_ttotVin %>%
                        filter(.data[["ttot"]] != t0),
                      by = c("ttot", "vin")) %>%
    select(-"share") %>%
    rename(absVal = "value") %>%
    left_join(v_construction, by = c(setdiff(dims, "vin"), "ttot")) %>%
    mutate(relVal = .data[["absVal"]] / .data[["value"]]) %>%
    select(-"value")

  # Test that for construction all entries with non-matching vintage are zero
  conTest <- conAll %>%
    dplyr::anti_join(p_dtVin, by = c("ttot", "vin"))
  if (any(conTest[["value"]] > 0)) {
    message("Ex-post lifetime probabilites of construction are implausible: ",
            "Non-zero entries for vintages that do not match the given time period.")
  }

  # Ex-post lifetime in renovations
  out[["renLtPost"]] <- renAll %>%
    select(-"share") %>%
    rename(absVal = "value") %>%
    left_join(v_renovationIn %>%
                select(-"dt"),
              by = c(dims, "ttot")) %>%
    mutate(relVal = .data[["absVal"]] / .data[["value"]]) %>%
    select(-"value")

  # COMPUTE LIFETIMES MATCHING EX-ANTE TO BRICK OUTFLOW ------------------------

  outAnte <- .prepareLtAnte(stockInitLtAnte, c(stock = "absVal")) %>%
    full_join(.prepareLtAnte(conLtAnte, c(con = "absVal"), sumTtot = TRUE), by = c(dims, "ttot2")) %>%
    full_join(.prepareLtAnte(renLtAnte, c(ren = "absVal"), sumTtot = TRUE), by = c(dims, "ttot2")) %>%
    replace_na(list(stock = 0, con = 0)) %>%
    mutate(value = .data[["stock"]] + .data[["con"]] + .data[["ren"]]) %>%
    select(-"stock", -"con", -"ren")

  stockInitLtDirect <- .computeLtDirect(stockInitLtAnte, outAnte, outflow, dims)
  conLtDirect <- .computeLtDirect(conLtAnte, outAnte, outflow, dims)
  renLtDirect <- .computeLtDirect(renLtAnte, outAnte, outflow, dims)

  # Compute leave times of initial stock and inflows
  # add results column to all DFs and fill with zero
  stockInitLtMixed <- .prepareLtMixed(stockInitLtDirect)
  conLtMixed <- .prepareLtMixed(conLtDirect)
  renLtMixed <- .prepareLtMixed(renLtDirect)

  for (t2 in tRun) {
    # Compute unattributed outflow
    stockInitLtMixed <- .computeUnattrOut(stockInitLtMixed, v_stockInit, t2, dims)
    conLtMixed <- .computeUnattrOut(conLtMixed, v_construction, t2, setdiff(dims, "vin"))
    renLtMixed <- .computeUnattrOut(renLtMixed, select(v_renovationIn, -"dt"), t2, dims)

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
    stockInitLtMixed <- .addResults(stockInitLtMixed, stockThis, t0, t2, dims, addDims = c("directVal", "remInflow", "unattr"))

    #TODO: Do all this properly! -> how to disentangle con and ren?
    for (t1 in tRun[tRun <= t2]) {
      # Compute lifetimes for construction and renovation flow
      conThis <- .computeLtMixed(conLtMixed, unattrTot, t1, t2, dims, dfStock = stockThis, dfShare = conShare)
      renThis <- .computeLtMixed(renLtMixed, unattrTot, t1, t2, dims, dfStock = stockThis, dfShare = renShare)

      conLtMixed <- .addResults(conLtMixed, .combineAddAttr(conThis, renThis, dims), t1, t2, dims,
                                addDims = c("directVal", "remInflow", "unattr"))
      renLtMixed <- .addResults(renLtMixed, .combineAddAttr(renThis, conThis, dims), t1, t2, dims,
                                addDims = c("directVal", "remInflow", "unattr"))
    }
  }

  out[["stockInitLtMixed"]] <- .processLtMixed(stockInitLtMixed, v_stockInit, dims)
  out[["conLtMixed"]] <- .processLtMixed(conLtMixed, v_construction, dims, removeVin = TRUE)
  out[["renLtMixed"]] <- .processLtMixed(renLtMixed, v_renovationIn, dims)


  # SAVE OUTFLOWS --------------------------------------------------------------

  out[["outflow"]] <- outflow %>%
    select(-"dt") %>%
    rename(ttot2 = "ttot") %>%
    mutate(ttot = "None", relVal = .data[["value"]], absVal = .data[["value"]]) %>%
    select(-"value")

  # Compute ex-ante LCC and LCOH

  # Compute ex-post LCC and LCOH

  # Compute logit share

  # Compute model share

  # WRITE ----------------------------------------------------------------------

  #(Partial) code duplicate of reportCalibration
  browser()
  # namesAll <- c("stockInitLtAnte", "stockInitLtPost", "stockInitLtMixed", "conLtAnte", "conLtPost", "conLtMixed", "renLtAnte", "renLtPost", "renLtMixed")
  # namesIndex <- expand.grid(sa = c(TRUE,FALSE), sp = c(TRUE,FALSE), sm = c(TRUE,FALSE), ca = c(TRUE,FALSE), cp = c(TRUE,FALSE), cm = c(TRUE,FALSE), ra = c(TRUE,FALSE), rp = c(TRUE,FALSE), rm = c(TRUE,FALSE))
  # for (i in seq_len(nrow(namesIndex))) {
  #   print(namesAll[as.vector(t(namesIndex[i, ]))])
  #   if (length(namesAll[as.vector(t(namesIndex[i, ]))]) > 1) {
  #     out <- do.call(rbind, lapply(namesAll[as.vector(t(namesIndex[i, ]))], function(varName) {
  #       mutate(out[[varName]], variable = varName, .before = 1)
  #     }))
  #   }
  # }
  out <- do.call(rbind, lapply(names(out), function(varName) {
    mutate(out[[varName]], variable = varName, .before = 1)
  }))

  write.csv(out, file.path(path, "BRICK_analysis_report.csv"), row.names = FALSE)

}

.cleanReadGdx <- function(df, vinExists, renAllowed = NULL, bsToZero = TRUE) {

  if ("qty" %in% colnames(df)) {
    df <- df %>%
      filter(.data[["qty"]] == "area")
  }

  if (all(c("vin", "ttot") %in% colnames(df))) {
    df <- df %>%
      dplyr::right_join(vinExists, by = c("vin", "ttot"))
  }

  if ("ttot" %in% colnames(df)) {
    df <- df %>%
      mutate(ttot = as.numeric(levels(.data[["ttot"]]))[.data[["ttot"]]])
  }

  if (all(c("hs", "hsr", "bs", "bsr") %in% colnames(df)) && !is.null(renAllowed)) {
    df <- df %>%
      dplyr::right_join(renAllowed, by = c("hs", "hsr", "bs", "bsr"))
  }

  df %>%
    mutate(across(any_of(c("bs", "bsr")), ~ "0"))
}

#' Determine leave time of initial stock
#'
.computeLeaveInitStock <- function(tOut, dfStockInit, dfStock, dfOut, rprt) {

  dfStock <- dfStock %>%
    filter(.data[["ttot2"]] < tOut) %>%
    group_by(across(any_of(rprt))) %>%
    summarise(valueCumSum = sum(value), .groups = "drop") %>%
    left_join(dfOut %>%
                rename(valueOut = "value", dtOut = "dt", ttot2 = "ttot") %>%
                filter(.data[["ttot2"]] == tOut),
              by = rprt) %>%
    left_join(dfStockInit %>%
                rename(valueTot = "value"),
              by = rprt) %>%
    mutate(value = pmin(.data[["dtOut"]] * .data[["valueOut"]], .data[["valueTot"]] - .data[["valueCumSum"]]))
  dfStock <- dfStock %>%
    select(-"valueOut", -"valueTot", -"valueCumSum", -"dtOut")
}

#' Determine leave time of inflows
#'
.computeLeaveInFlow <- function(tIn, tOut, dfInAll, dfStock, dfIn, dfOut, rprt) {

  dfInPrev <- dfInAll %>%
    filter(.data[["ttot"]] == tIn, .data[["ttot2"]] >= tIn, .data[["ttot2"]] <= tOut) %>%
    group_by(across(any_of(rprt))) %>%
    summarise(prevCumSum = sum(.data[["dt"]] * .data[["value"]]), .groups = "drop")

  dfInAll <- dfInAll %>%
    filter(.data[["ttot"]] <= tIn, .data[["ttot2"]] == tOut)
  dfInAll <- dfInAll %>%
    group_by(across(any_of(rprt))) %>%
    summarise(valueCumSum = sum(.data[["dt"]] * .data[["value"]]), .groups = "drop")
  dfInAll <- dfInAll %>%
    left_join(dfInPrev, by = rprt)
  dfInAll <- dfInAll %>%
    left_join(dfOut %>%
                rename(valueOut = "value", ttot2 = "ttot", dtOut = "dt") %>%
                filter(.data[["ttot2"]] == tOut),
              by = rprt)
  dfInAll <- dfInAll %>%
    dplyr::right_join(dfIn %>%
                rename(valueIn = "value") %>%
                filter(.data[["ttot"]] == tIn),
              by = rprt)
  dfInAll <- dfInAll %>%
    left_join(dfStock %>%
                rename(valueStock = "value") %>%
                filter(.data[["ttot2"]] == tOut),
              by = c(rprt, "ttot", "ttot2")) %>%
    replace_na(list(valueStock = 0))
  dfInAll <- dfInAll %>%
    mutate(value = pmin(.data[["dtOut"]] * .data[["valueOut"]] - .data[["valueStock"]] - .data[["valueCumSum"]],
                        .data[["dt"]] * .data[["valueIn"]] - .data[["prevCumSum"]]) / .data[["dt"]])
  dfInAll <- dfInAll %>%
    select(-"valueOut", -"valueIn", -"valueStock", -"valueCumSum", -"prevCumSum", -"dtOut")
}

.extractInitTime <- function(ttot, ttotNum = NULL) {
  if (!is.null(ttotNum) && ttot[1] != ttotNum[1]) {
    firstVal <- ttotNum[which(ttotNum == ttot[1]) - 1]
  } else {
    firstVal <- ttot[1]
  }
  t <- c(firstVal, ttot[1:length(ttot)-1])
  return(t)
}

.computeLtStock <- function(dfLt, standingLifetime = 12) {
  dfLt %>%
    mutate(value = (pweibull(.data[["tOut1"]] - .data[["ttot"]] + standingLifetime, .data[["shape"]], .data[["scale"]])
                    - pweibull(.data[["tOut0"]] - .data[["ttot"]] + standingLifetime, .data[["shape"]], .data[["scale"]]))) %>%
    select(-"tOut0", -"tOut1", -"shape", -"scale")
}

# TODO: Flexible integration limits not needed right now, as the function arguments aren't flexible.
# TODO: Arguments to function look really messy, maybe this can be cleared up
.computeLtAnte <- function(dfLt, func, n, a = "tIn0", b = "tIn1") {

  dfLtK <- dfLt %>%
    mutate(step = (.data[[b]] - .data[[a]]) / n) %>%
    crossing(k = seq(1, n-1)) %>%
    group_by(across(-all_of("k"))) %>%
    summarise(sumFunc = sum(func(.data[[a]] + .data[["k"]] * .data[["step"]],
                                 .data[["shape"]], .data[["scale"]], .data[["tOut0"]], .data[["tOut1"]])),
              .groups = "drop")

  dfLt %>%
    left_join(dfLtK, by = intersect(colnames(dfLt), colnames(dfLtK))) %>%
    mutate(value = (1 / (.data[["tIn1"]] - .data[["tIn0"]]) * .data[["step"]]
                    * (func(.data[[a]], .data[["shape"]], .data[["scale"]], .data[["tOut0"]], .data[["tOut1"]]) / 2
                       + .data[["sumFunc"]]
                       + func(.data[[b]], .data[["shape"]], .data[["scale"]], .data[["tOut0"]], .data[["tOut1"]]) / 2))) %>%
    select(-"tIn0", -"tIn1", -"tOut0", -"tOut1", -"step", -"shape", -"scale", -"sumFunc")
}

.weibullIntegrand <- function(x, shape, scale, tOut0, tOut1) {
  pweibull(tOut1 - x, shape, scale) - pweibull(tOut0 - x, shape, scale)
}

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
    mutate(value = .data[["absVal"]] * .data[["outflow"]] / .data[["outAnte"]]) %>%
    select(-"absVal", -"outflow", -"outAnte")


}

.prepareLtMixed <- function(dfLtDirect) {

  dfLtDirect %>%
    rename(directVal = "value") %>%
    mutate(addAttr = 0, value = 0)
}

.computeUnattrOut <- function(dfLtMixed, dfTotVal, tOut, dims) {

  dfLtMixed %>%
    filter(.data[["ttot2"]] <= tOut) %>%
    group_by(across(any_of(c(dims, "ttot")))) %>%
    summarise(sumVal = sum(.data[["value"]]), .groups = "drop") %>%
    right_join(dfLtMixed, by = c(dims, "ttot")) %>%
    left_join(dfTotVal %>%
                rename(totVal = "value"),
              by = c(dims, "ttot")) %>%
    mutate(remInflow =  .data[["totVal"]] - .data[["sumVal"]],
           unattr = pmax(.data[["directVal"]] - .data[["remInflow"]], 0)) %>%
    select(-"totVal", -"sumVal")

}

.computeLtMixed <- function(dfLtMixed, unattrTot, tIn, tOut, dims, dfStock = NULL, dfShare = NULL) {

  dfLtMixed <- filter(dfLtMixed, .data[["ttot2"]] == tOut)

  if (!is.null(dfStock)) {

    # Isolate additional attribution in stock data frame and filter for current tOut
    dfStock <- dfStock %>%
      rename(stockAttr = "addAttr") %>%
      select(all_of(c(dims, "ttot2", "stockAttr")))

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

  dfLtMixed <- dfLtMixed %>%
    left_join(unattrTot, by = c(dims, "ttot2")) %>%
    mutate(maxOutflow = if (is.null(dfStock)) .data[["directVal"]] + .data[["sumUnattr"]]
                        else .data[["directVal"]] + .data[["share"]] * (.data[["sumUnattr"]] - .data[["stockAttr"]] - .data[["addAttrSum"]]),
           value = pmin(.data[["maxOutflow"]], .data[["remInflow"]]),
           addAttr = pmax(.data[["value"]] - .data[["directVal"]], 0)) %>%
    select(-any_of(c("maxOutflow", "sumUnattr", "addAttrSum", "stockAttr", "share")))

  return(dfLtMixed)

}

.addResults <- function(dfOrig, dfNew, tIn, tOut, dims, addDims = NULL) {

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

.processLtMixed <- function(dfLt, dfVal, dims, removeVin = FALSE) {

  if (isTRUE(removeVin)) dims <- setdiff(dims, "vin")

  dfLt %>%
    select(-any_of(c("directVal", "remInflow", "unattr", "addAttr"))) %>%
    rename(absVal = "value") %>%
    left_join(dfVal, by = c(dims, "ttot")) %>%
    mutate(relVal = .data[["absVal"]] / .data[["value"]]) %>%
    select(-any_of(c("dt", "value")))

}
