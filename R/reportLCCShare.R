#' Compute life cycle costs and logit share
#'
#' @author Ricarda Rosemann
#' @importFrom dplyr %>% .data filter full_join group_by inner_join last left_join mutate rename right_join select summarise
#' @importFrom piamutils getSystemFile
#' @importFrom stats pweibull
#' @importFrom tidyr crossing pivot_wider replace_na
#' @importFrom utils read.csv write.csv

#TODO: Proper handling of hs vs hsr
#TODO: Proper handling of building shell!!! (Ignored for the time being)

reportLCCShare <- function(gdx, pathLt = NULL) {

  if (is.null(pathLt)) pathLt <- "C:/Users/ricardar/Documents/PIAM/brick/inst/input/f_lifetimeHeatingSystem.cs4r"
  pathHs <- getSystemFile("extdata", "sectoral", "heatingSystem.csv",
                          package = "brick", mustWork = TRUE)
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
  p_dt <- readGdxSymbol(gdx, "p_dt", asMagpie = FALSE) %>%
    rename(dt = "value") %>%
    mutate(ttot = as.numeric(levels(.data[["ttot"]]))[.data[["ttot"]]])

  p_dtVin <- p_dtVin %>%
    group_by(ttot) %>%
    summarise(vin = dplyr::last(.data[["vin"]]), dt = sum(.data[["value"]]),
              .groups = "drop") %>%
    mutate(ttot = as.numeric(levels(.data[["ttot"]]))[.data[["ttot"]]])
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
  p_specCostOpe <- readGdxSymbol(gdx, "p_specCostOpe", asMagpie = FALSE) %>%
    mutate(ttot = as.numeric(levels(.data[["ttot"]]))[.data[["ttot"]]])
  p_specCostCon <- readGdxSymbol(gdx, "p_specCostCon", asMagpie = FALSE) %>%
    mutate(ttot = as.numeric(levels(.data[["ttot"]]))[.data[["ttot"]]])
  p_specCostRen <- readGdxSymbol(gdx, "p_specCostRen", asMagpie = FALSE) %>%
    mutate(ttot = as.numeric(levels(.data[["ttot"]]))[.data[["ttot"]]])
  p_specCostDem <- readGdxSymbol(gdx, "p_specCostDem", asMagpie = FALSE)

  # Load parameters (Discount rate, price sensitivity, UE demand, Weibull parameters)
  p_discountFac <- readGdxSymbol(gdx, "p_discountFac", asMagpie = FALSE) %>%
    mutate(ttot = as.numeric(levels(.data[["ttot"]]))[.data[["ttot"]]])
  priceSensBs <- readGdxSymbol(gdx, "priceSensBs", asMagpie = FALSE)
  priceSensHs <- readGdxSymbol(gdx, "priceSensHs", asMagpie = FALSE) %>%
    pivot_wider(names_from = "var", values_from = "value") %>%
    unlist()
  p_ueDemand <- readGdxSymbol(gdx, "p_ueDemand", asMagpie = FALSE)
  # Weibull parameters have to be read from cs4r!
  lifeTimeHs <- read.csv(pathLt, header = FALSE, comment.char = "*")
  colnames(lifeTimeHs) <- c("reg", "typ", "hs", "variable", "value")
  # Energy ladder specifications have to be read from csv
  energyLadder <- read.csv(pathHs) %>%
    select("hs", "energyLadder")

  # TODO: Write the filtering properly
  lifeTimeHs <- pivot_wider(lifeTimeHs, names_from = "variable", values_from = "value") %>%
    filter(.data[["reg"]] %in% levels(v_construction[["reg"]]),
           .data[["typ"]] %in% levels(v_construction[["typ"]])) %>%
    mutate(across(where(is.character), as.factor))

  # Initialize output
  out <- list()

  # IN- AND OUTFLOW COMPUTATION ------------------------------------------------

  # Compute total construction by time period and attribute construction to vintages
  conVin <- v_construction %>%
    left_join(p_dtVin,
              by = intersect(colnames(v_construction), colnames(p_dtVin))) %>%
    mutate(value = .data[["value"]] * .data[["dt"]])

  v_constructionIn <- select(conVin, -"vin", -"dt")


  #TODO: Temporary, should handle bsr properly eventually
  # Compute renovation in- and outflow
  v_renovationOut <- v_renovation %>%
    filter(hsr != "0") %>%
    group_by(across(-all_of(c("hsr", "bsr", "value")))) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    left_join(p_dt, by = "ttot") %>%
    mutate(value = .data[["value"]] * .data[["dt"]])

  v_renovationIn <- v_renovation %>%
    filter(hsr != "0") %>%
    group_by(across(-all_of(c("hs", "bs", "value")))) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    rename(hs = "hsr", bs = "bsr") %>%
    left_join(p_dt, by = "ttot") %>%
    mutate(value = .data[["value"]] * .data[["dt"]])

  v_demolitionOut <- v_demolition %>%
    left_join(p_dt, by = "ttot") %>%
    mutate(value = .data[["value"]] * .data[["dt"]])

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

  # Compute life time probabilities of initial stock
  stockInitLtAnte <- computeLtAnte("stock", v_stockInit, ttotNum, lifeTimeHs, dims, p_dt = p_dt)
  conLtAnte <- computeLtAnte("construction", conVin, ttotNum, lifeTimeHs, dims)
  renLtAnte <- computeLtAnte("renovation", v_renovation, ttotNum, lifeTimeHs, dims,
                                      dataValue = v_renovationIn)

  out[["stockInitLtAnte"]] <- stockInitLtAnte
  out[["conLtAnte"]] <- conLtAnte
  out[["renLtAnte"]] <- renLtAnte

  checkLtAnte <- .checkLifetimeResults(renLtAnte, dims)
  checkLtAnteInitStock <- .checkLifetimeResults(stockInitLtAnte, dims)


  # COMPUTE EX-ANTE LIFETIME PROBABILITIES  (SIMPLE METHOD) --------------------

  # Compute life time probabilities of initial stock
  stockInitLtAnteS <- computeLtAnte("stock", v_stockInit, ttotNum, lifeTimeHs, dims,
                                   runSimple = TRUE, p_dt = p_dt)
  conLtAnteS <- computeLtAnte("construction", conVin, ttotNum, lifeTimeHs, dims,
                             runSimple = TRUE, p_dt = p_dt)
  renLtAnteS <- computeLtAnte("renovation", v_renovation, ttotNum, lifeTimeHs, dims,
                             runSimple = TRUE, p_dt = p_dt, dataValue = v_renovationIn)

  out[["stockInitLtAnteS"]] <- stockInitLtAnteS
  out[["conLtAnteS"]] <- conLtAnteS
  out[["renLtAnteS"]] <- renLtAnteS

  checkLtAnteS <- .checkLifetimeResults(renLtAnteS, dims)
  checkLtAnteS <- .checkLifetimeResults(stockInitLtAnteS, dims)


  # COMPUTE EX-POST LIFETIME PROBABILITIES -------------------------------------

  ltPost <- computeLtPost(
    inflow,
    outflow,
    list(stock = v_stockInit, construction = v_construction, renovation = v_renovationIn),
    conShare,
    p_ttotVin,
    ttotNum,
    dims
  )

  checkLtPost <- .checkLifetimeResults(ltPost[["stockInitLtPost"]], dims)

  out <- c(out, ltPost)

  # COMPUTE LIFETIMES MATCHING EX-ANTE TO BRICK OUTFLOW ------------------------

  ltMixed <- computeLtMixed(
    list(stock = stockInitLtAnte, construction = conLtAnte, renovation = renLtAnte),
    outflow,
    list(stock = v_stockInit, construction = v_constructionIn, renovation = v_renovationIn),
    conShare,
    ttotNum,
    dims
  )

  checkRenLtMixed <- .checkLifetimeResults(ltMixed[["renLtMixed"]], dims)
  checkStockLtMixed <- .checkLifetimeResults(ltMixed[["stockInitLtMixed"]], dims)
  checkConLtMixed <- .checkLifetimeResults(ltMixed[["conLtMixed"]], dims)

  out <- c(out, ltMixed)

  # SAVE OUTFLOWS --------------------------------------------------------------

  out[["outflow"]] <- outflow %>%
    select(-"dt") %>%
    rename(ttot2 = "ttot")

  # VERIFY THE LIFETIME INEQUALITY ---------------------------------------------

  ltIneq <- verifyLtHs(gdx, inflow, v_stockInit, outflow, lifeTimeHs, dims)

  out <- c(out, ltIneq)

  # COMPUTE LCC AND LCOH -------------------------------------------------------

  costCon <- p_specCostCon %>%
    pivot_wider(names_from = "cost", values_from = "value") %>%
    left_join(p_ttotVin, by = "ttot") %>%
    replace_na(list(intangible = 0))
  #TODO: Add proper handling to restore zeros from gdx

  costRen <- p_specCostRen %>%
    inner_join(renAllowed, by = c("bs", "hs", "bsr", "hsr")) %>%
    filter(.data[["bsr"]] == 0) %>%
    # group_by(across(-all_of(c("bs", "hs", "value")))) %>%
    # summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    # rename(bs = "bsr", hs = "hsr") %>%
    pivot_wider(names_from = cost, values_from = value) %>% replace_na(list(intangible = 0))

  # Compute ex-ante LCC and LCOH
  out[["conLccAnte"]] <- computeLCC(out[["conLtAnte"]], p_specCostOpe, costCon, p_dt, p_discountFac)
  out[["conLcohAnte"]] <- computeLCOH(out[["conLccAnte"]], out[["conLtAnte"]], p_ueDemand, p_dt, p_discountFac, dims)

  renLccAnteFull <- computeLCC(out[["renLtAnte"]], p_specCostOpe, costRen, p_dt, p_discountFac)
  out[["renLccAnte"]] <- mutate(
    .avgAlongDim(renLccAnteFull, c("bs", "hs"), hs = "hsr", bs = "bsr"),
    bs = "low"
  )
  out[["renLcohAnte"]] <- computeLCOH(out[["renLccAnte"]], out[["renLtAnte"]], p_ueDemand, p_dt, p_discountFac, dims)

  # Compute ex-post LCC and LCOH

  # Compute mixed LCC and LCOH
  out[["conLccMixed"]] <- computeLCC(out[["conLtMixed"]], p_specCostOpe, costCon, p_dt, p_discountFac)
  out[["conLcohMixed"]] <- computeLCOH(out[["conLccMixed"]], out[["conLtMixed"]], p_ueDemand, p_dt, p_discountFac, dims)

  renLccMixedFull <- computeLCC(out[["renLtMixed"]], p_specCostOpe, costRen, p_dt, p_discountFac)
  out[["renLccMixed"]] <- mutate(
    .avgAlongDim(renLccMixedFull, c("bs", "hs"), hs = "hsr", bs = "bsr"),
    bs = "low"
  )
  out[["renLcohMixed"]] <- computeLCOH(out[["renLccMixed"]], out[["renLtMixed"]], p_ueDemand, p_dt, p_discountFac, dims)


  # COMPUTE LOGIT HEATING SYSTEM SHARES ----------------------------------------

  renLogitAnteFull <- computeLogitShare("renovation", renLccAnteFull, c(dims, "bsr", "hsr"), priceSensHs[["renovation"]])
  out[["renLogitEl1Ante"]] <- aggregateShare(renLogitAnteFull, select(v_renovationIn, -"dt"),
                                                  energyLadder = energyLadder, energyLadderNo = 1)
  out[["renLogitAllAnte"]] <- aggregateShare(renLogitAnteFull, select(v_renovationIn, -"dt"))

  renLogitMixedFull <- computeLogitShare("renovation", renLccMixedFull, c(dims, "bsr", "hsr"), priceSensHs[["renovation"]])
  out[["renLogitEl1Mixed"]] <- aggregateShare(renLogitMixedFull, select(v_renovationIn, -"dt"),
                                                   energyLadder = energyLadder, energyLadderNo = 1)
  out[["renLogitAllMixed"]] <- aggregateShare(renLogitMixedFull, select(v_renovationIn, -"dt"))

  outRenLogitAnte <- .splitDataByHs(renLogitAnteFull, unique(renLogitAnteFull[["hs"]]), "renLogitAnte")
  outRenLogitMixed <- .splitDataByHs(renLogitMixedFull, unique(renLogitMixedFull[["hs"]]), "renLogitMixed")

  out <- c(out, outRenLogitAnte, outRenLogitMixed)


  # COMPUTE BRICK HEATING SYSTEM SHARES ----------------------------------------

  # Shares of initial systems when renovating
  out[["renBrickIn"]] <- computeBrickShare("renovationIn", select(v_renovationIn, -"dt"))

  renBrickFull <- computeBrickShare("renovation", v_renovation)
  out[["renBrickEl1"]] <- aggregateShare(renBrickFull, weight = select(v_renovationIn, -"dt"),
                                         energyLadder = energyLadder, energyLadderNo = 1)
  out[["renBrickAll"]] <- aggregateShare(renBrickFull, weight = select(v_renovationIn, -"dt"))

  outBrick <- .splitDataByHs(renBrickFull, unique(renBrickFull[["hs"]]), "renBrick")

  out <- c(out, outBrick)

  # NORMALIZE PRICE SENSITIVITY ------------------------------------------------

  normLambdaCon <- normalizePriceSensitivity(out[["conLccMixed"]], conVin, priceSensHs[["construction"]], dims)
  normLambdaRen <- normalizePriceSensitivity(out[["renLccMixed"]], v_renovationIn, priceSensHs[["renovation"]], dims)

  normLambdaConSubs <- normalizePriceSensitivity(out[["conLccMixed"]], conVin, priceSensHs[["construction"]], dims, groupCols = c("reg", "loc", "typ", "inc"))
  normLambdaRenSubs <- normalizePriceSensitivity(out[["renLccMixed"]], v_renovationIn, priceSensHs[["renovation"]], dims, groupCols = c("reg", "loc", "typ", "inc"))

  # WRITE ----------------------------------------------------------------------

  # Determine all dimensions present in output data
  allSets <- unique(unlist(lapply(out, colnames)))

  #(Partial) code duplicate of reportCalibration
  out <- do.call(rbind, lapply(names(out), function(varName) {
    .expandDims(out[[varName]], varName, allSets)
  }))

  write.csv(out, file.path(path, "BRICK_analysis_report.csv"), row.names = FALSE)

}

#' Read in Gdx and slightly modify
#'
#' @importFrom dplyr %>% across any_of .data filter mutate right_join
#'
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

  #TODO: Temporary fix - need to do this properly!
  df %>%
    mutate(across(any_of(c("bs", "bsr")), ~ "low"))
}

#' Check plausibility of lifetime results
#'
#' @importFrom dplyr %>% .data across all_of group_by mutate summarise
#'
.checkLifetimeResults <- function(dfLt, dims) {

  dfLt %>%
    group_by(across(all_of(c(dims, "ttot")))) %>%
    summarise(value = sum(.data[["relVal"]]), .groups = "drop") %>%
    mutate(error = .data[["value"]] <= 0.95)

}

#' Compute data average along given dimensions
#'
#' @importFrom dplyr
#'
.avgAlongDim <- function(df, dimToAvg, ...) {

  df <- df %>%
    group_by(across(-any_of(c(dimToAvg, "value")))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    rename(...)

  return(df)
}

#' Separate a large data set into several variables by hs
#'
#' @param data data frame to be split
#' @param hsrNames character, hsr values to filter for
#' @param varName character, name stem of resulting variable names
#'
#' @importFrom dplyr %>% .data filter rename select
#'
.splitDataByHs <- function(data, hsNames, varName) {
  stats::setNames(
    lapply(hsNames, function(h) {
      data %>%
        filter(.data[["hs"]] == h) %>%
        select(-"bs", -"hs") %>%
        rename(bs = "bsr", hs = "hsr")
    }),
    paste0(varName, hsNames)
  )
}

#' Extend dimensions of a data frame by adding NA entries, add variable name
#' Code duplicate from reportCalibration.R
#'
#' @param df data frame to be extended
#' @param varName character, variable name to be added
#' @param allSets character, sets that need to be included as column names
#' @returns data frame
#'
#' @importFrom dplyr %>% mutate last_col relocate

.expandDims <- function(df, varName, allSets) {

  # Add missing columns with NA entries
  df[setdiff(allSets, colnames(df))] <- NA

  # Add variable name as first column
  df <- df %>%
    mutate(variable = varName, .before = 1)
}
