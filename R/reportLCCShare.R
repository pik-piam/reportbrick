#' Compute life cycle costs and logit share
#' 
#' Estimate heating system lifetimes and use this to compute lifecycle costs,
#' levelized costs of heat and logit heating system shares.
#' Extract brick model shares for comparison.
#'
#' @author Ricarda Rosemann
#'
#' @param path character, path to the Brick model output folder
#' @param gdxName character, file name of the gdx to read data from
#' @param filterFullRen named list with name value pairs to filter full resolution
#'   renovation LCC/LCOH/Shares by. If \code{NULL}, no filtering is applied.
#'
#' @importFrom dplyr %>% .data cur_column filter full_join group_by inner_join last
#'   left_join mutate rename right_join select summarise
#' @importFrom piamutils getSystemFile
#' @importFrom stats pweibull
#' @importFrom tidyr crossing pivot_wider replace_na
#' @importFrom utils read.csv write.csv
#'

#TODO: Proper handling of building shell!!! (Ignored for the time being)

reportLCCShare <- function(path, gdxName = "output.gdx", filterFullRen = list(vin = "1980-1989", ttotIn = 2005)) {
  
  gdx <- file.path(path, gdxName)
  gdxInput <- file.path(path, "input.gdx")
  config <- yaml::read_yaml(file.path(path, "config", "config_COMPILED.yaml"))
  
  if (isFALSE(config[["ignoreShell"]])) {
    stop("This analysis currently only supports runs with ignoreShell set to TRUE.")
  }

  pathHs <- getSystemFile("extdata", "sectoral", "dim_hs.csv",
                          package = "brick", mustWork = TRUE)

  
  
  # READ IN DATA ---------------------------------------------------------------

  
  ## Time periods ====
  
  ttot <- readGdxSymbol(gdx, "ttot", asMagpie = FALSE)[["tall"]]
  t0 <- min(ttot)
  tRun <- setdiff(ttot, t0)
  ttotNum <- ttot

  
  ## Time period length and vintage ====
  
  p_dtVin <- readGdxSymbol(gdx, "p_dtVin", asMagpie = FALSE)
  p_dt <- readGdxSymbol(gdx, "p_dt", asMagpie = FALSE) %>%
    rename(dt = "value")

  p_dtVin <- p_dtVin %>%
    group_by(ttot) %>%
    summarise(vin = dplyr::last(.data[["vin"]]), dt = sum(.data[["value"]]),
              .groups = "drop")
  p_ttotVin <- select(p_dtVin, -"dt")
  vinExists <- readGdxSymbol(gdx, "vinExists", asMagpie = FALSE)
  renAllowed <- readGdxSymbol(gdx, "renAllowed", asMagpie = FALSE)


  ## Stocks and flows ====
  
  v_stock <- .cleanReadGdx(gdx, "v_stock", vinExists)
  v_construction <- .cleanReadGdx(gdx, "v_construction", vinExists)
  v_renovation <- .cleanReadGdx(gdx, "v_renovation", vinExists, renAllowed)
  v_demolition <- .cleanReadGdx(gdx, "v_demolition", vinExists)

  dims <- setdiff(colnames(v_stock), c("ttot", "value"))

  
  ## Cost components ====
  
  p_specCostOpe <- readGdxSymbol(gdx, "p_specCostOpe", asMagpie = FALSE)
  p_specCostCon <- readGdxSymbol(gdx, "p_specCostCon", asMagpie = FALSE)
  p_specCostRen <- readGdxSymbol(gdx, "p_specCostRen", asMagpie = FALSE)
  p_specCostDem <- readGdxSymbol(gdx, "p_specCostDem", asMagpie = FALSE)

  
  ## Further parameters ====
  
  # Discount rate
  p_discountFac <- readGdxSymbol(gdx, "p_discountFac", asMagpie = FALSE)
  
  # Price sensitivity
  priceSensBs <- unlist(config[["priceSens"]][["bs"]])
  priceSensHs <- unlist(config[["priceSens"]][["hs"]])
  
  # UE demand
  p_ueDemand <- readGdxSymbol(gdx, "p_ueDemand", asMagpie = FALSE)
  
  # Lifetimes given as required renovation shares
  p_shareRenHS <- .removeColNumbering(readGdxSymbol(gdx, "p_shareRenHS", asMagpie = FALSE))
  p_shareRenHSinit <- .removeColNumbering(readGdxSymbol(gdx, "p_shareRenHSinit", asMagpie = FALSE))
  p_shareRenHSfull <- .cleanReadGdx(gdxInput, "p_shareRenHSfull")
  p_shareRenHSfullInit <- .cleanReadGdx(gdxInput, "p_shareRenHSfullInit")

  # Energy ladder specifications have to be read from csv
  energyLadder <- read.csv(pathHs) %>%
    select("hs", "energyLadder")

  # Initialize output
  out <- list()

  
  
  # IN- AND OUTFLOW COMPUTATION ------------------------------------------------

  
  ## Attribute vintages to construction ====
  
  # Compute total construction by time period and attribute construction to vintages
  conVin <- v_construction %>%
    left_join(p_dtVin,
              by = intersect(colnames(v_construction), colnames(p_dtVin))) %>%
    mutate(value = .data[["value"]] * .data[["dt"]]) %>%
    select(-"dt") %>%
    rename(ttotIn = "ttot")

  v_constructionIn <- select(conVin, -"vin")

  
  ## Renovation in- and outflow ====
  
  v_renovationInDim <- v_renovation %>%
    select(-"value") %>%
    rename(ttotIn = "ttot")
  
  v_renovationOut <- v_renovation %>%
    filter(hsr != "0") %>%
    group_by(across(-all_of(c("hsr", "bsr", "value")))) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    left_join(p_dt, by = "ttot") %>%
    mutate(value = .data[["value"]] * .data[["dt"]]) %>%
    select(-"dt") %>%
    rename(ttotOut = "ttot")

  v_renovationIn <- v_renovation %>%
    filter(hsr != "0") %>%
    group_by(across(-all_of(c("hs", "bs", "value")))) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    rename(hs = "hsr", bs = "bsr") %>%
    left_join(p_dt, by = "ttot") %>%
    mutate(value = .data[["value"]] * .data[["dt"]]) %>%
    select(-"dt") %>%
    rename(ttotIn = "ttot")

  
  ## Demolition outflow ====
  
  v_demolitionOut <- v_demolition %>%
    left_join(p_dt, by = "ttot") %>%
    mutate(value = .data[["value"]] * .data[["dt"]]) %>%
    select(-"dt") %>%
    rename(ttotOut = "ttot")

  
  ## Total in- and outflow ====

  outflow <- v_renovationOut %>%
    rename(ren = "value") %>%
    full_join(v_demolitionOut %>%
                rename(dem = "value"),
              by = c(dims, "ttotOut")) %>%
    mutate(value = ren + dem) %>%
    select(-"ren", -"dem")

  inflow <- v_renovationIn %>%
    rename(ren = "value") %>%
    full_join(conVin %>%
                rename(con = "value"),
              by = c(dims, "ttotIn")) %>%
    replace_na(list(con = 0)) %>%
    mutate(value = .data[["con"]] + .data[["ren"]],
           share = ifelse(.data[["value"]] != 0, .data[["con"]] / .data[["value"]], 0)) %>%
    select(-"con", -"ren")

  # Construction and renovation shares
  conShare <- select(inflow, -"value")
  renShare <- mutate(conShare, share = 1 - .data[["share"]])
  
  inflow <- select(inflow, -"share")

  
  ## Initial stock ====

  v_stockInit <- v_stock %>%
    filter(.data$ttot == t0) %>%
    rename(ttotIn = "ttot")



  # LIFETIME PROBABILITIES --------------------
  
  
  ## Ex-ante ====
  
  # Remaining shares according to Weibull distribution at high time resolution
  out[["stockWbRemain"]] <- computeRemainingShare(p_shareRenHSfullInit, asDensity = FALSE, valueName = "value")
  out[["wbRemain"]] <- computeRemainingShare(p_shareRenHSfull, asDensity = FALSE, valueName = "value") %>%
    filter(.data$ttotIn != t0)

  stockInitLtAnte <- computeLtAnte(v_stockInit, p_shareRenHSinit, t0, isFlow = FALSE)
  conLtAnte <- computeLtAnte(conVin, p_shareRenHS, t0)
  renLtAnte <- computeLtAnte(v_renovationIn, p_shareRenHS, t0)
  
  out[["stockInitLtAnte"]] <- stockInitLtAnte
  out[["conLtAnte"]] <- conLtAnte
  out[["renLtAnte"]] <- renLtAnte
  
  # Remaining shares
  out[["stockInitLtAnteRemain"]] <- stockInitLtAnte %>%
    computeRemainingShare() %>%
    rescaleRemainingShare(out[["stockWbRemain"]], t0, dims)
  
  out[["conLtAnteRemain"]] <- computeRemainingShare(conLtAnte)
  out[["renLtAnteRemain"]] <- computeRemainingShare(renLtAnte)

  checkLtAnte <- .checkLifetimeResults(renLtAnte, dims)
  checkLtAnteInitStock <- .checkLifetimeResults(stockInitLtAnte, dims)


  ## Ex-Post ====

  ltPost <- computeLtPost(
    inflow,
    outflow,
    list(stock = v_stockInit, construction = conVin, renovation = v_renovationIn),
    conShare,
    p_ttotVin,
    ttotNum,
    dims
  )

  checkLtPost <- .checkLifetimeResults(ltPost[["stockInitLtPost"]], dims)
  
  ltPostRemain <- stats::setNames(lapply(ltPost, computeRemainingShare), paste0(names(ltPost), "Remain"))
  ltPostRemain[["stockInitLtPostRemain"]] <- rescaleRemainingShare(
    ltPostRemain[["stockInitLtPostRemain"]],
    out[["stockWbRemain"]],
    t0,
    dims
  )

  out <- c(out, ltPost, ltPostRemain)
  

  ## Mixed: Matching ex-ante lifetimes to brick outflow ====

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
  
  ltMixedRemain <- stats::setNames(lapply(ltMixed, computeRemainingShare), paste0(names(ltMixed), "Remain"))
  ltMixedRemain[["stockInitLtMixedRemain"]] <- rescaleRemainingShare(
    ltMixedRemain[["stockInitLtMixedRemain"]],
    out[["stockWbRemain"]],
    t0,
    dims
  )

  out <- c(out, ltMixed, ltMixedRemain)  
  

  ## Save outflows ====

  out[["outflow"]] <- outflow

  
  ## Verfication of the lifetime inequality ====
  
  ltIneq <- verifyLtHs(inflow, v_stockInit, outflow, p_shareRenHSinit, p_shareRenHS, dims)

  out <- c(out, ltIneq)


  
  # COMPUTE LCC AND LCOH -------------------------------------------------------

  
  ## Prepare cost data ====
  
  costCon <- p_specCostCon %>%
    pivot_wider(names_from = "cost", values_from = "value") %>%
    left_join(p_ttotVin, by = "ttot") %>%
    replace_na(list(intangible = 0))

  costRen <- p_specCostRen %>%
    inner_join(renAllowed, by = c("bs", "hs", "bsr", "hsr")) %>%
    filter(.data[["bsr"]] == 0) %>%
    pivot_wider(names_from = cost, values_from = value) %>%
    mutate(across(contains("hs"), as.character),
           # add status quo preference costs
           statusQuoPref = ifelse(.data$hs != .data$hsr, config[["statusQuoPreference"]], 0),
           across(contains("hs"), ~ factor(.x, levels = levels(v_renovation[[dplyr::cur_column()]])))) %>%
    replace_na(list(intangible = 0))
  
  
  ## Ex-ante LCC and LCOH ====
  
  out[["conLccAnte"]] <- computeLCC(out[["conLtAnte"]], p_specCostOpe, costCon, p_dt, p_discountFac)
  out[["conLccAnteScaled"]] <- normalizeLCC(out[["conLccAnte"]], out[["conLtAnte"]], p_dt)
  out[["conLcohAnte"]] <- computeLCOH(out[["conLccAnte"]], out[["conLtAnte"]], p_ueDemand, p_dt, p_discountFac, dims)

  renLccAnteFull <- computeLCC(out[["renLtAnte"]], p_specCostOpe, costRen, p_dt, p_discountFac)
  
  out[["renLccAnte"]] <- mutate(
    .avgAlongDim(renLccAnteFull, c("bs", "hs"), hs = "hsr", bs = "bsr"),
    bs = "low"
  )
  
  # If desired, filter the full resolution renovation data
  out[["renLccAnteFull"]] <- .filterAsUnion(renLccAnteFull, filterFullRen)

  # As a test: Scale LCC by to align with average lifetime
  out[["renLccAnteScaledFull"]] <- normalizeLCC(renLccAnteFull, out[["renLtAnte"]], p_dt) %>%
    .filterAsUnion(filterFullRen)
  
  out[["renLcohAnteFull"]] <- computeLCOH(renLccAnteFull, out[["renLtAnte"]], p_ueDemand, p_dt, p_discountFac, dims) %>%
    .filterAsUnion(filterFullRen)
  out[["renLcohAnte"]] <- computeLCOH(out[["renLccAnte"]], out[["renLtAnte"]], p_ueDemand, p_dt, p_discountFac, dims)
  

  ## Mixed LCC and LCOH ====
  
  out[["conLccMixed"]] <- computeLCC(out[["conLtMixed"]], p_specCostOpe, costCon, p_dt, p_discountFac)
  out[["conLccMixedScaled"]] <- normalizeLCC(out[["conLccMixed"]], out[["conLtMixed"]], p_dt)
  out[["conLcohMixed"]] <- computeLCOH(out[["conLccMixed"]], out[["conLtMixed"]], p_ueDemand, p_dt, p_discountFac, dims)

  renLccMixedFull <- computeLCC(out[["renLtMixed"]], p_specCostOpe, costRen, p_dt, p_discountFac)
  
  out[["renLccMixed"]] <- mutate(
    .avgAlongDim(renLccMixedFull, c("bs", "hs"), hs = "hsr", bs = "bsr"),
    bs = "low"
  )
  
  # If desired, filter the full resolution renovation data:
  out[["renLccMixedFull"]] <- .filterAsUnion(renLccMixedFull, filterFullRen)

  # As a test: Scale LCC by to align with average lifetime
  out[["renLccMixedScaledFull"]] <- normalizeLCC(renLccMixedFull, out[["renLtMixed"]], p_dt) %>%
    .filterAsUnion(filterFullRen)
  
  out[["renLcohMixedFull"]] <- computeLCOH(renLccMixedFull, out[["renLtMixed"]], p_ueDemand, p_dt, p_discountFac, dims) %>%
    .filterAsUnion(filterFullRen)
  out[["renLcohMixed"]] <- computeLCOH(out[["renLccMixed"]], out[["renLtMixed"]], p_ueDemand, p_dt, p_discountFac, dims)



  # COMPUTE LOGIT HEATING SYSTEM SHARES ----------------------------------------

  renLogitAnteFull <- computeLogitShare("renovation", renLccAnteFull, priceSensHs[["renovation"]])
  out[["renLogitAnteFull"]] <- renLogitAnteFull %>%
    .filterAsUnion(filterFullRen)
  out[["renLogitEl1Ante"]] <- aggregateShare(renLogitAnteFull, weight = v_renovationIn,
                                                  energyLadder = energyLadder, energyLadderNo = 1)
  out[["renLogitAllAnte"]] <- aggregateShare(renLogitAnteFull, weight = v_renovationIn)

  renLogitMixedFull <- computeLogitShare("renovation", renLccMixedFull, priceSensHs[["renovation"]])
  out[["renLogitMixedFull"]] <- renLogitMixedFull %>%
    .filterAsUnion(filterFullRen)
  out[["renLogitEl1Mixed"]] <- aggregateShare(renLogitMixedFull, weight = v_renovationIn,
                                                   energyLadder = energyLadder, energyLadderNo = 1)
  out[["renLogitAllMixed"]] <- aggregateShare(renLogitMixedFull, weight = v_renovationIn)

  

  # COMPUTE BRICK HEATING SYSTEM SHARES ----------------------------------------

  # Shares of initial systems when renovating
  out[["renBrickIn"]] <- computeBrickShare("renovationIn", v_renovationIn)

  renBrickFull <- computeBrickShare("renovation", rename(v_renovation, ttotIn = "ttot"))
  out[["renBrickFull"]] <- renBrickFull %>%
    .filterAsUnion(filterFullRen)
  out[["renBrickEl1"]] <- aggregateShare(renBrickFull, weight = v_renovationIn,
                                         energyLadder = energyLadder, energyLadderNo = 1)
  out[["renBrickAll"]] <- aggregateShare(renBrickFull, weight = v_renovationIn)

  
  
  # NORMALIZE PRICE SENSITIVITY ------------------------------------------------

  normLambdaCon <- normalizePriceSensitivity(out[["conLccMixed"]], conVin, priceSensHs[["construction"]], dims)
  normLambdaRen <- normalizePriceSensitivity(out[["renLccMixed"]], v_renovationIn, priceSensHs[["renovation"]], dims)

  normLambdaConSubs <- normalizePriceSensitivity(out[["conLccMixed"]], conVin, priceSensHs[["construction"]], dims, groupCols = c("region", "loc", "typ", "inc"))
  normLambdaRenSubs <- normalizePriceSensitivity(out[["renLccMixed"]], v_renovationIn, priceSensHs[["renovation"]], dims, groupCols = c("region", "loc", "typ", "inc"))

  
  
  # WRITE ----------------------------------------------------------------------

  # Determine all dimensions present in output data
  allSets <- unique(unlist(lapply(out, colnames)))
  orderedColnames <- union(
    c("variable", setdiff(colnames(v_renovation), c("ttot", "value")),
      "costType", "ttotIn", "ttotOut", "relVal", "absVal", "value"),
    allSets
  )

  #(Partial) code duplicate of reportCalibration
  out <- do.call(rbind, lapply(names(out), function(varName) {
    .expandDims(out[[varName]], varName, allSets)
  })) %>%
    select(all_of(orderedColnames))

  write.csv(out, file.path(path, "BRICK_analysis_report.csv"), row.names = FALSE)

}

#' Read data from gdx and clean
#'
#' @param df data frame with data as read from a gdx
#' @param vinExists data frame with existing vintage and time period combinations
#' @param renAllowed data frame with all possible renovation transitions
#'
#' @importFrom dplyr %>% across any_of .data filter mutate right_join
#'
.cleanReadGdx <- function(gdx, symbol, vinExists = NULL, renAllowed = NULL, asMagpie = FALSE) {
  
  df <- readGdxSymbol(gdx, symbol, asMagpie = asMagpie)

  if ("qty" %in% colnames(df)) {
    df <- df %>%
      filter(.data[["qty"]] == "area")
  }

  if (!is.null(vinExists) && all(c("vin", "ttot") %in% colnames(df))) {
    df <- df %>%
      dplyr::right_join(vinExists, by = c("vin", "ttot"))
  }

  if (all(c("hs", "hsr", "bs", "bsr") %in% colnames(df)) && !is.null(renAllowed)) {
    df <- df %>%
      dplyr::right_join(renAllowed, by = c("hs", "hsr", "bs", "bsr"))
  }

  #TODO: Temporary fix - need to do this properly!
  df %>%
    mutate(across(any_of(c("bs", "bsr")), ~ "low"))
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
.removeColNumbering <- function(df, offset = 1, suffix = "") {
  
  colnamesUnnmbd <- sub("\\_\\d{1-3}$", "", colnames(df))
  
  duplicates <- which(duplicated(colnamesUnnmbd))
  for (i in seq_along(duplicates)) {
    colnamesUnnmbd[duplicates[i]] <- paste0(colnamesUnnmbd[duplicates[i]], suffix, offset + i)
  }
  
  colnames(df) <- colnamesUnnmbd
  
  if (any(grepl("ttot\\d", colnamesUnnmbd[duplicates]))) {
    df <- df %>%
      mutate(ttot = as.numeric(as.character(.data[["ttot"]])),
             ttot2 = as.numeric(as.character(.data[["ttot2"]]))) %>%
      rename(ttotIn = "ttot", ttotOut = "ttot2")
  }
  
  return(df)
}

#' Check plausibility of lifetime results
#'
#' @param dfLt data frame with lifetime data
#' @param dims character, dimensions to group by
#'
#' @importFrom dplyr %>% .data across all_of group_by mutate summarise
#'
.checkLifetimeResults <- function(dfLt, dims) {

  dfLt %>%
    group_by(across(all_of(c(dims, "ttotIn")))) %>%
    summarise(value = sum(.data[["relVal"]]), .groups = "drop") %>%
    mutate(error = .data[["value"]] <= 0.95)

}

#' Compute data average along given dimensions
#'
#' @param df data frame with the data to be manipulated
#' @param dimToAvg character column name of the column to average over
#' @param ... named character vector to with tuples to be renamed
#'
#' @importFrom dplyr %>% .data across any_of group_by rename summarise
#'
.avgAlongDim <- function(df, dimToAvg, ...) {

  df <- df %>%
    group_by(across(-any_of(c(dimToAvg, "value")))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    rename(...)

  return(df)
}

# If desired, filter the full resolution renovation data:
# The data rows to be kept need to satisfy at least one of the filter criteria.
.filterAsUnion <- function(df, filterVals) {
  
  dfFiltered <- data.frame()
  for (nm in names(filterVals)) {
    dfFiltered <- df %>%
      filter(.data[[nm]] == filterVals[[nm]]) %>%
      rbind(dfFiltered)
  }
  if (is.null(filterVals)) df else unique(dfFiltered)
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
