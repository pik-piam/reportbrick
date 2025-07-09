#' Compute life cycle costs and logit share
#' 
#' Estimate heating system lifetimes and use this to compute lifecycle costs,
#' levelized costs of heat and logit heatings system shares.
#' Extract brick model shares for comparison.
#'
#' @author Ricarda Rosemann
#'
#' @param path character, path to the Brick model output folder
#' @param gdxName character, file name of the gdx to read data from
#' @param pathLt character, path to the lifetime parameters
#'
#' @importFrom dplyr %>% .data cur_column filter full_join group_by inner_join last
#'   left_join mutate rename right_join select summarise
#' @importFrom piamutils getSystemFile
#' @importFrom stats pweibull
#' @importFrom tidyr crossing pivot_wider replace_na
#' @importFrom utils read.csv write.csv
#'

#TODO: Proper handling of building shell!!! (Ignored for the time being)

reportLCCShare <- function(path, gdxName = "output.gdx", pathLt = NULL) {
  
  gdx <- file.path(path, gdxName)
  config <- yaml::read_yaml(file.path(path, "config", "config_COMPILED.yaml"))
  
  if (isFALSE(config[["ignoreShell"]])) {
    stop("This analysis currently only supports runs with ignoreShell set to TRUE.")
  }

  if (is.null(pathLt)) pathLt <- "C:/Users/ricardar/Documents/PIAM/brick/inst/input/f_lifetimeHeatingSystem.cs4r"
  pathHs <- getSystemFile("extdata", "sectoral", "dim_hs.csv",
                          package = "brick", mustWork = TRUE)

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
  priceSensBs <- unlist(config[["priceSens"]][["bs"]])
  priceSensHs <- unlist(config[["priceSens"]][["hs"]])
  p_ueDemand <- readGdxSymbol(gdx, "p_ueDemand", asMagpie = FALSE)
  # Weibull parameters have to be read from cs4r!
  lifeTimeHs <- read.csv(pathLt, header = FALSE, comment.char = "*")
  colnames(lifeTimeHs) <- c("region", "typ", "hs", "variable", "value")
  # Energy ladder specifications have to be read from csv
  energyLadder <- read.csv(pathHs) %>%
    select("hs", "energyLadder")

  # TODO: Write the filtering properly
  lifeTimeHs <- pivot_wider(lifeTimeHs, names_from = "variable", values_from = "value") %>%
    filter(.data[["region"]] %in% levels(v_construction[["region"]]),
           .data[["typ"]] %in% levels(v_construction[["typ"]])) %>%
    mutate(across(where(is.character), as.factor))

  # Initialize output
  out <- list()

  # IN- AND OUTFLOW COMPUTATION ------------------------------------------------

  # Compute total construction by time period and attribute construction to vintages
  conVin <- v_construction %>%
    left_join(p_dtVin,
              by = intersect(colnames(v_construction), colnames(p_dtVin))) %>%
    mutate(value = .data[["value"]] * .data[["dt"]]) %>%
    select(-"dt") %>%
    rename(ttotIn = "ttot")

  v_constructionIn <- select(conVin, -"vin")


  #TODO: Temporary, should handle bsr properly eventually
  # Compute renovation in- and outflow
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

  v_demolitionOut <- v_demolition %>%
    left_join(p_dt, by = "ttot") %>%
    mutate(value = .data[["value"]] * .data[["dt"]]) %>%
    select(-"dt") %>%
    rename(ttotOut = "ttot")

  # Compute total in- and outflow
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

  conShare <- select(inflow, -"value")
  renShare <- mutate(conShare, share = 1 - .data[["share"]])
  inflow <- select(inflow, -"share")

  # Compute initial stock
  v_stockInit <- v_stock %>%
    filter(.data$ttot == t0) %>%
    rename(ttotIn = "ttot")


  # COMPUTE EX-ANTE LIFETIME PROBABILITIES -------------------------------------

  # Compute life time probabilities of initial stock
  stockInitLtAnteC <- computeLtAnte("stock", v_stockInit, ttotNum, lifeTimeHs, dims, p_dt = p_dt)
  conLtAnteC <- computeLtAnte("construction", conVin, ttotNum, lifeTimeHs, dims, p_dt = p_dt)
  renLtAnteC <- computeLtAnte("renovation", v_renovationInDim, ttotNum, lifeTimeHs, dims,
                                      dataValue = v_renovationIn)

  out[["stockInitLtAnteC"]] <- stockInitLtAnteC
  out[["conLtAnteC"]] <- conLtAnteC
  out[["renLtAnteC"]] <- renLtAnteC

  checkLtAnteC <- .checkLifetimeResults(renLtAnteC, dims)
  checkLtAnteCInitStock <- .checkLifetimeResults(stockInitLtAnteC, dims)


  # COMPUTE EX-ANTE LIFETIME PROBABILITIES  (SIMPLE METHOD) --------------------

  # Compute life time probabilities of initial stock
  stockInitLtAnte <- computeLtAnte("stock", v_stockInit, ttotNum, lifeTimeHs, dims,
                                   runSimple = TRUE, p_dt = p_dt)
  conLtAnte <- computeLtAnte("construction", conVin, ttotNum, lifeTimeHs, dims,
                             runSimple = TRUE, p_dt = p_dt)
  renLtAnte <- computeLtAnte("renovation", v_renovationInDim, ttotNum, lifeTimeHs, dims,
                             runSimple = TRUE, p_dt = p_dt, dataValue = v_renovationIn)
  
  stockInitLtAnteRemain <- computeLtAnte("stock", v_stockInit, ttotNum, lifeTimeHs, dims,
                                   runSimple = TRUE, p_dt = p_dt, returnDistr = TRUE) %>%
    mutate(relVal = 1 - .data$relVal) %>%
    select(-"absVal")
  conLtAnteRemain <- computeLtAnte("construction", conVin, ttotNum, lifeTimeHs, dims,
                             runSimple = TRUE, p_dt = p_dt, returnDistr = TRUE) %>%
    mutate(relVal = 1 - .data$relVal) %>%
    select(-"absVal")
  renLtAnteRemain <- computeLtAnte("renovation", v_renovationInDim, ttotNum, lifeTimeHs, dims,
                             runSimple = TRUE, p_dt = p_dt, dataValue = v_renovationIn, returnDistr = TRUE) %>%
    mutate(relVal = 1 - .data$relVal) %>%
    select(-"absVal")

  out[["stockInitLtAnte"]] <- stockInitLtAnte
  out[["conLtAnte"]] <- conLtAnte
  out[["renLtAnte"]] <- renLtAnte
  
  out[["stockInitLtAnteRemain"]] <- stockInitLtAnteRemain
  out[["conLtAnteRemain"]] <- conLtAnteRemain
  out[["renLtAnteRemain"]] <- renLtAnteRemain

  checkLtAnte <- .checkLifetimeResults(renLtAnte, dims)
  checkLtAnteInitStock <- .checkLifetimeResults(stockInitLtAnte, dims)


  # COMPUTE EX-POST LIFETIME PROBABILITIES -------------------------------------

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
  
  ltPostRemain <- stats::setNames(lapply(ltPost, function(dfLt) {
    dfLt %>%
      select(-"absVal") %>%
      group_by(across(-all_of(c("ttotOut", "relVal")))) %>%
      mutate(relVal = 1 - cumsum(.data$relVal)) %>%
      ungroup()
  }), paste0(names(ltPost), "Remain"))

  out <- c(out, ltPost, ltPostRemain)
  
  

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
  
  ltMixedRemain <- stats::setNames(lapply(ltMixed, function(dfLt) {
    dfLt %>%
      select(-"absVal") %>%
      group_by(across(-all_of(c("ttotOut", "relVal")))) %>%
      mutate(relVal = 1 - cumsum(.data$relVal)) %>%
      ungroup()
  }), paste0(names(ltMixed), "Remain"))

  out <- c(out, ltMixed, ltMixedRemain)  
  
  

  # SAVE OUTFLOWS --------------------------------------------------------------

  out[["outflow"]] <- outflow

  
  
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
    pivot_wider(names_from = cost, values_from = value) %>%
    mutate(across(contains("hs"), as.character),
           statusQuoPref = ifelse(.data$hs != .data$hsr, config[["statusQuoPreference"]], 0),
           across(contains("hs"), ~ factor(.x, levels = levels(v_renovation[[dplyr::cur_column()]])))) %>%
    replace_na(list(intangible = 0))

  # Compute ex-ante LCC and LCOH
  out[["conLccAnte"]] <- computeLCC(out[["conLtAnte"]], p_specCostOpe, costCon, p_dt, p_discountFac)
  out[["conLccAnteScaled"]] <- normalizeLCC(out[["conLccAnte"]], out[["conLtAnte"]], p_dt)
  out[["conLcohAnte"]] <- computeLCOH(out[["conLccAnte"]], out[["conLtAnte"]], p_ueDemand, p_dt, p_discountFac, dims)

  renLccAnteFull <- computeLCC(out[["renLtAnte"]], p_specCostOpe, costRen, p_dt, p_discountFac)
  out[["renLccAnteFull"]] <- renLccAnteFull
  out[["renLccAnte"]] <- mutate(
    .avgAlongDim(out[["renLccAnteFull"]], c("bs", "hs"), hs = "hsr", bs = "bsr"),
    bs = "low"
  )
  renLccAnteScaledFull <- normalizeLCC(out[["renLccAnteFull"]], out[["renLtAnte"]], p_dt)
  out[["renLccAnteScaledFull"]] <- renLccAnteScaledFull
  out[["renLcohAnteFull"]] <- computeLCOH(out[["renLccAnteFull"]], out[["renLtAnte"]], p_ueDemand, p_dt, p_discountFac, dims)
  out[["renLcohAnte"]] <- computeLCOH(out[["renLccAnte"]], out[["renLtAnte"]], p_ueDemand, p_dt, p_discountFac, dims)

  # Compute ex-post LCC and LCOH

  # Compute mixed LCC and LCOH
  out[["conLccMixed"]] <- computeLCC(out[["conLtMixed"]], p_specCostOpe, costCon, p_dt, p_discountFac)
  out[["conLccMixedScaled"]] <- normalizeLCC(out[["conLccMixed"]], out[["conLtMixed"]], p_dt)
  out[["conLcohMixed"]] <- computeLCOH(out[["conLccMixed"]], out[["conLtMixed"]], p_ueDemand, p_dt, p_discountFac, dims)

  renLccMixedFull <- computeLCC(out[["renLtMixed"]], p_specCostOpe, costRen, p_dt, p_discountFac)
  out[["renLccMixedFull"]] <- renLccMixedFull
  out[["renLccMixed"]] <- mutate(
    .avgAlongDim(out[["renLccMixedFull"]], c("bs", "hs"), hs = "hsr", bs = "bsr"),
    bs = "low"
  )
  renLccMixedScaledFull <- normalizeLCC(out[["renLccMixedFull"]], out[["renLtMixed"]], p_dt)
  out[["renLccMixedScaledFull"]] <- renLccMixedScaledFull
  out[["renLcohMixedFull"]] <- computeLCOH(out[["renLccMixedFull"]], out[["renLtMixed"]], p_ueDemand, p_dt, p_discountFac, dims)
  out[["renLcohMixed"]] <- computeLCOH(out[["renLccMixed"]], out[["renLtMixed"]], p_ueDemand, p_dt, p_discountFac, dims)



  # COMPUTE LOGIT HEATING SYSTEM SHARES ----------------------------------------

  renLogitAnteFull <- computeLogitShare("renovation", renLccAnteFull, priceSensHs[["renovation"]])
  out[["renLogitAnteFull"]] <- renLogitAnteFull
  out[["renLogitEl1Ante"]] <- aggregateShare(renLogitAnteFull, weight = v_renovationIn,
                                                  energyLadder = energyLadder, energyLadderNo = 1)
  out[["renLogitAllAnte"]] <- aggregateShare(renLogitAnteFull, weight = v_renovationIn)

  renLogitMixedFull <- computeLogitShare("renovation", renLccMixedFull, priceSensHs[["renovation"]])
  out[["renLogitMixedFull"]] <- renLogitMixedFull
  out[["renLogitEl1Mixed"]] <- aggregateShare(renLogitMixedFull, weight = v_renovationIn,
                                                   energyLadder = energyLadder, energyLadderNo = 1)
  out[["renLogitAllMixed"]] <- aggregateShare(renLogitMixedFull, weight = v_renovationIn)

  outRenLogitAnte <- .splitDataByHs(renLogitAnteFull, unique(renLogitAnteFull[["hs"]]), "renLogitAnte")
  outRenLogitMixed <- .splitDataByHs(renLogitMixedFull, unique(renLogitMixedFull[["hs"]]), "renLogitMixed")

  out <- c(out, outRenLogitAnte, outRenLogitMixed)


  # COMPUTE BRICK HEATING SYSTEM SHARES ----------------------------------------

  # Shares of initial systems when renovating
  out[["renBrickIn"]] <- computeBrickShare("renovationIn", v_renovationIn)

  renBrickFull <- computeBrickShare("renovation", rename(v_renovation, ttotIn = "ttot"))
  out[["renBrickEl1"]] <- aggregateShare(renBrickFull, weight = v_renovationIn,
                                         energyLadder = energyLadder, energyLadderNo = 1)
  out[["renBrickAll"]] <- aggregateShare(renBrickFull, weight = v_renovationIn)

  outBrick <- .splitDataByHs(renBrickFull, unique(renBrickFull[["hs"]]), "renBrick")

  out <- c(out, outBrick)

  # NORMALIZE PRICE SENSITIVITY ------------------------------------------------

  normLambdaCon <- normalizePriceSensitivity(out[["conLccMixed"]], conVin, priceSensHs[["construction"]], dims)
  normLambdaRen <- normalizePriceSensitivity(out[["renLccMixed"]], v_renovationIn, priceSensHs[["renovation"]], dims)

  normLambdaConSubs <- normalizePriceSensitivity(out[["conLccMixed"]], conVin, priceSensHs[["construction"]], dims, groupCols = c("region", "loc", "typ", "inc"))
  normLambdaRenSubs <- normalizePriceSensitivity(out[["renLccMixed"]], v_renovationIn, priceSensHs[["renovation"]], dims, groupCols = c("region", "loc", "typ", "inc"))

  # WRITE ----------------------------------------------------------------------

  # Determine all dimensions present in output data
  allSets <- unique(unlist(lapply(out, colnames)))

  #(Partial) code duplicate of reportCalibration
  out <- do.call(rbind, lapply(names(out), function(varName) {
    .expandDims(out[[varName]], varName, allSets)
  }))

  write.csv(out, file.path(path, "BRICK_analysis_report.csv"), row.names = FALSE)

}

#' Clean data that has been read from gdx
#'
#' @param df data frame with data as read from a gdx
#' @param vinExists data frame with existing vintage and time period combinations
#' @param renAllowed data frame with all possible renovation transitions
#'
#' @importFrom dplyr %>% across any_of .data filter mutate right_join
#'
.cleanReadGdx <- function(df, vinExists, renAllowed = NULL) {

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
