#' Compute various heating system characteristics
#'
#' Estimate heating system lifetimes and use this to compute lifecycle costs,
#' levelized costs of heat and logit heating system shares.
#' Extract brick model shares for comparison.
#'
#' @param path character, path to the Brick model output folder
#' @param gdxName character, file name of the gdx to read data from
#' @param filterFullRen named list with name value pairs to filter full resolution
#'   renovation LCC/LCOH/Shares by.
#'   If several filtering criteria are given, the data is filtered to always match at least one criterion.
#'   If \code{NULL}, no filtering is applied.
#' @param hsRef character, reference hs for linearized logistic model
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% .data contains cur_column filter full_join group_by inner_join last
#'   left_join mutate rename right_join select summarise
#' @importFrom piamutils getSystemFile
#' @importFrom stats pweibull
#' @importFrom tidyr crossing pivot_wider replace_na
#' @importFrom utils read.csv write.csv
#' @export
#'
reportHeatingSystemDetails <- function(path, gdxName = "output.gdx",
                                       filterFullRen = list(vin = "1980-1989", ttotIn = 2015),
                                       hsRef = "gabo") {

  gdx <- file.path(path, gdxName)
  gdxInput <- file.path(path, "input.gdx")
  config <- yaml::read_yaml(file.path(path, "config", "config_COMPILED.yaml"))

  if (isFALSE(config[["ignoreShell"]])) {
    stop("This analysis currently only supports runs with ignoreShell set to TRUE.")
  }

  fixedBuildings <- config[["switches"]][["FIXEDBUILDINGS"]]

  pathHs <- getSystemFile("extdata", "sectoral", "dim_hs.csv",
                          package = "brick", mustWork = TRUE)



  # READ IN DATA ---------------------------------------------------------------


  ## Time periods ====

  ttot <- readGdxSymbol(gdx, "ttot", asMagpie = FALSE)[["tall"]]
  t0 <- min(ttot)
  ttotNum <- ttot


  ## Time period length and vintage ====

  p_dtVin <- readGdxSymbol(gdx, "p_dtVin", asMagpie = FALSE) %>%
    group_by(ttot) %>%
    summarise(vin = dplyr::last(.data[["vin"]]), dt = sum(.data[["value"]]),
              .groups = "drop")

  p_dt <- readGdxSymbol(gdx, "p_dt", asMagpie = FALSE) %>%
    rename(dt = "value")

  p_ttotVin <- select(p_dtVin, -"dt")
  vinExists <- readGdxSymbol(gdx, "vinExists", asMagpie = FALSE)
  renAllowed <- readGdxSymbol(gdx, "renAllowedHS", asMagpie = FALSE)


  ## Stocks and flows ====

  v_stock <- .cleanReadGdx(gdx, "v_stock", vinExists)
  v_construction <- .cleanReadGdx(gdx, "v_construction", vinExists)
  v_renovation <- .cleanReadGdx(gdx, "v_renovationHS", vinExists, renAllowed)
  v_demolition <- .cleanReadGdx(gdx, "v_demolition", vinExists)

  subs <- c("region", "loc", "typ", "inc")


  ## Cost components ====

  p_specCostOpe <- readGdxSymbol(gdx, "p_specCostOpe", asMagpie = FALSE)
  p_specCostCon <- readGdxSymbol(gdx, "p_specCostCon", asMagpie = FALSE)

  if (isTRUE(config[["switches"]][["SEQUENTIALREN"]])) {
    p_specCostRen <- readGdxSymbol(gdx, "p_specCostRenHS", asMagpie = FALSE)
  } else {
    p_specCostRen <- readGdxSymbol(gdx, "p_specCostRen", asMagpie = FALSE) %>%
      filter(.data$bsr == "0")
  }


  ## Further parameters ====

  # Discount rate
  p_discountFac <- readGdxSymbol(gdx, "p_discountFac", asMagpie = FALSE)

  # Price sensitivity
  priceSensBs <- unlist(config[["priceSens"]][["bs"]])
  priceSensHs <- unlist(config[["priceSens"]][["hs"]])

  # UE demand
  p_ueDemand <- readGdxSymbol(gdx, "p_ueDemand", asMagpie = FALSE)

  # Lifetimes given as required renovation shares
  p_shareRenHS <- .removeColNumbering(readGdxSymbol(gdxInput, "p_shareRenHS", asMagpie = FALSE))
  p_shareRenHSinit <- readGdxSymbol(gdxInput, "p_shareRenHSinit", asMagpie = FALSE) %>%
    .removeColNumbering() %>%
    .filterAndUnselect("level", "matched")
  p_shareRenHSfull <- .cleanReadGdx(gdxInput, "p_shareRenHSfull")

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

  v_renovationOut <- v_renovation %>%
    filter(.data$hsr != "0") %>%
    group_by(across(-all_of(c("hsr", "value")))) %>%
    summarise(value = sum(.data$value), .groups = "drop") %>%
    left_join(p_dt, by = "ttot") %>%
    mutate(value = .data[["value"]] * .data[["dt"]]) %>%
    select(-"dt") %>%
    rename(ttotOut = "ttot")

  v_renovationIn <- v_renovation %>%
    filter(.data$hsr != "0") %>%
    group_by(across(-all_of(c("hs", "value")))) %>%
    summarise(value = sum(.data$value), .groups = "drop") %>%
    rename(hs = "hsr") %>%
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
              by = c("qty", "bs", "hs", "vin", subs, "ttotOut")) %>%
    mutate(value = .data$ren + .data$dem) %>%
    select(-"ren", -"dem")

  inflow <- v_renovationIn %>%
    rename(ren = "value") %>%
    full_join(conVin %>%
                rename(con = "value"),
              by = c("qty", "bs", "hs", "vin", subs, "ttotIn")) %>%
    replace_na(list(con = 0)) %>%
    mutate(value = .data[["con"]] + .data[["ren"]],
           share = ifelse(.data[["value"]] != 0, .data[["con"]] / .data[["value"]], 0)) %>%
    select(-"con", -"ren")

  # Construction shares
  conShare <- select(inflow, -"value")

  inflow <- select(inflow, -"share")


  ## Initial stock ====

  v_stockInit <- v_stock %>%
    filter(.data$ttot == t0) %>%
    rename(ttotIn = "ttot")



  # LIFETIME PROBABILITIES --------------------


  ## Maximum lifetimes ====

  # Lifetimes according to Weibull distribution/matching lifetimes (for standing stock)
  stockInitLtMax <- computeLtMax(v_stockInit, p_shareRenHSinit, t0, isFlow = FALSE)
  conLtMax <- computeLtMax(conVin, p_shareRenHS, t0)
  renLtMax <- computeLtMax(v_renovationIn, p_shareRenHS, t0)

  out[["Max Lifetime|Initial Stock"]] <- stockInitLtMax
  if (isFALSE(fixedBuildings)) out[["Max Lifetime|Construction"]] <- conLtMax

  out[["Max Lifetime|Renovation|Agg: vin"]] <- aggregateShare(renLtMax, "vin", "ttotOut",
                                                              weight = v_renovationIn, ttotWeight = "ttotIn")

  out[["Max Lifetime|Renovation"]]  <- .filterAsUnion(renLtMax, filterFullRen)

  # Shares of hs that still remain according to Weibull distribution at high time resolution
  out[["Max Remaining Share|Initial Stock"]] <- computeRemainingShare(p_shareRenHSinit,
                                                                      asDensity = FALSE, valueName = "value")
  out[["Max Remaining Share|Renovation"]] <- computeRemainingShare(p_shareRenHSfull,
                                                                   asDensity = FALSE, valueName = "value") %>%
    filter(.data$ttotIn != t0)



  ## Lifetime estimates ====

  # Lifetimes assuming maximum lifetimes which are adjusted to match actual brick outflows
  lt <- computeLt(
    list(stock = stockInitLtMax, construction = conLtMax, renovation = renLtMax),
    outflow,
    list(stock = v_stockInit, construction = v_constructionIn, renovation = v_renovationIn),
    conShare,
    ttotNum
  )

  renLtAggVin <- aggregateShare(lt$renovation, "vin", "ttotOut", weight = v_renovationIn, ttotWeight = "ttotIn")

  # Remaining shares
  ltRemain <- lapply(lt, computeRemainingShare)
  ltRemain[["stock"]] <- rescaleRemainingShare(
    ltRemain[["stock"]],
    out[["Max Remaining Share|Initial Stock"]],
    t0
  )

  out <- c(
    out,
    stats::setNames(lt, paste("Lifetime", c("Initial Stock", "Construction", "Renovation"), sep = "|")),
    stats::setNames(ltRemain, paste("Remaining Share", c("Initial Stock", "Construction", "Renovation"), sep = "|"))
  )

  if (isTRUE(fixedBuildings)) {
    out[["Lifetime|Construction"]] <- NULL
    out[["Remaining Share|Construction"]] <- NULL
  }

  out[["Lifetime|Renovation"]] <- .filterAsUnion(lt$renovation, filterFullRen)
  out[["Remaining Share|Renovation"]] <- .filterAsUnion(ltRemain$renovation, filterFullRen)
  out[["Lifetime|Renovation|Agg: vin"]] <- renLtAggVin


  ## Verification of the lifetime inequality ====

  ltIneq <- verifyLtHs(inflow, v_stockInit, outflow, p_shareRenHSinit, p_shareRenHS)

  out <- c(out, stats::setNames(ltIneq, paste("Lifetime Inequality", sub("LtIneq$", "", names(ltIneq)), sep = "|")))



  # COMPUTE LCC AND LCOH -------------------------------------------------------


  ## Prepare cost data ====

  costCon <- p_specCostCon %>%
    pivot_wider(names_from = "cost", values_from = "value") %>%
    left_join(p_ttotVin, by = "ttot") %>%
    replace_na(list(intangible = 0))

  costRen <- p_specCostRen %>%
    inner_join(renAllowed, by = c("bs", "hs", "hsr")) %>%
    pivot_wider(names_from = "cost", values_from = "value") %>%
    mutate(across(contains("hs"), as.character),
           # add status quo preference costs
           statusQuoPref = ifelse(.data$hs != .data$hsr, config[["statusQuoPreference"]], 0),
           across(contains("hs"), ~ factor(.x, levels = levels(v_renovation[[dplyr::cur_column()]])))) %>%
    replace_na(list(intangible = 0))


  ## LCC and LCOH ====

  # Lifecycle costs and levelized costs of heat based on lifetime estimates

  ### construction ####

  if (isFALSE(fixedBuildings)) {
    out[["LCC|Construction"]] <- computeLCC(lt$construction, p_specCostOpe, costCon, p_dt, p_discountFac) %>%
      .renameCostType()
    out[["LCOH|Construction"]] <- computeLCOH(out[["LCC|Construction"]], lt$construction,
                                              p_ueDemand, p_dt, p_discountFac)
  }

  ### renovation ####

  renLcc <- computeLCC(lt$renovation, p_specCostOpe, costRen, p_dt, p_discountFac) %>%
    .renameCostType()

  out[["LCC|Renovation|Agg: vin"]] <- mutate(
    .avgAlongDim(renLcc, v_renovation, "vin", c("qty", "bs", "hs", "hsr", "vin", subs, ttotIn = "ttot")),
    bs = "low"
  )

  # If desired, filter the full resolution renovation data:
  out[["LCC|Renovation"]] <- .filterAsUnion(renLcc, filterFullRen)

  renLcoh <- computeLCOH(
    renLcc, lt$renovation,
    p_ueDemand, p_dt, p_discountFac
  )

  out[["LCOH|Renovation"]] <- .filterAsUnion(renLcoh, filterFullRen)

  out[["LCOH|Renovation|Agg: vin"]] <- .avgAlongDim(
    renLcoh,
    v_renovation,
    "vin",
    c("qty", "bs", "hs", "hsr", "vin", subs, ttotIn = "ttot")
  )

  out[["LCOH|Renovation|Agg: hs, vin"]] <- .avgAlongDim(
    renLcoh,
    v_renovation,
    c("hs", "vin"),
    c("qty", "bs", "hs", "hsr", "vin", subs, ttotIn = "ttot")
  )



  # COMPUTE LOGIT HEATING SYSTEM SHARES ----------------------------------------

  renLogit <- computeLogitShare("renovation", renLcc, priceSensHs[["renovation"]])
  out[["Logit Share|Renovation"]] <- renLogit %>%
    .filterAsUnion(filterFullRen)

  # Aggregate over initial hs
  out[["Logit Share|Renovation|Agg: hs"]] <- aggregateShare(renLogit, dimAgg = "hs", weight = v_renovationOut)
  # Aggregate and filter for energy ladder level 1
  out[["Logit Share|Renovation|Energy Ladder 1|Agg: hs"]] <- aggregateShare(
    renLogit,
    dimAgg = "hs",
    weight = v_renovationOut,
    energyLadder = energyLadder,
    energyLadderNo = 1
  )
  # Aggregate over vintage
  out[["Logit Share|Renovation|Agg: vin"]] <- aggregateShare(renLogit, dimAgg = "vin", weight = v_renovationOut)



  # COMPUTE BRICK HEATING SYSTEM SHARES ----------------------------------------

  renBrick <- computeBrickShare("renovation", rename(v_renovation, ttotIn = "ttot"))
  out[["Brick Share|Renovation"]] <- renBrick %>%
    .filterAsUnion(filterFullRen)

  # Aggregate over initial hs
  out[["Brick Share|Renovation|Agg: hs"]] <- aggregateShare(renBrick, dimAgg = "hs", weight = v_renovationOut)
  # Aggregate and filter for energy ladder level 1
  out[["Brick Share|Renovation|Energy Ladder 1|Agg: hs"]] <- aggregateShare(
    renBrick,
    dimAgg = "hs",
    weight = v_renovationOut,
    energyLadder = energyLadder,
    energyLadderNo = 1
  )
  # Aggregate over vin
  out[["Brick Share|Renovation|Agg: vin"]] <- aggregateShare(renBrick, dimAgg = "vin", weight = v_renovationOut)



  # LINEARIZED LOGISTIC MODEL --------------------------------------------------

  hsNames <- unique(v_renovationIn$hs)

  # Ratio of all renovation flows w.r.t to the flow of a reference heating system (as hsr)
  out[[paste0("Log Ratio to ", hsRef, "|Renovation")]] <- v_renovation %>%
    rename(ttotIn = "ttot") %>%
    filter(.data$hsr != "0") %>%
    pivot_wider(names_from = "hsr") %>%
    mutate(across(
      setdiff(hsNames, hsRef),
      ~ ifelse(.x == 0 | .data[[hsRef]] == 0, NA, log(.x / .data[[hsRef]]))
    )) %>%
    pivot_longer(cols = setdiff(hsNames, hsRef), names_to = "hsr") %>%
    select(-hsRef)

  # Difference in LCC w.r.t to the value of a reference heating system (as hsr)
  out[[paste0("LCC Difference to ", hsRef, "|Renovation")]] <- renLcc %>%
    group_by(across(-all_of(c("costType", "value")))) %>%
    summarise(value = sum(.data$value), .groups = "drop") %>%
    pivot_wider(names_from = "hsr") %>%
    mutate(across(setdiff(hsNames, hsRef), ~ .x - .data[[hsRef]])) %>%
    pivot_longer(cols = setdiff(hsNames, hsRef), names_to = "hsr") %>%
    select(-hsRef)

  # Price sensitivity
  out <- c(
    out,
    lapply(
      stats::setNames(
        priceSensBs,
        c("Price Sensitivity|Building Shell|Construction", "Price Sensitivity|Building Shell|Renovation")
      ),
      function(x) data.frame(value = x)
    ),
    lapply(
      stats::setNames(
        priceSensHs,
        c("Price Sensitivity|Heating System|Construction", "Price Sensitivity|Heating System|Renovation")
      ),
      function(x) data.frame(value = x)
    )
  )



  # NORMALIZE PRICE SENSITIVITY ------------------------------------------------

  if (isFALSE(fixedBuildings)) {
    out[["Normalized Price Sensitivity|Heating System|Construction"]] <- normalizePriceSensitivity(
      out[["LCC|Construction"]], conVin, priceSensHs[["construction"]],
    )
    out[["Normalized Price Sensitivity by all Subsets|Heating System|Construction"]] <- normalizePriceSensitivity(
      out[["LCC|Construction"]], conVin, priceSensHs[["construction"]],
      groupCols = c("region", "loc", "typ", "inc")
    )
  }

  out[["Normalized Price Sensitivity|Heating System|Renovation"]] <- normalizePriceSensitivity(
    renLcc, v_renovation, priceSensHs[["renovation"]], timeDimWeight = "ttot",
  )

  out[["Normalized Price Sensitivity by all Subsets|Heating System|Renovation"]] <- normalizePriceSensitivity(
    renLcc, v_renovation, priceSensHs[["renovation"]], timeDimWeight = "ttot",
    groupCols = c("region", "loc", "typ", "inc")
  )



  # WRITE ----------------------------------------------------------------------

  # Determine all dimensions present in output data
  allSets <- unique(unlist(lapply(out, colnames)))
  orderedColnames <- union(
    c("variable", setdiff(colnames(v_renovation), c("ttot", "value")),
      "costType", "ttotIn", "ttotOut", "relVal", "absVal", "value"),
    allSets
  )

  # Convert to one data frame
  out <- do.call(rbind, lapply(names(out), function(varName) {
    .expandDims(out[[varName]], varName, allSets)
  })) %>%
    select(all_of(orderedColnames))

  write.csv(out, file.path(path, "BRICK_Heating_System_report.csv"), row.names = FALSE)

}

#' Read data from gdx and clean
#'
#' @param gdx path to the gdx
#' @param symbol character, symbol to read from gdx
#' @param vinExists data frame with existing vintage and time period combinations
#' @param renAllowed data frame with all possible renovation transitions
#' @param asMagpie logical, whether to convert the data to a magpie opject
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

  if (all(c("hs", "hsr", "bs") %in% colnames(df)) && !is.null(renAllowed)) {
    df <- df %>%
      dplyr::right_join(renAllowed, by = c("hs", "hsr", "bs"))
  }

  df
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

#' Filter a column for a specific value and remove that column
#'
#' @param df data frame with the data to be filtered
#' @param col character, name of column to be filtered
#' @param val single value of varying type, value to filter \code{col} for
#'
.filterAndUnselect <- function(df, col, val) {
  if (col %in% colnames(df)) {
    df <- df %>%
      filter(.data[[col]] == val) %>%
      select(-col)
  }
  df
}

#' Rename the entries of the cost type column to more legible names
#'
#' @param df data frame with cost type column to be modified
#'
.renameCostType <- function(df) {
  if ("costType" %in% colnames(df)) {
    costNameMap <- c(
      tangible = "Investment (tangible)",
      intangible = "Investment (intangible)",
      lccOpe = "Operational",
      statusQuoPref = "Status quo preference"
    )

    df <- mutate(df, costType = costNameMap[.data$costType])
  }

  df
}

#' Compute data average along given dimensions
#'
#' @param df data frame with the data to be manipulated
#' @param weight data frame with weights to apply to the average
#' @param dimToAvg character column name of the column to average over
#' @param joinCols character, columns to join \code{df} and \code{weight} by
#' @param ... named character vector with tuples to be renamed
#'
#' @importFrom dplyr %>% .data across any_of group_by rename summarise
#'
.avgAlongDim <- function(df, weight, dimToAvg, joinCols, ...) {

  df %>%
    left_join(weight,
              by = joinCols,
              suffix = c("", ".weight")) %>%
    group_by(across(-any_of(c(dimToAvg, "value", "value.weight")))) %>%
    mutate(value.weight = if (all(.data$value.weight == 0)) 1 else .data$value.weight) %>%
    summarise(value = stats::weighted.mean(.data[["value"]], .data$value.weight), .groups = "drop") %>%
    rename(...)
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
