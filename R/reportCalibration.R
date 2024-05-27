#' Read in model results from calibration for each iteration, calculate deviation
#'
#'
#' @param gdx path to a gdx; it is assumed that for each iteration a gdx is present
#'  with this path and the iteration number inserted at the end.
#' @param flowTargets logical, if set to FALSE, the function does not expect the presence of targets
#'   for the flows
#'
#' @author Ricarda Rosemann
#'
#' @importFrom tidyr crossing replace_na
#' @importFrom utils read.csv write.csv
#' @importFrom yaml read_yaml
#' @export
#'
reportCalibration <- function(gdx, flowTargets = TRUE) {



  # READ -----------------------------------------------------------------------


  ## Auxiliary data and config info ====

  # Determine last iteration
  path <- dirname(gdx)
  allFiles <- list.files(path = path,
                         pattern = paste0(sub("_0\\.gdx$", "", basename(gdx)),
                                          "_\\d{1,3}\\.gdx"))
  maxIter <- max(as.numeric(sub(".*_(\\d{1,3})\\.gdx$", "\\1", allFiles)))

  gdxInp <- file.path(path, "input.gdx")

  # Read config
  cfg <- read_yaml(file = file.path(path, "config", "config_COMPILED.yaml"))

  # Read relevant time periods
  tCalib <- cfg[["calibperiods"]]

  # Read calibration type and whether vintages are aggregated
  calibOptim <- cfg[["switches"]][["RUNTYPE"]] == "calibrationOptimization"
  aggVin <- cfg[["switches"]][["AGGREGATEDIM"]] == "vin"


  ## Diagnostic parameters ====

  diagnostics <- FALSE
  if (all(file.exists(file.path(path, "stepSizeParamsIter.csv"),
                      file.path(path, "deviationConIter.csv"),
                      file.path(path, "deviationRenIter.csv"),
                      file.path(path, "outerObjectiveAllIter.csv")))) {
    stepSize <- read.csv(file.path(path, "stepSizeParamsIter.csv")) %>%
      select(-any_of(c("delta", "phiDeriv", "minOuterObj", "minStepSize"))) %>%
      rename(value = "stepSize")
    descDirCon <- read.csv(file.path(path, "deviationConIter.csv")) %>%
      rename(value = "d") %>%
      .replaceVarName()
    descDirRen <- read.csv(file.path(path, "deviationRenIter.csv")) %>%
      rename(value = "d")
    outerObjective <- read.csv(file.path(path, "outerObjectiveAllIter.csv")) %>%
      filter(.data$iterA == 1) %>%
      select(-any_of(c("fA", "iterA"))) %>%
      rename(value = "f")
    diagnostics <- TRUE
  }


  ## Stock and flow results ====

  # Potentially shift the time filter to a later stage if I want to save and plot pure stock/flow data
  v_stock <- .readGdxIter(gdx,
                          if (calibOptim) "p_stock" else "v_stock",
                          maxIter, asMagpie = FALSE, ttotFilter = tCalib, replaceVar = TRUE)

  v_construction <- .readGdxIter(gdx,
                                 if (calibOptim) "p_construction" else "v_construction",
                                 maxIter,
                                 asMagpie = FALSE, ttotFilter = tCalib, replaceVar = TRUE)

  v_renovation <- .readGdxIter(gdx,
                               if (calibOptim) "p_renovation" else "v_renovation", maxIter,
                               asMagpie = FALSE, ttotFilter = tCalib)

  dims <- .getDims(list(stock = v_stock, construction = v_construction, renovation = v_renovation),
                   removeVin = aggVin)

  # Apply potential removal of vintage dimension
  v_stock <- .computeSum(v_stock, rprt = dims$stock)
  v_construction <- .computeSum(v_construction, rprt = dims$construction) # Currently not necessary
  v_renovation <- .computeSum(v_renovation, rprt = dims$renovation)


  ## Specific costs ====

  p_specCostCon <- .readGdxIter(gdx, "p_specCostCon",
                                maxIter, asMagpie = FALSE, ttotFilter = tCalib, replaceVar = TRUE) %>%
    filter(.data$cost == "intangible")

  p_specCostRen <- .readGdxIter(gdx, "p_specCostRen",
                                maxIter, asMagpie = FALSE, ttotFilter = tCalib) %>%
    filter(.data$cost == "intangible")


  ## Calibration targets ====

  p_stockCalibTarget <- readGdxSymbol(gdxInp, "p_stockCalibTarget", asMagpie = FALSE) %>%
    .replaceVarName() %>%
    .computeSum(rprt = dims$stock)

  p_stockCalibTargetTot <- .computeSum(p_stockCalibTarget, rprt = c("iteration", "reg", "typ", "loc", "inc", "ttot"))
  p_stockCalibTargetTotHs <- .computeSum(
    p_stockCalibTarget,
    rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot")
  )

  if (isTRUE(flowTargets)) {
    p_constructionCalibTarget <- readGdxSymbol(gdxInp, "p_constructionCalibTarget", asMagpie = FALSE) %>%
      .replaceVarName() %>%
      .computeSum(rprt = dims$construction)

    p_constructionCalibTargetTot <- .computeSum(
      p_constructionCalibTarget,
      rprt = c("iteration", "reg", "typ", "loc", "inc", "ttot")
    )
    p_constructionCalibTargetTotHs <- .computeSum(
      p_constructionCalibTarget,
      rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot")
    )

    p_renovationCalibTarget <- readGdxSymbol(gdxInp, "p_renovationCalibTarget", asMagpie = FALSE) %>%
      .computeSum(rprt = dims$renovation)

    p_renovationCalibTargetTot <- .computeSum(
      p_renovationCalibTarget %>%
        filter(.data$hsr != "0"),
      rprt = c("iteration", "reg", "typ", "loc", "inc", "ttot")
    )
    p_renovationCalibTargetTotHs <- .computeSum(
      p_renovationCalibTarget,
      rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot")
    )
  }



  # Aggregate quantities -------------------------------------------------------

  v_stockTot <- .computeSum(v_stock, rprt = c("iteration", "reg", "typ", "loc", "inc", "ttot"))
  v_stockTotHs <- .computeSum(v_stock, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot"))

  if (isTRUE(flowTargets)) {
    v_constructionTot <- .computeSum(v_construction, rprt = c("iteration", "reg", "typ", "loc", "inc", "ttot"))
    v_constructionTotHs <- .computeSum(v_construction, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot"))

    v_renovationTot <- .computeSum(
      v_renovation %>%
        filter(.data$hsr != 0),
      rprt = c("iteration", "reg", "typ", "loc", "inc", "ttot")
    )
    v_renovationTotHs <- .computeSum(v_renovation, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot"))
  }



  # COMPUTE DEVIATIONS ---------------------------------------------------------

  v_stockDev <- .computeDeviation(v_stock, p_stockCalibTarget)

  v_stockTotDev <- .computeDeviation(v_stockTot, p_stockCalibTargetTot)
  v_stockTotHsDev <- .computeDeviation(v_stockTotHs, p_stockCalibTargetTotHs)

  if (isTRUE(flowTargets)) {
    v_constructionDev <- .computeDeviation(v_construction, p_constructionCalibTarget)

    v_constructionTotDev <- .computeDeviation(v_constructionTot, p_constructionCalibTargetTot)
    v_constructionTotHsDev <- .computeDeviation(v_constructionTotHs, p_constructionCalibTargetTotHs)

    v_renovationDev <- .computeDeviation(v_renovation, p_renovationCalibTarget)

    v_renovationTotDev <- .computeDeviation(v_renovationTot, p_renovationCalibTargetTot)
    v_renovationTotHsDev <- .computeDeviation(v_renovationTotHs, p_renovationCalibTargetTotHs)

    p_renovationDevSepGabo <- v_renovationDev %>%
      mutate(hsr = as.character(.data[["hsr"]]),
             hsr = ifelse(.data[["hsr"]] == "gabo" & .data[["hs"]] == "gabo", "gabo_id", .data[["hsr"]]),
             hsr = factor(.data[["hsr"]]))
  }



  # STORE OUTPUT QUANTITIES TO LIST --------------------------------------------

  out <- list()


  ## Aggregate total quantities ====

  if (isTRUE(diagnostics)) {
    out[["stepSize"]] <- stepSize

    out[["descDirCon"]] <- .computeAvg(descDirCon, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot"))

    out[["descDirRen"]] <- .computeAvg(descDirRen, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot"))

    # Aggregate d, i.e. the direction of steepest descent by heating system for late iterations
    out[["descDirConLate"]] <- out[["descDirCon"]] %>%
      filter(.data[["iteration"]] >= floor(0.4 * maxIter))

    out[["descDirRenLate"]] <- out[["descDirRen"]] %>%
      filter(.data[["iteration"]] >= floor(0.4 * maxIter))

    out[["outerObjective"]] <- outerObjective
  }

  # Aggregate specific costs
  out[["specCostCon"]] <- .computeAvg(p_specCostCon, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot"))

  out[["specCostRen"]] <- .computeAvg(p_specCostRen, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot"))

  # Aggregate by heating system (hs)
  out[["stockHs"]] <- v_stockTotHs

  out[["conHs"]] <- v_constructionTotHs

  out[["renHs"]] <- v_renovationTotHs

  out[["flowHs"]] <- .computeFlowSum(out[["conHs"]], out[["renHs"]])

  # Aggregate by vintage (vin)
  if (isFALSE(aggVin)) {
    out[["stockVin"]] <- .computeSum(v_stock, rprt = c("iteration", "reg", "typ", "loc", "inc", "vin", "ttot"))

    out[["renVin"]] <- .computeSum(v_renovation, rprt = c("iteration", "reg", "typ", "loc", "inc", "vin", "ttot"))
  }


  ## Aggregate deviations ====

  # Aggregate across all dimensions
  out[["stockDevAgg"]] <- .computeSumSq(v_stockDev, rprt = c("iteration", "reg", "typ", "loc", "inc", "ttot"),
                                        addSign = FALSE)

  if (isTRUE(flowTargets)) {
    out[["conDevAgg"]] <- .computeSumSq(v_constructionDev, rprt = c("iteration", "reg", "typ", "loc", "inc", "ttot"),
                                        addSign = FALSE)

    out[["renDevAgg"]] <- .computeSumSq(v_renovationDev, rprt = c("iteration", "reg", "typ", "loc", "inc", "ttot"),
                                        addSign = FALSE)

    out[["flowDevAgg"]] <- .computeFlowSum(out[["conDevAgg"]], out[["renDevAgg"]])
  }

  # Aggregate by heating system (hs)
  out[["stockDevHs"]] <- .computeSumSq(v_stockDev, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot"))

  if (isTRUE(flowTargets)) {
    out[["conDevHs"]] <- .computeSumSq(
      v_constructionDev,
      rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot")
    )

    out[["renDevHs"]] <- .computeSumSq(
      v_renovationDev,
      rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot")
    )

    out[["flowDevHs"]] <- .computeFlowSum(out[["conDevHs"]], out[["renDevHs"]])

    out[["renDevSepGabo"]] <- .computeSumSq(
      p_renovationDevSepGabo,
      rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot")
    )
  }

  # Aggregate by vintage (vin)
  if (isFALSE(aggVin)) {
    out[["stockDevVin"]] <- .computeSumSq(v_stockDev, rprt = c("iteration", "reg", "typ", "loc", "inc", "vin", "ttot"))

    if (isTRUE(flowTargets)) {
      out[["renDevVin"]] <- .computeSumSq(
        v_renovationDev,
        rprt = c("iteration", "reg", "typ", "loc", "inc", "vin", "ttot")
      )
    }
  }


  ## Relative aggregate deviations ====

  # Across all dimensions
  out[["stockDevRel"]] <- .computeRelDev(out[["stockDevAgg"]], p_stockCalibTarget, tCalib)

  if (isTRUE(flowTargets)) {
    out[["conDevRel"]] <- .computeRelDev(out[["conDevAgg"]], p_constructionCalibTarget, tCalib)

    out[["renDevRel"]] <- .computeRelDev(out[["renDevAgg"]], p_renovationCalibTarget, tCalib)

    out[["flowDevRel"]] <- .computeRelDev(out[["flowDevAgg"]], list(p_constructionCalibTarget, p_renovationCalibTarget),
                                          tCalib)
  }

  # Separately for all heating systems (hs)
  out[["stockDevHsRel"]] <- .computeRelDev(out[["stockDevHs"]], p_stockCalibTarget, tCalib, notInTargetGrp = "hsr")

  if (isTRUE(flowTargets)) {
    out[["conDevHsRel"]] <- .computeRelDev(out[["conDevHs"]], p_constructionCalibTarget, tCalib, notInTargetGrp = "hsr")

    out[["renDevHsRel"]] <- .computeRelDev(out[["renDevHs"]], p_renovationCalibTarget, tCalib, notInTargetGrp = "hsr")

    out[["flowDevHsRel"]] <- .computeRelDev(
      out[["flowDevHs"]],
      list(p_constructionCalibTarget, p_renovationCalibTarget),
      tCalib, notInTargetGrp = "hsr"
    )

    out[["renDevSepGaboRel"]] <- .computeRelDev(out[["renDevSepGabo"]], p_renovationCalibTarget,
                                                tCalib, notInTargetGrp = "hsr")
  }

  # Deviation share for all heating systems (hs)
  out[["stockDevHsShare"]] <- .computeRatioSq(out[["stockDevHs"]], out[["stockDevAgg"]])

  if (isTRUE(flowTargets)) {
    out[["conDevHsShare"]] <- .computeRatioSq(out[["conDevHs"]], out[["conDevAgg"]])

    out[["renDevHsShare"]] <- .computeRatioSq(out[["renDevHs"]], out[["renDevAgg"]])

    out[["flowDevHsShare"]] <- .computeRatioSq(out[["flowDevHs"]], out[["flowDevAgg"]])
  }

  # Separately for all vintages (vin)
  if (isFALSE(aggVin)) {
    out[["stockDevVinRel"]] <- .computeRelDev(out[["stockDevVin"]], p_stockCalibTarget, tCalib, notInTargetGrp = "vin")

    if (isTRUE(flowTargets)) {
      out[["renDevVinRel"]] <- .computeRelDev(out[["renDevVin"]], p_renovationCalibTarget,
                                              tCalib, notInTargetGrp = "vin")
    }
  }

  ## Deviations of aggregates

  out[["stockTotDev"]] <- v_stockTotDev

  out[["stockTotHsDev"]] <- v_stockTotHsDev

  if (isTRUE(flowTargets)) {
    out[["conTotDev"]] <- v_constructionTotDev

    out[["conTotHsDev"]] <- v_constructionTotHsDev

    out[["renTotDev"]] <- v_renovationTotDev

    out[["renTotHsDev"]] <- v_renovationTotHsDev
  }


  # EXPAND DIMENSIONS AND COMBINE IN ONE DATA FRAME ----------------------------

  # Determine all dimensions present in output data
  allSets <- unique(unlist(lapply(out, colnames)))

  out <- do.call(rbind, lapply(names(out), function(varName) {
    .expandDims(out[[varName]], varName, allSets)
  }))


  # WRITE OUTPUT FILE ----------------------------------------------------------

  outName <- "BRICK_calibration_report.csv"
  write.csv(out, file.path(path, outName), row.names = FALSE)

}


#' Get dimension names from stock and flow objects
#'
#' Gives the column names of a list of data frames except the first and the last
#' one, i.e. \code{qty} and \code{value}.
#'
#' @param calibObj list, calibration targets
#' @param removeVin logical, whether vintage dimension should be removed
#'
.getDims <- function(calibObj, removeVin) {
  removeDim <- if (isTRUE(removeVin)) "vin" else NULL
  lapply(calibObj, function(obj) {
    dims <- colnames(obj)
    setdiff(dims[2:(length(dims) - 1)], removeDim)
  })
}

#' Read a symbol from several gdx files and combine in one data frame
#'
#' @param gdx Path to gdx files; all gdx file names are assumed to be numbered and
#'  adhere to the structure: If \code{gdx = <path/to/file/name.gdx}, the function looks for
#'  \code{<path/to/file/name_0.gdx>}, \code{<path/to/file/name_1.gdx>}, ...
#' @param symbol Symbol to be read from the gdxes
#' @param maxIter Last iteration to be read
#' @param minIter First iteration to be read
#' @param asMagpie logical, convert to magpie object?
#' @param ttotFilter numeric/factor, time periods to filter for
#' @param replaceVar logical, replace column names \code{bs} and \code{hs} by \code{bsr} and \code{hsr}?
#' @param dims character, containing the colum names expected for the object that is read in.
#'   Only required if the object does not exist to create replacement with NAs
#'
#' @returns data frame of all results read in
#'
#' @importFrom dplyr %>% .data filter mutate
#'
.readGdxIter <- function(gdx, symbol, maxIter, minIter = 0, asMagpie = TRUE,
                         ttotFilter = NULL, replaceVar = FALSE, dims = NULL) {

  # Loop over all iterations and read in gdx files
  res <- data.frame()
  for (i in seq(minIter, maxIter)) {
    fileName <- file.path(dirname(gdx), paste0(gsub("_0\\.gdx$", "", basename(gdx)), "_", i, ".gdx"))
    if (file.exists(fileName)) {
      resThis <- readGdxSymbol(fileName, symbol, asMagpie = asMagpie)
      if (!is.null(resThis)) {
        res <- rbind(
          res,
          resThis %>%
            mutate(iteration = i, .before = "value")
        )
      } else {
        warning("The variable ", symbol, " is null. This might lead to subsequent errors.")
        if (!is.null(dims)) {
          resThis <- data.frame(matrix(NA, nrow = 1, ncol = length(dims)))
          names(resThis) <- dims
          return(resThis)
        } else {
          return(NULL)
        }
      }
    } else {
      warning(paste("Data for iteration", i, "is missing. Skipping this iteration"))
    }
  }

  # Might move this elsewhere, e.g. to a separate function
  # Filter for given time periods
  if (!is.null(ttotFilter)) {
    res <- res %>%
      filter(.data[["ttot"]] %in% ttotFilter)
  }

  # If specified, replace bs and hs column names
  if (isTRUE(replaceVar)) {
    res <- .replaceVarName(res)
  }

  return(res)
}

#' Replace column names \code{bs} and \code{hs} by \code{bsr} and \code{hsr}
#'
#' @param df data frame
#' @returns data frame where column names have been replaced
#'
.replaceVarName <- function(df) {
  var <- names(df) %in% c("hs", "bs")
  names(df)[var] <- paste0(names(df)[var], "r")
  return(df)
}

#' Compute the deviation to target data
#'
#' @param df data frame, containing data from calibration run
#' @param dfTarget data frame, containing historical data as calibration target
#' @returns data frame where the value column contains the deviation from target data
#'
#' @importFrom dplyr %>% .data filter full_join mutate rename select
#' @importFrom tidyr crossing replace_na
#'
.computeDeviation <- function(df, dfTarget) {

  # Add iteration column to historical data and filter for time periods of calibration data in df
  dfTarget <- dfTarget %>%
    crossing(iteration = df[["iteration"]]) %>%
    filter(.data[["ttot"]] %in% unique(df[["ttot"]])) %>%
    rename(target = "value")

  # Replace all data points not present in either data frames by zero and compute the difference
  df <- df %>%
    full_join(dfTarget,
              by = intersect(colnames(df), colnames(dfTarget))) %>% # Rather unsure about this ...
    replace_na(list(value = 0, target = 0)) %>%
    mutate(value = .data[["value"]] - .data[["target"]]) %>%
    select(-"target")

}

#' Compute the mean value in a data frame
#'
#' @param df data frame, containing the data to be evaluated
#' @param rprt character, column names for which the mean should be reported separately
#'
#' @returns data frame averages as value column
#'
#' @importFrom dplyr %>% across any_of .data group_by summarise
#'
.computeAvg <- function(df, rprt = "") {

  df %>%
    group_by(across(any_of(rprt))) %>%
    summarise(value = mean(.data[["value"]], na.rm = TRUE), .groups = "drop")

}

#' Compute the sum
#'
#' @param df data frame containing the data to be evaluated
#' @param rprt character, column names for which the sum should be reported separately
#'
#' @returns data frame with summed values
#' @importFrom dplyr %>% .data across any_of group_by summarise
#'
.computeSum <- function(df, rprt = "") {

  df %>%
    group_by(across(any_of(rprt))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop")

}

#' Compute the sum of the squares in a data frame
#'
#' @param df data frame, containing the data to be evaluated
#' @param rprt character, column names for which the sum of the squares should be reported separately
#' @param addSign logical indicating whether the heuristic sign of the square should be computed
#'
#' @returns data frame sum of squares as value column
#'
#' @importFrom dplyr %>% across all_of any_of .data group_by rename_with summarise ungroup
#'
.computeSumSq <- function(df, rprt = "", addSign = TRUE) {

  df %>%
    mutate(sgn = if (isTRUE(addSign)) sign(.data[["value"]]) else 1) %>%
    group_by(across(any_of(rprt))) %>%
    summarise(valuePos = sum(.data[["value"]] ^ 2, na.rm = TRUE),
              valueSgn = sum(.data[["sgn"]] * .data[["value"]] ^ 2, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(sgn = if (isTRUE(addSign)) sign(.data[["valueSgn"]]) else 1,
           value = .data[["sgn"]] * sqrt(abs(.data[["valuePos"]])),
           valuePos = sqrt(.data[["valuePos"]])) %>%
    select(-"sgn", -"valueSgn", -"valuePos")

}

#' Compute the relative deviation to calibration target data as the relative euclidean distance
#'
#' @param dfDev data frame, containing deviation between calibration result and target data
#' @param dfTarget data frame or a list of one or two data frames, containing target data.
#'  The option to pass two data frames is used to combine construction and renovation flows.
#'  If two data frames are passed, the first one is assumed to be construction, the second renovation
#' @param tCalib numerical/factor, calibration time periods to filter calibration target data
#' @param notInTargetGrp character vector, pass columns that target data should not be grouped by
#'  when computing the sum of the squares, although they exist in the data
#' @returns data frame with relative deviation in value column
#'
#' @importFrom dplyr %>% .data filter left_join mutate rename select
#'
.computeRelDev <- function(dfDev, dfTarget, tCalib, notInTargetGrp = NULL) {

  # Determine the reported columns in the calibration data
  rprt <- setdiff(colnames(dfDev), c("iteration", "value", notInTargetGrp))

  # Convert target data to list if necessary
  if (is.data.frame(dfTarget)) dfTarget <- list(dfTarget)

  # Compute the sum of the squares for target data
  dfTargetSumList <- lapply(dfTarget, function(df) {
    df %>%
      filter(.data[["ttot"]] %in% tCalib) %>%
      .computeSumSq(rprt = rprt, addSign = FALSE)
  })

  if (length(dfTargetSumList) == 1) {
    dfTargetSum <- dfTargetSumList[[1]] %>%
      rename(target = "value")
  } else if (length(dfTargetSumList) >= 2) {

    # If two data frames with target data have been passed:
    # Combine them by treating the first as construction flows and the second as renovation flows
    dfTargetSum <- .computeFlowSum(dfTargetSumList[[1]], dfTargetSumList[[2]]) %>%
      rename(target = "value")
  }

  if (length(dfTargetSumList) > 2) {
    warning(paste("Only two data frames of historical data can be combined.",
                  "The remaining data frames are ignored."))
  }

  # Compute the relative deviation as the square root of the deviation divided
  # by the square root of the historical data
  dfDev <- dfDev %>%
    left_join(dfTargetSum, by = rprt) %>%
    mutate(value = sign(.data[["value"]]) * sqrt(abs(.data[["value"]])) / sqrt(.data[["target"]])) %>%
    select(-"target")
}

#' Compute the sum of construction and renovation flow values
#'
#' @param con data frame, contains construction flow quantities
#' @param ren data frame, contains renovation flow quantities
#' @returns data frame with the sum in the value column
#'
#' @importFrom dplyr %>% across all_of .data full_join mutate rename select
#'
.computeFlowSum <- function(con, ren) {

  # If columns bsr and hsr are present in the data:
  # Convert the construction data to the factor levels of the renovation data
  for (var in c("hsr", "bsr")) {

    if (var %in% colnames(ren)) {
      if (var %in% colnames(con)) {
        con <- con %>%
          mutate(across(all_of(var), ~ factor(.data[[var]], levels(ren[[var]]))))
      } else {
        stop("Construction and renovation flow data do not match.")
      }
    }
  }

  # Compute the combined flow data as the sum of construction and renovation;
  # NA values in the construction flows are replaced by zeros, thus for "0" flows only renovation is reflected.
  full_join(con %>%
              rename(con = "value"),
            ren %>%
              rename(ren = "value"),
            by = setdiff(intersect(colnames(con), colnames(ren)), "value")) %>%
    replace_na(list(con = 0)) %>%
    mutate(value = sign(.data[["con"]] + .data[["ren"]]) * abs(.data[["con"]] + .data[["ren"]])) %>%
    select(-"con", -"ren")
}

#' Compute the ratio of the squares for two data sets
#'
#' @param dfx data frame containing the data of the nominator
#' @param dfy data frame containing the data of the denominator
#' @param addSign logical indicating whether a heuristic sign of the squares should be computed
#'
#' @returns data frame with squared ratio
#'
#' @importFrom dplyr %>% .data left_join mutate select
#'
.computeRatioSq <- function(dfx, dfy, addSign = TRUE) {
  left_join(dfx, dfy, by = setdiff(intersect(colnames(dfx), colnames(dfy)), "value")) %>%
    mutate(sgn = if (isTRUE(addSign)) sign(.data[["value.x"]] / .data[["value.y"]]) else 1,
           value = .data[["sgn"]] * .data[["value.x"]] ^ 2 / .data[["value.y"]] ^ 2) %>%
    select(-"value.x", -"value.y", -"sgn")
}

#' Extend dimensions of a data frame by adding NA entries, add variable name
#'
#' @param df data frame to be extended
#' @param varName character, variable name to be added
#' @param allSets character, sets that need to be included as column names
#' @returns data frame
#'
#' @importFrom dplyr %>% mutate last_col relocate
#'
.expandDims <- function(df, varName, allSets) {

  # Add missing columns with NA entries
  df[setdiff(allSets, colnames(df))] <- NA

  # Add variable name as first column
  df <- df %>%
    mutate(variable = varName, .before = 1) %>%
    relocate("value", .after = last_col())
}
