#' Read in model results from calibration for each iteration, calculate deviation
#'
#'
#' @param gdx path to a gdx; it is assumed that for each iteration a gdx is present
#'  with this path and the iteration number inserted at the end.
#'
#' @author Ricarda Rosemann
#'
#' @importFrom tidyr crossing replace_na
#' @importFrom utils read.csv write.csv
#' @importFrom yaml read_yaml
#' @export

reportCalibration <- function(gdx) {

  # READ -----------------------------------------------------------------------

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

  calibOptim <- cfg[["switches"]][["RUNTYPE"]] == "calibrationOptimization"

  # Read diagnostic parameters from csv
  diagnostics <- FALSE
  if (all(file.exists(file.path(path, "stepSizeParamsIter.csv"),
                      file.path(path, "deviationConIter.csv"),
                      file.path(path, "deviationRenIter.csv")))) {
    stepSize <- read.csv(file.path(path, "stepSizeParamsIter.csv")) %>%
      select(-"delta", -"phiDeriv") %>%
      rename(value = "stepSize")
    descDirCon <- read.csv(file.path(path, "deviationConIter.csv")) %>%
      rename(value = "d") %>%
      .replaceVarName()
    descDirRen <- read.csv(file.path(path, "deviationRenIter.csv")) %>%
      rename(value = "d")
    diagnostics <- TRUE
  }

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

  # Read input gdx file
  p_stockCalibTarget <- .replaceVarName(readGdxSymbol(gdxInp,
                                                      if (calibOptim) "p_stockCalibTarget" else "p_stockHist",
                                                      asMagpie = FALSE))

  p_constructionCalibTarget <- .replaceVarName(readGdxSymbol(gdxInp, "p_constructionCalibTarget", asMagpie = FALSE))

  p_renovationCalibTarget <- readGdxSymbol(gdxInp, "p_renovationCalibTarget", asMagpie = FALSE)


  # COMPUTE DEVIATIONS ---------------------------------------------------------

  v_stockDev <- .computeDeviation(v_stock, p_stockCalibTarget)

  v_constructionDev <- .computeDeviation(v_construction, p_constructionCalibTarget)

  v_renovationDev <- .computeDeviation(v_renovation, p_renovationCalibTarget)

  # WRITE DIAGNOSTIC OUTPUT ----------------------------------------------------

  out <- list()

  if (isTRUE(diagnostics)) {
    out[["stepSize"]] <- stepSize

    out[["descDirCon"]] <- .computeAvg(descDirCon, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot"))

    out[["descDirRen"]] <- .computeAvg(descDirRen, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr", "ttot"))
  }

  # AGGREGATE QUANTITIES -------------------------------------------------------

  # Aggregate across all dimensions
  out[["stockDevAgg"]] <- .computeSumSq(v_stockDev, rprt = c("iteration", "region", "typ", "loc", "inc"))

  out[["conDevAgg"]] <- .computeSumSq(v_constructionDev, rprt = c("iteration", "region", "typ", "loc", "inc"))

  out[["renDevAgg"]] <- .computeSumSq(v_renovationDev, rprt = c("iteration", "region", "typ", "loc", "inc"))

  out[["flowDevAgg"]] <- .computeFlowSum(out[["conDevAgg"]], out[["renDevAgg"]])

  # Aggregate by heating system (hs)
  out[["stockDevHs"]] <- .computeSumSq(v_stockDev, rprt = c("iteration", "region", "typ", "loc", "inc", "hsr"))

  out[["conDevHs"]] <- .computeSumSq(v_constructionDev, rprt = c("iteration", "region", "typ", "loc", "inc", "hsr"))

  out[["renDevHs"]] <- .computeSumSq(v_renovationDev, rprt = c("iteration", "region", "typ", "loc", "inc", "hsr"))

  out[["flowDevHs"]] <- .computeFlowSum(out[["conDevHs"]], out[["renDevHs"]])


  # COMPUTE RELATIVE AGGREGATE QUANTITIES --------------------------------------

  # Across all dimensions
  out[["stockDevRel"]] <- .computeRelDev(out[["stockDevAgg"]], p_stockCalibTarget, tCalib)

  out[["conDevRel"]] <- .computeRelDev(out[["conDevAgg"]], p_constructionCalibTarget, tCalib)

  out[["renDevRel"]] <- .computeRelDev(out[["renDevAgg"]], p_renovationCalibTarget, tCalib)

  out[["flowDevRel"]] <- .computeRelDev(out[["flowDevAgg"]], list(p_constructionCalibTarget, p_renovationCalibTarget),
                                        tCalib)

  # Separately for all heating systems (hs)
  out[["stockDevHsRel"]] <- .computeRelDev(out[["stockDevHs"]], p_stockCalibTarget, tCalib, notInTargetGrp = "hsr")

  out[["conDevHsRel"]] <- .computeRelDev(out[["conDevHs"]], p_constructionCalibTarget, tCalib, notInTargetGrp = "hsr")

  out[["renDevHsRel"]] <- .computeRelDev(out[["renDevHs"]], p_renovationCalibTarget, tCalib, notInTargetGrp = "hsr")

  out[["flowDevHsRel"]] <- .computeRelDev(out[["flowDevHs"]], list(p_constructionCalibTarget, p_renovationCalibTarget),
                                          tCalib, notInTargetGrp = "hsr")


  # EXPAND DIMENSIONS AND COMBINE IN ONE DATA FRAME ----------------------------

  # Determine all dimensions present in output data
  allSets <- unique(unlist(lapply(out, colnames)))

  out <- do.call(rbind, lapply(names(out), function(varName) {
    .expandDims(out[[varName]], varName, allSets)
  }))


  # WRITE OUTPUT FILE ----------------------------------------------------------

  write.csv(out, file.path(path, "BRICK_calibration_report.csv"), row.names = FALSE)

}

#' Read a symbol from several gdx files and combine in one data frame
#'
#' @param gdx Path to gdx files; all gdx file names are assumed to be numbered and
#'  adhere to the structure: If \code{gdx = <path/to/file/name.gdx}, the function looks for
#'  \code{<path/to/file/name_0.gdx>}, \code{<path/to/file/name_1.gdx>}, ...
#' @param symbol Symbol to be read from the gdxes
#' @param maxIter Last iteration to be read
#' @param asMagpie logical, convert to magpie object?
#' @param ttotFilter numeric/factor, time periods to filter for
#' @param replaceVar logical, replace column names \code{bs} and \code{hs} by \code{bsr} and \code{hsr}?
#' @returns data frame of all results read in
#'
#' @importFrom dplyr %>% .data filter mutate

.readGdxIter <- function(gdx, symbol, maxIter, asMagpie = TRUE, ttotFilter = NULL, replaceVar = FALSE) {

  # Loop over all iterations and read in gdx files
  res <- data.frame()
  for (i in seq(0, maxIter)) {
    fileName <- file.path(dirname(gdx), paste0(gsub("_0\\.gdx$", "", basename(gdx)), "_", i, ".gdx"))
    if (file.exists(fileName)) {
      res <- rbind(res, readGdxSymbol(fileName, symbol, asMagpie = asMagpie) %>%
                     mutate(iteration = i))
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

#' Compute the sum of the squares in a data frame
#'
#' @param df data frame, containing the data to be evaluated
#' @param rprt character, column names for which the sum of the squares should be reported separately
#' @returns data frame averages as value column
#'
#' @importFrom dplyr %>% across any_of .data group_by summarise
#'
.computeAvg <- function(df, rprt = "") {

  df %>%
    group_by(across(any_of(rprt))) %>%
    summarise(value = mean(.data[["value"]], na.rm = TRUE), .groups = "drop")

}

#' Compute the sum of the squares in a data frame
#'
#' @param df data frame, containing the data to be evaluated
#' @param rprt character, column names for which the sum of the squares should be reported separately
#' @returns data frame sum of squares as value column
#'
#' @importFrom dplyr %>% across all_of any_of .data group_by rename_with summarise ungroup

.computeSumSq <- function(df, rprt = "") {

  df %>%
    group_by(across(any_of(rprt))) %>%
    summarise(value = sum(.data[["value"]] ^ 2, na.rm = TRUE), .groups = "drop")

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

.computeRelDev <- function(dfDev, dfTarget, tCalib, notInTargetGrp = NULL) {

  # Determine the reported columns in the calibration data
  rprt <- setdiff(colnames(dfDev), c("iteration", "value", notInTargetGrp))

  # Convert target data to list if necessary
  if (is.data.frame(dfTarget)) dfTarget <- list(dfTarget)

  # Compute the sum of the squares for target data
  dfTargetSumList <- lapply(dfTarget, function(df) {
    df %>%
      filter(.data[["ttot"]] %in% tCalib) %>%
      .computeSumSq(rprt = rprt)
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
    mutate(value = sqrt(.data[["value"]]) / sqrt(.data[["target"]])) %>%
    select(-"target")
}

#' Compute the sum of construction and renovation flow values
#'
#' @param con data frame, contains construction flow quantities
#' @param ren data frame, contains renovation flow quantities
#' @returns data frame with the sum in the value column
#'
#' @importFrom dplyr %>% across all_of .data full_join mutate rename select

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
    mutate(value = con + ren) %>%
    select(-"con", -"ren")
}

#' Extend dimensions of a data frame by adding NA entries, add variable name
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
    mutate(variable = varName, .before = 1) %>%
    relocate("value", .after = last_col())
}
