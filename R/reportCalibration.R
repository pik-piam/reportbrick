#' Read in model results from calibration for each iteration, calculate deviation
#'
#'
#' @param gdx path to a gdx; it is assumed that for each iteration a gdx is present
#'  with this path and the iteration number inserted at the end.
#' @param flowTargets logical, if set to FALSE, the function does not expect the presence of targets
#'  for the flows
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
  calibOptim <- identical(cfg[["switches"]][["RUNTYPE"]], "optimization")
  aggVin <- identical(cfg[["switches"]][["AGGREGATEDIM"]], "vin")
  removeDims <- if (isTRUE(aggVin)) "vin" else NULL

  # Determine relevant variables
  if (isTRUE(cfg[["switches"]][["SEQUENTIALREN"]])) {
    varAll <- c("stock", "construction", "renovationBS", "renovationHS")
  } else {
    varAll <- c("stock", "construction", "renovation")
  }
  varFlow <- setdiff(varAll, "stock")

  # Exclude renovationHS for computations on building shell
  varFlowBs <- setdiff(varFlow, "renovationHS")

  # Exclude renovationBS for computations on heating system
  varAllHs <- setdiff(varAll, "renovationBS")
  varFlowHs <- setdiff(varFlow, "renovationBS")

  # Exclude construction for computations on vintage
  varAllVin <- setdiff(varAll, "construction")

  # Naming suffixes for each variable
  namingMap <- c(
    stock = "stock",
    construction = "con",
    renovation = "ren",
    renovationBS = "renBS",
    renovationHS = "renHS"
  )


  ## Diagnostic parameters ====

  diagDevFiles <- list(
    construction = "deviationConIter.csv",
    renovation = "deviationRenIter.csv",
    renovationBS = "deviationRenBSIter.csv",
    renovationHS = "deviationRenHSIter.csv"
  )

  diagFiles <- c(
    list(
      stepSize = "stepSizeParamsIter.csv",
      outerObjective = "outerObjectiveAllIter.csv"
    ),
    diagDevFiles[varFlow]
  )

  diagValNames <- list(
    stepSize = "stepSize",
    outerObjective = "f"
  )
  diagValNames[varFlow] <- "d"

  diagnosticsExist <- all(file.exists(file.path(path, diagFiles)))
  if (diagnosticsExist) {
    diagnostics <- lapply(stats::setNames(nm = names(diagFiles)), function(nm) {
      read.csv(file.path(path, diagFiles[[nm]])) %>%
        select(-any_of(c("delta", "phiDeriv", "minOuterObj", "minStepSize", "fA", "iterA"))) %>%
        rename(value = diagValNames[[nm]])
    })

    diagnostics[["construction"]] <- .replaceVarName(diagnostics[["construction"]])
    diagnostics[["outerObjective"]] <- unique(diagnostics[["outerObjective"]])
  }


  ## Stock and flow results ====

  # Potentially shift the time filter to a later stage if I want to save and plot pure stock/flow data
  brickRes <- lapply(stats::setNames(nm = varAll), function(var) {
    .readGdxIter(gdx,
                 paste(if (calibOptim) "p" else "v", var, sep = "_"),
                 maxIter, asMagpie = FALSE, ttotFilter = tCalib,
                 replaceVar = !grepl("renovation", var))
  })

  dims <- .getDims(brickRes, removeDims = removeDims)

  # Apply potential removal of vintage dimension
  brickRes <- lapply(stats::setNames(nm = varAll), function(var) {
    .computeSum(brickRes[[var]], dims[[var]])
  })

  # Aggregate brick results to assess deviations of aggregates
  brickResTot <- lapply(stats::setNames(nm = varAll), function(var) {
    thisBrickRes <- brickRes[[var]]
    if (var %in% c("renovation", "renovationHS")) {
      thisBrickRes <- filter(thisBrickRes, .data$hsr != 0)
    }
    .computeSum(thisBrickRes, rprt = c("iteration", "region", "typ", "loc", "inc", "ttot"))
  })

  brickResTotHs <- lapply(brickRes[varAllHs], function(res) {
    .computeSum(res, rprt = c("iteration", "region", "typ", "loc", "inc", "hsr", "ttot"))
  })


  ## Specific costs ====

  costSym <- list(
    construction = "p_specCostCon",
    renovation = "p_specCostRen",
    renovationBS = "p_specCostRenBS",
    renovationHS = "p_specCostRenHS"
  )
  p_intangCost <- lapply(stats::setNames(nm = varFlow), function(var) {
    .readGdxIter(gdx, costSym[[var]],
                 maxIter, asMagpie = FALSE, ttotFilter = tCalib,
                 replaceVar = (identical(var, "construction"))) %>%
      filter(.data$cost == "intangible")
  })


  ## Calibration targets ====

  p_calibTarget <- lapply(stats::setNames(nm = varAll), function(var) {
    thisCalibTarget <- readGdxSymbol(gdxInp, paste0("p_", var, "CalibTarget"), asMagpie = FALSE)
    if (!grepl("renovation", var)) {
      thisCalibTarget <- .replaceVarName(thisCalibTarget)
    }
    thisCalibTarget %>%
      .computeSum(rprt = dims[[var]])
  })

  # Aggregate to assess deviation of aggregate quantities
  p_calibTargetTot <- lapply(stats::setNames(nm = varAll), function(var) {
    thisCalibTarget <- p_calibTarget[[var]]
    if (var %in% c("renovation", "renovationHS")) {
      thisCalibTarget <- filter(thisCalibTarget, .data$hsr != "0")
    }
    .computeSum(thisCalibTarget, rprt = c("iteration", "region", "typ", "loc", "inc", "ttot"))
  })

  p_calibTargetTotHs <- lapply(p_calibTarget[varAllHs], function(target) {
    .computeSum(target, rprt = c("iteration", "region", "typ", "loc", "inc", "hsr", "ttot"))
  })



  # COMPUTE DEVIATIONS ---------------------------------------------------------

  deviation <- lapply(stats::setNames(nm = varAll), function(var) {
    .computeDeviation(brickRes[[var]], p_calibTarget[[var]])
  })
  deviationTot <- lapply(stats::setNames(nm = varAll), function(var) {
    .computeDeviation(brickResTot[[var]], p_calibTargetTot[[var]])
  })
  deviationTotHs <- lapply(stats::setNames(nm = varAllHs), function(var) {
    .computeDeviation(brickResTotHs[[var]], p_calibTargetTotHs[[var]])
  })



  # STORE OUTPUT QUANTITIES TO LIST --------------------------------------------

  out <- list()


  ## Aggregate total quantities ====

  # Direction of steepest descent by heating system
  if (isTRUE(diagnosticsExist)) {

    diagnostics <- c(
      diagnostics[c("stepSize", "outerObjective")],
      lapply(stats::setNames(varFlowHs, paste0(namingMap[varFlowHs], "DescDirHs")), function(var) {
        .computeAvg(diagnostics[[var]],
                    rprt = c("iteration", "region", "typ", "loc", "inc", "hsr", "ttot"),
                    exclude = list(hs = "h2bo", hsr = "h2bo"))
      })
    )

    # Direction of steepest descent by heating system for late iterations
    diagnostics <- c(
      diagnostics,
      lapply(
        stats::setNames(paste0(namingMap[varFlowHs], "DescDirHs"), paste0(namingMap[varFlowHs], "DescDirHsLate")),
        function(var) {
          diagnostics[[var]] %>%
            filter(.data[["iteration"]] >= floor(0.4 * maxIter))
        }
      )
    )

    out <- c(out, diagnostics)
  }

  # Specific costs
  out <- c(
    out,
    lapply(stats::setNames(varFlowBs, paste0(namingMap[varFlowBs], "SpecCostBs")), function(var) {
      .computeAvg(
        p_intangCost[[var]],
        rprt = c("iteration", "region", "typ", "loc", "inc", "bsr", "ttot"),
        exclude = list(hs = "h2bo", hsr = "h2bo")
      )
    }),
    lapply(stats::setNames(varFlowHs, paste0(namingMap[varFlowHs], "SpecCostHs")), function(var) {
      .computeAvg(
        p_intangCost[[var]],
        rprt = c("iteration", "region", "typ", "loc", "inc", "hsr", "ttot"),
        exclude = list(hs = "h2bo", hsr = "h2bo")
      )
    })
  )

  # Aggregated brick results by heating system (hs)
  out <- c(
    out,
    stats::setNames(
      brickResTotHs,
      paste0(sub("HS", "", unlist(namingMap[varAllHs], use.names = FALSE)), "Hs")
    ),
    list(flowHs = .computeFlowSum(
      list(
        construction = brickResTotHs[["construction"]],
        renovation = brickResTotHs[[grep("renovation($|HS$)", varFlow, value = TRUE)]]
      )
    ))
  )

  # Brick results by vintage (vin)
  if (isFALSE(aggVin)) {
    out <- c(
      out,
      lapply(stats::setNames(varAllVin, paste0(namingMap[varAllVin], "Vin")), function(var) {
        .computeSum(brickRes[[var]], rprt = c("iteration", "region", "typ", "loc", "inc", "vin", "ttot"))
      })
    )
  }


  ## Aggregate deviations ====

  # Across all dimensions
  devAgg <- lapply(deviation, function(dev) {
    .computeSumSq(dev, rprt = c("iteration", "region", "typ", "loc", "inc", "ttot"),
                  addSign = FALSE)
  })
  out <- c(
    out,
    stats::setNames(devAgg, paste0(namingMap[varAll], "DevAgg")),
    list(flowDevAgg = .computeFlowSum(devAgg[varFlow]))
  )

  # By heating system (hs)
  devHs <- lapply(deviation[varAllHs], function(dev) {
    .computeSumSq(
      dev,
      rprt = c("iteration", "region", "typ", "loc", "inc", "hsr", "ttot")
    )
  })
  out <- c(
    out,
    stats::setNames(devHs, paste0(namingMap[varAllHs], "DevHs")),
    list(flowDevHs = .computeFlowSum(devHs[varFlowHs]))
  )

  # By vintage (vin)
  if (isFALSE(aggVin)) {
    devVin <- lapply(deviation[varAllVin], function(dev) {
      .computeSumSq(
        dev,
        rprt = c("iteration", "region", "typ", "loc", "inc", "vin", "ttot")
      )
    })
    out <- c(
      out,
      stats::setNames(devVin, paste0(namingMap[varAllVin], "DevVin"))
    )
  }


  ## Relative aggregate deviations ====

  # Across all dimensions
  out <- c(
    out,
    lapply(stats::setNames(varAll, paste0(namingMap[varAll], "DevRel")), function(var) {
      .computeRelDev(devAgg[[var]], p_calibTarget[[var]], tCalib)
    }),
    list(flowDevRel = .computeRelDev(out[["flowDevAgg"]], p_calibTarget[varFlow],
                                     tCalib))
  )

  # Separately for all heating systems (hs)
  out <- c(
    out,
    lapply(stats::setNames(varAllHs, paste0(namingMap[varAllHs], "DevHsRel")), function(var) {
      .computeRelDev(devHs[[var]], p_calibTarget[[var]], tCalib, notInTargetGrp = "hsr")
    }),
    list(flowDevHsRel = .computeRelDev(
      out[["flowDevHs"]],
      p_calibTarget[varFlowHs],
      tCalib,
      notInTargetGrp = "hsr"
    ))
  )

  # Deviation share for all heating systems (hs)
  out <- c(
    out,
    lapply(stats::setNames(varAllHs, paste0(namingMap[varAllHs], "DevHsShare")), function(var) {
      .computeRatioSq(devHs[[var]], devAgg[[var]])
    }),
    list(flowDevHsShare = .computeRatioSq(out[["flowDevHs"]], out[["flowDevAgg"]]))
  )

  # Separately for all vintages (vin)
  if (isFALSE(aggVin)) {
    out <- c(
      out,
      lapply(stats::setNames(varAllVin, paste0(namingMap[varAllVin], "DevVinRel")), function(var) {
        .computeRelDev(devVin[[var]], p_calibTarget[[var]], tCalib, notInTargetGrp = "vin")
      })
    )
  }

  ## Deviations of aggregated brick results
  out <- c(
    out,
    stats::setNames(deviationTot, paste0(namingMap[varAll], "TotDev")),
    stats::setNames(deviationTotHs, paste0(namingMap[varAllHs], "TotHsDev"))
  )



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
#' @param removeDims character, additional dimensions to be removed from the output
#'
.getDims <- function(calibObj, removeDims = NULL) {
  lapply(calibObj, function(obj) {
    dims <- colnames(obj)
    setdiff(dims, c(removeDims, "qty", "value"))
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

#' Exclude data matching giving filter criteria from a data set
#'
#' @param data data frame with the data set
#' @param exclude named list with entries to be excluded from the data.
#'   The name gives the column name from which the entries given by the value should
#'   be removed.
#' @returns the given data set without the rows identified by the filter criteria
#'
#' @importFrom dplyr .data filter
#'
.excludeRows <- function(data, exclude) {
  for (nm in names(exclude)) {
    if (nm %in% colnames(data)) {
      data <- filter(data, .data[[nm]] != exclude[[nm]])
    }
  }
  data
}

#' Compute the mean value in a data frame
#'
#' @param df data frame, containing the data to be evaluated
#' @param rprt character, column names for which the mean should be reported separately
#' @param valueName character, name of the column containing the values to be manipulated
#' @param exclude named list with entries to be excluded from the data.
#'   The name gives the column name from which the entries given by the value should
#'   be removed.
#'
#' @returns data frame averages as value column
#'
#' @importFrom dplyr %>% across any_of .data group_by summarise
#'
.computeAvg <- function(df, rprt = "", valueName = "value", exclude = list()) {

  df %>%
    .excludeRows(exclude) %>%
    group_by(across(any_of(rprt))) %>%
    summarise(value = mean(.data[[valueName]], na.rm = TRUE), .groups = "drop")

}

#' Compute the sum
#'
#' @param df data frame containing the data to be evaluated
#' @param rprt character, column names for which the sum should be reported separately
#' @param exclude named list with entries to be excluded from the data.
#'   The name gives the column name from which the entries given by the value should
#'   be removed.
#'
#' @returns data frame with summed values
#' @importFrom dplyr %>% .data across any_of group_by summarise
#'
.computeSum <- function(df, rprt = "", exclude = list()) {

  df %>%
    .excludeRows(exclude) %>%
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
    dfTargetSum <- .computeFlowSum(dfTargetSumList) %>%
      rename(target = "value")
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
#' @param flows named list of data frames, each containing flow quantities.
#'   Entries need to include \code{construction}
#'   and one entry with \code{renovation} as part of the name.
#' @returns data frame with the sum in the value column
#'
#' @importFrom dplyr %>% across all_of .data full_join mutate rename select
#'
.computeFlowSum <- function(flows) {

  # If columns bsr and hsr are present in the data:
  # Convert the construction data to the factor levels of the renovation data
  renFlows <- flows[grep("^renovation", names(flows))]
  for (var in c("hsr", "bsr")) {
    containsVar <- purrr::map_lgl(renFlows, ~ var %in% colnames(.x))
    if (any(containsVar)) {
      if (var %in% colnames(flows[["construction"]])) {
        renWithVar <- renFlows[containsVar] # nolint: object_usage_linter
        flows[["construction"]] <- flows[["construction"]] %>%
          mutate(across(all_of(var), ~ factor(.x, levels(renWithVar[[which(containsVar)[1]]][[var]]))))
      } else {
        warning("Construction and renovation flow data do not match.")
        return(NULL)
      }
    }
  }

  # Compute the combined flow data as the sum of construction and renovation;
  # NA values in the construction flows are replaced by zeros, thus for "0" flows only renovation is reflected.
  purrr::reduce(
    lapply(names(flows), function(var) {
      flows[[var]] %>%
        rename_with(~ var, .cols = "value")
    }),
    ~ full_join(.x, .y, by = intersect(colnames(.x), colnames(.y)))
  ) %>%
    replace_na(list(construction = 0)) %>%
    tidyr::pivot_longer(cols = names(flows), names_to = "flow") %>%
    group_by(across(-all_of(c("flow", "value")))) %>%
    summarise(value = sum(.data$value), .groups = "drop")
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
