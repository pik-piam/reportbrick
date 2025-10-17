#' Plotting function, mainly for LCC analysis plots
#'
#' Framework to create various plots required for the LCC analysis.
#' Available plot types are:
#'  - line plot
#'  - (stacked) bar plot
#'  - comparison bar plot with two stacked bars of different transparency
#'
#' @author Ricarda Rosemann
#'
#' @param plotType character, desired plot type.
#'   Needs to be one of \code{line}, \code{bar} or \code{twoBar}.
#' @param data data frame containing the data to be plotted
#' @param varName character, variables to be plotted
#' @param yname character, column to be used as y-aesthetic
#' @param color character, column to be used as color aesthetic
#' @param facets character, columns to be used as facets.
#'   Can either be one or two columns.
#' @param rprt character, columns for which a separate plot should be created for
#'   each entry
#' @param avg character, columns over which the average should be computed
#' @param remCols character, columns to be removed before creating the plot
#' @param xname character, column to be used as x-aesthetic
#' @param valueName character, column to be used as the value column
#' @param suppressLateTtot logical, whether to remove the last three time steps
#'   from the plots shown
#' @param xlabName character, x-axis label
#' @param ylabName character, y-axis label
#' @param tmpl character, name or path to reportbrick reporting template.
#' @param filterRows named list, key-value pairs to filter the data by
#' @param valueCap numeric, maximum value for value data to be included in the plot
#' @param ... additional parameters to be passed to the plotting functions
#'
#' @importFrom dplyr %>% .data across all_of any_of cur_column filter group_by lag mutate
#'   pull rename_with select slice_max slice_min summarise ungroup where
#' @importFrom ggplot2 aes coord_fixed facet_grid element_text expansion facet_wrap
#'   geom_bar geom_col geom_line geom_point geom_text geom_tile ggplot ggtitle
#'   scale_alpha_manual scale_color_identity scale_color_manual scale_fill_manual scale_shape_manual
#'   sec_axis sym theme theme_minimal vars xlab ylab
#' @importFrom tidyr crossing pivot_longer
#'
showAnalysisPlot <- function(plotType, data, varName, yname, color = NULL, #nolint: cyclocomp_linter.
                             facets = c("loc", "typ"), rprt = NULL, avg = NULL, remCols = NULL,
                             xname = "ttotOut", valueName = yname, suppressLateTtot = TRUE,
                             xlabName = NULL, ylabName = NULL, tmpl = NULL,
                             filterRows = list(hs = "h2bo", hsr = "h2bo"),
                             valueCap = Inf, ...) {



  # Functions ------------------------------------------------------------------

  # Remove columns without data
  .removeEmptyCols <- function(plData, keepCols = NULL) {
    plData %>%
      select(keepCols, where(~ !all(is.na(.))))
  }

  # Additional scatter plot
  .addLtPoint <- function(pl, plDataMain, plDataAdd, xname, yname, y2labName) {

    scaleFactor <- max(plDataMain[[yname]]) / max(plDataAdd[[yname]])

    pl + geom_point(
      data = plDataAdd,
      mapping = aes(x = .data[[xname]], y = .data[[yname]] * scaleFactor),
      color = "black",
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
      scale_y_continuous(
        sec.axis = sec_axis(~ . / scaleFactor, name = y2labName)
      ) +
      theme(
        axis.title.y.right = element_text(angle = 90)
      )
  }

  # Bar plot
  .createLtBar <- function(plData, xname, yname, color, addPoints = NULL) {
    if (!is.null(addPoints)) {
      plDataMain <- plData %>%
        filter(.data$variable != addPoints$variable)
      plDataAdd <- plData %>%
        filter(.data$variable == addPoints$variable) %>%
        .removeEmptyCols()
    } else {
      plDataMain <- plData
    }

    alphaMap <- c(`FALSE` = 1, `TRUE` = 0.3)

    maxYlim <- plDataMain %>%
      filter(!.data$exceedCap) %>%
      group_by(across(-all_of(c(color, "exceedCap", yname)))) %>%
      summarise(value = sum(.data[[yname]])) %>%
      pull("value") %>%
      max()

    plDataMain <- plDataMain %>%
      group_by(across(-all_of(c(color, "exceedCap", yname)))) %>%
      mutate(value = ifelse(
        .data$exceedCap,
        (maxYlim - sum(.data[[yname]][!.data$exceedCap])) / sum(.data$exceedCap),
        .data[[yname]]
      ))

    pl <- plDataMain %>%
      .removeEmptyCols() %>%
      ggplot(mapping = aes(x = !!sym(xname), y = .data$value, fill = !!sym(color), alpha = .data[["exceedCap"]])) +
      geom_col() +
      scale_alpha_manual(values = alphaMap, guide = "none")

    if (!is.null(addPoints) && nrow(plDataAdd) > 0) {
      pl <- .addLtPoint(pl, plDataMain, plDataAdd, xname, yname, addPoints$y2labName)
    }

    pl
  }

  .computeResolution <- function(v, default = 10) {
    if (length(v) == 1) {
      return(default)
    } else {
      vDiff <- diff(v)
      return(pmin(c(vDiff[1], vDiff), c(vDiff, vDiff[length(vDiff)])))
    }
  }

  # Two bar plot
  .createLtTwoBar <- function(plData, varName, xName, yName, color) {
    alphaMap <- c(1, 0.5)
    names(alphaMap) <- varName

    plData <- plData %>%
      mutate(
        variable = factor(.data$variable, levels = varName),
      )

    # Check if x-axis column is numeric or categorical (factor/character)
    if (is.numeric(plData[[xName]])) {
      # Numeric x: compute width from resolution
      plData <- plData %>%
        group_by(across(-all_of(c(xName, yName)))) %>%
        mutate(width = .computeResolution(.data[[xName]])) %>%
        ungroup() %>%
        mutate(
          xShifted = ifelse(.data$variable == varName[1],
                            .data[[xName]] - 0.225 * .data$width,
                            .data[[xName]] + 0.225 * .data$width),
          width = 0.4 * .data[["width"]]
        )

      pl <- ggplot(plData, aes(x = .data$xShifted, y = .data[[yName]],
                               fill = .data[[color]], width = .data$width, alpha = .data$variable)) +
        geom_col() +
        scale_alpha_manual(values = alphaMap)

    } else {
      # Categorical x: convert to numeric positions
      plData[[xName]] <- factor(plData[[xName]])
      plData <- plData %>%
        mutate(
          xPos = as.numeric(.data[[xName]]),
          xShifted = ifelse(.data$variable == varName[1],
                            .data$xPos - 0.225,
                            .data$xPos + 0.225)
        )

      pl <- ggplot(plData, aes(x = .data$xShifted, y = .data[[yName]],
                               fill = .data[[color]], alpha = .data$variable)) +
        geom_col(width = 0.4) +
        scale_alpha_manual(values = alphaMap) +
        scale_x_continuous(
          breaks = seq_along(levels(plData[[xName]])),
          labels = levels(plData[[xName]]),
          name = xName
        )
    }
    pl

  }

  # Line plot
  .createLtLine <- function(plData, xname, yname, color, linetype) {
    plData %>%
      .removeEmptyCols() %>%
      ggplot() +
      geom_line(
        mapping = aes(
          x = .data[[xname]], y = .data[[yname]],
          color = .data[[color]],
          linetype = .data[[linetype]]
        ),
        linewidth = 1
      )
  }

  .addLinearReg <- function(pl, plData, xname, yname) {
    modelData <- plData %>%
      mutate(x = .data[[xname]], y = .data[[yname]], .keep = "none")
    linearModel <- stats::lm(y ~ x, modelData)

    lineData <- data.frame(
      x = seq(min(plData[[xname]]), max(plData[[xname]])),
      slopeX = summary(linearModel)$coefficients["x", "Estimate"],
      rSquared = summary(linearModel)$r.squared
    )
    lineData <- lineData %>%
      mutate(y = stats::predict(linearModel, lineData))

    labelPoint <- lineData %>%
      slice_max(order_by = .data$x, n = 1)

    lab <- paste0(
      "atop(lambda == ", -signif(labelPoint$slopeX, 2),
      ", R^2 == ", signif(labelPoint$rSquared, 2), ")"
    )

    pl +
      geom_line(mapping = aes(x = .data$x, y = .data$y), data = lineData) +
      geom_text(mapping = aes(x = .data$x, y = .data$y),
                data = labelPoint,
                label = lab,
                parse = TRUE, size = 3, hjust = 1.1, vjust = 1.1)

  }

  # Scatter plot
  .createScatter <- function(plData, xname, yname, color, shape, slope = NULL, fitLinear = FALSE) {
    plData <- plData %>%
      .removeEmptyCols(keepCols = c(xname, yname)) %>%
      filter(is.finite(.data[[xname]]), is.finite(.data[[yname]])) %>%
      mutate(across(shape, as.factor))

    pl <- plData %>%
      ggplot() +
      geom_point(mapping = aes(
        x = .data[[xname]],
        y = .data[[yname]],
        color = .data[[color]],
        shape = .data[[shape]]
      )) +
      scale_shape_manual(values = 0:(length(levels(plData[[shape]])) - 1))

    if (!is.null(slope)) {
      lineData <- data.frame(
        x = seq(min(plData[[xname]]), max(plData[[xname]]))
      ) %>%
        mutate(y = slope * .data$x)

      labelPoint <- lineData %>%
        slice_min(order_by = .data$x, n = 1)

      lab <- paste0("lambda == ", -slope)

      pl <- pl +
        geom_line(data = lineData, mapping = aes(x = .data$x, y = .data$y), colour = "#3366FF") +
        geom_text(mapping = aes(x = .data$x, y = .data$y),
                  data = labelPoint,
                  label = lab,
                  parse = TRUE, size = 3, hjust = 0, vjust = 1.5, colour = "#3366FF")
    }

    if (isTRUE(fitLinear)) pl <- .addLinearReg(pl, plData, xname, yname)

    pl
  }

  # Matrix pie chart
  .createLtPieChart <- function(plData, varName, xname, yname, color, valueName = "value") {

    # Compute pie fractions
    plData <- plData %>%
      rename(x = xname, y = yname) %>%
      mutate(
        x = as.factor(.data$x),
        y = as.factor(.data$y),
        xNum = as.numeric(.data$x),
        yNum = as.numeric(.data$y)
      ) %>%
      group_by(across(all_of(c("x", "y")))) %>%
      mutate(
        total = sum(.data[[valueName]]),
        exceedCapTot = any(.data$exceedCap),
        fraction = ifelse(.data$total == 0, 0, .data[[valueName]] / .data$total),
        end = 2 * pi * cumsum(.data$fraction),
        start = lag(.data$end, default = 0),
        r0 = 0
      ) %>%
      ungroup()

    # Normalize pie size
    maxTotal <- plData %>%
      filter(!.data$exceedCapTot) %>%
      pull("total") %>%
      max(na.rm = TRUE)
    desiredMaxRadius <- 0.4
    plData <- plData %>%
      mutate(r = sqrt(pmin(.data$total, maxTotal) / maxTotal) * desiredMaxRadius)

    # Compute label positions (aligned across each row)
    dfLabel <- plData %>%
      select("x", "y", "xNum", "yNum", "total", "r") %>%
      unique() %>%
      mutate(total = signif(.data$total, 3)) %>%
      group_by(across(all_of("y"))) %>%
      mutate(yLabel = max(.data$yNum + .data$r + 0.2, na.rm = TRUE)) %>%  # row-aligned above pies
      ungroup()

    # Final plot
    ggplot(plData) +
      ggforce::geom_arc_bar(
        aes(x0 = .data$xNum, y0 = .data$yNum, r0 = .data$r0, r = .data$r,
            start = .data$start, end = .data$end, fill = .data[[color]], alpha = .data$exceedCap),
        color = "black", size = 0.3
      ) +
      geom_text(
        data = dfLabel,
        aes(x = .data$xNum, y = .data$yLabel, label = .data$total),
        size = 3.5, fontface = "bold"
      ) +
      coord_fixed() +
      scale_fill_brewer(palette = "Set2") +
      scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.3), guide = "none") +
      scale_x_continuous(
        breaks = seq_along(levels(plData$x)),
        labels = levels(plData$x),
        expand = expansion(mult = 0.1)
      ) +
      scale_y_continuous(
        breaks = seq_along(levels(plData$y)),
        labels = levels(plData$y),
        expand = expansion(mult = 0.2)
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        legend.position = "right",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
  }

  .createLtBarChart <- function(plData, varName, xname, yname, color, valueName = "value") {

    # Prepare data with factor conversion
    plData <- plData %>%
      rename(x = xname, y = yname) %>%
      mutate(
        x = as.factor(.data$x),
        y = as.factor(.data$y)
      )

    # Compute total values per facet (x, y)
    dfTotals <- plData %>%
      group_by(across(all_of(c("x", "y")))) %>%
      summarise(
        total = signif(sum(.data[[valueName]]), 3),
        yLabel = sum(pmax((.data[[valueName]]), 0)),
        .groups = "drop"
      )

    # Plot
    ggplot(plData, aes(x = 1, y = .data[[valueName]], fill = .data[[color]])) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(
        data = dfTotals,
        aes(x = 1, y = .data$yLabel, label = .data$total),
        position = "stack",
        inherit.aes = FALSE,
        size = 3.5,
        vjust = -0.5
      ) +
      facet_grid(rows = vars(.data$y), cols = vars(.data$x)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +  # Top padding
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 12),
        legend.position = "right"
      )
  }

  .createLtHeatMap <- function(plData, varName, xname, yname, valueName = "value") {

    plData <- plData %>%
      filter(.data$costType == "intangible") %>%
      mutate(fillValue = ifelse(
        .data$exceedCap,
        max(.data[[valueName]][!.data$exceedCap]),
        .data[[valueName]]
      ))

    # Compute dynamic threshold (median of value column)
    valueThreshold <- stats::median(plData$fillValue, na.rm = TRUE)

    # Assign adaptive text color
    plData <- plData %>%
      mutate(
        textColor = ifelse(.data$fillValue > valueThreshold, "white", "black"),
        value = round(.data[[valueName]], 1)
      )

    # Build the plot
    ggplot(plData, aes(x = .data[[xname]], y = .data[[yname]], fill = .data$fillValue, alpha = .data$exceedCap)) +
      geom_tile(color = "white") +
      geom_text(aes(label = .data$value, color = .data$textColor), size = 4) +
      colorspace::scale_fill_continuous_sequential(palette = "Purple-Yellow", rev = TRUE) +
      scale_color_identity() +
      scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.3), guide = "none") +
      coord_fixed() +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        legend.position = "right"
      )

  }



  # Prepare the data -----------------------------------------------------------

  plData <- data %>%
    filter(.data[["variable"]] %in% varName, !is.na(.data[[valueName]])) %>%
    select(-any_of(remCols))

  for (nm in names(filterRows)) {
    plData <- filter(plData, !.data[[nm]] %in% filterRows[[nm]])
  }

  if (all(!c(xname, yname) %in% colnames(plData)) && all(c(xname, yname) %in% plData$variable)) {
    plData <- plData %>%
      pivot_wider(names_from = "variable") %>%
      mutate(variable = yname) %>%
      rename(value = yname)
    yname <- "value"
  }

  # If any faceting columns are NA for a given variable:
  # Fill with all entries occurring for any other variable
  if (!is.null(facets)) {
    colsWithNA <- plData %>%
      group_by(across("variable")) %>%
      summarise(across(facets, ~ all(is.na(.x)))) %>%
      pivot_longer(facets, names_to = "cols") %>%
      filter(.data$value)

    for (i in seq_len(nrow(colsWithNA))) {
      plData <- plData %>%
        filter(.data$variable == colsWithNA[[i, "variable"]]) %>%
        select(-colsWithNA[[i, "cols"]]) %>%
        crossing(newCol = setdiff(unique(plData[[colsWithNA[[i, "cols"]]]]), NA)) %>%
        rename_with(~ colsWithNA[[i, "cols"]], "newCol") %>%
        rbind(plData) %>%
        filter(.data$variable != colsWithNA[[i, "variable"]] | !is.na(.data[[colsWithNA[[i, "cols"]]]]))
    }
  }

  # Remove late time step from data due to end of horizon effects
  if (isTRUE(suppressLateTtot) && "ttotIn" %in% colnames(plData) && length(unique(plData[["ttotIn"]])) > 4) {
    plData <- filter(plData, .data[["ttotIn"]] < sort(unique(.data[["ttotIn"]]), decreasing = TRUE)[[3]])
  }

  # Average data along given averaging columns
  if (!is.null(avg)) {
    plData <- plData %>%
      group_by(across(-any_of(c(avg, yname, "absVal", "relVal")))) %>%
      summarise(value = mean(.data[[yname]], na.rm = TRUE), .groups = "drop")
    yname <- "value"
  }

  # If the data is now empty, return NULL invisibly
  if (nrow(plData) == 0) {
    return(invisible(NULL))
  }



  # Color and facet map --------------------------------------------------------

  # Prepare and apply color map
  if (!is.null(color)) {
    colorMap <- unlist(readBrickSets(tmpl)[[color]][["elements"]])
    if (!is.null(colorMap)) {
      plData <- mutate(plData, across(any_of(color), ~ factor(colorMap[.x], levels = colorMap)))
    } else if (color == "costType") {
      plData <- mutate(plData, across(
        any_of(color),
        ~ factor(.x, levels = c("statusQuoPref", "intangible", "tangible", "lccOpe"))
      ))
    }
  } else {
    colorMap <- NULL
  }

  # Prepare and apply facet map
  facetMap <- stats::setNames(lapply(facets, function(fac) {
    map <- unlist(readBrickSets(tmpl)[[fac]][["elements"]])
    if (any(map == "")) map <- stats::setNames(names(map), names(map))
    map
  }), facets)
  if (!is.null(facetMap)) {
    plData <- mutate(plData, across(
      any_of(facets),
      ~ factor(facetMap[[dplyr::cur_column()]][.x], levels = facetMap[[dplyr::cur_column()]])
    ))
  }



  # Prepare loop through reported plots ----------------------------------------

  # Prepare filtering along certain columns to write several plots
  if (length(rprt) >= 1) {
    filterVals <- stats::setNames(lapply(rprt, function(col) {
      setdiff(unique(plData[[col]]), NA)
    }), rprt)
    # Remove empty entries and stop if no filter available afterwards
    filterVals[lapply(filterVals, length) == 0] <- NULL
    if (length(filterVals) == 0) {
      stop("Reporting columns are given, but all yield an empty filter. ",
           "Please check that reporting columns are not NA.")
    }
  } else {
    filterVals <- "None"
  }

  count <- stats::setNames(rep(1, length.out = length(rprt)), rprt)
  complete <- FALSE

  heading <- paste(paste(varName, collapse = " and "), "by", color)



  # Loop through all desired plots ---------------------------------------------

  while (isFALSE(complete)) {


    ## Filter the data ====

    plDataFiltered <- plData

    # Filter to obtain data for the desired single plot
    for (col in rprt) {
      plDataFiltered <- plDataFiltered %>%
        mutate(across(col, ~ ifelse(
          is.na(.data[[col]]) & !is.na(.data[[yname]]),
          filterVals[[col]][count[col]],
          .x
        )))
      plDataFiltered <- plDataFiltered %>%
        filter(.data[[col]] == filterVals[[col]][count[col]])
    }
    plDataFiltered <- mutate(plDataFiltered, exceedCap = .data[[valueName]] > valueCap & .data$variable == varName)


    ## Handle the heading ====

    thisHeading <- paste(heading, paste(lapply(rprt, function(col) {
      paste(col, "=", filterVals[[col]][count[col]])
    }), collapse = " | "), sep = " | ")


    ## Update the looping variable ====

    countAtMax <- count == unlist(lapply(filterVals, length))
    if (!all(countAtMax)) {
      count[!countAtMax][1] <- count[!countAtMax][1] + 1
      if (any(which(countAtMax)) < which(!countAtMax)[1]) count[countAtMax[1:which(!countAtMax)[1]]] <- 1
    } else {
      complete <- TRUE
    }

    if (nrow(plDataFiltered) == 0
        || (plotType == "scatter" && length(plDataFiltered[[yname]][!is.na(plDataFiltered[[yname]])]) == 0)) next


    ## Create the plot according to the plot type ====

    # Filter for main plotting variables
    plDataMain <- filter(plDataFiltered, .data$variable %in% varName)

    pl <- switch(
      plotType,
      bar = .createLtBar(plDataMain, xname, yname, color, ...),
      twoBar = .createLtTwoBar(plDataMain, varName, xname, yname, color),
      line = .createLtLine(plDataMain, xname, yname, color, ...),
      scatter = .createScatter(plDataMain, xname, yname, color, ...),
      pieChart = .createLtPieChart(plDataMain, varName, xname, yname, color,
                                   valueName = valueName),
      barMatrix = .createLtBarChart(plDataMain, varName, xname, yname, color, valueName = valueName),
      heatMap = .createLtHeatMap(plDataMain, varName, xname, yname, valueName = valueName)
    ) +
      ggtitle(thisHeading)


    ## Adjust the plot according to options ====

    if (is.null(pl)) next
    if (!grepl("ttot", xname)) pl <- pl + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    if (length(facets) == 1) pl <- pl + facet_wrap(facets = vars(.data[[facets[1]]]))
    if (length(facets) == 2) pl <- pl + facet_grid(rows = vars(.data[[facets[1]]]), cols = vars(.data[[facets[2]]]))
    if (!is.null(colorMap) && plotType %in% c("line", "scatter")) {
      pl <- pl + ggplot2::scale_color_manual(values = mip::plotstyle(colorMap))
    } else if (!is.null(colorMap)) {
      pl <- pl + ggplot2::scale_fill_manual(values = mip::plotstyle(colorMap))
    }
    if (!is.null(xlabName)) pl <- pl + xlab(xlabName)
    if (!is.null(ylabName)) pl <- pl + ylab(ylabName)
    print(pl)
  }
}
