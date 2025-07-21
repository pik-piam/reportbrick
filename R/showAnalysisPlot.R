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
#' @param linetype character, column to be used as linetype aesthetic
#' @param facets character, columns to be used as facets.
#'   Can either be one or two columns.
#' @param rprt character, columns for which a separate plot should be created for
#'   each entry
#' @param avg character, columns over which the average should be computed
#' @param remCols character, columns to be removed before creating the plot
#' @param xname character, column to be used as x-aesthetic
#' @param suppressLateTtot logical, whether to remove the last three time steps
#'   from the plots shown
#' @param xlabName character, x-axis label
#' @param ylabName character, y-axis label
#' @param tmpl character, name or path to reportbrick reporting template.
#'
#' @importFrom dplyr %>% .data across all_of any_of cur_column filter group_by mutate
#'   rename_with select summarise ungroup 
#' @importFrom ggplot2 aes coord_fixed facet_grid element_text facet_wrap geom_col geom_line ggplot
#'   ggtitle scale_alpha_manual scale_color_manual scale_fill_manual sym theme vars xlab ylab
#' @importFrom tidyr crossing pivot_longer
#'
showAnalysisPlot <- function(plotType, data, varName, yname, color = NULL,
                             linetype = NULL, facets = c("loc", "typ"),
                             rprt = NULL, avg = NULL, remCols = NULL,
                             xname = "ttotOut", valueName = "value", suppressLateTtot = TRUE,
                             xlabName = NULL, ylabName = NULL, tmpl = NULL,
                             addPoints = NULL, filterRows = list(hs = "h2bo", hsr = "h2bo")) {
  
  
  
  # Functions ------------------------------------------------------------------
  
  # Remove columns without data
  .removeEmptyCols <- function(plData) {
    plData %>%
      select(where(~ !all(is.na(.))))
  }
  
  # Bar plot
  .createLtBar <- function(plData, xname, yname, color) {
    plData %>%
      .removeEmptyCols() %>%
      ggplot(mapping = aes(x = !!sym(xname), y = !!sym(yname), fill = !!sym(color))) +
      geom_col()
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
      
      p <- ggplot(plData, aes(x = .data$xShifted, y = .data[[yName]],
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
      
      p <- ggplot(plData, aes(x = .data$xShifted, y = .data[[yName]],
                                       fill = .data[[color]], alpha = .data$variable)) +
        geom_col(width = 0.4) +
        scale_alpha_manual(values = alphaMap) +
        scale_x_continuous(
          breaks = seq_along(levels(plData[[xName]])),
          labels = levels(plData[[xName]]),
          name = xName
        )
    }
    
  }
  
  # Line plot
  .createLtLine <- function(plData, xname, yname, color, linetype) {
    plData %>%
      .removeEmptyCols() %>%
      ggplot() +
      geom_line(mapping = aes(
        x = .data[[xname]], y = .data[[yname]],
        color = .data[[color]],
        linetype = .data[[linetype]]),
        linewidth = 1
      )
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
        fraction = ifelse(total == 0, 0, .data[[valueName]] / .data$total),
        end = 2 * pi * cumsum(.data$fraction),
        start = lag(.data$end, default = 0),
        r0 = 0
      ) %>%
      ungroup()
    
    # Normalize pie size
    maxTotal <- max(plData$total, na.rm = TRUE)
    desiredMaxRadius <- 0.4
    plData <- plData %>%
      mutate(r = sqrt(.data$total / maxTotal) * desiredMaxRadius)
    
    # Compute label positions (aligned across each row)
    dfLabel <- plData %>%
      select("x", "y", "xNum", "yNum", "total", "r") %>%
      unique() %>%
      mutate(total = signif(.data$total, 3)) %>%
      group_by(y) %>%
      mutate(yLabel = max(.data$yNum + .data$r + 0.2, na.rm = TRUE)) %>%  # row-aligned above pies
      ungroup()
    
    # Final plot
    ggplot(plData) +
      ggforce::geom_arc_bar(
        aes(x0 = xNum, y0 = yNum, r0 = r0, r = r,
            start = start, end = end, fill = .data[[color]]),
        color = "black", size = 0.3
      ) +
      geom_text(
        data = dfLabel,
        aes(x = xNum, y = yLabel, label = total),
        size = 3.5, fontface = "bold"
      ) +
      coord_fixed() +
      scale_fill_brewer(palette = "Set2") +
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
        .groups = "drop")
    
    # Plot
    ggplot(plData, aes(x = 1, y = .data[[valueName]], fill = .data[[color]])) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(
        data = dfTotals,
        aes(x = 1, y = yLabel, label = .data$total),
        position = "stack",
        inherit.aes = FALSE,
        size = 3.5,
        vjust = -0.5
      ) +
      facet_grid(rows = vars(y), cols = vars(x)) +
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
    
    plData <- filter(plData, .data$costType == "intangible")
    
    # Compute dynamic threshold (median of value column)
    valueThreshold <- median(plData[[valueName]], na.rm = TRUE)
    
    # Assign adaptive text color
    plData <- plData %>%
      mutate(
        textColor = ifelse(.data[[valueName]] > valueThreshold, "white", "black"),
        value = round(.data[[valueName]], 1)
      )
    
    # Build the plot
    ggplot(plData, aes(x = .data[[xname]], y = .data[[yname]], fill = .data[[valueName]])) +
      geom_tile(color = "white") +
      geom_text(aes(label = .data[[valueName]], color = .data$textColor), size = 4) +
      colorspace::scale_fill_continuous_sequential(palette = "Purple-Yellow", rev = TRUE) +
      scale_color_identity() +
      coord_fixed() +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        legend.position = "right"
      )
    
  }
  
  # Additional scatter plot
  .addLtPoint <- function(pl, plData, mainVar, addVar, xname, yname, y2labName) {
    
    plDataMain <- filter(plData, .data$variable %in% mainVar)
    plDataAdd <- filter(plData, .data$variable == addVar) %>%
      .removeEmptyCols()
    
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
  
  
  
  # Prepare the data -----------------------------------------------------------
  
  plData <- data %>%
    filter(.data[["variable"]] %in% c(varName, addPoints$variable), !is.na(.data[[yname]])) %>%
    select(-any_of(remCols))
  
  for (nm in names(filterRows)) {
    plData <- filter(plData, !.data[[nm]] %in% filterRows[[nm]])
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
      plData <- mutate(plData, across(any_of(color), ~ factor(.x, levels = c("statusQuoPref", "intangible", "tangible", "lccOpe"))))
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
    
    if (nrow(plDataFiltered) == 0) next
    
    
    ## Create the plot according to the plot type ====
    
    # Filter for main plotting variables
    plDataMain <- filter(plDataFiltered, .data$variable %in% varName)
    
    pl <- switch(
      plotType,
      bar = .createLtBar(plDataMain, xname, yname, color),
      twoBar = .createLtTwoBar(plDataMain, varName, xname, yname, color),
      line = .createLtLine(plDataMain, xname, yname, color, linetype),
      pieChart = .createLtPieChart(plDataMain, varName, xname, yname, color,
                                    valueName = valueName),
      barMatrix = .createLtBarChart(plDataMain, varName, xname, yname, color, valueName = valueName),
      heatMap = .createLtHeatMap(plDataMain, varName, xname, yname, valueName = valueName)
    ) +
      ggtitle(thisHeading)
    
    
    ## Adjust the plot according to options ====
    
    if (!is.null(addPoints)) {
      pl <- .addLtPoint(pl, plDataFiltered, varName, addPoints$variable, xname, yname, addPoints$y2labName)
    }
    if (!grepl("ttot", xname)) pl <- pl + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    if (length(facets) == 1) pl <- pl + facet_wrap(facets = vars(.data[[facets[1]]]))
    if (length(facets) == 2) pl <- pl + facet_grid(rows = vars(.data[[facets[1]]]), cols = vars(.data[[facets[2]]]))
    if (!is.null(colorMap) && plotType == "line") {
      pl <- pl + ggplot2::scale_color_manual(values = mip::plotstyle(colorMap))
    } else if (!is.null(colorMap)) {
      pl <- pl + ggplot2::scale_fill_manual(values = mip::plotstyle(colorMap))
    }
    if (!is.null(xlabName)) pl <- pl + xlab(xlabName)
    if (!is.null(ylabName)) pl <- pl + ylab(ylabName)
    print(pl)
  }
}