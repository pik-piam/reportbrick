#' Plot Sankey diagram
#'
#' Visualise transitions from one state to another. This plot helps to follow
#' the exact renovation flows, though it quickly gets very complex.
#'
#' @param path character, path to the run
#' @param fill character, dimension mapped to fill, either \code{"bs"} (building
#'   shell) or \code{"hs"} (heating system).
#' @param filterData named list to filter the data before plotting
#' @param maxPeriodsInRow maximum number of time steps to plot in one row. If
#'   there is more data, it is broken across multiple lines. If \code{NULL}, all
#'   data is shown in one line.
#' @param save control how the plot should be saved:
#' \itemize{
#'   \item `TRUE`: plot is saved in plots folder of run path
#'   \item `FALSE`: plot is not saved (but always returned invisibly)
#'   \item character: plot is saved in given directory
#' }
#'
#' @author Robin Hasse
#'
#' @importFrom mip plotstyle
#' @importFrom yaml read_yaml
#' @importFrom utils stack head
#' @importFrom gamstransfer Container
#' @importFrom tidyr replace_na separate_wider_delim
#' @importFrom ggpubr ggarrange annotate_figure text_grob
#' @importFrom scales breaks_pretty
#' @importFrom dplyr %>% .data lead filter mutate left_join select group_by pull
#'   across all_of summarise arrange ungroup reframe everything rename case_when
#' @importFrom ggplot2 ggplot aes geom_col position_stack geom_segment theme
#'   theme_classic scale_x_continuous scale_y_continuous scale_fill_manual
#'   scale_color_manual element_line element_blank ggsave
#' @export

showSankey <- function(path, # nolint: cyclocomp_linter.
                       fill = c("bs", "hs"),
                       filterData = NULL,
                       maxPeriodsInRow = NULL,
                       save = TRUE) {


  fill <- match.arg(fill)





  # AESTHETICS -----------------------------------------------------------------

  # relative x-position between previous stock (0) and next stock (1)
  xPos <- list(
    con = 0.125,
    ren_start = 0.3,
    ren_end = 0.7,
    dem = 0.875
  )

  # gap between construction from max. stock and demolition from zero w.r.t.
  # max stock in x direction
  relGap <- 0.05

  # stock bar width w.r.t. minimum time step length
  relBarWidth <- 0.1

  # bar types that have a black outline
  barsWithOutline <- c("Stock", "Construction", "Demolition_end")

  # colors that are not related to the fill dimension
  colors <- list(
    construction = "#7CAEAF",
    demolition   = "#D26868",
    untouched    = "#f1daab"
  )

  # transparency of untouched and identical replacement flows
  alphaSemiTransparent <- 0.5





  # FUNCTIONS ------------------------------------------------------------------


  # get data.frame with labels for the elements of the fill dimension
  .getFillLabels <- function() {
    tmplFile <- file.path(path, "config", "brickSets_COMPILED.yaml")
    if (!file.exists(tmplFile)) {
      stop("Cannot find this reporting template: ", tmplFile)
    }
    tmpl <- read_yaml(tmplFile)
    fillLabels <- tmpl[[fill]][["elements"]] %>%
      stack() %>%
      mutate(across(everything(), as.character))
    colnames(fillLabels) <- c("label", fill)
    return(fillLabels)
  }



  # shift x position by fraction of the time step length
  .shiftXbyDt <- function(df, ...) {
    shift <- list(...)
    for (col in names(shift)) {
      df[[col]] <- df[["ttot"]]
      if (shift[[col]] != 0) {
        dtShift <- if (shift[[col]] < 0) "dt" else "dtNext"
        df[[col]] <- df[[col]] + shift[[col]] * df[[dtShift]]
      }
    }
    return(df)
  }



  # set values of node columns
  .setNode <- function(df, ...) {
    nodes  <- list(...)
    for (col in names(nodes)) {
      if (nodes[[col]] %in% c("bs", "bsr", "hs", "hsr")) {
        df[[col]] <- df[[nodes[[col]]]]
      } else {
        df[[col]] <- nodes[[col]]
      }
    }
    return(df)
  }



  # create data frame with nodes and other characteristics for given flow
  .defineFlow <- function(flow,
                          node = fill,
                          next_node = fill,
                          shift_x = 0,
                          shift_next_x = 0,
                          drop = "none") {

    var <- sub("_.*$", "", flow)

    dataFlow <- data[[var]] %>%
      filter(drop == "none" |
               (drop == "start" & .data$ttot > min(.data$ttot)) |
               (drop == "end"   & .data$ttot < max(.data$ttot))) %>%
      mutate(flow = flow) %>%
      left_join(dt, by = "ttot") %>%
      .shiftXbyDt(x = shift_x, next_x = shift_next_x) %>%
      .setNode(node = node, next_node = next_node)

    if (var != "Stock") {
      dataFlow[["value"]] <- dataFlow[["value"]] * dataFlow[["dt"]]
    }

    dataFlow  %>%
      select("flow", "ttot", "x", "next_x", "node", "next_node", "value")
  }



  # add suffix column that marks untouched and identical replacement flows
  .addSuffixCol <- function(df) {
    df$suffix <- case_when(
      df$flow %in% c("Stock_in", "Stock_out") | df$next_node == "0" ~ "untouch",
      df$flow == "Renovation" & df$node == df$next_node             ~ "ident",
      df$flow == "Demolition"                                       ~ "demol"
    )
    return(df)
  }



  # set target node of zero flows identical to origin
  .attachZeroFlows <- function(df) {
    df %>%
      mutate(next_node = ifelse(.data$next_node == "0",
                                .data$node,
                                .data$next_node))
  }



  # paste suffix col to given cols and drop suffix col
  .pasteSuffix <- function(df, to) {
    for (col in to) {
      df[[col]] <- case_when(is.na(df$suffix) ~ df[[col]],
                             is.na(df[[col]]) ~ NA,
                             .default = paste(df[[col]], df$suffix, sep = "_"))
    }
    df$suffix <- NULL
    return(df)
  }



  # add invisible outflows where there are none (e.g. demolition)
  .addVirtualOutFlows <- function(df) {
    df %>%
      group_by(across(all_of(c(x = "next_x", node = "next_node", "ttot")))) %>%
      summarise(flow = if (all(.data$flow == "Demolition")) "Demolition_end" else "Flow_end",
                next_x = NA,
                next_node = NA,
                value = sum(.data$value),
                .groups = "drop") %>%
      rbind(df)
  }



  # add col for shift along x-axis for construction and demolition
  .addShiftCol <- function(df) {
    totStock <- df %>%
      filter(.data$flow == "Stock_in") %>%
      group_by(.data$x) %>%
      summarise(value = sum(.data$value), .groups = "drop")
    maxStock <- totStock %>%
      getElement("value") %>%
      max()

    gap <- relGap * maxStock

    shiftDemolition <- df %>%
      filter(.data$flow == "Demolition_end") %>%
      group_by(across(all_of(c("x", "flow")))) %>%
      summarise(shift = -sum(.data$value) - gap,
                .groups = "drop")

    df <- df %>%
      left_join(shiftDemolition, by = c("x", "flow")) %>%
      mutate(shift = replace_na(.data$shift, 0))
    df[df$flow == "Construction", "shift"] <- maxStock + gap

    return(df)
  }



  # shift nodes that have both in and outflows invisibly on x-axis
  .shiftXbyEps <- function(df, eps = 1E-3) {
    df$x <- df$x + ifelse(grepl("_end$", df$flow), 0, eps)
    return(df)
  }



  # turn vector of node levels into data frame with characterising columns
  .levelsAsDf <- function(nodeLevels) {
    data.frame(level = nodeLevels) %>%
      separate_wider_delim("level", "_",
                           names = c("fillDim", "flowType"),
                           too_few = "align_start",
                           cols_remove = FALSE) %>%
      mutate(flowType = replace_na(.data$flowType, "effective"))
  }



  # order node levels in the stacking order
  .orderNodeLevels <- function(nodeLevels, mapping) {
    nodeLevels %>%
      .levelsAsDf() %>%
      mutate(flowType = factor(.data$flowType,
                               c("demol", "untouch", "effective", "ident")),
             fillDim = factor(.data$fillDim, rev(mapping[[fill]]))) %>%
      group_by(.data$fillDim) %>%
      arrange(.data$flowType) %>%
      ungroup() %>%
      arrange(.data$fillDim) %>%
      getElement("level")
  }



  # turn node columns into factors
  .flowNodesAsFactor <- function(df, fillLabels) {
    nodeLevels <- union(df$node, df$next_node) %>%
      .orderNodeLevels(fillLabels)

    df$node <- factor(df$node, nodeLevels)
    df$next_node <- factor(df$next_node, nodeLevels)
    return(df)
  }



  # give "ineff" for all elements with suffix
  .getIneffSuffix <- function(x) {
    ifelse(grepl("_(untouch|ident)", x), "ineff", NA)
  }



  # paste suffix to both nodes of ineffective flows
  .markIneffFlows <- function(df) {
    nodeLevels <- levels(df[["node"]])
    nodeLevels <- paste(nodeLevels, .getIneffSuffix(nodeLevels), sep = "_")
    nodeLevels <- sub("_NA$", "", nodeLevels)

    df %>%
      mutate(node = as.character(.data$node),
             next_node = as.character(.data$next_node),
             suffix = .getIneffSuffix(.data$node)) %>%
      .pasteSuffix(to = c("node", "next_node")) %>%
      mutate(node = factor(.data$node, nodeLevels),
             next_node = factor(.data$next_node, nodeLevels))
  }



  # give data frame of bars for the start and end of renovation
  .defineRenBars <- function(df) {
    renovation <- df %>%
      filter(.data$flow == "Renovation")
    rbind(select(renovation, "x", "node", "value"),
          select(renovation, x = "next_x", node = "next_node", "value")) %>%
      mutate(node = sub("_.*$", "", .data$node)) %>%
      group_by(across(all_of(c("x", "node")))) %>%
      summarise(value = sum(.data$value), .groups = "drop") %>%
      mutate(bar = "Renovation")
  }



  # define height and shift of the bars for construction and demolition
  .defineFlowBars <- function(df) {
    df %>%
      filter(.data$flow %in% c("Construction", "Demolition_end")) %>%
      group_by(across(all_of(c("x", bar = "flow", node = "flow")))) %>%
      reframe(node = c(unique(.data$flow), "Shift"),
              value = c(sum(.data$value), mean(.data$shift))) %>%
      # demolition bar is below zero
      mutate(value = .data$value * ifelse(.data$node == "Demolition_end", -1, 1),
             demolition = ifelse(.data$node == "Demolition_end", .data$value, 0)) %>%
      group_by(.data$x) %>%
      # correct demolition shift for the bar height
      mutate(value = .data$value - case_when(
        .data$node == "Shift" & .data$bar == "Demolition_end" ~ sum(.data$demolition),
        .default = 0
      )) %>%
      ungroup() %>%
      select(-"demolition")
  }



  # give stock data with required columns
  .getStock <- function(data) {
    data[["Stock"]] %>%
      select(x = "ttot", node = !!fill, "value") %>%
      mutate(bar = "Stock")
  }



  # turn node column into factor in stacking order
  .barNodesAsFactor <- function(df, fillLabels) {
    fillLevels <- rev(fillLabels[[fill]])
    nodeLevels <- unique(df[["node"]])
    nodeLevels <- c(nodeLevels[nodeLevels == "Shift"], # Shift first to move following bars
                    nodeLevels[!nodeLevels %in% c("Shift", fillLevels)],
                    fillLevels)
    df$node <- factor(df$node, nodeLevels)
    return(df)
  }



  # set bar widths and outline
  .setBarGeometry <- function(df) {
    stockBarWidth <- relBarWidth * min(dt[["dt"]])
    df %>%
      group_by(.data$x) %>%
      mutate(width = stockBarWidth * case_when("Construction" %in% .data$node   ~ 0.5,
                                               "Demolition_end" %in% .data$node ~ 0.5,
                                               .data$bar == "Renovation"        ~ 0.2,
                                               .default                         = 1.0),
             outline = .data$bar %in% barsWithOutline & .data$node != "Shift")
  }



  # get maximum total stock across all time steps
  .getMaxStock <- function(df) {
    df %>%
      filter(.data$bar == "Stock") %>%
      group_by(.data$x) %>%
      summarise(value = sum(.data$value), .groups = "drop") %>%
      getElement("value") %>%
      max()
  }



  # get x-axis limits of plot in each row
  .getLimits <- function(bars, flows) {
    periods <- sort(unique(flows[["ttot"]]))
    width <- max(bars[["width"]])

    if (length(periods) <= maxPeriodsInRow) {
      return(data.frame(lower = NA, upper = NA))
    }

    yearsInRow <- periods[maxPeriodsInRow] - periods[1]

    rowStartYear <- c()

    while (length(periods) > 0) {
      barsInRow <- max(which(periods <= periods[1] + yearsInRow))
      if (barsInRow < 2) {
        stop("Please increase 'maxPeriodsInRow'. Can't plot this.")
      }
      rowStartYear <- c(rowStartYear, periods[1])
      if (barsInRow == length(periods)) {
        break
      }
      periods <- tail(periods, -(barsInRow - 1))
    }

    data.frame(lower = rowStartYear - 0.6 * width,
               upper = rowStartYear + yearsInRow + 0.5 * width)
  }



  # drop all rows outside of x limits
  .limitData <- function(df, limits) {
    df[replace_na(df$x > limits[["lower"]] & df$x < limits[["upper"]], TRUE), ]
  }



  # append alpha value to color hex code
  .makeTransparent <- function(color, alpha = alphaSemiTransparent) { # nolint: object_usage_linter.
    alphaHex <- sprintf("%02X", round(alpha * 255))
    paste0(color, alphaHex)
  }



  # get named list of fill colors
  .getFillColors <- function(nodeLevels, mapping) {
    nodeLevels %>%
      .levelsAsDf() %>%
      left_join(mapping[, c(fill, "label")], by = c(fillDim = fill)) %>%
      mutate(color = plotstyle(.data$label),
             color = case_when(.data$flowType == "ident"   ~ .makeTransparent(.data$color),
                               .data$flowType == "untouch" ~ .makeTransparent(colors$untouched),
                               .default                    = .data$color)) %>%
      pull("color", "level") %>%
      c(Construction   = colors$construction,
        Demolition_end = colors$demolition)
  }



  # calculate pretty y breaks from max stock
  .getYBreaks <- function(maxStock) {
    y <- pretty(c(0, maxStock), 5)

    dy <- mean(diff(y))

    yMinor <- sort(c(y, y + dy / 2))

    y <- y[y <= maxStock]
    yMinor <- yMinor[yMinor <= maxStock]

    list(major = y, minor = yMinor)
  }



  # get alluvial geom for flow
  .geomFlow <- function(data) {
    ggsankey::geom_alluvial(aes(next_x = .data$next_x,
                                node = .data$node,
                                next_node = .data$next_node,
                                value = .data$value,
                                shift = .data$shift),
                            data) %>%
      suppressWarnings()
  }



  # get bar geom
  .geomBar <- function(data, bar, just = 0.5) {
    geom_col(aes(y = .data$value,
                 colour = .data$outline,
                 width = .data$width),
             filter(data, .data$bar %in% !!bar),
             just = just,
             linewidth = 0.25,
             position = position_stack(reverse = TRUE)) %>%
      suppressWarnings()
  }



  # print y axis until max stock
  .extendYAxis <- function(bars, maxStock, minX = NA) {
    if (is.na(minX)) {
      minX <- min(bars[["x"]]) - max(bars[["width"]])
    }
    geom_segment(aes(x = minX, xend = minX, y = 0, yend = maxStock),
                 inherit.aes = FALSE)
  }



  # give ggplot2 sankey plot
  .plot <- function(bars,
                    flows,
                    flowsMarked,
                    fillLabels,
                    maxStock,
                    yName,
                    xLimits = c(lower = NA, upper = NA)) {

    bars <- .limitData(bars, xLimits)
    flows <- .limitData(flows, xLimits)
    flowsMarked <- .limitData(flowsMarked, xLimits)

    fillColors <- .getFillColors(levels(flows[["node"]]), fillLabels)
    fillLabels <- pull(fillLabels, "label", fill)
    legendTitle <- switch(fill, bs = "Building shell", hs = "Heating system")

    yBreaks <- .getYBreaks(maxStock)
    nXBreaks <- if (is.null(maxPeriodsInRow)) 5 else maxPeriodsInRow

    ggplot(mapping = aes(x = .data$x, fill = .data$node)) +
      .geomFlow(flows) +
      .geomFlow(flowsMarked) + # highlight effective flows
      .geomBar(bars, c("Stock", "Renovation")) +
      .geomBar(bars, "Construction", just = 1) +
      .geomBar(bars, "Demolition_end", just = 0) +
      .extendYAxis(bars, maxStock, xLimits[["lower"]]) +
      scale_y_continuous(yName,
                         breaks = yBreaks$major,
                         minor_breaks = yBreaks$minor,
                         expand = c(0.02, 0)) +
      scale_x_continuous(NULL,
                         breaks = breaks_pretty(n = nXBreaks),
                         limits = xLimits,
                         expand = c(0, 0.01)) +
      scale_fill_manual(values = fillColors,
                        labels = fillLabels,
                        breaks = names(fillLabels),
                        name = legendTitle,
                        na.value = NA) +
      scale_color_manual(values = c(`TRUE` = "black", `FALSE` = NA),
                         na.value = NA, guide = "none") +
      theme_classic() +
      theme(panel.grid.major.y = element_line(color = "lightgrey"),
            panel.grid.minor.y = element_line(color = "lightgrey"),
            axis.line.y = element_blank())
  }



  # save plot as PDF if required
  .save <- function(p) {
    savePath <- if (isTRUE(save)) {
      file.path(path, "plots")
    } else if (is.character(save)) {
      if (!dir.exists(save)) {
        stop("Can't save the plot. This directory doesn't exist: ", save)
      }
      save
    } else {
      return()
    }
    ggsave(file.path(savePath, paste0("sankey_", fill, ".pdf")), p,
           height = 21, width = 29.7, units = "cm")
  }





  # CHECK INPUT ----------------------------------------------------------------

  # find gdx file in given path
  gdxNames <- c("output.gdx",
                "abort.gdx")
  gdxFiles <- file.path(path, gdxNames)
  gdx <- head(gdxFiles[which(file.exists(gdxFiles))], 1)
  if (length(gdx) == 0) {
    warning("No suitable gdx file found to plot in ", path)
    return(NULL)
  }





  # READ DATA ------------------------------------------------------------------

  m <- gamstransfer::Container$new(gdx)

  dt <- readGdxSymbol(gdx, "p_dt", asMagpie = FALSE) %>%
    select("ttot", dt = "value") %>%
    mutate(dtNext = lead(.data$dt))

  vars <- c(
    Stock        = "v_stock",
    Construction = "v_construction",
    Demolition   = "v_demolition",
    Renovation   = if (m$hasSymbols("v_renovationBS")) {
      switch(fill, bs = "v_renovationBS", hs = "v_renovationHS")
    } else {
      "v_renovation"
    }
  )

  data <- lapply(vars, function(v) {
    var <- readGdxSymbol(gdx, v, asMagpie = FALSE, stringAsFactor = FALSE) %>%
      filter(.data$qty == "area") %>%
      select(-"qty")
    if (!is.null(filterData)) {
      for (dim in names(filterData)) {
        var <- filter(var, .data[[dim]] %in% filterData[[dim]])
      }
    }

    return(var)
  })

  fillLabels <- .getFillLabels()





  # AGGREGATE DATA -------------------------------------------------------------

  # sum over vintages and stock subsets
  aggDim <- setdiff(
    c("bs", "hs", "bsr", "hsr", "vin", "reg", "loc", "typ", "inc", "value"),
    c(fill, paste0(fill, "r"))
  )
  data <- lapply(data, function(var) {
    var %>%
      group_by(across(-any_of(aggDim))) %>%
      summarise(value = sum(.data$value), .groups = "drop") %>%
      mutate(value = .data$value / 1000) # million m2 -> billion m2
  })





  # PREPARE DATA ---------------------------------------------------------------

  # define flows with origin and target nodes
  flows <- rbind(
    .defineFlow("Stock_in", shift_x = xPos$ren_end - 1, drop = "start"),
    .defineFlow("Stock_out", shift_next_x = xPos$ren_start, drop = "end"),
    .defineFlow("Construction", shift_x = xPos$con - 1, shift_next_x = xPos$ren_start - 1, drop = "start"),
    .defineFlow("Demolition", shift_x = xPos$ren_end - 1, shift_next_x = xPos$dem - 1, drop = "start"),
    .defineFlow("Renovation", shift_x = xPos$ren_start - 1, shift_next_x = xPos$ren_end - 1,
                next_node = switch(fill, bs = "bsr", hs = "hsr"), drop = "start")
  )

  # prepare data for correct plotting
  flows <- flows %>%
    .addSuffixCol() %>%
    .attachZeroFlows() %>%
    .pasteSuffix(c("node", "next_node")) %>%
    .addVirtualOutFlows() %>% # all visible nodes need outflows
    .addShiftCol() %>%
    .shiftXbyEps() %>% # avoid unwanted stacking
    .flowNodesAsFactor(fillLabels)

  # same data with ineffective flows marked as such
  flowsMarked <- .markIneffFlows(flows)

  bars <- rbind(.defineRenBars(flows),
                .defineFlowBars(flows),
                .getStock(data)) %>%
    .barNodesAsFactor(fillLabels) %>%
    .setBarGeometry()

  maxStock <- .getMaxStock(bars)





  # PLOT -----------------------------------------------------------------------

  yName <- "Floor space in billion m2"

  if (is.null(maxPeriodsInRow)) {

    ## primary plot ====

    p <- .plot(bars, flows, flowsMarked, fillLabels, maxStock, yName)

  } else {

    ## multi-row plot ====

    limits <- .getLimits(bars, flows)

    plotlist <- apply(limits, 1, function(row) {
      .plot(bars, flows, flowsMarked, fillLabels, maxStock,
            yName = NULL,
            xLimits = row[c("lower", "upper")])
    })

    p <- ggarrange(plotlist = plotlist,
                   ncol = 1,
                   common.legend = TRUE,
                   legend = "right") %>%
      annotate_figure(left = text_grob(yName, rot = 90, size = 11))
  }





  # OUTPUT ---------------------------------------------------------------------

  .save(p)

  return(invisible(p))
}
