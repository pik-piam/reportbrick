#' Stacked bar plot summarising the stock evolution
#'
#' @param data data.frame from BRICK mif
#' @param brickSets character, BRICK reporting template
#' @param vars variables shown as rows
#' @param cols subsets of the building stock shown as columns
#' @param fill characteristi mapped to the fill colour
#' @param identTransp logical, if TRUE, identical replacement in renovation is
#'   rendered semi transparent
#' @param revStackOrder boolean, reverse stacking order?
#' @returns ggplot2 object with bar plot
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr %>% .data filter arrange mutate left_join select
#' @importFrom tidyr unite separate
#' @importFrom mip plotstyle
#' @importFrom stringr str_extract str_escape
#' @importFrom ggplot2 ggplot aes geom_col facet_grid vars theme theme_classic
#'   geom_hline element_blank scale_x_continuous scale_y_continuous element_line
#'   scale_fill_manual
#' @export

showStockAndFlows <- function(
    data,
    brickSets = NULL,
    vars = c("Stock", "Construction", "Demolition"),
    cols = NULL,
    fill = "hs",
    identTransp = FALSE,
    revStackOrder = FALSE) {

  # FUNCTIONS ------------------------------------------------------------------

  # get elements mapped to the fill colour from reporting template
  getFillElements <- function(bricksets, fill, revStackOrder) {
    tmpl <- readBrickSets(brickSets)
    if (!fill %in% names(tmpl)) {
      stop("Can't find the fill dimension '", fill,
           "' in the reporting template ", attr(tmpl, "file"))
    }
    elements <- unname(unlist(tmpl[[fill]][["elements"]]))
    if (length(elements) == 0) {
      stop("Can't find elements of the fill dimension '", fill,
           "' in the reporting template ", attr(tmpl, "file"))
    }
    if (isTRUE(revStackOrder)) {
      elements <- rev(elements)
    }
    return(elements)
  }



  # all mif variable names for given variable
  getAllVarNames <- function(var, cols, fillElements) {
    vars <- if (is.null(cols)) {
      expand.grid(var = var, fill = fillElements) %>%
        unite("vars", "var", "fill", sep = "|")
    } else {
      expand.grid(var = var, cols = cols, fill = fillElements) %>%
        unite("vars", "var", "cols", "fill", sep = "|")
    }
    getElement(vars, "vars")
  }



  # bind plot data of each variable to one data.frame
  preparePlotData <- function(data, vars, cols, fillElements) {
    pData <- do.call(rbind,
                     lapply(vars, preparePlotVar, data, cols, fillElements))
    pData <- pData %>%
      addPositionAndWidth() %>%
      mutate(variable = factor(.data[["variable"]], vars),
             fill = factor(.data[["fill"]], fillElements))
  }



  # prepare plot data for given variable
  preparePlotVar <- function(var, data, cols, fillElements) {
    vars <- getAllVarNames(var, cols, fillElements)

    missingVars <- setdiff(vars, data[["variable"]])
    if (length(missingVars) > 0) {
      stop("Some variables are missing to create this plot:\n    ",
           paste(missingVars, collapse = "\n    "))
    }

    data %>%
      filter(.data[["variable"]] %in% vars) %>%
      separateCols("variable", variable = var, col = cols, fill = fillElements)

  }



  separateCols <- function(df, .col, ...) {
    elements <- list(...)
    x <- df[[.col]]
    df[[.col]] <- NULL
    for (c in names(elements)) {
      df[[c]] <- str_extract(x, paste(str_escape(elements[[c]]), collapse = "|"))
    }
    return(df)
  }


  addPositionAndWidth <- function(pData) {
    dt <- pData %>%
      select("period") %>%
      unique() %>%
      arrange(.data[["period"]]) %>%
      mutate(dt = c(diff(.data[["period"]])[1], diff(.data[["period"]])))

    pData %>%
      left_join(dt, by = "period") %>%
      mutate(x = .data[["period"]] - ifelse(.data[["variable"]] == "Stock",
                                            0,
                                            .data[["dt"]] / 2),
             width = ifelse(.data[["variable"]] == "Stock",
                            0.3 * min(.data[["dt"]]),
                            .data[["dt"]]) - 0.05 * min(.data[["dt"]])) %>%
      select(-"dt")
  }


  showPlot <- function(pData) {
    ggplot(pData) +
      suppressWarnings(
        geom_col(aes(x = .data[["x"]],
                     y = .data[["value"]],
                     fill = .data[["fill"]],
                     width = .data[["width"]]))
      ) +
      geom_hline(yintercept = 0) +
      facet_grid(rows = vars(.data[["variable"]], .data[["unit"]]),
                 cols = vars(.data[["col"]]),
                 scales = "free_y",
                 switch = "y") +
      scale_x_continuous(NULL, expand = c(0, 0)) +
      scale_y_continuous(NULL, expand = c(0, 0, 0.05, 0)) +
      scale_fill_manual(values = plotstyle(levels(pData[["fill"]])),
                        name = NULL) +
      theme_classic() +
      theme(strip.placement = "outside",
            strip.background = element_blank(),
            panel.grid.major.y = element_line(colour = "lightgrey"))
  }



  # PLOT -----------------------------------------------------------------------


  fillElements <- getFillElements(brickSets, fill, revStackOrder)
  pData <- preparePlotData(data, vars, cols, fillElements)
  showPlot(pData)
}
