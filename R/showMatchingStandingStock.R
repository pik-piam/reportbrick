#' Plot share of initial heating systems removed
#'
#' Show the share of heating systems standing in the initial time step that have
#' been removed over time. The tunnel marks the allowed space between the lower
#' and upper limit defined in \code{p_shareRenHSinit} and the solid line shows
#' the value eventually chosen in the matching by \code{v_shareRenHSinit}. The
#' dashed line shows the extrapolation of this result to future time steps that
#' go beyond the temporal scope of the matching. The dotted line marks the
#' central value that results from an evenly distributed installation up until
#' the initial time steps. The central value is not considered in the
#' optimisation but used as supporting point in the extrapolation.
#'
#' @param path character, path to the run
#' @returns named list of ggplot2 objects
#'
#' @author Robin Hasse
#'
#' @importFrom mip plotstyle
#' @importFrom tidyr pivot_wider
#' @importFrom utils read.csv
#' @importFrom dplyr .data %>% filter mutate semi_join cur_column across ungroup
#'   all_of group_by select
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line facet_grid theme_classic
#'   theme scale_x_continuous scale_y_continuous element_blank element_line unit
#'   scale_linewidth_manual scale_linetype_manual as_labeller
#' @export

showMatchingStandingStock <- function(path) {

  # FUNCTIONS ------------------------------------------------------------------

  .combineData <- function(p_shareRenHSinit, v_shareRenHSinit, f_shareRenHSinit) {
    dataExtrapolated <- f_shareRenHSinit %>%
      filter(.data$ttotOut >= max(p_shareRenHSinit$ttotOut),
             .data$level == "matched") %>%
      mutate(level = "extrapolated")
    v_shareRenHSinit %>%
      mutate(level = "matched") %>%
      rbind(p_shareRenHSinit) %>%
      rbind(dataExtrapolated) %>%
      # recover factor levels
      mutate(across(all_of(c("hs", "region", "typ", "vin")),
                    ~ factor(.x, levels = levels(p_shareRenHSinit[[cur_column()]])))) %>%
      mutate(level = factor(.data$level,
                            c("lower", "upper", "matched", "extrapolated", "central")))
  }


  .removeIrrelevantData <- function(data, stock, tinit) {
    relevantStock <- stock %>%
      filter(.data$qty == "area",
             .data$ttot == tinit,
             .data$value > 0)
    relevantData <- data %>%
      semi_join(relevantStock, by = c("hs", "vin", "region", "typ"))
    # drop later extrapolated periods when all values are almost one
    # to see more of the S-curve
    relevantData %>%
      group_by(.data$ttotOut) %>%
      filter(min(.data$value) < 0.95 | .data$ttotOut <= max(stock$ttot)) %>%
      ungroup()
  }


  .getNames <- function(set, elements) {
    brickSets <- suppressMessages(readBrickSets())
    unlist(brickSets[[set]][["elements"]][elements])
  }


  .getHsColours <- function(hs) {
    hsNames <- .getNames("hsr", hs)
    colours <- plotstyle(unlist(hsNames))
    names(colours) <- hs
    colours
  }


  .completeInitial <- function(data, tinit) {
    data %>%
      select(-"value", -"ttotOut") %>%
      unique() %>%
      mutate(ttotOut = tinit,
             value = 0) %>%
      rbind(data)
  }


  .getTunnelData <- function(pData, tinit) {
    pData %>%
      filter(.data$level %in% c("lower", "upper")) %>%
      .completeInitial(tinit) %>%
      pivot_wider(names_from = "level")
  }


  .getLineData <- function(pData) {
    pData %>%
      filter(.data$level %in% c("central", "matched", "extrapolated"))
  }


  .plot <- function(pData, facetLabels, hsColours, tinit) {
    tunnel <- .getTunnelData(pData, tinit)
    line <- .getLineData(pData)

    ggplot(mapping = aes(x = .data$ttotOut)) +
      geom_ribbon(aes(ymin = .data$lower,
                      ymax = .data$upper,
                      fill = .data$hs),
                  data = tunnel,
                  alpha = 0.3) +
      geom_line(aes(y = .data$value,
                    colour = .data$hs,
                    linetype = .data$level,
                    linewidth = .data$level),
                data = line) +
      facet_grid(vin ~ hs, labeller = as_labeller(facetLabels)) +
      scale_x_continuous(NULL) +
      scale_y_continuous("Share of initial heating systems removed",
                         limits = c(0, 1),
                         expand = c(0, 0),
                         breaks = seq(0, 1, 0.5),
                         minor_breaks = seq(0, 1, 0.25),
                         labels = c("0", "0.5", "1")) +
      scale_color_manual(values = hsColours) +
      scale_fill_manual(values = hsColours) +
      scale_linetype_manual(values = c(central = "dotted",
                                       matched = "solid",
                                       extrapolated = "dashed")) +
      scale_linewidth_manual(values = c(central = 0.7,
                                        matched = 0.9,
                                        extrapolated = 0.9)) +
      theme_classic() +
      theme(strip.background = element_blank(),
            panel.grid.major.y = element_line(color = "lightgrey"),
            panel.grid.minor.y = element_line(color = "lightgrey"),
            panel.spacing.y = unit(1, "lines"),
            legend.title = element_blank())
  }



  # READ DATA ------------------------------------------------------------------

  gdx <- file.path(path, "output.gdx")

  # stop here if run is too old to have flexible lifetime
  m <- Container$new(gdx)
  if (!"v_shareRenHSinit" %in% m$listVariables()) {
    return(NULL)
  }

  v_stock <- readGdxSymbol(gdx, "v_stock", asMagpie = FALSE)
  p_shareRenHSinit <- readGdxSymbol(gdx, "p_shareRenHSinit", asMagpie = FALSE)
  v_shareRenHSinit <- readGdxSymbol(gdx, "v_shareRenHSinit", asMagpie = FALSE)
  f_shareRenHSinit <- read.csv(file.path(path, "f_shareRenHSinit.csv"))

  tinit <- readGdxSymbol(gdx, "tinit")[[1]]



  # COMBINE --------------------------------------------------------------------

  data <- .combineData(p_shareRenHSinit, v_shareRenHSinit, f_shareRenHSinit) %>%
    .removeIrrelevantData(v_stock, tinit)

  regions <- levels(data$region)
  typs <- levels(data$typ)
  hs <- levels(data$hs)
  vins <- levels(data$vin)



  # PLOT -----------------------------------------------------------------------

  hsNames <- .getNames("hsr", hs)
  hsColours <- .getHsColours(hs)
  vinNames <- .getNames("vin", vins)

  lapply(setNames(nm = regions), function(r) {
    lapply(setNames(nm = typs), function(t) {
      pData <- data %>%
        filter(.data$region == r,
               .data$typ == t)

      .plot(pData, c(hsNames, vinNames), hsColours, tinit)
    })
  })
}
