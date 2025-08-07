#' Plot comparison between matching references and model values
#'
#' Create stacked bar plots to verify matching accuracy
#'
#' @param path character, path to the run
#' @param showTitles logical, if TRUE, the references name and basic variable
#'   name are shown as title in each plot. No titles otherwise.
#' @returns named list of ggplot2 objects
#'
#' @author Robin Hasse
#'
#' @importFrom ggplot2 ggplot geom_col aes facet_grid theme_bw ggtitle guides
#'   scale_linetype_manual geom_hline guide_legend scale_alpha_manual
#'   scale_fill_brewer
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr right_join matches if_else
#' @export

showMatchingComparison <- function(path, showTitles = TRUE) {

  .combineModelAndTargetData <- function(p_refVals, v_refVals, valTypes) {
    right_join(p_refVals, v_refVals,
               by = c("reference", "refVar", "region", ttot = "t"),
               suffix = valTypes)
  }


  .addConsiderationCol <- function(x, refVarConsidered) {
    refVarConsidered %>%
      mutate(considered = TRUE) %>%
      right_join(x, by = c("reference", "refVar")) %>%
      replace_na(list(considered = FALSE)) %>%
      mutate(considered = .data$considered & !is.na(.data$valuetarget))
  }


  .makeRelValsPercent <- function(x, refVarBasic, v_refValsBasic, valCols) {
    x %>%
      # assign basic value to model values of relative references
      left_join(refVarBasic, by = c("reference", "refVar")) %>%
      left_join(rename(v_refValsBasic, valuebasic = "value"),
                by = c("reference", "refVarGroup", "region", ttot = "t")) %>%
      # calculate shares for relative references to match target values
      mutate(valuemodel = .data$valuemodel / ifelse(is.na(.data$valuebasic), 1, .data$valuebasic),
             # shares in percent
             across(all_of(valCols), ~ . * if_else(is.na(.data$valuebasic), 1, 100)))
  }


  .dropRefVarGroupsWithoutTarget <- function(x) {
    x %>%
      group_by(across(all_of(c("region", "reference", "refVarGroup")))) %>%
      filter(any(!is.na(.data$valuetarget))) %>%
      ungroup()
  }

  .placeX <- function(data) {
    minDt <- min(diff(sort(unique(data$ttot))))
    data %>%
      mutate(x = .data$ttot + minDt * case_when(.data$valType == "model"  ~ - 0.2,
                                                .data$valType == "target" ~ + 0.2,
                                                .default                  = 0),
             width = minDt * ifelse(.data$valType == "target", 0.2, 0.3))
  }


  .setUnit <- function(yLabel, unit) {
    sub("\\[.*\\]", paste0("[", unit, "]"), yLabel)
  }


  .plot <- function(pData, yLabel = NULL) {
    ggplot(pData) +
      geom_hline(yintercept = 0) +
      suppressWarnings(geom_col(aes(x = .data$x,
                                    y = .data$value,
                                    fill = .data$refVar,
                                    linetype = .data$valType,
                                    width = .data$width, # throws warning
                                    alpha = .data$considered,
                                    color = .data$considered))) +
      facet_grid(region ~ ., scales = "free_y") +
      scale_x_continuous(NULL, expand = c(0, 0)) +
      scale_y_continuous(yLabel, expand = c(0, 0)) +
      scale_fill_brewer(palette = "Set1") +
      scale_linetype_manual(values = c(model = "solid", target = "dashed")) +
      scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.4), guide = "none") +
      scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "darkgrey"), guide = "none") +
      guides(linetype = guide_legend(override.aes = list(fill = "white", color = "black"))) +
      theme_classic() +
      theme(strip.background = element_blank(),
            panel.grid.major.y = element_line(color = "lightgrey"),
            panel.grid.minor.y = element_line(color = "lightgrey"),
            legend.title = element_blank())
  }



  # READ DATA ------------------------------------------------------------------

  gdx <- file.path(path, "output.gdx")

  v_refVals <- readGdxSymbol(gdx, "v_refVals", asMagpie = FALSE)
  p_refVals <- readGdxSymbol(gdx, "p_refVals", asMagpie = FALSE)
  v_refValsBasic <- readGdxSymbol(gdx, "v_refValsBasic", asMagpie = FALSE)
  p_refWeight <- readGdxSymbol(gdx, "p_refWeight", asMagpie = FALSE)

  refs <- readGdxSymbol(gdx, "ref", asMagpie = FALSE, stringAsFactor = FALSE, removeDescription = FALSE)
  refsRel <- readGdxSymbol(gdx, "refRel", asMagpie = FALSE, stringAsFactor = FALSE)[[1]]
  refVarBasic <- readGdxSymbol(gdx, "refVarBasic", asMagpie = FALSE)
  refVarConsidered <- readGdxSymbol(gdx, "refVarConsidered", asMagpie = FALSE)



  # COMBINE --------------------------------------------------------------------

  description <- dplyr::pull(refs, var = "element_text", name = "reference")
  refs <- refs$reference

  refsWithWeight <- p_refWeight %>%
    filter(.data$value > 0) %>%
    getElement("reference")

  refs <- intersect(refs, refsWithWeight)

  valTypes <- c("target", "model")
  valCols <- paste0("value", valTypes)

  data <- .combineModelAndTargetData(p_refVals, v_refVals, valTypes) %>%
    .addConsiderationCol(refVarConsidered) %>%
    .makeRelValsPercent(refVarBasic, v_refValsBasic, valCols) %>%
    .dropRefVarGroupsWithoutTarget() %>%
    pivot_longer(cols = valCols, names_to = "valType", names_prefix = "value") %>%
    .placeX()



  # PLOT -----------------------------------------------------------------------

  lapply(stats::setNames(nm = refs), function(ref) {

    pData <- data %>%
      filter(.data$reference == ref)
    yLabel <- description[[ref]]

    # skip unmapped reference variables
    if (!any(pData$value[pData$valType == "model"] != 0, na.rm = TRUE)) {
      return(NULL)
    }

    if (ref %in% refsRel) {
      refVarGroups <- unique(pData$refVarGroup)

      # skip if all basic values are zero
      if (!any(pData$valuebasic != 0, na.rm = TRUE)) {
        return(NULL)
      }

      yLabel <- .setUnit(yLabel, "%")

      # one plot for each reference variable group (rvg)
      p <- lapply(setNames(nm = refVarGroups), function(rvg) {
        pRvg <- pData %>%
          filter(.data$refVarGroup == rvg) %>%
          .plot(yLabel)
        if (showTitles) {
          pRvg <- pRvg + ggtitle(ref, rvg)
        }
        pRvg
      })

    } else {

      p <- .plot(pData, yLabel)

      if (showTitles) {
        p <- p + ggtitle(ref)
      }
    }
    p
  })

}
