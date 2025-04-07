#' Read in GDX from BRICK and write *.mif reporting
#'
#' Read in all information from GDX file that was generated with BRICK and
#' create the *.mif reporting
#'
#' @param gdx file path to a BRICK gdx
#' @param tmpl character, BRICK reporting template. There has to be a brickSets
#'   mapping named with the same suffix: \code{brickSets_<tmpl>.yaml}
#' @param file name of the mif file which will be written, if no name is
#'   provided a magpie object containing all the reporting information is
#'   returned
#' @param scenario scenario name that is used in the *.mif reporting
#' @param t numeric vector of reporting periods (years)
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind add_dimension write.report getSets<-
#' @importFrom utils capture.output
#' @importFrom gamstransfer Container
#' @export

convGDX2MIF <- function(gdx,
                        tmpl = NULL,
                        file = NULL,
                        scenario = "default",
                        t = NULL,
                        silent = TRUE) {

  # PREPARE --------------------------------------------------------------------

  # common time steps
  if (is.null(t)) {
    t <- as.numeric(as.character(readGdxSymbol(gdx, "ttot", asMagpie = FALSE)[[1]]))
  }

  brickSets <- readBrickSets(tmpl)
  inconsistencies <- .findInconsistenSetElements(brickSets, gdx)
  if (!is.null(inconsistencies)) {
    stop("The reporting template is not consistent with the gdx:\n  ",
         paste(capture.output(inconsistencies), collapse = "\n  "))
  }

  # Filter Brick sets for vintages existing in any time period
  vintages <- unique(readGdxSymbol(gdx, "vinExists", asMagpie = FALSE)$vin)
  brickSets$vin$elements <- brickSets$vin$elements[vintages]
  brickSets$vin$subsets <- lapply(brickSets$vin$subsets, function(subset) {
    subset[subset %in% vintages]
  })

  # central object containing all output data
  output <- NULL



  # REPORT VARIABLES -----------------------------------------------------------

  ## Stock ====
  message("running reportBuildingStock ...")
  output <- mbind(output, extendPeriods(reportBuildingStock(gdx, brickSets, silent = silent), t))

  ## Construction ====
  message("running reportConstruction ...")
  output <- mbind(output, extendPeriods(reportConstruction(gdx, brickSets, silent = silent), t))

  ## Renovation ====
  message("running reportRenovation ...")
  output <- mbind(output, extendPeriods(reportRenovation(gdx, brickSets, silent = silent), t))

  ## Demolition ====
  message("running reportDemolition ...")
  output <- mbind(output, extendPeriods(reportDemolition(gdx, brickSets, silent = silent), t))

  ## Energy ====
  message("running reportEnergy ...")
  output <- mbind(output, extendPeriods(reportEnergy(gdx, brickSets, silent = silent), t))


  # FINISH ---------------------------------------------------------------------

  if (length(output) == 0) {
    stop("Unable to report any variable.")
  }

  # Add dimension names "scenario.model.variable"
  getSets(output)[3] <- "variable"
  output <- add_dimension(output, dim = 3.1, add = "model", nm = "BRICK")
  output <- add_dimension(output, dim = 3.1, add = "scenario", nm = scenario)

  # either write the *.mif or return the magpie object
  if (!is.null(file)) {
    write.report(output, file = file, ndigit = 7)
    message("MIF file written: ", file)
  } else {
    return(output)
  }

}



#' Find inconsistencies in set elements between reporting template and gdx
#'
#' @param brickSets character, BRICK reporting template
#' @param gdx file path to a BRICK gdx
#' @returns named list of missing and surplus sets elements

.findInconsistenSetElements <- function(brickSets, gdx) {
  m <- Container$new(gdx)
  sets <- unique(.split(names(brickSets)))
  setsGdx <- setNames(m$getSymbols(sets), sets)
  do.call(rbind, lapply(names(brickSets), function(s) {
    elementsGdx <- .combinations(lapply(.split(s), function(ps) {
      as.character(setsGdx[[ps]]$records[[1]])
    }))
    elementsMap <- names(brickSets[[s]][["elements"]])
    inconsistencies <- list(missing = setdiff(elementsGdx, elementsMap),
                            surplus = setdiff(elementsMap, elementsGdx))
    do.call(rbind, lapply(names(inconsistencies), function(i) {
      if (length(inconsistencies[[i]]) > 0) {
        data.frame(set = s, element = inconsistencies[[i]], inconsistency = i)
      } else {
        NULL
      }
    }))
  }))
}
