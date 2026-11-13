#' Report energy demand
#'
#' Report final and useful energy demand for space heating
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind getNames<- getNames mselect collapseDim
#'   complete_magpie addDim

reportEnergy <- function(gdx, brickSets = NULL, silent = TRUE) {

  # READ -----------------------------------------------------------------------

  # floor-space specific energy demand
  specDemand <- list(UE = readGdxSymbol(gdx, "p_ueDemand"),
                     FE = readGdxSymbol(gdx, "p_feDemand"))

  # filter for vintages present in the Brick output
  specDemand <- lapply(specDemand, function(x) {
    mselect(x, vin = names(brickSets$vin$elements))
  })

  # stock variable
  v_stock <- readGdxSymbol(gdx, "v_stock") %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")

  # add demand dimension to stock
  hsCarrier <- readGdxSymbol(gdx, "hsCarrier", stringAsFactor = FALSE)
  stock <- .addCarrierDimension(v_stock, hsCarrier)
  stock <- .addEnduseDimension(stock, specDemand[[1]])
  stock <- complete_magpie(stock, fill = 0)


  # REPORT ---------------------------------------------------------------------

  out <- NULL

  for (energyLevel in names(specDemand)) {

    # ensure backwards compatibility
    specDemand[[energyLevel]] <- .addEnduseDimension(specDemand[[energyLevel]])

    energyDemand <- stock * specDemand[[energyLevel]]
    energyDemand <- energyDemand * 3.6E-6 # GWh to EJ

    out <- mbind(out, reportDemandVars(energyDemand, energyLevel, "EJ/yr", brickSets, silent))
  }


  return(out)
}
