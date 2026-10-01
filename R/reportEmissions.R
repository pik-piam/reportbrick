#' Report emissions
#'
#' Report CO2 emissions from space heating
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind getNames<- getNames mselect collapseDim
#'   complete_magpie

reportEmissions <- function(gdx, brickSets = NULL, silent = TRUE) {

  # READ -----------------------------------------------------------------------

  # stock variable
  v_stock <- readGdxSymbol(gdx, "v_stock") %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")


  # floor-space specific energy demand
  specFeDemand <- readGdxSymbol(gdx, "p_feDemand")

  # emission intensity
  emissionIntensity <- readGdxSymbol(gdx, "p_carrierEmi")

  # carrier dimension needed to report carriers
  hsCarrier <- readGdxSymbol(gdx, "hsCarrier", stringAsFactor = FALSE)
  stock <- .addCarrierDimension(v_stock, hsCarrier)
  stock <- .addEnduseDimension(stock, specFeDemand)
  stock <- complete_magpie(stock, fill = 0)

  # harmonise
  specFeDemand <- mselect(specFeDemand, vin = getItems(stock, "vin"))

  for (carrier in setdiff(getItems(stock, "carrier"),
                          getItems(emissionIntensity, "carrier"))) {
    emissionIntensity <- magclass::add_columns(emissionIntensity,
                                               addnm = carrier,
                                               dim = "carrier",
                                               fill = 0)
  }



  # REPORT ---------------------------------------------------------------------

  # nolint start: commented_code_linter.
  energyDemand <- stock * specFeDemand # Mm2 * kWh/m2/yr = GWh/yr
  emissions <- energyDemand * emissionIntensity # GWh/yr * t/kWh = Mt/yr
  # nolint end

  reportDemandVars(emissions, "Emi|CO2", "Mt CO2/yr", brickSets, silent)
}
