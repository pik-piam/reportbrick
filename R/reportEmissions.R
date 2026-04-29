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

  # carrier dimension needed to report carriers
  hsCarrier <- readGdxSymbol(gdx, "hsCarrier", stringAsFactor = FALSE)
  stock <- .addCarrierDimension(v_stock, hsCarrier)
  stock <- complete_magpie(stock, fill = 0)

  # floor-space specific energy demand
  specFeDemand <- readGdxSymbol(gdx, "p_feDemand")


  # emission intensity
  emissionIntensity <- readGdxSymbol(gdx, "p_carrierEmi")

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

  out <- mbind(

    ## total ====
    reportAgg(emissions,
              "Emi|CO2|Residential|Space heating (Mt CO2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", carrier = "all", vin = "all", typ = "res", loc = "all", inc = "all"),
              silent = silent),


    ## by building type ====
    reportAgg(emissions,
              "Emi|CO2|Residential|{typ}|Space heating (Mt CO2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", carrier = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res"),
              silent = silent),


    ## by location ====
    reportAgg(emissions,
              "Emi|CO2|Residential|{loc}|Space heating (Mt CO2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", carrier = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all"),
              silent = silent),


    ## by carrier ====
    reportAgg(emissions,
              "Emi|CO2|Residential|Space heating|{carrier} (Mt CO2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(carrier = "all"),
              silent = silent),


    ## by heating technology ====
    reportAgg(emissions,
              "Emi|CO2|Residential|Space heating|{hs} (Mt CO2/yr)", brickSets,
              agg = c(bs = "all", carrier = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all"),
              silent = silent),


    ## by building type + carrier ====
    reportAgg(emissions,
              "Emi|CO2|Residential|{typ}|Space heating|{carrier} (Mt CO2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(carrier = "all", typ = "res"),
              silent = silent),


    ## by location + carrier ====
    reportAgg(emissions,
              "Emi|CO2|Residential|{loc}|Space heating|{carrier} (Mt CO2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(carrier = "all", loc = "all"),
              silent = silent)

  )

  return(out)
}
