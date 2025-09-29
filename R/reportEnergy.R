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
#'   complete_magpie

reportEnergy <- function(gdx, brickSets = NULL, silent = TRUE) {

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
  specDemand <- list(UE = readGdxSymbol(gdx, "p_ueDemand"),
                     FE = readGdxSymbol(gdx, "p_feDemand"))

  # filter for vintages present in the Brick output
  specDemand <- lapply(specDemand, function(x) {
    mselect(x, vin = names(brickSets$vin$elements))
  })

  out <- NULL


  # REPORT ---------------------------------------------------------------------

  for (energyLevel in names(specDemand)) {
    energyDemand <- stock * specDemand[[energyLevel]]
    energyDemand <- energyDemand * 3.6E-6 # GWh to EJ
    out <- mbind(out,

      ## total ====
      reportAgg(energyDemand,
                paste0(energyLevel, "|Residential|Space heating (EJ/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", carrier = "all", vin = "all", typ = "res", loc = "all", inc = "all"),
                silent = silent),


      ## by building type ====
      reportAgg(energyDemand,
                paste0(energyLevel, "|Residential|{typ}|Space heating (EJ/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", carrier = "all", vin = "all", loc = "all", inc = "all"),
                rprt = c(typ = "res"),
                silent = silent),


      ## by location ====
      reportAgg(energyDemand,
                paste0(energyLevel, "|Residential|{loc}|Space heating (EJ/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", carrier = "all", vin = "all", typ = "res", inc = "all"),
                rprt = c(loc = "all"),
                silent = silent),


      ## by carrier ====
      reportAgg(energyDemand,
                paste0(energyLevel, "|Residential|Space heating|{carrier} (EJ/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
                rprt = c(carrier = "all"),
                silent = silent),


      ## by heating technology ====
      reportAgg(energyDemand,
                paste0(energyLevel, "|Residential|Space heating|{hs} (EJ/yr)"), brickSets,
                agg = c(bs = "all", carrier = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
                rprt = c(hs = "all"),
                silent = silent),


      ## by building type + carrier ====
      reportAgg(energyDemand,
                paste0(energyLevel, "|Residential|{typ}|Space heating|{carrier} (EJ/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", loc = "all", inc = "all"),
                rprt = c(carrier = "all", typ = "res"),
                silent = silent),


      ## by location + carrier ====
      reportAgg(energyDemand,
                paste0(energyLevel, "|Residential|{loc}|Space heating|{carrier} (EJ/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", typ = "res", inc = "all"),
                rprt = c(carrier = "all", loc = "all"),
                silent = silent)

    )
  }



  return(out)
}





#' Add carrier dimesion based on heating system technology
#'
#' @param v_stock MagPIE object, BRICK variable
#' @param hsCarrier data.frame, mapping between heating technology and energy
#'  carrier
#' @returns MagPIE object with additional carrier dimension

.addCarrierDimension <- function(v_stock, hsCarrier) {
  stock <- v_stock %>%
    add_dimension(dim = 3.3, add = "carrier", "carrier")
  for (i in seq_len(nrow(hsCarrier))) {
    getNames(stock) <- sub(paste0(hsCarrier[i, "hs"], "\\.carrier"),
                           paste(as.character(hsCarrier[i, ]), collapse = "."),
                           getNames(stock))
  }
  return(stock)
}
