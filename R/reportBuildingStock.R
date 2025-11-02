#' Report building Stock
#'
#' Report quantities describing the stock of buildings
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getYears getYears<- mbind setNames dimSums mselect collapseDim

reportBuildingStock <- function(gdx, brickSets = NULL, silent = TRUE) {

  # Compute the stock difference between subsequent time periods
  # The data dimension of 'stock' needs to be identical for the whole magclass object
  .computeStockDelta <- function(stock) {

    years <- getYears(stock)

    stockBefore <- stock[, years[1:(length(years) - 1)], ]
    getYears(stockBefore) <- lead(years)[1:(length(years) - 1)]
    stockAfter <- stock[, years[2:length(years)], ]

    stockZero <- stock[, years[1], ]
    stockZero[, , ] <- NA

    mbind(stockZero, stockAfter - stockBefore)
  }

  # READ -----------------------------------------------------------------------

  # stock variable
  v_stock <- readGdxSymbol(gdx, "v_stock") %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")




  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    reportAgg(v_stock,
              "Stock|Buildings (mn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "resCom", inc = "all"),
              silent = silent),
    reportAgg(v_stock,
              "Stock|Residential (mn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_stock,
              "Stock|Commercial (mn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "com", inc = "all"),
              silent = silent),


    ## by building type ====
    reportAgg(v_stock,
              "Stock|Residential|{typ} (mn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res"),
              silent = silent),


    ## by location ====
    reportAgg(v_stock,
              "Stock|Residential|{loc} (mn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all"),
              silent = silent),


    ## by vintage ====
    reportAgg(v_stock,
              "Stock|Residential|{vin} (mn m2)", brickSets,
              agg = c(bs = "all", hs = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(vin = "all"),
              silent = silent),


    ## by heating system ====
    reportAgg(v_stock,
              "Stock|Residential|{hs} (mn m2)", brickSets,
              agg = c(bs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all"),
              silent = silent),


    ## by building type + heating system ====
    reportAgg(v_stock,
              "Stock|Residential|{typ}|{hs} (mn m2)", brickSets,
              agg = c(bs = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(hs = "all", typ = "res"),
              silent = silent)

  )

  out <- mbind(

    out,

    ## Net stock changes of total residential stock ====
    setNames(
      .computeStockDelta(out[, , "Stock|Residential (mn m2)"]),
      "Stock Delta|Residential (mn m2)"
    ),

    ## Net stock changes by hs ====
    do.call(mbind, lapply(brickSets[["hs"]][["subsets"]][["all"]], function(hs) {
      hsName <- brickSets[["hs"]][["elements"]][[hs]]
      varName <- paste0("Stock|Residential|", hsName, " (mn m2)")
      setNames(
        .computeStockDelta(out[, , varName]),
        paste0("Stock Delta|Residential|", hsName, " (mn m2)")
      )
    }))
  )

  return(out)
}
