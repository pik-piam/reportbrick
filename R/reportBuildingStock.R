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
#' @importFrom magclass addDim getItems  getSets getYears getYears<- mbind
#'   setNames dimSums mselect collapseDim getItems<-
#' @importFrom dplyr %>%

reportBuildingStock <- function(gdx, brickSets = NULL, silent = TRUE) {

  # Compute the stock difference between subsequent time periods and bind to data
  # The data dimension of 'stock' needs to be identical for the whole magclass object
  .bindStockDelta <- function(data, var, dt) {

    if (!var %in% getItems(data, 3)) return(data)

    # new variable name
    pattern <- "^([^\\|]+)(.*) \\((.*)\\)$"
    newVar <- paste0("Net ", tolower(sub(pattern, "\\1", var)), " change",
                     sub(pattern, "\\2", var),
                     " (", sub(pattern, "\\3", var), "/yr)")

    stock <- data[, , var]
    years <- getYears(stock)

    stockBefore <- stock[, head(years, -1), ]
    getYears(stockBefore) <- head(lead(years), -1)
    stockAfter <- stock[, tail(years, -1), ]

    deltaStockZero <- stock[, years[1], ]
    deltaStockZero[, , ] <- NA

    # Expand `dt` to dimensions of `stock`
    dt <- dt %>%
      mselect(ttot = tail(years, -1)) %>%
      addDim(dim = 1, dimName = getSets(stock)["d1.1"], item = getItems(stock, dim = "region")) %>%
      # Work around as `addDim` can't handle existing dim name
      addDim(dim = 3, dimName = "tmpName", item = getItems(stock, dim = 3))
    getSets(dt)["d3.1"] <- getSets(stock)["d3.1"]

    deltaStock <- mbind(deltaStockZero, (stockAfter - stockBefore) / dt)
    getItems(deltaStock, 3) <- newVar

    mbind(data, deltaStock)
  }



  # READ -----------------------------------------------------------------------

  # stock variable
  v_stock <- readGdxSymbol(gdx, "v_stock") %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")

  # Time step length
  p_dt <- readGdxSymbol(gdx, "p_dt") %>%
    collapseDim()

  # add demand dimension to stock
  hsCarrier <- readGdxSymbol(gdx, "hsCarrier", stringAsFactor = FALSE)
  stock <- .addCarrierDimension(v_stock, hsCarrier)
  stock <- complete_magpie(stock, fill = 0)



  # REPORT ---------------------------------------------------------------------

  sectors <- c(res = "Residential", com = "Commercial", resCom = "Buildings")
  out <- NULL

  for (sec in names(sectors)) {
    sector <- sectors[[sec]]

    out <- mbind(out,

      ## Total ====
      reportAgg(v_stock,
                c("Stock", sector, "(mn m2)"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = sec, inc = "all"),
                silent = silent),


      ## by location ====
      reportAgg(v_stock,
                c("Stock", sector, "{loc} (mn m2)"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", typ = sec, inc = "all"),
                rprt = c(loc = "all"),
                silent = silent),


      ## by vintage ====
      reportAgg(v_stock,
                c("Stock", sector, "{vin} (mn m2)"), brickSets,
                agg = c(bs = "all", hs = "all", loc = "all", typ = sec, inc = "all"),
                rprt = c(vin = "all"),
                silent = silent),


      ## by heating system ====
      reportAgg(v_stock,
                c("Stock", sector, "{hs} (mn m2)"), brickSets,
                agg = c(bs = "all", vin = "all", loc = "all", typ = sec, inc = "all"),
                rprt = c(hs = "all"),
                silent = silent),



      ## by carrier (+ heating technology) ====
      reportAgg(stock,
                c("Stock", sector, "{carrier} (mn m2)"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = sec, inc = "all"),
                rprt = c(carrier = "all"),
                silent = silent),

      reportAgg(stock,
                c("Stock", sector, "{carrier.hs} (mn m2)"), brickSets,
                agg = c(bs = "all", vin = "all", loc = "all", typ = sec, inc = "all"),
                rprt = c(carrier.hs = "multiHsCarriers"),
                silent = silent)
    )


    if (sec == "res") {
      out <- mbind(out,

        ## by building type ====
        reportAgg(v_stock,
                  c("Stock", sector, "{typ} (mn m2)"), brickSets,
                  agg = c(bs = "all", hs = "all", vin = "all", loc = "all", inc = "all"),
                  rprt = c(typ = sec),
                  silent = silent),


        ## by building type + carrier (+ heating technology) ====
        reportAgg(stock,
                  c("Stock", sector, "{typ}|{carrier} (mn m2)"), brickSets,
                  agg = c(bs = "all", hs = "all", vin = "all", loc = "all", inc = "all"),
                  rprt = c(carrier = "all", typ = sec),
                  silent = silent),

        reportAgg(stock,
                  c("Stock", sector, "{typ}|{carrier.hs} (mn m2)"), brickSets,
                  agg = c(bs = "all", vin = "all", loc = "all", inc = "all"),
                  rprt = c(carrier.hs = "multiHsCarrier", typ = sec),
                  silent = silent)
      )
    }


    ## Net stock changes of total residential stock ====
    out <- .bindStockDelta(out, .collapseName(c("Stock", sector, "(mn m2)")), p_dt)


    ## Net stock changes by hs ====
    for (hs in brickSets[["hs"]][["subsets"]][["all"]]) {
      hsName <- brickSets[["hs"]][["elements"]][[hs]]
      varName <- .collapseName(c("Stock", sector, hsName, "(mn m2)"))
      out <- .bindStockDelta(out, varName, p_dt)
    }
  }

  return(out)
}
