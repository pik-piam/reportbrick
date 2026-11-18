#' Report renovation and exchange rates
#'
#' Yearly renovation activity relative to the stock
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind setNames mselect collapseDim

reportRates <- function(gdx, brickSets = NULL, silent = TRUE) {

  # READ -----------------------------------------------------------------------

  ## variables ====

  v_renovation <- readGdxSymbol(gdx, "v_renovation")
  v_stock <- readGdxSymbol(gdx, "v_stock")

  # unit conversion: million m2 / yr -> billion m2 / yr
  v_renovation <- .prepVar(v_renovation)
  v_stock      <- .prepVar(v_stock)


  ## parameters ====

  p_renDepth <- readGdxSymbol(gdx, "p_renDepth") %>%
    collapseDim()



  # REPORT ---------------------------------------------------------------------

  out <- mbind(


    ## Renovation rate ====

    setNames(
      reportAgg(v_renovation * p_renDepth,
                brickSets = brickSets,
                agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all",
                        loc = "all", typ = "res", inc = "all"),
                silent = silent)
      / reportAgg(v_stock,
                  brickSets = brickSets,
                  agg = c(bs = "all", hs = "all", loc = "all", vin = "all",
                          typ = "res", inc = "all"),
                  silent = silent)
      * 100
      ,
      "Renovation rate|Residential (%/yr)"
    ),

    ### by building type ####
    reportAgg(v_renovation * p_renDepth,
              name = "Renovation rate|Residential|{typ} (%/yr)",
              brickSets = brickSets,
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all",
                      loc = "all", inc = "all"),
              rprt = c(typ = "res"),
              silent = silent)
    / reportAgg(v_stock,
                name = "Renovation rate|Residential|{typ} (%/yr)",
                brickSets = brickSets,
                agg = c(bs = "all", hs = "all", loc = "all", vin = "all",
                        inc = "all"),
                rprt = c(typ = "res"),
                silent = silent)
    * 100
  )



  return(out)
}
