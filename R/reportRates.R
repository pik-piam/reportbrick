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

  v_renovation <- readGdxSymbol(gdx, "v_renovation") %>%
    .prepVar()
  v_stock <- readGdxSymbol(gdx, "v_stock") %>%
    .prepVar() %>%
    .shiftTime(-0.5, "na")


  ## parameters ====

  p_renDepth <- readGdxSymbol(gdx, "p_renDepth") %>%
    collapseDim()



  # REPORT ---------------------------------------------------------------------

  out3 <- mbind(


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
