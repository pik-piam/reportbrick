#' Report renovation of the building shell
#'
#' Report quantities describing the renovation of the building shell
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Ricarda Rosemann
#'
#' @importFrom magclass mbind setNames mselect collapseDim complete_magpie

reportRenovationBS <- function(gdx, brickSets = NULL, silent = TRUE) {

  # READ -----------------------------------------------------------------------

  # renovation variable
  v_renovationBS <- readGdxSymbol(gdx, "v_renovationBS") %>%
    complete_magpie(fill = 0)

  # unit conversion: million m2 / yr-> billion m2 / yr
  v_renovationBS <- (v_renovationBS / 1000) %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")




  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    reportAgg(v_renovationBS,
              "Renovation|Buildings|Shell (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all", vin = "all",
                      loc = "all", typ = "resCom", inc = "all"),
              silent = silent),
    reportAgg(v_renovationBS,
              "Renovation|Residential|Shell (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all", vin = "all",
                      loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_renovationBS,
              "Renovation|Commercial|Shell (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all", vin = "all", loc = "all", typ = "com", inc = "all"),
              silent = silent),


    ## by building type ====
    reportAgg(v_renovationBS,
              "Renovation|Residential|Shell|{typ} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res"),
              silent = silent),


    ## by location ====
    reportAgg(v_renovationBS,
              "Renovation|Residential|Shell|{loc} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all"),
              silent = silent),

    ## by final building shell ====
    reportAgg(v_renovationBS,
              "Renovation|Residential|Shell|Final|{bsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(bsr = "all"),
              silent = silent),


    ## only identical replacement of the building shell ====
    reportAgg(v_renovationBS,
              "Renovation|Residential|Shell|Identical replacement (bn m2/yr)", brickSets,
              agg = c(bs.bsr = "identRepl", hs = "all", vin = "all", loc = "all",
                      typ = "res", inc = "all"),
              silent = silent),


    ## only identical shell replacement by building shell ====
    reportAgg(v_renovationBS,
              "Renovation|Residential|Shell|Identical replacement|{bs.bsr} (bn m2/yr)", brickSets,
              agg = c(hs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(bs.bsr = "identRepl"),
              silent = silent)
  )

  return(out)
}
