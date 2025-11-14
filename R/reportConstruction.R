#' Report construction
#'
#' Report quantities describing the construction of new buildings
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind setNames dimSums mselect collapseDim

reportConstruction <- function(gdx, brickSets = NULL, silent = TRUE) {

  # READ -----------------------------------------------------------------------

  # construction variable
  v_construction <- readGdxSymbol(gdx, "v_construction") %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")




  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    reportAgg(v_construction,
              "Construction|Buildings (mn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", loc = "all", typ = "resCom", inc = "all"),
              silent = silent),
    reportAgg(v_construction,
              "Construction|Residential (mn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_construction,
              "Construction|Commercial (mn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", loc = "all", typ = "com", inc = "all"),
              silent = silent),


    ## by building type ====
    reportAgg(v_construction,
              "Construction|Residential|{typ} (mn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res"),
              silent = silent),


    ## by location ====
    reportAgg(v_construction,
              "Construction|Residential|{loc} (mn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all"),
              silent = silent),


    ## by heating system ====
    reportAgg(v_construction,
              "Construction|Residential|{hs} (mn m2/yr)", brickSets,
              agg = c(bs = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all"),
              silent = silent),


    ## by building type + heating system ====
    reportAgg(v_construction,
              "Construction|Residential|{typ}|{hs} (mn m2/yr)", brickSets,
              agg = c(bs = "all", loc = "all", inc = "all"),
              rprt = c(hs = "all", typ = "res"),
              silent = silent)

  )

  return(out)
}
