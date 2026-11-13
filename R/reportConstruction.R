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

  sectors <- c(res = "Residential", com = "Commercial", resCom = "Buildings")
  out <- NULL

  for (sec in names(sectors)) {
    sector <- sectors[[sec]]

    out <- mbind(out,

      ## Total ====
      reportAgg(v_construction,
                c("Construction", sector, "(mn m2/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", loc = "all", typ = sec, inc = "all"),
                silent = silent),


      ## by location ====
      reportAgg(v_construction,
                c("Construction", sector, "{loc}", "(mn m2/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", typ = sec, inc = "all"),
                rprt = c(loc = "all"),
                silent = silent),


      ## by heating system ====
      reportAgg(v_construction,
                c("Construction", sector, "{hs}", "(mn m2/yr)"), brickSets,
                agg = c(bs = "all", loc = "all", typ = sec, inc = "all"),
                rprt = c(hs = "all"),
                silent = silent)
    )


    if (sec == "res") {

      out <- mbind(out,

        ## by building type ====
        reportAgg(v_construction,
                  c("Construction", sector, "{typ}", "(mn m2/yr)"), brickSets,
                  agg = c(bs = "all", hs = "all", loc = "all", inc = "all"),
                  rprt = c(typ = sec),
                  silent = silent),


        ## by building type + heating system ====
        reportAgg(v_construction,
                  c("Construction", sector, "{typ}|{hs}", "(mn m2/yr)"), brickSets,
                  agg = c(bs = "all", loc = "all", inc = "all"),
                  rprt = c(hs = "all", typ = sec),
                  silent = silent)
      )
    }
  }

  return(out)
}
