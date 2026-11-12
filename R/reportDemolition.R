#' Report demolition
#'
#' Report quantities describing the demolition of buildings
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind setNames dimSums mselect collapseDim

reportDemolition <- function(gdx, brickSets = NULL, silent = TRUE) {

  # READ -----------------------------------------------------------------------

  # demolition variable
  v_demolition <- readGdxSymbol(gdx, "v_demolition") %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")




  # REPORT ---------------------------------------------------------------------

  sectors <- c(res = "Residential", com = "Commercial", resCom = "Buildings")
  out <- NULL

  for (sec in names(sectors)) {
    sector <- sectors[[sec]]

    out <- mbind(out,

      ## Total ====
      reportAgg(v_demolition,
                c("Demolition", sector, "(mn m2/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = sec, inc = "all"),
                silent = silent),


      ## by location ====
      reportAgg(v_demolition,
                c("Demolition", sector, "{loc}", "(mn m2/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", typ = sec, inc = "all"),
                rprt = c(loc = "all"),
                silent = silent),


      ## by vintage ====
      reportAgg(v_demolition,
                c("Demolition", sector, "{vin}", "(mn m2/yr)"), brickSets,
                agg = c(bs = "all", hs = "all", loc = "all", typ = sec, inc = "all"),
                rprt = c(vin = "all"),
                silent = silent),


      ## by heating system ====
      reportAgg(v_demolition,
                c("Demolition", sector, "{hs}", "(mn m2/yr)"), brickSets,
                agg = c(bs = "all", vin = "all", loc = "all", typ = sec, inc = "all"),
                rprt = c(hs = "all"),
                silent = silent)
    )


    if (sec == "res") {

      out <- mbind(out,

        ## by building type ====
        reportAgg(v_demolition,
                  c("Demolition", sector, "{typ}", "(mn m2/yr)"), brickSets,
                  agg = c(bs = "all", hs = "all", vin = "all", loc = "all", inc = "all"),
                  rprt = c(typ = sec),
                  silent = silent),


        ## by building type + heating system ====
        reportAgg(v_demolition,
                  c("Demolition", sector, "{typ}|{hs}", "(mn m2/yr)"), brickSets,
                  agg = c(bs = "all", vin = "all", loc = "all", inc = "all"),
                  rprt = c(hs = "all", typ = sec),
                  silent = silent)
      )
    }
  }

  return(out)
}
