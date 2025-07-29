#' Report renovation of heating systems
#'
#' Report quantities describing the renovation of heating systems
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Ricarda Rosemann
#'
#' @importFrom magclass mbind setNames mselect collapseDim complete_magpie

reportRenovationHS <- function(gdx, brickSets = NULL, silent = TRUE) {

  # READ -----------------------------------------------------------------------

  # renovation variable
  v_renovationHS <- readGdxSymbol(gdx, "v_renovationHS") %>%
    complete_magpie(fill = 0)

  # unit conversion: million m2 / yr-> billion m2 / yr
  v_renovationHS <- (v_renovationHS / 1000) %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")




  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    reportAgg(v_renovationHS,
              "Renovation|Buildings|Heating (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", hsr = "all", vin = "all",
                      loc = "all", typ = "resCom", inc = "all"),
              silent = silent),
    reportAgg(v_renovationHS,
              "Renovation|Residential|Heating (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", hsr = "all", vin = "all",
                      loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_renovationHS,
              "Renovation|Commercial|Heating (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", hsr = "all", vin = "all", loc = "all", typ = "com", inc = "all"),
              silent = silent),


    ## by building type ====
    reportAgg(v_renovationHS,
              "Renovation|Residential|Heating|{typ} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", hsr = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res"),
              silent = silent),


    ## by location ====
    reportAgg(v_renovationHS,
              "Renovation|Residential|Heating|{loc} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", hsr = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all"),
              silent = silent),


    ## by initial heating system ====
    reportAgg(v_renovationHS,
              "Renovation|Residential|Heating|Initial|{hs} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hsr = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all"),
              silent = silent),


    ## by building type + initial heating system ====
    reportAgg(v_renovationHS,
              "Renovation|Residential|Heating|{typ}|Initial|{hs} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hsr = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(hs = "all", typ = "res"),
              silent = silent),


    ## by final heating system ====
    reportAgg(v_renovationHS,
              "Renovation|Residential|Heating|Final|{hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hsr = "allr"),
              silent = silent),


    ## only identical replacement of the heating system ====
    reportAgg(v_renovationHS,
              "Renovation|Residential|Heating|Identical replacement (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs.hsr = "identRepl", vin = "all", loc = "all",
                      typ = "res", inc = "all"),
              silent = silent),


    ## only identical heating replacement by heating system ====
    reportAgg(v_renovationHS,
              "Renovation|Residential|Heating|Identical replacement|{hs.hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs.hsr = "identRepl"),
              silent = silent)

  )

  out <- mbind(

    out,


    ## only changes of heating systems ====
    setNames(
      out[, , "Renovation|Residential|Heating (bn m2/yr)", drop = TRUE]
      - out[, , "Renovation|Residential|Heating|Identical replacement (bn m2/yr)", drop = TRUE],
      "Renovation|Residential|Heating|Change of heating system (bn m2/yr)"
    ),


    ## only changes of heating systems by heating system ====
    do.call(mbind, lapply(brickSets[["hsr"]][["subsets"]][["all"]], function(elemName) {
      elem <- brickSets[["hsr"]][["elements"]][[elemName]]
      renFinal <- paste0("Renovation|Residential|Heating|Final|", elem, " (bn m2/yr)")
      renIdentRepl <- paste0("Renovation|Residential|Heating|Identical replacement|", elem, " (bn m2/yr)")

      renChangeFinal <- paste0("Renovation|Residential|Heating|Change of heating system|Final|", elem, " (bn m2/yr)")
      setNames(out[, , renFinal, drop = TRUE] - out[, , renIdentRepl, drop = TRUE],
               renChangeFinal)
    }))
  )

  return(out)
}
