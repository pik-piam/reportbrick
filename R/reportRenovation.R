#' Report renovation
#'
#' Report quantities describing the renovation of new buildings
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Ricarda Rosemann
#'
#' @importFrom magclass mbind setNames mselect collapseDim complete_magpie

reportRenovation <- function(gdx, brickSets = NULL, silent = TRUE) {

  # READ -----------------------------------------------------------------------

  # renovation variable
  v_renovation <- readGdxSymbol(gdx, "v_renovation") %>%
    complete_magpie(fill = 0)

  # unit conversion: million m2 / yr-> billion m2 / yr
  v_renovation <- (v_renovation / 1000) %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")




  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    reportAgg(v_renovation,
              "Renovation|Buildings (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all", loc = "all", typ = "resCom", inc = "all"),
              silent = silent),
    reportAgg(v_renovation,
              "Renovation|Residential (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_renovation,
              "Renovation|Residential|Shell (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all", hsr = "allr", vin = "all",
                      loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_renovation,
              "Renovation|Residential|Heating (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "allr", hsr = "all", vin = "all",
                      loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_renovation,
              "Renovation|Commercial (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all", loc = "all", typ = "com", inc = "all"),
              silent = silent),


    ## by building type ====
    reportAgg(v_renovation,
              "Renovation|Residential|{typ} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res"),
              silent = silent),


    ## by location ====
    reportAgg(v_renovation,
              "Renovation|Residential|{loc} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all"),
              silent = silent),

    ## by final building shell ====
    reportAgg(v_renovation,
              "Renovation|Residential|Final|{bsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", hsr = "allr", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(bsr = "all"),
              silent = silent),


    ## by initial heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|Initial|{hs} (bn m2/yr)", brickSets,
              agg = c(bs = "all", bsr = "allr", hsr = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all"),
              silent = silent),


    ## by building type + initial heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|{typ}|Initial|{hs} (bn m2/yr)", brickSets,
              agg = c(bs = "all", bsr = "allr", hsr = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(hs = "all", typ = "res"),
              silent = silent),


    ## by final heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|Final|{hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "allr", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hsr = "allr"),
              silent = silent),

    ## only identical replacement of the building shell ====
    reportAgg(v_renovation,
              "Renovation|Residential|Shell|Identical replacement (bn m2/yr)", brickSets,
              agg = c(bs.bsr = "identRepl", hs = "all", hsr = "allr", vin = "all", loc = "all",
                      typ = "res", inc = "all"),
              silent = silent),

    ## only identical replacement of the heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|Heating|Identical replacement (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs.hsr = "identRepl", bsr = "allr", vin = "all", loc = "all",
                      typ = "res", inc = "all"),
              silent = silent),

    ## only identical shell replacement by building shell ====
    reportAgg(v_renovation,
              "Renovation|Residential|Identical replacement|{bs.bsr} (bn m2/yr)", brickSets,
              agg = c(hs = "all", hsr = "allr", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(bs.bsr = "identRepl"),
              silent = silent),

    ## only identical heating replacement by heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|Identical replacement|{hs.hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", bsr = "allr", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs.hsr = "identRepl"),
              silent = silent)

  )

  out <- mbind(

    out,

    ## only changes of heating systems ====
    setNames(
      out[, , "Renovation|Residential|Heating (bn m2/yr)", drop = TRUE]
      - out[, , "Renovation|Residential|Heating|Identical replacement (bn m2/yr)", drop = TRUE],
      "Renovation|Residential|Change of heating system (bn m2/yr)"
    ),

    ## only changes of heating systems by heating system ====
    do.call(mbind, lapply(brickSets[["hsr"]][["subsets"]][["all"]], function(elemName) {
      elem <- brickSets[["hsr"]][["elements"]][[elemName]]
      renFinal <- paste0("Renovation|Residential|Final|", elem, " (bn m2/yr)")
      renIdentRepl <- paste0("Renovation|Residential|Identical replacement|", elem, " (bn m2/yr)")

      renChangeFinal <- paste0("Renovation|Residential|Change of heating system|Final|", elem, " (bn m2/yr)")
      setNames(out[, , renFinal, drop = TRUE] - out[, , renIdentRepl, drop = TRUE],
               renChangeFinal)
    }))
  )

  return(out)
}
