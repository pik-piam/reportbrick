#' Report energy demand and emissions
#'
#' @param x MagPIE object
#' @param var character, high-level variable name
#' @param unit character, unit to be pasted at the end of the variable name
#' @param brickSets character, BRICK reporting template
#' @param silent logical, suppress warnings and printing of dimension mapping
#' @returns MagPIE object with reporting data
#'
#' @author Robin Hasse

reportDemandVars <- function(x, var, unit, brickSets, silent) {
  sectors <- c(res = "Residential", com = "Commercial", resCom = "Buildings")
  out <- NULL
  for (sec in names(sectors)) {
    sector = sectors[[sec]]

    out <- mbind(out,

      # total ------------------------------------------------------------------

      reportAgg(x,
                paste(var, sector, "{enduse}", sep = "|"), brickSets,
                agg = c(bs = "all", hs = "all", carrier = "all", vin = "all", typ = sec, loc = "all", inc = "all"),
                rprt = c(enduse = "all"),
                silent = silent),


      # by building type -------------------------------------------------------

      reportAgg(x,
                paste(var, sector, "{typ}|{enduse}", sep = "|"), brickSets,
                agg = c(bs = "all", hs = "all", carrier = "all", vin = "all", loc = "all", inc = "all"),
                rprt = c(enduse = "all", typ = sec),
                silent = silent),


      # by location ------------------------------------------------------------

      reportAgg(x,
                paste(var, sector, "{loc}|{enduse}", sep = "|"), brickSets,
                agg = c(bs = "all", hs = "all", carrier = "all", vin = "all", typ = sec, inc = "all"),
                rprt = c(enduse = "all", loc = "all"),
                silent = silent),


      # by carrier (+ heating technology) --------------------------------------

      reportAgg(x,
                paste(var, sector, "{enduse}|{carrier}", sep = "|"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = sec, inc = "all"),
                rprt = c(enduse = "all", carrier = "all"),
                silent = silent),

      reportAgg(x,
                paste(var, sector, "{enduse}|{carrier.hs}", sep = "|"), brickSets,
                agg = c(bs = "all", vin = "all", loc = "all", typ = sec, inc = "all"),
                rprt = c(enduse = "all", carrier.hs = "multiHsCarriers"),
                silent = silent),


      # by building type + carrier (+ heating technology) ----------------------

      reportAgg(x,
                paste(var, sector, "{typ}|{enduse}|{carrier}", sep = "|"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", loc = "all", inc = "all"),
                rprt = c(enduse = "all", carrier = "all", typ = sec),
                silent = silent),

      reportAgg(x,
                paste(var, sector, "{typ}|{enduse}|{carrier.hs}", sep = "|"), brickSets,
                agg = c(bs = "all", vin = "all", loc = "all", inc = "all"),
                rprt = c(enduse = "all", carrier.hs = "multiHsCarriers", typ = sec),
                silent = silent),


      # by location + carrier (+ heating technology) ---------------------------

      reportAgg(x,
                paste(var, sector, "{loc}|{enduse}|{carrier}", sep = "|"), brickSets,
                agg = c(bs = "all", hs = "all", vin = "all", typ = sec, inc = "all"),
                rprt = c(enduse = "all", carrier = "all", loc = "all"),
                silent = silent),

      reportAgg(x,
                paste(var, sector, "{loc}|{enduse}|{carrier.hs}", sep = "|"), brickSets,
                agg = c(bs = "all", vin = "all", typ = sec, inc = "all"),
                rprt = c(enduse = "all", carrier.hs = "multiHsCarriers", loc = "all"),
                silent = silent)
    )
  }

  getNames(out) <- paste0(getNames(out), " (", unit, ")")

  return(out)
}
