#' Report renovation of heating systems
#'
#' Report quantities describing the renovation of heating systems
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param renovatedObj character, renovation measure considered,
#'   needs to be either \code{bs} (building shell) or \code{hs} (heating system).
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Ricarda Rosemann
#'
#' @importFrom magclass mbind setNames mselect collapseDim complete_magpie

reportRenovation <- function(gdx, renovatedObj = c("bs", "hs"),
                             brickSets = NULL, silent = TRUE) {

  renovatedObj <- match.arg(renovatedObj)

  initialCol <- renovatedObj
  finalCol <- paste0(initialCol, "r")
  renSymbol <- paste0("v_renovation", toupper(renovatedObj))

  renName <- if (identical(renovatedObj, "bs")) "Shell" else "Heating"


  # READ -----------------------------------------------------------------------

  # renovation variable
  v_renovation <- readGdxSymbol(gdx, renSymbol) %>%
    complete_magpie(fill = 0)

  # unit conversion: million m2 / yr-> billion m2 / yr
  v_renovation <- (v_renovation / 1000) %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")



  # FUNCTION -------------------------------------------------------------------

  # Replace the pattern if it is a separate vector entry
  .replaceInVec <- function(pattern, replacement, char) {
    char[char == pattern] <- replacement
    char
  }

  # Replace the pattern if it is surrounded by "{}"
  .replaceInVariableName <- function(pattern, replacement, char) {
    sub(paste0("{", pattern, "}"), paste0("{", replacement, "}"), char, fixed = TRUE)
  }

  # Replace occurences of "ren" and "ren.renr" by the correct column names
  .replaceRenr <- function(char, replaceFunc) {
    char <- replaceFunc("ren.renr", paste(initialCol, finalCol, sep = "."), char)
    char <- replaceFunc("ren", initialCol, char)
    char <- replaceFunc("renr", finalCol, char)
    char <- replaceFunc("nonRen", setdiff(c("bs", "hs"), initialCol), char)
    char
  }

  # Perform column name replacements on column-set specifications
  .replaceRenrCol <- function(cols) {
    if (!is.null(cols)) {
      names(cols) <- .replaceRenr(names(cols), .replaceInVec)
    }
    cols
  }

  # Insert "Heating" or "Shell" in variable name and replace placeholders for "bs(r)/hs(r)"
  .constructVariableName <- function(baseName) {
    sub("{renName}", renName, .replaceRenr(baseName, .replaceInVariableName), fixed = TRUE)
  }

  # Adjust agg and rprt and the variable name and call reportAgg
  .reportAggRen <- function(x, baseName, agg = NULL, rprt = NULL) {

    agg <- .replaceRenrCol(agg)
    rprt <- .replaceRenrCol(rprt)

    variableName <- .constructVariableName(baseName)
    reportAgg(x, variableName,
              brickSets = brickSets,
              agg = agg, rprt = rprt, silent = silent)
  }


  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    .reportAggRen(v_renovation,
                  "Renovation|Buildings|{renName} (bn m2/yr)",
                  agg = c(bs = "all", hs = "all", renr = "all", vin = "all",
                          loc = "all", typ = "resCom", inc = "all")),
    .reportAggRen(v_renovation,
                  "Renovation|Residential|{renName} (bn m2/yr)",
                  agg = c(bs = "all", hs = "all", renr = "all", vin = "all",
                          loc = "all", typ = "res", inc = "all")),
    .reportAggRen(v_renovation,
                  "Renovation|Commercial|{renName} (bn m2/yr)",
                  agg = c(bs = "all", hs = "all", renr = "all", vin = "all", loc = "all", typ = "com", inc = "all")),


    ## by building type ====
    .reportAggRen(v_renovation,
                  "Renovation|Residential|{renName}|{typ} (bn m2/yr)",
                  agg = c(bs = "all", hs = "all", renr = "all", vin = "all", loc = "all", inc = "all"),
                  rprt = c(typ = "res")),


    ## by location ====
    .reportAggRen(v_renovation,
                  "Renovation|Residential|{renName}|{loc} (bn m2/yr)",
                  agg = c(bs = "all", hs = "all", renr = "all", vin = "all", typ = "res", inc = "all"),
                  rprt = c(loc = "all")),


    ## by initial state ====
    .reportAggRen(v_renovation,
                  "Renovation|Residential|{renName}|Initial|{ren} (bn m2/yr)",
                  agg = c(nonRen = "all", renr = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
                  rprt = c(ren = "all")),


    ## by building type + initial state ====
    .reportAggRen(v_renovation,
                  "Renovation|Residential|{renName}|{typ}|Initial|{ren} (bn m2/yr)",
                  agg = c(nonRen = "all", renr = "all", vin = "all", loc = "all", inc = "all"),
                  rprt = c(ren = "all", typ = "res")),


    ## by final state ====
    .reportAggRen(v_renovation,
                  "Renovation|Residential|{renName}|Final|{renr} (bn m2/yr)",
                  agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
                  rprt = c(renr = "allr")),


    ## only identical replacement of the shell/heating system ====
    .reportAggRen(v_renovation,
                  "Renovation|Residential|{renName}|Identical replacement (bn m2/yr)",
                  agg = c(nonRen = "all", ren.renr = "identRepl", vin = "all", loc = "all",
                          typ = "res", inc = "all")),


    ## only identical heating replacement by shell/heating system ====
    .reportAggRen(v_renovation,
                  "Renovation|Residential|{renName}|Identical replacement|{ren.renr} (bn m2/yr)",
                  agg = c(nonRen = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
                  rprt = c(ren.renr = "identRepl"))

  )

  out <- mbind(

    out,


    ## only changes of heating systems ====
    setNames(
      out[, , .constructVariableName("Renovation|Residential|{renName} (bn m2/yr)"), drop = TRUE]
      - out[, , .constructVariableName("Renovation|Residential|{renName}|Identical replacement (bn m2/yr)"),
            drop = TRUE],
      .constructVariableName("Renovation|Residential|{renName}|Effective change (bn m2/yr)")
    ),


    ## only changes of heating systems by heating system ====
    do.call(mbind, lapply(brickSets[[finalCol]][["subsets"]][["all"]], function(elemName) {
      elem <- brickSets[[finalCol]][["elements"]][[elemName]]
      renFinal <- paste0(.constructVariableName("Renovation|Residential|{renName}|Final|"), elem, " (bn m2/yr)")
      renIdentRepl <- paste0(
        .constructVariableName("Renovation|Residential|{renName}|Identical replacement|"),
        elem,
        " (bn m2/yr)"
      )

      renChangeFinal <- paste0(
        .constructVariableName("Renovation|Residential|{renName}|Effective change|Final|"),
        elem,
        " (bn m2/yr)"
      )
      setNames(out[, , renFinal, drop = TRUE] - out[, , renIdentRepl, drop = TRUE],
               renChangeFinal)
    }))
  )

  return(out)
}
