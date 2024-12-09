#' Split dimension names
#'
#' Split each entry of a character vector and return one unnested character
#' vector.
#'
#' @author Robin Hasse
#'
#' @param x character vector
#' @param split character used to split \code{x}
#' @returns character vector with each dimension as an own entry

.split <- function(x, split = "\\.") {
  if (is.null(x)) {
    return(NULL)
  }
  unlist(strsplit(x, split))
}






#' All Combinations of dimension elements
#'
#' @param lst names list of dimension entries
#' @returns character vector with all combinations of the dimension elements
#'   each separated by \code{.}
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr everything %>%
#' @importFrom tidyr unite

.combinations <- function(lst) {
  do.call(expand.grid, lst) %>%
    unite("combinations", everything(), sep = ".") %>%
    getElement("combinations")
}





#' Escape tag in curly brackets
#'
#' @param tag character tag
#' @returns character, tag in curly brackets
#'
#' @author Robin Hasse

.embrace <- function(tag) {
  paste0("{", tag, "}")
}





#' Add carrier dimension based on heating system technology
#'
#' @param v_stock MagPIE object, BRICK variable
#' @param hsCarrier data.frame, mapping between heating technology and energy
#'   carrier
#' @returns MagPIE object with additional carrier dimension
#'
#' @author Robin Hasse

.addCarrierDimension <- function(v_stock, hsCarrier) {
  stock <- v_stock %>%
    add_dimension(dim = 3.3, add = "carrier", "carrier")
  for (i in seq_len(nrow(hsCarrier))) {
    getNames(stock) <- sub(paste0(hsCarrier[i, "hs"], "\\.carrier"),
                           paste(as.character(hsCarrier[i, ]), collapse = "."),
                           getNames(stock))
  }
  return(stock)





#' Prepare variable
#'
#' Convert values by multiplying with given factor and select the area quantity.
#'
#' @param x MAgPIE object of brick variable
#' @param factor numeric, scalar factor that \code{x} is multiplied with
#' @param onlyArea logical, if TRUE, select 'area' quantity and collapse
#'   quantity dimension
#' @returns scales and filtered MAgPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getSets mselect collapseDim
#' @importFrom dplyr %>%

.prepVar <- function(x, factor = 1, onlyArea = TRUE) {
  stopifnot("factor needs to be a scalar" =
              is.numeric(factor) && length(factor) == 1)
  x <- x * factor
  if (isTRUE(onlyArea) && "qty" %in% getSets(x)) {
    x <- x %>%
      mselect(qty = "area") %>%
      collapseDim(dim = "qty")
  }
  return(x)
}





#' Shift data in time
#'
#' The nominal periods are maintained.
#'
#' @param x MAgPIE object of brick variable
#' @param by fraction of time step length to shift by
#' @param unknownEnd character, how to handle the unknown end value
#'   \itemeize {
#'    \item \code{"drop"} the returned object has one period less
#'    \item \code{"na} the returned object has the same periods but NA values at
#'                     the unknown end
#'   }
#' @returns MagPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom utils head tail
#' @importFrom magclass getYears getYears<- add_columns

.shiftTime <- function(x, by = 0, unknownEnd = c("drop", "na")) {

  unknownEnd <- match.arg(unknownEnd)

  if (by == 0) return(x)
  if (abs(by) > 1) stop("Can't shift by more than one time step.")

  t <- sort(getYears(x))
  tNext <- if (by > 0) tail(t, -1) else head(t, -1)
  tKnown <- if (by > 0) head(t, -1) else tail(t, -1)
  tUnknown <- setdiff(t, tKnown)

  xNext <- x[, tNext, ]
  getYears(xNext) <- tKnown

  xShift <- xNext[, tKnown, ] * abs(by) + x[, tKnown, ] * (1 - abs(by))

  if (unknownEnd == "na") {
    xShift <- add_columns(xShift, tUnknown, 2)
  }

  return(xShift)
}
