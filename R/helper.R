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
}





#' Add dimension of not yet existing
#'
#' wrapper around \code{magclass::addDim} that does nothing if a dimension of
#' the given name already exists
#'
#' @param x	MAgPIE object which should be extended.
#' @param dimName	The name of the new dimension
#' @param dim	The dimension number of the new dimension (e.g. 3.1)
#' @param item One or more names of items in the new dimension.
#' @returns The extended MAgPIE object
#'
#' @author Robin Hasse

.addDim <- function(x, dimName, item, dim = 3.1) {
  if (dimName %in% getSets(x)) return(x)
  addDim(x, dim = dim, dimName = dimName, item = item)
}




.addEnduseDimension <- function(x, ref = NULL, dim = 3.1) {
  enduses <- if (is.null(ref) || !"enduse" %in% getSets(ref)) {
    "space_heating"
  } else {
    getItems(ref, "enduse")
  }
.addDim(x, dimName = "enduse", item = enduses, dim = dim)
}
