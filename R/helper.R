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
