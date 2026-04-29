#' @param path character, path to the run
#' @param outputFolder character, directory where output file is stored. If
#'   NULL, the plots folder inside the given path is used.
#' @rdname plotMatching
#' @export

plotMatchingComparison <- function(path, outputFolder = NULL) {
  .Deprecated("plotMatching", package = "reportbrick")
  plotMatching(path, outputFolder = NULL)
}
