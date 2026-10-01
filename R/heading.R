#' Print Rmd heading
#'
#' @param text character, header text or sector tag
#' @param level numeric, level of heading (number of #s)
#' @export

heading <- function(text, level = 1) {
  if (is.null(level)) {
    stop("Provide a level for this heading: ", text)
  }

  # replace sector tags by full sector heading
  text <- switch(text,
                 all = "All buildings",
                 res = "Residential",
                 com = "Commercial",
                 text)

  cat(paste(rep("\n", level - 1), collapse = ""),
      paste(rep("#", level), collapse = ""), " ",
      text, "\n",
      sep = "")
}
