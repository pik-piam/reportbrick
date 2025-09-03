#' Compare matching references with model values
#'
#' @param path character, path to the run
#' @param outputFolder cahracter, directory where output file is stored. If
#'   NULL, the plots folder inside the given path is used.
#' @returns file path to created output file
#'
#' @author Robin Hasse
#'
#' @export

plotMatchingComparison <- function(path, outputFolder = NULL) {

  # FUNCTIONS ------------------------------------------------------------------

  .glue <- function(x, ...) {
    lst <- list(...)
    for (tag in names(lst)) {
      x <- gsub(.embrace(tag), lst[[tag]], x, fixed = TRUE)
    }
    return(x)
  }

  .fillTemplate <- function(file, ...) {
    filePath <- piamutils::getSystemFile("plotsMatchingComparison", file,
                                         package = "reportbrick",
                                         mustWork = TRUE)
    txt <- readLines(filePath)
    .glue(txt, ...)
  }

  .countRegions <- function(cfg) {
    length(cfg$regions)
  }



  # generate HTML --------------------------------------------------------------

  fileName <- "matchingComparison"

  config <- yaml::read_yaml(file.path(path, "config", "config_COMPILED.yaml"))
  nRegions <- .countRegions(config)

  if (is.null(outputFolder)) {
    outputFolder <- file.path(path, "plots")
  }

  pathRmd <- normalizePath(file.path(outputFolder, paste0(fileName, ".Rmd")))
  pathHtml <- normalizePath(file.path(outputFolder, paste0(fileName, ".html")))

  rmd <- .fillTemplate("template.Rmd",
                       path = normalizePath(path, winslash = "/"),
                       date = Sys.Date(),
                       figHeight = nRegions)

  writeLines(rmd, pathRmd)

  rmarkdown::render(pathRmd,
                    output_format = "html_document",
                    output_file = pathHtml)

  file.remove(pathRmd)



  # OUTPUT ---------------------------------------------------------------------

  return(invisible(pathRmd))

}
