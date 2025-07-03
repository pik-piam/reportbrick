#' Render additional Brick plotting routine
#'
#' Renders specified Rmd file to create a pdf of additional Brick plots.
#' The Rmd file needs to be present in \code{inst/plotsAdditional}. plotCalibration.Rmd to create the plots for the BRICK calibration
#' Currently available files are:
#'   - plotsCalibration.Rmd
#'   - plotsLcc.Rmd
#' Mutiple scenarios are only supported for \code{plotsCalibration.Rmd}.
#'
#' @param path (named) character vector, path(s) to output directories.
#'  If several paths are given, the names can be used to pass short scenario names.
#' @param file character vector, name(s) of file(s) with reporting results to be plotted.
#'   - For \code{plotsCalibration.Rmd}, this is usually the file \code{BRICK_calibration_report.csv}
#'   - For \code{plotsLcc.Rmd}, this is usually the file \code{BRICK_analysis_report.csv}
#' @param plottingRoutine Name of Rmd file to be rendered to generate the desired plots.
#'   - \code{plotsCalibration.Rmd} generates plots of calibration results
#'   - \code{plotsLcc.Rmd} generates analysis plots including life time assessments and LCC results
#' @param outName character, string added to the pdf file name and names of additionally saved plots
#' @param scenNames character vector, scenario names for different paths.
#'  Needs to be specified if \code{path} is unnamed and contains more than one element.
#'
#' @author Ricarda Rosemann
#'
#' @importFrom piamutils getSystemFile
#' @importFrom rmarkdown render
#' @export

plotBRICKAdditional <- function(path = ".", file = NULL,
                                plottingRoutine = "plotsCalibration.Rmd",
                                outName = "", scenNames = NULL) {

  docTitles <- c(plotsCalibration.Rmd = "BRICK Calibration Report",
                 plotsLcc.Rmd = "BRICK Analysis report",
                 plotsLccExt.Rmd = "BRICK Analysis report (Extended)")
  allFiles <- c(plotsCalibration.Rmd = "BRICK_calibration_report",
                plotsLcc.Rmd = "BRICK_analysis_report",
                plotsLccExt.Rmd = "BRICK_analysis_report")
  if (grepl("Ext", plottingRoutine)) outName <- paste0("Ext", outName)

  # Extract the scenario name from the output directory
  scenario <- sub("_\\d{4}-\\d{2}-\\d{2}_\\d{2}\\.\\d{2}\\.\\d{2}", "", basename(path))

  # Check length of given output name
  if (length(outName) > 1) {
    stop("You passed more than one output name. outName needs to have length 1.")
  }

  # Extract scenario names if more than one output directory was passed
  if (length(path) > 1 && is.null(scenNames)) {
    if (!is.null(names(path))) {
      scenNames <- names(path)
    } else {
      stop("You passed more than one output directory, but did not provide the scenario names.",
           "Please either specify scenNames or pass path as a named vector.")
    }
  }

  if (is.null(file)) file <- paste0(allFiles[plottingRoutine], ".csv")

  # Assemble the parameters to be passed to the markdown file
  yamlParams <- list(
    path = normalizePath(path),
    file = file,
    docTitle = paste(docTitles[plottingRoutine], paste(scenario, collapse = " - ")),
    scenNames = scenNames
  )

  # All output will be stored in the first directory passed
  finalOutputDir <- unname(path[1])

  # Copy markdown file to the final output directory
  file.copy(
            getSystemFile("plotsAdditional", plottingRoutine,
                          package = "reportbrick"),
            finalOutputDir, overwrite = TRUE)

  # Call the Rmd file
  render(
    file.path(finalOutputDir, plottingRoutine),
    output_dir = finalOutputDir,
    output_file = paste0(allFiles[[plottingRoutine]], outName, ".pdf"),
    output_format = "pdf_document",
    params = yamlParams
  )
}
