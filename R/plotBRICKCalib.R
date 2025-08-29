#' Render the BRICK calibration plotting routine
#'
#' Renders the file plotCalibration.Rmd to create the plots for the BRICK calibration
#'
#' @param path (named) character vector, path(s) to output directories.
#'  If several paths are given, the names can be used to pass short scenario names.
#' @param cal character vector, name(s) of file(s) with calibration reporting results
#' @param outName character, string added to the pdf file name and names of additionally saved plots
#' @param scenNames character vector, scenario names for different paths.
#'  Needs to be specified if \code{path} is unnamed and contains more than one element.
#'
#' @author Ricarda Rosemann
#'
#' @importFrom piamutils getSystemFile
#' @importFrom rmarkdown render
#' @export

plotBRICKCalib <- function(path = ".", cal = "BRICK_calibration_report.csv",
                           outName = "", scenNames = NULL) {

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

  # Assemble the parameters to be passed to the markdown file
  yamlParams <- list(
    path = normalizePath(path),
    cal = cal,
    docTitle = paste("BRICK Calibration Report", paste(scenario, collapse = " - ")),
    scenNames = scenNames
  )

  # All output will be stored in the first directory passed
  finalOutputDir <- unname(path[1])

  # Copy markdown file to the final output directory
  file.copy(
            getSystemFile("plotsCalibrationReporting", "plotsCalibration.Rmd",
                          package = "reportbrick"),
            finalOutputDir, overwrite = TRUE)

  # Call the Rmd file
  render(
    file.path(finalOutputDir, "plotsCalibration.Rmd"),
    output_dir = finalOutputDir,
    output_file = paste0("BRICK_calibration_report_", outName, ".pdf"),
    output_format = "pdf_document",
    params = yamlParams
  )
}
