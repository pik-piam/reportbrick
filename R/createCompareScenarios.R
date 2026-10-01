createCompareScenarios <- function(settingsFile = "settings.yaml", runs = NULL, ...) {

  settings <- yaml::read_yaml(settingsFile)


  allRuns <- file.path("output", settings$run, "BRICK_general.mif")

  args <- list(...)

  argsDefaul <- list(
    projectLibrary = "reportbrick",
    mifHist = NULL,
    outputDir = file.path()
  )

  piamPlotComparison::compareScenarios(
    "reportbrick",
    mifScen = mifScen,
    mifHist = "../REMIND/remind/output/EnSec_2023-01-26_12.25.01/historical.mif",
    outputFile = "matchings_v8",
    mainReg = "EU27",
    sections = 1,
    yearsBarPlot = c(2005, 2010, 2015),
    yearsScen = 2000:2023
  )
}
