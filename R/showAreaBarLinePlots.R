#' Show Area and bar plots followed by multiple line plots
#'
#' @param data A quitte object
#' @param tot character, a total value to be shown in the area plots. If
#'   multiple totals are passed, plots are created for each total.
#' @param items A character vector. Appending these items tot the total yields
#'   the variables to plot.
#' @param heading character, header text
#' @param nHeading numeric, level of heading
#' @param showLines logical, should line plots be shown?
#' @param showTot logical, should total line be plotted on are and bar plots?
#'
#' @author Robin Hasse
#'
#' @export

showAreaBarLinePlots <- function(data, tot, items,
                                 heading = NULL, nHeading = NULL,
                                 showLines = TRUE, showTot = TRUE) {

  allVars <- outer(tot, items, paste, sep = "|")

  if (!any(allVars %in% data$variable)) {
    return(invisible(NULL))
  }

  if (!is.null(heading)) {
    heading(heading, nHeading)
  }

  for (t in tot) {
    vars <- paste(t, items, sep = "|")

    mip::showAreaAndBarPlots(data, vars = vars, tot = if (showTot) t else NULL,
                             orderVars = "user", scales = "fixed")

    if (showLines) {
      purrr::walk(vars, mip::showLinePlots, data = data)
    }
  }

}
