#' Read symbol from gams container
#'
#' @param gdx character, file path to GDX file
#' @param symbol character, name of gams object
#' @param field character, field to read (only relevant for variables)
#' @param asMagpie boolean, return Magpie object
#' @param stringAsFactor logical, keep default factors from gams
#' @returns MagPIE object with data of symbol
#'
#' @author Robin Hasse
#'
#' @importFrom gamstransfer Container
#' @importFrom dplyr select rename %>% all_of
#' @importFrom magclass as.magpie
#'
readGdxSymbol <- function(gdx, symbol, field = "level", asMagpie = TRUE,
                          stringAsFactor = TRUE) {

  allFields <- c("level", "marginal", "lower", "upper", "scale")

  if (!file.exists(gdx)) {
    stop("This file does not exist: ", gdx)
  }

  if (!is.character(symbol) || length(symbol) != 1) {
    stop("symbol has to be a character object of length 1.")
  }

  if (length(field) != 1 || !field %in% allFields) {
    stop("'field' has to be one out of ",
         paste(allFields, collapse = ", "),
         "; not: ", field)
  }

  m <- Container$new(gdx)
  obj <- m$getSymbols(symbol)[[1]]

  data <- obj$records

  # convert factors to character
  if (!stringAsFactor) {
    for (dim in setdiff(colnames(data), "value")) {
      data[[dim]] <- as.character(data[[dim]])
    }
  }

  # remove columns
  switch(class(obj)[1],
    Variable = {
      data <- data %>%
        select(-all_of(setdiff(allFields, field))) %>%
        rename(value = field)
    },
    Set = {
      data <- data %>%
        select(-"element_text")
      if (isTRUE(asMagpie)) {
        warning("Sets are not reported as Magpie object.")
      }
      asMagpie <- FALSE
    }
  )

  # convert to MagPIE object
  if (isTRUE(asMagpie)) {
    out <- data %>%
      as.magpie(spatial = "region", temporal = "ttot", datacol = "value")
  } else {
    out <- data
  }


  return(out)
}
