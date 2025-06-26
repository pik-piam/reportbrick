#' Report aggregated quantities
#'
#' @param x MagPIE object, BRICK object
#' @param name character, name of reporting variable. reported dimensions passed
#'   with \code{rprt} have to be escaped with curly brackets.
#' @param brickSets named list, BRICK reporting template
#' @param agg named vector of dimensions to aggregate. Names are dimension names
#'   of \code{x} and values are either set elements or subsets of set elements
#'   to consider.
#' @param rprt named vector of dimensions to report individually. Names are
#'   dimension names of \code{x} and values are either set elements or subsets
#'   of set elements to report.
#' @param silent boolean, suppress warnings and printing of dimension mapping
#' @note To consider specific combinations of dimensions in both \code{agg} or
#'   \code{rprt}, the combination can be given just like one dimension
#'   separated by \code{.}.
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getSets ndim
#' @importFrom utils tail capture.output
#' @importFrom tidyr separate
#' @importFrom dplyr %>%
#' @importFrom stringr str_escape

reportAgg <- function(x,
                      name,
                      brickSets = readBrickSets(NULL),
                      agg = NULL,
                      rprt = NULL,
                      silent = TRUE) {

  # CHECK INPUT ----------------------------------------------------------------

  dims <- unname(tail(getSets(x, 3), -2))

  # check that each dimension is either aggregated or reported
  specifiedDims <- .split(c(names(agg), names(rprt)))
  missingDims <- setdiff(dims, specifiedDims)
  if (length(missingDims) > 0) {
    stop("The following dimensions are neither aggregated nor reported ",
         "individually: ", paste(missingDims, collapse = ", "))
  }
  if (!setequal(dims, specifiedDims)) {
    stop("Each of the following dimension has to be either aggregated or ",
         "reported individually: ", paste(dims, collapse = ", "), ". ",
         "You want to aggegate ", paste(.split(names(agg)), collapse = ", "),
         " and report ", paste(.split(names(rprt)), collapse = ", "), ".")
  }

  tagsInName <- .findTags(name)

  # check if all reported dimensions are tagged in name
  if (is.null(rprt)) {
    if (!is.null(tagsInName)) {
      stop("Found the following dimension tags in 'name', though 'rprt' is NULL: ",
           paste(tagsInName, collapse = ", "))
    }
  } else {
    if (!setequal(.split(names(rprt)), .split(tagsInName))) {
      stop("Inconsistency between name tags (", paste(tagsInName, collapse = ", "),
           ") and reported dimensions (", paste(.split(names(rprt))), ").")
    }
  }



  # PREPARE --------------------------------------------------------------------

  # list with dimension elements to consider for aggregation and reporting
  map <- .constructDimMapping(agg, rprt, brickSets, silent)

  nullInMap <- unlist(lapply(map, function(x) unlist(lapply(x, is.null))))
  if (any(nullInMap)) {
    if (isFALSE(silent)) {
      message("Can't report '", name, "' as mapping can't be constructed for ",
              paste(names(nullInMap)[nullInMap], collapse = ", "), ".")
    }
    return(NULL)
  }

  if (isFALSE(silent)) {
    message("For the variable '", name, "' using this map:\n  ",
            paste(capture.output(map), collapse = "\n  "))
  }



  # AGGREGATE TO REPORTING VARS ------------------------------------------------

  if (is.null(rprt)) {
    out <- .agg(x, agg = map$agg, silent = silent) %>%
      .setNames(name)
  } else {
    # combination of entries of reporting dimensions
    rprtCombinations <- do.call(expand.grid, map$rprt)
    for (cd in grep("\\.", colnames(rprtCombinations), value = TRUE)) {
      if (cd %in% tagsInName) {
        # the combined dimension tag is used -> nothing to do
        next
      }
      # the combined dimension is tagged individually -> split the combined
      # dimension and their entries to treat them as multiple primary dimensions
      rprtCombinations <- rprtCombinations %>%
        separate(cd, .split(cd))
    }

    # loop over reporting combinations
    out <- do.call(mbind, apply(rprtCombinations, 1, function(comb) {
      # replace dimension tags to get final variable name
      outName <- name

      for (r in names(comb)) {
        label <- brickSets[[r]][["elements"]][[comb[[r]]]]
        if (label == "") {
          if (isFALSE(silent)) {
            message("No label given to item '", comb, "'. Respective variable is skipped.")
          }
          return(NULL)
        }
        outName <- sub(.tag(r),
                       label,
                       outName, fixed = TRUE)
      }

      # select combination of reporting values
      combData <- .select(x, comb)
      if (length(combData) == 0) {
        if (isFALSE(silent)) {
          message("Missing elements to report. Skip '", outName, "'.")
        }
        return(NULL)
      }

      # aggregate to final variable
      combData %>%
        .agg(agg = map$agg, silent = silent) %>%
        .setNames(outName)
    }, simplify = FALSE))
  }

  return(out)
}





#' Construct dimension mapping (either aggregation or reporting) with explicit
#' set elements for each dimension
#'
#' @param agg named vector of dimensions to aggregate
#' @param rprt named vector of dimensions to report individually
#' @param brickSets named list with definition of common set elements
#' @param silent logical, suppress warnings and printing of dimension mapping
#' @returns nested named list with dimension mapping

.constructDimMapping <- function(agg, rprt, brickSets, silent) {
  lapply(list(agg = agg, rprt = rprt), function(m) {

    mElements <- lapply(names(m), function(d) {
      # check if dim is defined in brickSets
      if (!(d %in% names(brickSets))) {
        stop("The brick sets file ", attr(brickSets, "file"), " has no mapping ",
             "for the dimension '", d, "'.")
      }

      # explicit list of dimension elements
      unlist(lapply(m[[d]], function(val) {
        if (val %in% names(brickSets[[d]][["elements"]])) {
          return(val)
        }
        if (val %in% names(brickSets[[d]][["subsets"]])) {
          return(brickSets[[d]][["subsets"]][[val]])
        } else {
          if (isFALSE(silent)) {
            message("No correspondant for '", val, "' as an element of '", d,
                    "' in brick sets file: ", attr(brickSets, "file"))
          }
          return(list(NULL))
        }
      }))
    })
    names(mElements) <- names(m)
    return(mElements)
  })
}





#' Create tag
#'
#' Escape dimension name in curly brackets
#'
#' @param dim character dimension name
#' @returns character, dimension tag

.tag <- function(dim) {
  paste0("{", dim, "}")
}





#' Aggregate
#'
#' @param x MagPIE object, BRICK object
#' @param agg named vector of dimensions to aggregate.
#' @param silent logical, suppress warnings and printing of dimension mapping
#' @returns aggregated MagPIE objects without sub dimensions in dim 3
#'
#' @importFrom magclass dimSums mselect

.agg <- function(x, agg, silent = TRUE) {

  if (length(x) == 0) {
    return(NULL)
  }

  missingElements <- .missingElements(x, agg)
  if (length(missingElements) > 0) {
    if (isFALSE(silent)) {
      message("Missing elements to aggregate: ",
              paste(missingElements, collapse = ", "))
    }
    return(NULL)
  }

  do.call(.select, c(list(x = x, agg))) %>%
    dimSums(na.rm = TRUE)
}





#' List any missing elements in any given dimension
#'
#' @param x MagPIE object, BRICK object
#' @param dimLst named vector, dimension ans names and elements as values
#' @returns vector of missing dimension elements in x
#'
#' @importFrom magclass getItems

.missingElements <- function(x, dimLst) {
  if (length(x) == 0) {
    stop("'x' has length zero.")
  }

  missingDims <- setdiff(.split(names(dimLst)), getSets(x))
  if (length(missingDims) > 0) {
    stop("The following dimensions are listed in 'dimLst' but missing in 'x': ",
         paste(missingDims, collapse = ", "))
  }
  unlist(lapply(names(dimLst), function(dim) {
    if (!all(.split(dim) %in% getSets(x))) {
      stop("x has no dimension(s) called ", paste(.split(dim), collapse = ", "))
    }
    if (grepl("\\.", dim)) {
      existingElements <- unique(sub(
        pattern = paste(rep("(.*)", magclass::ndim(x, 3)), collapse = "\\."),
        replacement = paste(paste0("\\", match(.split(dim), tail(getSets(x), -2))), collapse = "\\."),
        x = getItems(x, dim = 3)
      ))
      return(setdiff(dimLst[[dim]], existingElements))
    }
    return(setdiff(dimLst[[dim]], getItems(x, dim = dim)))
  }))
}





#' Wrapper around setNames
#'
#' @param object MAgPIE object
#' @param nm a vector of names current names should be replaced with. If only
#'   one data element exists you can also set the name to NULL.
#' @returns NULL if object is NULL else object with manipulated names
#'
#' @importFrom magclass setNames

.setNames <- function(object, nm) {
  if (is.null(object)) {
    return(NULL)
  }
  setNames(object, nm)
}





#' Find all tags in name escaped in curly brackets
#'
#' @param name character, variable name
#' @returns vector of tags in name, NULL if there are none

.findTags <- function(name) {
  tags <- gregexpr("\\{[a-z\\.]+\\}", name)[[1]]
  tags <- if (all(tags == -1)) {
    NULL
  } else {
    unlist(lapply(seq_along(tags), function(i) {
      substr(name,
             tags[i] + 1,
             tags[i] + attr(tags, "match.length")[i] - 2)
    }))
  }
  return(tags)
}





#' select values from MAgPIE-obect
#'
#' This is a wrapper around \code{magclass::mselect} that also allows the
#' selection of combinations of multiple dimensions.
#'
#' @param x MAgPIE object
#' @param ... entry selection. Combined dimensions have to be seperated with
#'   \code{.} for both the set names and the set elements.
#' @returns MAgPIE object containing only selected entries

.select <- function(x, ...) {
  dims <- as.list(...)
  combinedDims <- grep("\\.", names(dims), value = TRUE)
  singleDims <- setdiff(names(dims), combinedDims)

  if (length(singleDims) > 0) {
    x <- mselect(x, dims[singleDims])
  }

  if (length(combinedDims) > 0) {
    # successively select combined dimensions
    x <- Reduce(x = combinedDims, init = x, f = function(data, cd) {
      # wild card selection that should find every element in each dimension
      patternRaw <- rep("[a-zA-Z0-9-]+", ndim(data, 3))
      names(patternRaw) <- tail(getSets(data), -2)

      # build a regex that combines the patterns for each element combination
      # with a OR relationship
      regex <- paste(unlist(lapply(dims[[cd]], function(elem) {
        # take wild card selection and replace elements that have to be matched
        pattern <- patternRaw
        pattern[.split(cd)] <- str_escape(.split(elem))
        # subdimensions of MAgPIE objects are separated with .
        paste(pattern, collapse = "\\.")
      })), collapse = "|")

      # select matching items
      items <- grep(regex, getItems(x, 3), value = TRUE)
      data[, , items]
    })
  }

  return(x)
}
