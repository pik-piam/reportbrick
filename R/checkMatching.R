#' Check the matching results for inconsistencies
#'
#' @param path character, path to the matching result files
#' @param tol numeric, absolute tolerance in million m2 against which to check the stock balance equations
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% .data across all_of group_by filter inner_join left_join mutate
#'   select summarise
#' @importFrom madrat toolGetMapping
#' @importFrom tidyr crossing
#' @importFrom utils read.csv write.csv
#'
#' @export
#'
checkMatching <- function(path, tol = 1E-2) {



  # Read in matching results ---------------------------------------------------


  ## Read from target files ====

  stockCalibTarget <- read.csv(
    file.path(path, "f_stockCalibTarget.cs4r"),
    col.names = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", "ttot", "stock"),
    header = FALSE, comment.char = "*"
  )
  conCalibTarget <- read.csv(
    file.path(path, "f_constructionCalibTarget.cs4r"),
    col.names = c("qty", "bs", "hs", "reg", "loc", "typ", "inc", "ttot", "con"),
    header = FALSE, comment.char = "*"
  )
  renCalibTarget <- read.csv(
    file.path(path, "f_renovationCalibTarget.cs4r"),
    col.names = c("qty", "bs", "hs", "bsr", "hsr", "vin", "reg", "loc", "typ", "inc", "ttot", "ren"),
    header = FALSE, comment.char = "*"
  )
  demCalibTarget <- read.csv(
    file.path(path, "f_demolitionCalibTarget.cs4r"),
    col.names = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", "ttot", "dem"),
    header = FALSE, comment.char = "*"
  )


  ## Compute length of time steps ====

  dt <- data.frame(ttot = unique(stockCalibTarget$ttot)) %>%
    mutate(dt = c(NA, diff(.data$ttot))) %>%
    filter(!is.na(.data$dt))
  dtNext <- mutate(dt, ttot = .data$ttot - .data$dt)
  vintages <- toolGetMapping(name = system.file("extdata", "sectoral", "vintage.csv", package = "brick"),
                             where = "local",
                             error.missing = TRUE,
                             returnPathOnly = FALSE)
  p_dtVin <- vintages %>%
    crossing(ttot = unique(stockCalibTarget$ttot)) %>%
    left_join(dt, by = "ttot") %>%
    mutate(dtCon = pmax(
      0,
      pmin(.data$to, .data$ttot)
      - pmax(.data$from - 1, .data$ttot - .data$dt)
    )) %>%
    filter(!is.na(.data$dtCon)) %>%
    select("ttot", "vin", "dtCon")



  # Compute values of stock balance --------------------------------------------


  ## Stock balance for previous time period ====

  renPrev <- renCalibTarget %>%
    group_by(across(all_of(c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", "ttot")))) %>%
    summarise(renSum = sum(.data$ren), .groups = "drop")

  stockBalPrev <- stockCalibTarget %>%
    left_join(dtNext, by = "ttot") %>%
    mutate(ttot = .data$ttot + .data$dt) %>%
    filter(!is.na(.data$ttot)) %>%
    select(-"dt") %>%
    left_join(conCalibTarget, by = c("qty", "bs", "hs", "reg", "loc", "typ", "inc", "ttot")) %>%
    left_join(renPrev, by = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", "ttot")) %>%
    left_join(dt, by = "ttot") %>%
    left_join(p_dtVin, by = c("vin", "ttot")) %>%
    mutate(lhs = .data$stock + .data$con * .data$dtCon,
           rhs = .data$renSum * .data$dt,
           eqn = .data$lhs - .data$rhs)


  ## Stock balance for next time period ====

  renNext <- renCalibTarget %>%
    mutate(bsr = ifelse(.data$bsr == "0", .data$bs, .data$bsr),
           hsr = ifelse(.data$hsr == "0", .data$hs, .data$hsr)) %>%
    group_by(across(all_of(c("qty", "bsr", "hsr", "vin", "reg", "loc", "typ", "inc", "ttot")))) %>%
    summarise(renSum = sum(.data$ren), .groups = "drop")

  stockBalNext <- stockCalibTarget %>%
    left_join(demCalibTarget, by = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", "ttot")) %>%
    left_join(renNext, by = c("qty", bs = "bsr", hs = "hsr", "vin", "reg", "loc", "typ", "inc", "ttot")) %>%
    inner_join(dt, by = "ttot") %>%
    mutate(lhs = .data$stock + .data$dem * .data$dt,
           rhs = .data$renSum * .data$dt,
           eqn = .data$lhs - .data$rhs)



  # Check for deviation and write results to file ------------------------------

  .checkForDeviation(stockBalPrev, "Previous", tol = tol)
  .checkForDeviation(stockBalNext, "Next", tol = tol)

  write.csv(stockBalPrev, file = file.path(path, "stockBalPrev.csv"), row.names = FALSE, quote = FALSE)
  write.csv(stockBalNext, file = file.path(path, "stockBalNext.csv"), row.names = FALSE, quote = FALSE)

}

#' Check if the tolerance is violated anywhere
#'
#' @param stockBal data frame with evaluation of a stock balance equation
#' @param name character, name/identifier of the stock balance equation
#' @param tol numeric, tolerance to check the equation against
#'
.checkForDeviation <- function(stockBal, name, tol = 1E-2) {
  if (any(abs(stockBal$eqn[!is.na(stockBal$eqn)]) > tol)) {
    message("The stock balance equation ", name, " deviates by more than ", tol, ".\n",
            "For details, please check the file 'stockBal", name, ".csv'.")
  } else {
    message("The stock balance equation ", name, " is satisfied with a tolerance of ", tol, ".")
  }
}
