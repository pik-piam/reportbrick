#' Prototype / place holder for price sensitivity plot
#'
#' Early trials on plotting logarithmic quantity shares vs. relative lcc
#'
#' @author Ricarda Rosemann

generateQuantShares <- function() {

  # Compute relative shares and logarithmic quantity shares
  # renLccGabo <- renLcc %>%
  #   ungroup() %>%
  #   filter(.data[["hs"]] == "gabo") %>%
  #   rename(lccGabo = "lcc") %>%
  #   select(-"hs", -"lccOpe", -"lccRen", -"discount")
  #
  # v_renovationGabo <- v_renovationIn %>%
  #   filter(.data[["hs"]] == "gabo") %>%
  #   rename(valGabo = "value") %>%
  #   select(-"hs", -"dt")
  #
  # renLccDiff <- renLcc %>%
  #   left_join(renLccGabo, by = c("bs", "vin", "reg", "loc", "typ", "inc", "ttot")) %>%
  #   mutate(value = .data[["lcc"]] - .data[["lccGabo"]]) %>%
  #   select(-"lccOpe", -"lccRen", -"discount")
  #
  # renQuantShare <- v_renovationIn %>%
  #   left_join(v_renovationGabo, by = c("qty", "bs", "vin", "reg", "loc", "typ", "inc", "ttot")) %>%
  #   mutate(value = log(.data[["value"]] / .data[["valGabo"]])) %>%
  #   select(-"dt", -"valGabo")
  #
  # renLogitLcc <- renLcc %>%
  #   mutate(value = exp(-priceSensHs * (.data[["lcc"]] - .data[["lcc"]][["gabo"]]))) %>%
  #   select(-"lccOpe", -"lccRen")

}
