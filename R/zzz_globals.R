#' @keywords internal
#' @noRd
utils::globalVariables(c(
  # dplyr/data.table column names seen bare in your code:
  "network", "site_code", "variable", "value", "time", "barcode",
  # add other frequent offenders here as they appear in notes
  "."
))
