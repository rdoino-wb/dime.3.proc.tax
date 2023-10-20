#' Format Numbers with Big Marks
#'
#' This function takes a numeric value and formats it using big marks (commas)
#' for better readability. It also allows for specifying the number of decimal places.
#'
#' @param x Numeric value to be formatted.
#' @param digits Number of digits to round to. Default is 3.
#'
#' @return Formatted string.
#'
#' @examples
#' format_bigmark(1234567.89)
#' format_bigmark(1234567.89, digits = 2)
#'
#' @export
format_bigmark <- function(x, digits = 3) {
  format(round(x, digits), big.mark = ",", scientific = FALSE)
}
