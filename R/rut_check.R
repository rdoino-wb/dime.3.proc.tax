#' Compute the Check Digit for a Chilean RUT
#'
#' This function calculates the check digit for a given RUT (Rol Único Tributario),
#' which is a Chilean tax identification number.
#'
#' @param rut The RUT number as a numeric or character string. Dots and characters after '-' in the string will be removed.
#'
#' @return Returns the calculated check digit as a character string.
#'
#' @examples
#' rut_number <- "76086428"
#' rut_check(rut_number)
#'
#' rut_number_with_dots <- "76.086.428"
#' rut_check(rut_number_with_dots)
#'
#' rut_with_hyphen <- "76.086.428-K"
#' rut_check(rut_with_hyphen)
#'
#' @export
#'
#'
rut_check <- function(rut) {
  # Make sure the input is a numeric or character type
  if (!is.numeric(rut) && !is.character(rut)) {
    stop("The RUT should be numeric or character.")
  }

  # Convert the RUT to a string if it's numeric
  if (is.numeric(rut)) {
    rut <- as.character(rut)
  }

  # Remove characters after '-'
  rut <- sub("-.*$", "", rut)

  # Remove any dots
  rut <- gsub("\\.", "", rut)

  if (nchar(rut) != 8) {
    return(rut)
  }

  # Reverse the RUT string for calculation
  rut_reversed <- rev(unlist(strsplit(rut, "")))

  # Initialize sum and multiplier
  sum = 0
  multiplier = 2

  # Calculate the sum for check digit
  for (digit in rut_reversed) {
    sum <- sum + as.numeric(digit) * multiplier
    multiplier <- multiplier + 1
    if (multiplier == 8) {
      multiplier = 2
    }
  }

  # Calculate check digit
  check_digit_value <- 11 - (sum %% 11)

  # Transform the check digit value to appropriate format
  if (check_digit_value == 11) {
    check_digit <- "0"
  } else if (check_digit_value == 10) {
    check_digit <- "K"
  } else {
    check_digit <- as.character(check_digit_value)
  }

  # we paste together the id rut
  id_rut_firm <- paste0(rut, "-", check_digit)

  return(id_rut_firm)

}
