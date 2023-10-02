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

  # Initialize an empty vector to store the results
  id_rut_firm <- character(length = length(rut))

  # Loop through each element of the input vector
  for (i in seq_along(rut)) {

    elem <- rut[i]

    # Check the type of the element
    if (!is.numeric(elem) && !is.character(elem)) {
      id_rut_firm[i] <- NA
      next
    }

    # Convert to character if numeric
    if (is.numeric(elem)) {
      elem <- as.character(elem)
    }

    # Remove characters after "-" and dots
    elem <- sub("-.*$", "", elem)
    elem <- gsub("\\.", "", elem)

    # If element has any alphabetic character or isn't 8 characters long, skip it
    if (grepl("[a-zA-Z]", elem)) {
      id_rut_firm[i] <- NA
      next
    }

    # Calculate the check digit
    elem_reversed <- rev(unlist(strsplit(elem, "")))
    sum = 0
    multiplier = 2
    for (digit in elem_reversed) {
      sum <- sum + as.numeric(digit) * multiplier
      multiplier <- multiplier + 1
      if (multiplier == 8) {
        multiplier = 2
      }
    }

    check_digit_value <- 11 - (sum %% 11)

    # Determine the check digit
    if (check_digit_value == 11) {
      check_digit <- "0"
    } else if (check_digit_value == 10) {
      check_digit <- "K"
    } else {
      check_digit <- as.character(check_digit_value)
    }

    # Construct the full RUT with the check digit
    id_rut_firm[i] <- paste0(elem, "-", check_digit)

  }

  return(id_rut_firm)
}
