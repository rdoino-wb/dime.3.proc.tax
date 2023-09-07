#' Clean and Transform String
#'
#' This function cleans and transforms a given string by:
#' 1. Removing unnecessary whitespaces.
#' 2. Removing non-ASCII characters.
#' 3. Converting to lowercase.
#' 4. Removing all non-alphanumeric characters.
#'
#' @param x Character vector to be cleaned and transformed.
#'
#' @return Cleaned and transformed character vector.
#' @export
#'
#' @importFrom stringr str_squish str_to_lower str_remove_all
#' @importFrom stringi stri_trans_nfd
#'
#' @examples
#' \dontrun{
#' clean_transform_string("  Hello Wörld!  ")
#' }
clean_transform_string <- function(x) {
  x <- stringr::str_squish(x)  # Remove unnecessary whitespaces
  x <- stringi::stri_trans_nfd(x)  # Decompose non-ASCII characters
  x <- stringr::str_replace_all(x, "[^\\x01-\\x7F]", "")  # Remove non-ASCII characters
  x <- stringr::str_to_lower(x)  # Convert to lowercase
  x <- stringr::str_remove_all(x, "[^[:alnum:]]")  # Remove all non-alphanumeric characters
  return(x)
}
