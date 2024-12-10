#' Create a Named List from Objects in Environment
#'
#' Creates a list where elements are automatically named using their input variable names.
#' Uses non-standard evaluation to capture the variable names as they appear in the function call.
#'
#' @param ... Unquoted variable names that will become both the list elements and their names
#' @return A named list where each element's name matches its input variable name
#' @examples
#' x <- 1
#' y <- "hello"
#' z <- data.frame(a = 1:3)
#' 
#' # Creates list with names "x", "y", "z"
#' my_list <- named_list(x, y, z)
#'
#' @importFrom rlang enquos quo_name
#' @export
named_list <- function(...) {
  UseMethod("named_list")
}

#' @export
#' @method named_list default
named_list.default <- function(...) {
  vars <- rlang::enquos(...)
  result <- list(...)
  names(result) <- vapply(vars, rlang::quo_name, character(1))
  return(result)
}

#' @export
#' @method named_list data.frame
named_list.data.frame <- function(...) {
  vars <- rlang::enquos(...)
  result <- list(...)
  names(result) <- vapply(vars, rlang::quo_name, character(1))
  return(result)
}