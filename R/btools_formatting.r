# btools_formatting.r 
# Don Boyd 12/22/2022

# library(devtools)
# library(btools)

# NOTE: put @export as first tag, NOT earlier.

# usethis::use_pipe(export = TRUE)
# usethis::use_package("scales")

# wrappers for scale functions -------------------------------------------------------

#' Format numeric vector as percent
#'
#' \code{f_pct} Percent format - calls scales::label_percent
#' 
#' @export f_pct
#' @usage f_pct(num, ...)
#' @param num numeric vector to format
#' @param ... arguments passed to scales::label_percent
#' @details format the vector using scales::label_percent
#' @return formatted values as a character vector.
#' @keywords f_pct
#' @examples
#' f_pct(1.2345)
#' f_pct(1.2345, scale=.1)
#' f_pct(1.2345, scale=.1, accuracy=0.01)
f_pct <- function(num, ...) {
  scales::label_percent(...)(num)
  }

#' Format numeric vector in comma format
#'
#' \code{f_comma} comma format - calls scales::label_comma
#' 
#' @export f_comma
#' @usage f_comma(num, ...)
#' @param num numeric vector to format
#' @param ... arguments passed to scales::label_comma
#' @details format the vector using scales::label_comma
#' @return formatted values as a character vector.
#' @keywords f_comma
#' @examples
#' f_comma(1.2345)
#' f_comma(1.2345, scale=1000)
#' f_comma(1.2345, scale=1000, accuracy=0.01)
f_comma <- function(num, ...) {
  scales::label_comma(...)(num)
}


#' Format numeric vector in dollar format
#'
#' \code{f_dollar} dollar format - calls scales::label_dollar
#' 
#' @export f_dollar
#' @usage f_dollar(num, ...)
#' @param num numeric vector to format
#' @param ... arguments passed to scales::label_dollar
#' @details format the vector using scales::label_dollar
#' @return formatted values as a character vector.
#' @keywords f_dollar
#' @examples
#' f_dollar(1.2345)
#' f_dollar(1.2345, scale=1000)
#' f_dollar(1.2345, scale=1000, accuracy=0.01)
f_dollar <- function(num, ...) {
  scales::label_dollar(...)(num)
}



