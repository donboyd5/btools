# btools_dates.r
# Don Boyd
# 4/21/2015

# Now that I use lubridate, very few date functions are necessary

#' @title First day of quarter
#'
#' @description \code{fdoq} create a date for start of quarter, from quarter, year
#' @usage fdoq(date)
#' @param date a vector of dates. No default.
#' @details You must ensure that the inputs are acceptable (e.g., the input data are dates). The function does not check.
#' @keywords fdoq
#' @export
#' @examples fdoq(as.Date("2010-01-03"))
#' x <- as.Date(c("2010-01-03", "2011-03-03", "2011-04-07"))
#' fdoq(x)
fdoq <- function(date) as.Date(lubridate::floor_date(date, "quarter"))


#' @title Create a date from month, day, year
#'
#' @description \code{mdyfn} create a date from month, day, year
#' @usage mdyfn(m, d, y)
#' @param m the month input. No default.
#' @param d the day input. No default.
#' @param y the year input. No default.
#' @details You must ensure that the inputs are acceptable (e.g., month is in 1:12). The function does not check.
#' @keywords mdyfn
#' @export
#' @examples mdyfn(10, 1, 1988)
mdyfn <- function(m, d, y) {
  as.Date(ISOdate(y, m, d))
  }


#' @title Create a date for start of quarter, from quarter, year
#'
#' @description \code{qy} create a date for start of quarter, from quarter, year
#' @usage qy(q, y)
#' @param q the quarter input. No default.
#' @param y the year input. No default.
#' @details You must ensure that the inputs are acceptable (e.g., quarter is in 1:4). The function does not check.
#' @keywords qy
#' @export
#' @examples qy(2, 1985)
qy <- function(q, y) {
  mdyfn(q * 3-2, 1, y)
  }
