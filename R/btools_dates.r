# btools_dates.r Don Boyd 3/27/2020

# Now that I use lubridate, very few date functions are necessary

#' First day of quarter
#'
#' @description \code{fdoq} create a date for start of quarter, from quarter, year
#' @usage fdoq(date)
#' @param date a vector of dates. No default.
#' @details You must ensure that the inputs are acceptable (e.g., the input data are dates). The function does not check.
#' @keywords fdoq
#' @export
#' @examples fdoq(as.Date('2010-01-03'))
#' x <- as.Date(c('2010-01-03', '2011-03-03', '2011-04-07'))
#' fdoq(x)
fdoq <- function(date) {
  as.Date(lubridate::floor_date(date, "quarter"))
}


#' Last day of month
#'
#' @description \code{ldom} get the last day of month, for a given date
#' @usage ldom(date)
#' @param date a vector of dates. No default.
#' @details You must ensure that the inputs are acceptable (e.g., the input data are dates). The function does not check.
#' @keywords ldom
#' @export
#' @examples ldom(as.Date("2010-01-03"))
#' x <- as.Date(c("2010-01-01", "2010-01-31","2019-02-01", "2020-02-01"))
#' ldom(x)
ldom <- function(date) {
  # get the first day of the NEXT month and subract 1 day
}


#' Create a date from month, day, year
#'
#' @description \code{mdy_fn} create a date from month, day, year
#' @usage mdy_fn(m, d, y)
#' @param m the month input. No default.
#' @param d the day input. No default.
#' @param y the year input. No default.
#' @details You must ensure that the inputs are acceptable (e.g., month is in 1:12). The function does not check.
#' @keywords mdy_fn
#' @export
#' @examples mdy_fn(10, 1, 1988)
mdy_fn <- function(m, d, y) {
    as.Date(ISOdate(y, m, d))
}


#' Create a date for start of quarter, from quarter, year
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
    btools::mdy_fn(q * 3 - 2, 1, y)
}
