# btools_dates.r 
# Don Boyd 2/11/2022

# Now that I use lubridate, very few date functions are necessary

#' First day of quarter, given a date as string or date
#' 
#' @export fdoq
#'
#' @description \code{fdoq} create a date for start of quarter, from quarter, year
#' @usage fdoq(date)
#' @param date a vector of dates as strings or dates. No default.
#' @examples 
#' x <- c('2010-01-03', '2011-03-03', '2011-04-07')
#' fdoq(x)
fdoq <- function(date) {
  date <- as.Date(date)
  as.Date(lubridate::floor_date(date, "quarter"))
}


#' Last day of month
#' 
#' @export ldom
#'
#' @description \code{ldom} get the last day of month, for a given date
#' @usage ldom(date)
#' @param date a vector of dates as strings or dates. No default.
#' @examples
#' x <- c('2010-01-01', '2010-01-31','2019-02-01', '2020-02-01')
#' ldom(x)
ldom <- function(date) {
  # get the first day of the NEXT month and subract 1 day
  date <- as.Date(date)
  lubridate::ceiling_date(date, "month") - 1
}


#' First day of quarter, from year, quarter
#' 
#' @export yq2
#'
#' @description \code{yq2} first day quarter, with separate year, quarter inputs
#' @usage yq2(y, q)
#' @param y the year input. No default.
#' @param q the quarter input. No default.
#' @examples
#' yq2(2020, 1)
#' yq2("2020", "1")
yq2 <- function(y, q) {
  # lubridate
  s <- paste0(y, ".", q)
  lubridate::yq(s)
  # mdy_fn(q * 3 - 2, 1, y)  # no longer needed
}
