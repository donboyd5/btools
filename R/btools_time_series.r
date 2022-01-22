# Rolling mean and sum functions ---- rollmean versions are fast but cannot handle NA input values rollapply version is slower but handles NAs, so use
# it

#' Get seasonally adjusted values for a vector
#' 
#' @export sa
#'  
# Get seasonally adjusted values for a vector.
#' 
#' @usage sa(value, freq, s.window=9)
#' @param value The vector to operate on.
#' @param freq Frequency of the time series (e.g., 1, 4, 12)
#' @param s.window Window parameter that is passed to stats::stl default=9
#' @details Seasonally adjusted version of value
#' @keywords sa
#' @examples
#' set.seed(1234)
#' n <- 28
#' x <- 1:n + rnorm(n)
#' x <- x * c(1.1, 1, .9, 1)
#' cbind(x, sa(x, 4))
sa <- function(value, freq, s.window = 9) {
    vts <- stats::ts(value, start = 1, frequency = freq)
    # s.window='periodic' not as smooth as a numeric s.window numeric 9 seems pretty good
    vts <- forecast::seasadj(stats::stl(vts, s.window = s.window))
    as.numeric(vts)
}
