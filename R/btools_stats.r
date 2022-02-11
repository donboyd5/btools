
# simple differences ----
#' Compute year-over-year percent change for annual data
#' 
#' @export pchya
#' 
#' @param value numeric vector
#' @param year integer or numeric vector
#' @return numeric vector
#' @examples
#' set.seed(1234)
#' year <- 1970:1990
#' value <- 100 * (1 + .05 + rnorm(length(year), 0, .002))
#' cbind(year, value, pchya(value, year))
pchya <- function(value, year){
  value / value[match(year - 1, year)] -1
}

#' Compute year-over-year difference for annual data
#' 
#' @export diffya
#' 
#' @param value numeric vector
#' @param year integer or numeric vector
#' @return numeric vector
#' @examples
#' set.seed(1234)
#' year <- 1970:1990
#' value <- 100 * (1 + .05 + rnorm(length(year), 0, .002))
#' cbind(year, value, diffya(value, year))
diffya <- function(value, year){
  value - value[match(year - 1, year)]
}


# quantiles ----
#' Compute the sample 25th percentile
#' 
#' @export p25
#' 
#' @param x a numeric vector containing the values whose 25th percentile is to be computed.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return numeric
#' @examples
#' p25(1:100)
#' p25(c(1:10, NA, 11:100), na.rm=TRUE)
p25 <- function(x, na.rm = FALSE) {
  as.numeric(stats::quantile(x, 0.25, na.rm = na.rm))
}


#' Compute the sample 50th percentile (median).
#' 
#' @export p50
#' 
#' @param x a numeric vector containing the values whose 50th percentile is to be computed.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return numeric
#' @examples
#' p50(1:100)
#' p50(c(1:10, NA, 11:100), na.rm=TRUE)
p50 <- function(x, na.rm = FALSE) {
  as.numeric(stats::quantile(x, 0.5, na.rm = na.rm))
}


#' Compute the sample 75th percentile.
#' 
#' @export p75
#' @param x a numeric vector containing the values whose 75th percentile is to be computed.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return numeric
#' @examples
#' p75(1:100)
#' p75(c(1:10, NA, 11:100), na.rm=TRUE)
p75 <- function(x, na.rm = FALSE) {
  as.numeric(stats::quantile(x, 0.75, na.rm = na.rm))
}


#' Compute the sample value for a specific percentile, p.
#' @export pany
#' 
#' @param x a numeric vector containing the values whose p-th percentile is to be computed.
#' @param p the percentile.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return numeric
#' @examples
#' pany(1:100, .33)
#' pany(c(1:10, NA, 11:100), .33, na.rm=TRUE)
pany <- function(x, p, na.rm = FALSE) {
  as.numeric(stats::quantile(x, p, na.rm = na.rm))
}


# window functions -- moving averages, sums, etc. ----
# Rolling mean and sum functions ---- rollmean versions are fast but cannot handle NA input values rollapply version is slower but handles NAs, so use
# it

# look at slide package as alternative


#' Get trailing moving average
#'
#' \code{ma} Get trailing moving average
#' 
#' @export ma
#' @usage ma(x, n)
#' @param x The vector to operate on.
#' @param n The period of the moving average.
#' @details Moving average of the vector x.
#' @keywords ma
#' @examples
#' ma(7:21, 3)
ma <- function(x, n) {
  zoo::rollapply(x, n, function(x) mean(x, na.rm = TRUE), fill = NA, align = "right")
}


#' Get 4-period moving average (3 lags + current)
#'
#' \code{ma4} get 4-period moving average
#' 
#' @export ma4
#' @usage ma4(x)
#' @param x The vector to operate on.
#' @details 4-period moving average
#' @keywords ma4
#' @examples
#' ma4(7:21)
ma4 <- function(x) {
  # note that this requires zoo, which is on the Depends line in the Description file
  zoo::rollapply(x, 4, function(x) mean(x, na.rm = TRUE), fill = NA, align = "right")
}


#' Get 4-period moving sum (3 lags + current)
#'
#' \code{sum4} get 4-period moving sum
#' 
#' @export
#' @usage sum4(x)
#' @param x The vector to operate on.
#' @details 4-period moving sum
#' @keywords sum4
#' @examples
#' sum4(7:21)
sum4 <- function(x) {
  ma4(x) * 4
}


#' Rolling mean
#'
#' \code{rollmean} rolling sample standard deviation
#' 
#' @export
#' @usage rollmean(x, nobs)
#' @param x The vector to operate on
#' @param nobs Number of observations to include in window, including current
#' @details rolling mean
#' @keywords rollmean
#' @examples
#' rollmean(1:100, 4)
rollmean <- function(x, nobs) {
  zoo::rollapply(x, nobs, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")
}


#' Rolling minimum
#'
#' \code{rollmin} rolling minimum
#' 
#' @export
#' @usage rollmin(x, nobs)
#' @param x The vector to operate on
#' @param nobs Number of observations to include in window, including current
#' @details rolling minimum
#' @keywords rollmin
#' @examples
#' rollmean(1:100, 4)
rollmin <- function(x, nobs) {
  zoo::rollapply(x, nobs, function(x) min(x, na.rm=TRUE), fill=NA, align="right")
}


#' Rolling sample standard deviation
#'
#' \code{rollsd} rolling sample standard deviation
#' 
#' @export
#' @usage rollsd(x, nobs)
#' @param x The vector to operate on
#' @param nobs Number of observations to include in window, including current
#' @details rolling sample standard deviation
#' @examples
#' rollsd(1:100, 4)
rollsd <- function(x, nobs) {
  # note that this is sample standard deviation
  zoo::rollapply(x, nobs, function(x) stats::sd(x, na.rm=TRUE), fill=NA, align="right")
}


#' Rolling populatoin standard deviation
#'
#' \code{rollsd_p} rolling population standard deviation
#' 
#' @export
#' @usage rollsd_p(x, nobs)
#' @param x The vector to operate on
#' @param nobs Number of observations to include in window, including current
#' @details rolling population standard deviation
#' @keywords rollsd_p
#' @examples
#' rollsd_p(1:100, 4)
rollsd_p <- function(x, nobs) {
  # population standard deviation
  sdp <- function(x){
    n <- sum(!is.na(x))
    stats::sd(x, na.rm=TRUE) * sqrt((n - 1) / n)
  }
  zoo::rollapply(x, nobs, function(x) sdp(x),
                 fill=NA, align="right")
}




# stats across columns of a data frame or set of vectors ----

#' Vectorized mean, similiar to \code{pmin} and \code{pmax}
#'
#' @export
#'
#' @param ... numeric vectors to average
#' @param na.rm a logical indicating whether missing values should be removed
#'
#' @return a vector with mean of \code{...} arguments
pmean <- function(..., na.rm = FALSE) {
  d <- do.call(cbind, list(...))
  res <- rowMeans(d, na.rm = na.rm)
  idx_na <- !rowMeans(!is.na(d))
  res[idx_na] <- NA
  return(res)
}


#' Vectorized sum, similiar to \code{pmin} and \code{pmax}
#' 
#' @export
#'
#' @param ... numeric vectors to sum
#' @param na.rm a logical indicating whether missing values should be removed
#'
#' @return a vector with sum of \code{...} arguments
psum <- function(..., na.rm = FALSE) {
  d <- do.call(cbind, list(...))
  res <- rowSums(d, na.rm = na.rm)
  idx_na <- !rowSums(!is.na(d))
  res[idx_na] <- NA
  return(res)
}


# model-related functions ----
# se <- function(model) {sqrt(diag(vcov(model)))} # we don't need this now

