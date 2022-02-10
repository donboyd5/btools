# Statistical functions ----
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


# Rolling mean and sum functions ---- rollmean versions are fast but cannot handle NA input values rollapply version is slower but handles NAs, so use
# it

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

