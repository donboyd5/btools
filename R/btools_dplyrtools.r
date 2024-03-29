# btools_dplyrtools.r
# Don Boyd 4/4/2022

# tools that generally are helpful with dplyr


#' Convert names of a data frame to lower case
#'
#' Convert names of a data frame to lower case, return as data frame. This works
#' with the pipe operator introduced in R 4.1.
#'
#' @export lcnames
#' @usage lcnames(df)
#' @param df Data frame.
#' @return Data frame with lower-case names
#' @keywords lcnames
#' @examples
#' library(dplyr)
#' df <- tibble(YEAR=2000:2010, X=10:20, y=30:40)
#' df
#' df |>
#'   lcnames()
lcnames <- function(df) {
  vnames <- stringr::str_to_lower(names(df))
  stats::setNames(df, vnames)
}


#' Get quantiles and number of not-NA observations for a vector, return as tibble
#' @export qtiledf
#'
#' @description \code{qtiledf} get quantiles and number of not-NA observations for a vector, return as data frame
#' @usage qtiledf(vec, probs)
#' @param vec Numeric vector. No default.
#' @param probs Numeric vector of quantiles. Default is c(0, .1, .25, .5, .75, .9, 1).
#' @details Very little error checking.
#' Useful after dplyr's group_by, in do command, which requires data frame input.
#' @return Data frame with columns as quantiles
#' @keywords qtiledf
#' @examples
#' library(dplyr)
#' df <- tibble(year=c(rep(1, 5), rep(2, 7)), x=c(seq(1, 2, length.out=11), NA))
#' df
#' df %>%
#'   group_by(year) %>%
#'   summarise(qtiledf(.$x, c(.1, .25, .5, .75, .9)))
qtiledf <- function(vec,
                    probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)) {
    # cbind(n = length(vec), n.notNA = sum(!is.na(vec)), as.data.frame(t(stats::quantile(vec, na.rm = TRUE, probs))))
  cbind(n = length(vec),
        n.notNA = sum(!is.na(vec)),
        tibble::as_tibble(t(stats::quantile(vec, na.rm = TRUE, probs))))
}


#' Get trend, seasonal, remainder for a vector that has time-series data
#' @export stldf
#'
#' @description \code{stldf} get trend, seasonal, remainder for a vector that has time-series data
#' @usage stldf(vec, freq)
#' @param vec Numeric vector with time-series data. No default.
#' @param freq Frequency of the data. Numeric. Should be 4 (quarterly) or 12 (monthly). No default.
#' @details Returns a data frame with 3 columns: trend, seasonal, remainder. Very little error checking.
#' Useful after dplyr's group_by, in do command, which requires data frame input. Make sure data are sorted by time before using.
#' @return Data frame with 3 columns: trend, seasonal, remainder
#' @keywords stldf
#' @examples
#' library(bdata) # so that spop.q is available
#' library(dplyr)
#' spop.q %>%
#'     group_by(stabbr) %>%
#'     dplyr::arrange(date) %>% # BE SURE DATA HAVE BEEN SORTED BY DATE WITHIN GROUPING VARS!!!
#'     do(cbind(., stldf(.$value, 4)))
stldf <- function(vec, freq) {
    # decompose time series; assume 'date' var exists; has minor error handling arguments: numeric vector (vec) and its frequency (freq) return: data
    # frame (tsr) with trend, seasonal, and remainder columns
    
    # a typical call would be: do(stldf(.$value, 12) but this will only return trend, seasonal, remainder
    
    # if other variables on the data frame are desired, incorporate them into the call via cbind, such as: do(cbind(., stldf(.$value, 12))), or
    # do(cbind(.[, group_vars(.)], stldf(.$value, 12)))
    
    lvec <- length(vec)
    badout <- function(lvec) data.frame(trend = rep(NA, lvec), seasonal = rep(NA, lvec), remainder = rep(NA, lvec))
    
    if (lvec < 2 * freq) 
        return(badout(lvec))
    if (sum(is.na(vec) > 0)) 
        return(badout(lvec))  # djb new fix!!! 4/2/2018
    
    varts <- stats::ts(vec, start = 1, frequency = freq)
    decomp <- stats::stl(varts, s.window = freq + 1, na.action = zoo::na.approx)  # na.approx replaces missing values with interpolated values  
    tsr <- data.frame(trend = as.vector(decomp$time.series[, "trend"]), seasonal = as.vector(decomp$time.series[, "seasonal"]), remainder = as.vector(decomp$time.series[, 
        "remainder"]))
    if (nrow(tsr) != lvec) 
        return(badout(lvec))
    
    return(tsr)
}
