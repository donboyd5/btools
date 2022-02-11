#' get state names given postal abbreviations
#' 
#' @export stname
#' 
#' @param stabbr character vector of state postal abbreviations
#' @return character vector of state names
#' @examples
#' stname(c("AK", "DC", "US"))
stname <- function(stabbr) {
  stabbrs <- c(datasets::state.abb, "DC", "US")
  stnames <- c(datasets::state.name, "District of Columbia", "United States")
  stnames[match(stabbr, stabbrs)]
}


#' get postal abbreviations given state names
#' 
#' @export stabbr
#' 
#' @param stname character vector of state names
#' @return character vector of state postal abbreviations
#' @examples
#' stabbr(c("Alaska", "District of Columbia", "United States"))
stabbr <- function(stname) {
  stabbrs <- c(datasets::state.abb, "DC", "US")
  stnames <- c(datasets::state.name, "District of Columbia", "United States") %>% 
    stringr::str_to_upper()
  stabbrs[match(stringr::str_to_upper(stname), stnames)]
}

