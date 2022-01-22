# btools_utilities.r 
# Don Boyd 1/22/2022

# library(devtools)
# library(btools)

# NOTE: put @export as first tag, NOT earlier.

# String manipulation functions ----

#' Detect whether any vector elements are in a string
#' 
#' @export str_detect_any
#' @param s A string.
#' @param elements Vector elements to look for.
#' @return Logical indicating whether any of \code{elements} are found in \code{s}.
#' @examples
#' str_detect_any("abc defg ijk", c("123", "def", "11"))
#' str_detect_any("abc defg ijk", c("123", "xyz", "11"))
str_detect_any <- function(s, elements){
  # check whether each item in the string vector s
  # has at least one item in the string vector elements
  
  # get a list: one "row" per item in s
  #   each row is a logical vector with same length as elements
  logical_list <- purrr::map(s, stringr::str_detect, elements)
  
  # are any of the items in each "row" of the list true?
  purrr::map_lgl(logical_list, any)
  
  # test with the following code:
  # s <- c("str one", "str two", "str 3", "str 4", "my 8")
  # elements <- c("one", "3", "str", "7")
  # 
  # str_detect_any(s, elements)
}

#' Return portion of a string before first occurrence of a pattern
#' 
#' @export str_extract_before_first
#' @param s A string.
#' @param first Pattern to look for.
#' @return Substring of \code{s} before first occurrence of \code{first}.
#' @examples
#' str_extract_before_first("abcde#yzy", "#")
#' str_extract_before_first("abcde#yzy", "y")
str_extract_before_first <- function(s, first){
  # stringr::str_extract("abc!def", '^[^!]+')  # everything before first !
  pattern <- paste0("^[^", first, "]+")
  stringr::str_extract(s, pattern)
}

#' Return portion of a string after last occurrence of a pattern
#'
#' @export str_extract_after_last
#' @param s A string.
#' @param last Pattern to look for.
#' @return Substring of \code{s} after last occurrence of \code{last}.
#' @examples
#' str_extract_after_last("ab#cde#yzy", "#")
#' str_extract_after_last("abcde#yzy", "c")
str_extract_after_last <- function(s, last){
  # stringr::str_extract("abc!def", '[^!]+$')  # everything after last !
  pattern <- paste0("[^", last, "]+$")
  stringr::str_extract(s, pattern)
}


#..regex notes ----
# str_extract(ulabel, '![^!]+$'),  # everything after last !
# str_extract(ulabel, '^[^!]+'),  # everything before first !
#  "^[^,]+"  # everything before first ,
# x <- c("abc, def", "hijklm ,zyz")  str_extract(x, "^[^,]+")


# numeric functions -------------------------------------------------------

#' Convert NA to zero
#'
#' \code{naz} converts NA to zero
#' 
#' @export naz
#' @usage naz(vec)
#' @param vec The vector to convert.
#' @details Converts all NAs in a vector to zero.
#' @return The revised vector.
#' @keywords naz
#' @examples
#' naz(NA)
naz <- function(vec) {
    return(ifelse(is.na(vec), 0, vec))
}


#' Convert Excel numeric date to date.
#' 
#' @export xldate
#' @param xldate a numeric vector containing dates in Excel format.
#' @return date
#' @examples
#' xldate(30000)
xldate <- function(xldate) {
    # convert Excel numeric date to date
    date <- as.Date(as.numeric(xldate), origin = "1899-12-30")
    return(date)
}


# Statistical functions ----
#' Compute the sample 25th percentile
#' 
#' @export p25
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


# Miscellaneous functions ----

#' Factor to numeric
#' 
#' Returns a numeric vector, converted from factor.
#' 
#' @export fton
#' @usage fton(fctr)
#' @param fctr factor that we want to convert to numeric
#' @details
#' Returns a pure numeric vector
#' @return numeric vector
#' @keywords fton
#' @examples
#' set.seed(1234)
#' fctr <- factor(sample(1:4, 50, replace=TRUE), levels=1:4)
#' fctr
#' fton(fctr)
fton <- function(fctr) {
    as.numeric(levels(fctr)[fctr])
}


#' Show head and tail of a vector, matrix, table, data frame or function
#'
#' \code{ht} head and tail of a vector, matrix, table, data frame or function
#' 
#' @export ht
#' @usage ht(df, nrecs=6)
#' @param df The object. No default.
#' @param nrecs number of records, rows, whatever to show at head and at tail
#' @details show head and tail of a vector, matrix, table, data frame or function
#' @keywords ht
#' @examples
#' ht(mtcars, 4)
ht <- function(df, nrecs = 6) {
    print(utils::head(df, nrecs))
    print(utils::tail(df, nrecs))
}


#' function to deal with NA logical values
#' 
#' @export is.true
#' @param x vector.
#' @return logical vector
is.true <- function(x) {
    !is.na(x) & x
}


#' Describe memory usage and collect garbage
#'
#' \code{memory} describe memory usage and collect garbage
#' 
#' @export memory
#' @usage memory(maxnobjs=5)
#' @param maxnobjs The number of objects to display. Default is 5.
#' @details Describes memory usage and collects garbage 
#' @keywords memory
#' @examples
#' memory(4)
memory <- function(maxnobjs = 5) {
    # function for getting the sizes of objects in memory
    objs <- ls(envir = globalenv())
    nobjs <- min(length(objs), maxnobjs)
    
    getobjs <- function() {
        f <- function(x) utils::object.size(get(x))/1048600
        sizeMB <- sapply(objs, f)
        tmp <- data.frame(sizeMB)
        tmp <- cbind(name = row.names(tmp), tmp) %>% dplyr::arrange(dplyr::desc(sizeMB))
        # tmp <- tmp[order(-tmp$sizeMB), ]
        row.names(tmp) <- NULL
        tmp$sizeMB <- formatC(tmp$sizeMB, format = "f", digits = 2, big.mark = ",", preserve.width = "common")
        return(tmp)
    }
    
    print(paste0("Memory available: ", utils::memory.size(NA)))
    print(paste0("Memory in use before: ", utils::memory.size()))
    
    if (nobjs > 0) {
        print("Memory for selected objects: ")
        print(utils::head(getobjs(), nobjs))
    }
    print(gc())
    print(paste0("Memory in use after: ", utils::memory.size()))
}


#' create vector of sorted names of a data frame
#' 
#' @export ns
#' @param df Data frame
#' @return a vector of sorted names
#' @examples
#' names(iris) # unsorted
#' ns(iris)
ns <- function(df) {
    names(df) %>% sort
}


#' ifelse that can be used safely with dates
#' 
#' @export
#' @param cond Logical expression.
#' @param yes Resulting value if cond is TRUE.
#' @param no Resulting value if cond is FALSE.
#' @examples
#' # snippet: mutate(date=safe.ifelse(freq=='A', as.Date(paste0(olddate, '-01-01')), date))
safe.ifelse <- function(cond, yes, no) {
    # so dates don't lose their class!
    structure(ifelse(cond, yes, no), class = class(yes))
}


#' which elements are not in the intersection of two vectors?
#' 
#' @export
#' @param v1 vector
#' @param v2 vector of the same type as v1
#' @return a vector of items that are not in the intersection of two sets
#' @examples
#' v1 <- c(1, 3, 4, 5, 6, 7)
#' v2 <- c(2, 4, 6, 8, 10)
#' setdiff_all(v1, v2)
#' # compare to setdiff
#' setdiff(v1, v2)
#' setdiff(v2, v1)
setdiff_all <- function(v1, v2) {
    setdiff(union(v1, v2), intersect(v1, v2))
}

