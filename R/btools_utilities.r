# btools_utilities.r Don Boyd 3/27/2020

# library(devtools) library(btools)

# String manipulation functions ---- NOTE: These probably aren't needed anymore, as I use stringr for almost everything.

#' Trim white space at either end of strings
#'
#' @description \code{trim.ws} trims white space around strings
#' @usage trim.ws(s)
#' @param s The string to trim.
#' @details All white space is removed from the ends.
#' @return The trimmed string.
#' @keywords trim.ws
#' @export
#' @examples
#' trim.ws('   original string has leading and trailing spaces   ')
trim.ws <- function(s) {
    gsub("^\\s+|\\s+$", "", s)
}


# Numeric and date manipulation functions ----
#' Convert character to numeric
#'
#' @description \code{cton} converts character to numeric
#' @usage cton(cvar)
#' @param cvar The character string input. No default.
#' @details Replaces spaces, comma, $, and percent sign in a string with NULL and then converts to numeric.
#' Keeps letters so that scientific notation will be evaluated properly.
#' Also look at \code{extract_numeric} in package stringr
#' @keywords cton
#' @export
#' @examples
#' char <- '$198,234.75'
#' cton(char)
cton <- function(cvar) {
    as.numeric(gsub("[ ,$%]", "", cvar))
}


#' Convert NA to zero
#'
#' @description \code{naz} converts NA to zero
#' @usage naz(vec)
#' @param vec The vector to convert
#' @details Converts all NAs in a vector to zero.
#' @return The revised vector.
#' @keywords naz
#' @export
#' @examples
#' naz(NA)
naz <- function(vec) {
    return(ifelse(is.na(vec), 0, vec))
}


#' Convert Excel numeric date to date.
#' 
#' @param xdate a numeric vector containing dates in Excel format.
#' @return date
#' @export
#' @examples
#' xdate(30000)
xdate <- function(xdate) {
    # convert Excel numeric date to date
    date <- as.Date(as.numeric(xdate), origin = "1899-12-30")
    return(date)
}


# Statistical functions ----
#' Compute the sample 25th percentile.
#' 
#' @param x a numeric vector containing the values whose 25th percentile is to be computed.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return numeric
#' @export
#' @examples
#' p25(1:100)
#' p25(c(1:10, NA, 11:100), na.rm=TRUE)
p25 <- function(x, na.rm = FALSE) {
    as.numeric(stats::quantile(x, 0.25, na.rm = na.rm))
}


#' Compute the sample 50th percentile (median).
#' 
#' @param x a numeric vector containing the values whose 50th percentile is to be computed.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return numeric
#' @export
#' @examples
#' p50(1:100)
#' p50(c(1:10, NA, 11:100), na.rm=TRUE)
p50 <- function(x, na.rm = FALSE) {
    as.numeric(stats::quantile(x, 0.5, na.rm = na.rm))
}


#' Compute the sample 75th percentile.
#' 
#' @param x a numeric vector containing the values whose 75th percentile is to be computed.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return numeric
#' @export
#' @examples
#' p75(1:100)
#' p75(c(1:10, NA, 11:100), na.rm=TRUE)
p75 <- function(x, na.rm = FALSE) {
    as.numeric(stats::quantile(x, 0.75, na.rm = na.rm))
}


#' Compute the sample value for a specific percentile, p.
#' 
#' @param x a numeric vector containing the values whose p-th percentile is to be computed.
#' @param p the percentile.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return numeric
#' @export
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
#' @description \code{ma} Get trailing moving average
#' @usage ma(x, n)
#' @param x The vector to operate on.
#' @param n The period of the moving average.
#' @details Moving average of the vector x.
#' @keywords ma
#' @export
#' @examples
#' ma(7:21, 3)
ma <- function(x, n) {
    zoo::rollapply(x, n, function(x) mean(x, na.rm = TRUE), fill = NA, align = "right")
}


#' Get 4-period moving average (3 lags + current)
#'
#' @description \code{ma4} get 4-period moving average
#' @usage ma4(x)
#' @param x The vector to operate on.
#' @details 4-period moving average
#' @keywords ma4
#' @export
#' @examples
#' ma4(7:21)
ma4 <- function(x) {
    # note that this requires zoo, which is on the Depends line in the Description file
    zoo::rollapply(x, 4, function(x) mean(x, na.rm = TRUE), fill = NA, align = "right")
}


#' @title Get 4-period moving sum (3 lags + current)
#'
#' @description \code{sum4} get 4-period moving sum
#' @usage sum4(x)
#' @param x The vector to operate on.
#' @details 4-period moving sum
#' @keywords sum4
#' @export
#' @examples
#' sum4(7:21)
sum4 <- function(x) {
    ma4(x) * 4
}


# Miscellaneous functions ----

#' Convenience 'not-in' operator
#'
#' Complement of the built-in operator \code{\%in\%}. Returns the elements of \code{x} that are not in \code{y}.
#' @title \%nin\%
#' @param x vector of items
#' @param y vector of all values
#' @return logical vecotor of items in x not in y
#' @author Kieran Healy
#' @rdname nin
#' @examples
#' fruit <- c("apples", "oranges", "banana")
#' "apples" %nin% fruit
#' "pears" %nin% fruit
#' @export
"%nin%" <- function(x, y) {
    return( !(x %in% y) )
}


#' Factor to numeric
#'
#' \code{fton} returns a numeric vector, converted from factor
#'
#' @usage fton(fctr)
#' @param fctr factor that we want to convert to numeric
#' @details
#' Returns a pure numeric vector
#' @return numeric vector
#' @keywords fton
#' @export
#' @examples
#' set.seed(1234)
#' fctr <- factor(sample(1:4, 50, replace=TRUE), levels=1:4)
#' fctr
#' fton(fctr)
fton <- function(fctr) {
    as.numeric(levels(fctr)[fctr])
}


#' @title Show head and tail of a vector, matrix, table, data frame or function
#'
#' @description \code{ht} head and tail of a vector, matrix, table, data frame or function
#' @usage ht(df, nrecs=6)
#' @param df The object. No default.
#' @param nrecs number of records, rows, whatever to show at head and at tail
#' @details show head and tail of a vector, matrix, table, data frame or function
#' @keywords ht
#' @export
#' @examples
#' ht(mtcars, 4)
ht <- function(df, nrecs = 6) {
    print(utils::head(df, nrecs))
    print(utils::tail(df, nrecs))
}


#' function to deal with NA logical values
#' 
#' @param x vector.
#' @return logical vector
#' @export
is.true <- function(x) {
    !is.na(x) & x
}


#' @title Describe memory usage and collect garbage
#'
#' @description \code{memory} describe memory usage and collect garbage
#' @usage memory(maxnobjs=5)
#' @param maxnobjs The number of objects to display. Default is 5.
#' @details Describes memory usage and collects garbage 
#' @keywords memory
#' @export
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
#' @param df Data frame
#' @return a vector of sorted names
#' @export
#' @examples
#' names(iris) # unsorted
#' ns(iris)
ns <- function(df) {
    names(df) %>% sort
}


#' ifelse that can be used safely with dates
#' 
#' @param cond Logical expression.
#' @param yes Resulting value if cond is TRUE.
#' @param no Resulting value if cond is FALSE.
#' @export
#' @examples
#' # snippet: mutate(date=safe.ifelse(freq=='A', as.Date(paste0(olddate, '-01-01')), date))
safe.ifelse <- function(cond, yes, no) {
    # so dates don't lose their class!
    structure(ifelse(cond, yes, no), class = class(yes))
}


#' which elements are not in the intersection of two vectors?
#' 
#' @param v1 vector
#' @param v2 vector of the same type as v1
#' @return a vector of items that are not in the intersection of two sets
#' @export
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

