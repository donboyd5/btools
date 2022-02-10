# btools_utilities.r 
# Don Boyd 1/22/2022

# library(devtools)
# library(btools)

# NOTE: put @export as first tag, NOT earlier.



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


#' which elements of a vector are even
#' 
#' @export is.even
#' 
#' @param x vector.
#' @return logical vector
#' @examples
#' is.even(1:10)
is.even <- function(x) {(x %% 2) == 0}

#' return even elements of a vector
#' 
#' @export even
#' 
#' @param x vector
#' @return numeric vector
#' @examples
#' even(1:10)
even <- function(x) {x[is.even(x)]}


#' which elements of a vector are odd
#' 
#' @export is.odd
#' 
#' @param x vector
#' @return logical vector
#' @examples
#' is.odd(1:10)
is.odd <- function(x) {!is.even(x)}

#' return odd elements of a vector
#' 
#' @export odd
#' 
#' @param x vector
#' @return numeric vector
#' @examples
#' odd(1:10)
odd <- function(x) {x[is.odd(x)]}


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

# safe.ifelse may no longer be needed if using tidyr safely

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
#' 
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

