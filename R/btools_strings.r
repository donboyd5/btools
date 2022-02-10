# String manipulation functions

#' Detect whether any vector elements are in a string
#' 
#' @export str_detect_any
#' 
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
#' 
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
