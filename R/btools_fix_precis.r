#' Display a succinct summary of a data frame
#'
#' @param x A tabular structure like a data frame.
#' @inheritParams precis::precis_v
#' @export
#' @examples
#' precis2(mtcars)
#' precis2(mtcars, histogram = TRUE)
precis2 <- function(x, ..., width = 60) {
  # "fix" this part of the precis package
  out <- tibble::tibble(
    name = names(x),
    type = vapply(x, tibble::type_sum, character(1)),
    precis = vapply(x, precis::precis_v, ..., FUN.VALUE = character(1))
  )
  attr(out, "obj_sum") <- tibble::obj_sum(x)
  # class(out) <- c("precis", "tbl_df", "tbl", "data.frame") # this is deprecated, I think, and eliminates the output!!
  out
}



