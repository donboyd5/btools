
# navigation notes ---- alt-o, shift-alt-o alt-l, shift-alt-l

# alt-r

# finding functions: Click a function name in code and press F2, or Press Ctrl +

# package building --- http://r-pkgs.had.co.nz/ # IMPORTANT

# Update documentation: ctrl-shift-d Install Package: 'Ctrl + Shift + B' Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'

# devtools::load_all() reloads all code Ctrl/Cmd + Shift + L saves all open files and reloads code This keyboard shortcut leads to a fluid development
# workflow: Edit an R file.  Press Ctrl/Cmd + Shift + L.  Explore the code in the console.  Rinse and repeat.

library(devtools)
session_info()  # or devtools::session_info()

formatR::tidy_dir("R")
lintr::lint_package()

# edit the DESCRIPTION file using these commands library(usethis) This block will update the DESCRIPTION file when devtools::document() is run (or via
# shift-ctrl-d) ####
usethis::use_package("dplyr")
usethis::use_package("forecast")
usethis::use_package("lubridate")
usethis::use_package("magrittr")
# usethis::use_package("precis")
usethis::use_package("scales")
usethis::use_package("stats")
usethis::use_package("tibble")
usethis::use_package("zoo")

# roxygen2 ---- https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html


# This block will update the NAMESPACE file when devtools::document() is run (or via shift-ctrl-d) ####

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL





# documenting code ---- Add roxygen comments to your .R files.  Run devtools::document() (or press Ctrl/Cmd + Shift + D in RStudio) to convert roxygen
# comments to .Rd files. (devtools::document() calls roxygen2::roxygenise() to do the hard work.)  Preview documentation with ?.  Rinse and repeat
# until the documentation looks the way you want.
