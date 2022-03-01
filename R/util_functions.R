#' Utility Functions
#'
#' A suite of functions that may or may not be useful.
#'
#' Details: As above
#'
#' @param x a continuous numeric column
#' @param na.rm remove the NAs before calculating
#' @importFrom stats sd
#' @export
CV <- function(x, na.rm=TRUE){
  if(na.rm){
    (sd(x, na.rm=TRUE) / mean(x,na.rm=TRUE))*100 }
  else{
    (sd(x)/mean(x))*100
  }
}

#' cfs to cms or back
#'
#' @param x a continuous numeric column
#' @param cfs convert to cfs as default, FALSE for cms
#'
#' @export
cfs_cms <- function(x, cfs=TRUE){
  if(cfs){
    x * 0.028316847 # cfs * cms
  } else {
    x / 0.028316847
  }
}

#' list package functions: requires unquoted package name
#' the package must be first loaded into the envivronment
#' @param package_name an unquoted package name
#' @param all.names TRUE/FALSE, include ALL named functions
#' @param pattern a grep pattern to use in the /ls() function
#' @export
ls_pkg_functions <-function(package_name, all.names = FALSE, pattern) {
  package <- deparse(substitute(package_name))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}
