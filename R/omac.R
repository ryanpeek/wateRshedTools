#' Open Directory on OSX
#'
#' This is a helper function to open current project directory
#'
#' It doesn't require any specific parameters
#'
#' @param ... empty space
#'
#' @export
omac <- function(...) if(Sys.info()[1]=="Darwin") system("open .")
