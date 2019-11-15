#' Open Directory on PC
#'
#' This is a helper function to open current project directory
#'
#' It doesn't require any specific parameters
#'
#' @export
#'

owin <- function(...) if(Sys.info()[1]=="Windows") shell(cmd="explorer .",intern=F,wait=F)
