#' Utility Functions
#'
#' A suite of functions that may or may not be useful.
#'
#' Details: As above
#'
#' @param x a continuous numeric column
#' @export
CV <- function(x, na.rm=TRUE){
  if(na.rm){
    (sd(x, na.rm=TRUE) / mean(x,na.rm=TRUE))*100 }
  else{
    (sd(x)/mean(x))*100
  }
}


# Function to replace / in path to \\ and copy to clipboard
#' @export
repath <- function() {
  cat('Paste file path and hit RETURN twice')
  x <- scan(what = "")
  xa <- gsub('\\\\', '/', x)
  writeClipboard(paste(xa, collapse=" "))
  cat('Here\'s your de-windowsified path. (It\'s also on the clipboard.)\n', xa, '\n')
}


# Function to add a pause or break in script or function
#' @export
Pause <- function () {
  cat("Hit <enter> to continue...")
  readline()
  invisible()
}

#' cfs to cms or back
#'
#' @param x a continuous numeric column
#'
#' @export
cfs_cms <- function(x, cfs=TRUE){
  if(cfs){
    x * 0.028316847 # cfs * cms
  } else {
    x / 0.028316847
  }
}

# make new proj folders
#' @export
new.folders<-function(all=TRUE){
  if(all){
    dir.create(path="data",showWarnings=T) # create data folder
    dir.create(path="data_output",showWarnings=T)
    dir.create(path="docs",showWarnings=T)
    dir.create(path="output",showWarnings=T)
    dir.create(path="output/figs",showWarnings=T)
    dir.create(path="output/models",showWarnings=T)
    dir.create(path="code",showWarnings=T)
    dir.create(path="code/functions",showWarnings=T)
    cat("Folders created: \n
        \t code
        \t\t\t code/functions
        \t data
        \t data_output
        \t docs
        \t output
        \t\t\t output/figs
        \t\t\t output/models")
  } else {
    dir.create(path="code",showWarnings=T)
    dir.create(path="data",showWarnings=T) # create data folder
    dir.create(path="data_output",showWarnings=T)
    dir.create(path="docs",showWarnings=T)
    dir.create(path="output",showWarnings=T)
    cat("Folders created: \n
        \t code
        \t data
        \t data_output
        \t docs
        \t output")
  }
}

#' list package functions: requires unquoted package name
#' the package must be first loaded into the envivronment
#' @param package_name an unquoted package name
#' @export
ls_pkg_functions <-function(package_name, all.names = FALSE, pattern) {
  package <- deparse(substitute(package_name))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}
