#' Add Water Year
#'
#' This function takes a \emph{date} column and
#' returns the Water Year (from Oct 1 to Sep 30).
#' Can specify the starting month for a given water year.
#'
#' Details: Here
#'
#' @param dates at Date formatted column in a dataframe
#' @param start_month defaults to Oct 1, the starting month
#' @export

wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  return(adj.year)
}
