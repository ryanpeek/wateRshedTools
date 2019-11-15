#' Add Day of Water Year
#'
#' This function takes a \emph{date} or \emph{datetime} column and
#' returns the Day of Water Year (from Oct 1 to Sep 30).
#'
#' Details: As above
#'
#' @param YYYYMMDD_HMS a POSIXct formatted column
#'
#' @export
dowy<-function(YYYYMMDD_HMS) {   # Dates must be POSIXct

  YYYYMMDD_HMS<-YYYYMMDD_HMS
  doy<-lubridate::yday(YYYYMMDD_HMS)

  # make DOWY
  offsetday = ifelse(month(YYYYMMDD_HMS) > 9, -273, 92)
  DOWY = doy + offsetday

  # adjust for leap year
  offsetyr = ifelse(lubridate::leap_year(YYYYMMDD_HMS), 1, 0) # Leap Year offset
  adj.wyd = ifelse(offsetyr==1 & doy > 274, DOWY - 1, DOWY)

  return(adj.wyd)
}
