#' Add Day of Year, Day of Water Year.
#'
#' This function takes a POSIXct formatted column name and
#' adds new columns to a dataframe for Day of Year (Julian),
#' Day of Water Year (starting Oct 1 and ending Sep 30),
#' and Water Year (from Oct 1 to Sep 30).
#'
#' This function should be used as follows:
#' Reassign or create the dataframe you would like to add new Water Year columns to. This function will generate three new columns, \bold{DOY} (Day of Year), \bold{WY} (Water Year, starting Oct 1), and \bold{DOWY} (Day of Water Year, starting Oct 1). It relies on \code{dowy} and \code{wt_yr} functions, as well as the \code{lubridate} package.
#' @examples
#'
#' # get data
#' airq <- datasets::airquality
#' # add a year (this data from 1973)
#' airq$year <- 1973
#' # make a date col
#' airq$date <- with(airq, paste0(year, "-", Month, "-", Day))
#' head(airq)
#' # now format into POSIX with lubridate
#' require(lubridate)
#' airq$date <- lubridate::ymd(airq$date)
#'
#' # now run function:
#' airq <- add_WYD(airq, "date")
#'
#' @param df a dataframe
#' @param datecolumn quoted, requires a POSIXct formatted date or datetime
#'
#' @export
#'
add_WYD <- function(df, datecolumn){
  datecolumn=datecolumn
  df["DOY"] <- as.integer(sapply(df[,c(datecolumn)], lubridate::yday))
  df["WY"] <- as.integer(sapply(df[,c(datecolumn)], wtr_yr))
  df["DOWY"] <- as.integer(sapply(df[,c(datecolumn)], dowy))
  return(df)

}
