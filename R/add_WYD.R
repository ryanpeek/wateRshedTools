#' Add Day of Year, Day of Water Year.
#'
#' This function takes a POSIXct formatted column name and
#' adds new columns to a dataframe for Day of Year (Julian),
#' Day of Water Year (starting Oct 1 and ending Sep 30),
#' and Water Year (from Oct 1 to Sep 30).
#'
#' Here's the specific details:
#'
#' @param df a dataframe
#' @param datecolumn requires a POSIXct formatted date or datetime column
#' @export
add_WYD <- function(df, datecolumn){
  datecolumn=datecolumn
  df["DOY"] <- as.integer(sapply(df[,c(datecolumn)], yday))
  df["WY"] <- as.integer(sapply(df[,c(datecolumn)], wtr_yr))
  df["DOWY"] <- as.integer(sapply(df[,c(datecolumn)], dowy))
  return(df)

}
