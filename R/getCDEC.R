#' Get CDEC Data
#'
#' This function takes a \emph{station} and \emph{sensor} along
#' with a \emph{duration} and \emph{start} and \emph{end} date.
#' It returns associated CDEC sensor data in a dataframe.
#' This function can download data for a single station or
#' a list of multiple stations at once using \strong{purrr}.
#'
#' Commonly used sensors include:
#'  \itemize{
#'    \item 1  stage (ft)
#'    \item 20 flow (cfs)
#'    \item 2  rain accum (in)
#'    \item 16 precip tippingbucket (in)
#'    \item 45 ppt incremental (in)
#'    \item 3  snow water content (in)
#'    \item 18 snow depth (in)
#'    \item 6  reservoir elevation (ft)
#'    \item 15 reservoir storage (ac-ft)
#'    \item 76 reservoir inflow
#'    \item 25 water temp
#'    \item 4  air temp
#'    }
#' @seealso
#' \itemize{
#'   \item To see a list of Real-Time Stations: \url{http://cdec.water.ca.gov/misc/realStations.html}
#'   \item To see a list of Daily Stations: \url{http://cdec.water.ca.gov/misc/dailyStations.html}
#'   \item To see a list of sensors:  \url{http://cdec.water.ca.gov/misc/senslist.html}
#'   }
#' @param station # Station is 3 letter abbreviation (see \url{https://info.water.ca.gov/staMeta.html})
#' @param sensor # Sensor is number.
#' @param duration # Duration is E=event, D=Daily, H=Hourly.
#' @param start # A start date formatted as "YYYY-MM-DD".
#' @param end # A end date formatted as "YYYY-MM-DD".
#' @return Will return a dataframe with station ID and associated sensor data.
#' @examples
#' # set up the parameters to use in function:
#' sens <- c(20)
#' startT <- c("2018-12-01")
#' endT <- c("2019-03-01")
#' stations <- c("CLW", "MLW", "TIS")
#' dur  <- c("H") # hourly here, try this with "H" "D" or "E"
#' # combine into one argument list (note, order matches function order)
#' varList <- list(stations, sens, dur, startT, endT)
#' # now run function:
#' # library(dplyr); library(purrr); library(magrittr)
#' # now map each argument in parallel, then combine
#' cdec_hrly <- pmap(varList, get_cdec) %>% bind_rows()
#' # check records by station
#' table(cdec_hrly$station_id)
#' # check date range
#' cdec_hrly %>%
#'   group_by(station_id) %>%
#'   summarize(mx=max(datetime), mn=min(datetime))
#' # Format to Daily Data (requires dplyr)
#' cdec <- cdec_hrly %>%
#'   mutate(date = as.Date(datetime)) %>%
#'   group_by(station_id, date) %>%
#'   summarize("daily_cfs" = mean(value, na.rm = T)) %>%
#'   filter(!is.na(daily_cfs))
#' @export
get_cdec <- function(
  station,
  sensor,
  duration,
  start,
  end){

  # Set up the Path ---------------------------------------------------------

  linkCDEC <- paste("http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=", station,
                    "&SensorNums=", sensor,
                    "&dur_code=", duration,
                    "&Start=", start,
                    "&End=", end, sep="")

# Read in and Format ------------------------------------------------------

  df <- readr::read_csv(linkCDEC) %>%
    dplyr::select(-`OBS DATE`, -DATA_FLAG) %>%
    dplyr::rename(datetime = `DATE TIME`) %>%
    purrr::set_names(tolower(names(.))) %>%
    data.frame()

  # coerce to numeric for value col, create NAs for missing values, sometimes listed as "---"
  df$value <- suppressWarnings(as.numeric(df$value))

  cdec<-paste0("cdec_",duration,"_",station) # make a station name

  if(dim(df)[1]>0){
    cat(paste0("Downloaded Station ", cdec," successfully, in current workspace!\n\n"))
    return(df)
  } else {
    cat(paste0("No data available for Station ", cdec, " for this daterange or interval! \n\n"))
  }
}


# FORMAT TO DAILY ---------------------------------------------------------

# load("data/cdec_hrly_tis_clw_mlw.rda")
#
# cdec <- cdec_hrly %>%
#   mutate(date = as.Date(datetime)) %>%
#   group_by(station_id, date) %>%
#   summarize("daily_cfs" = mean(value, na.rm = T)) %>%
#   filter(!is.na(daily_cfs))
#
# cdec <- cdec %>%
#   add_WYD("date") %>%
#   mutate(month = month(date),
#          mday = mday(date),
#          md = format(date, "%m/%d"))

# ADD WATER YEAR TYPES ----------------------------------------------------

# wytypes <- read_csv("~/Downloads/water_year_types_2017_SAC_SJ.csv") %>%
#   select(WY, Yr_type_SAC)
# head(wytypes)
#
# ## Join
# cdec <- left_join(cdec, wytypes, by="WY")
# cdec$Yr_type_SAC <- factor(cdec$Yr_type_SAC, levels = c("C","D","BN","AN","W"))
# summary(cdec$Yr_type_SAC)
# cdec$station_id <- factor(cdec$station_id,levels = c("MLW", "CLW","TIS"))
# cdec$WY <- as.factor(cdec$WY)
#
# ## filter out 2018 for now:
# cdec <- cdec %>% filter(!WY=="2018")

# FILTER TO WEIRS ---------------------------------------------------------

# # Tisdale overtopping at 45.45 feet or approx:
# tis<- cdec %>%
#   filter(station_id == "TIS") %>%
#   mutate(WY_rev = as.numeric(as.character(WY))+.3)
#
# #Colusa Weir overtopping
# col<- cdec %>%
#   filter(station_id == "CLW") %>%
#   mutate(WY_rev = as.numeric(as.character(WY))+.6)
#
# #Moulton Weir overtopping
# mol<- cdec %>%
#   filter(station_id == "MLW") %>%
#   mutate(WY_rev = as.numeric(as.character(WY))+.9)

# SAME LINE ---------------------------------------------------------------

# ggplot() +
#   geom_point(data=tis, aes(x=DOWY, y=WY_rev, shape=station_id,
#                            color=Yr_type_SAC), size=2.5, alpha=0.9) +
#   geom_point(data=col, aes(x=DOWY, y=WY_rev, shape=station_id,
#                            color=Yr_type_SAC), size=2, alpha=0.9) +
#   geom_point(data=mol, aes(x=DOWY, y=WY_rev,
#                            shape=station_id, color=Yr_type_SAC), size=1, alpha=0.9) +
#   theme_bw(base_size = 10) +
#   scale_shape_manual("Station", values = c(16,17,15))+
#   labs(x= "", y= "Water Year")+
#   scale_color_viridis_d("WYT", direction = -1, option = "D") +
#   scale_y_continuous(breaks= seq(1998.5, 2018.5, 1), labels=c(seq(1998, 2018, 1))) +
#   scale_x_continuous(breaks = seq(0, 365, 61),
#                      labels = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"),
#                      limits = c(0, 340)) +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = c(0.9, 0.7))
# #facet_grid(station_id~.)
# ggsave(filename = "figs/example_cdec_weirs_flow_same_line.png", width = 11, height = 7, units = "in", dpi = 250)

# GEOM TILE -------------------------------------------------------------

# ggplot() +
#   geom_tile(data=tis, aes(x=DOWY, y=WY_rev, fill=Yr_type_SAC, color=station_id, height=0.4)) +
#   geom_tile(data=col, aes(x=DOWY, y=WY_rev, color=station_id, fill=Yr_type_SAC, height=0.3)) +
#   geom_tile(data=mol, aes(x=DOWY, y=WY_rev, color=station_id, fill=Yr_type_SAC, height=0.25)) +
#   theme_bw(base_size = 10) +
#   scale_color_manual("Station", values = c("gray70","white", "black"), guide=FALSE)+
#   labs(x= "", y= "Water Year", title = "Overtopping at Tisdale, Moulton, and Colusa Weirs, 1998-2017",
#        caption = "CDEC stations top to bottom for each year (smallest to largest boxes): CLW, MLW, TIS")+
#   scale_fill_viridis_d("WYT", direction = -1, option = "D") +
#   scale_y_continuous(breaks= seq(1998.5, 2018.5, 1), labels=c(seq(1998, 2018, 1))) +
#   scale_x_continuous(breaks = seq(0, 365, 61),
#                      labels = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"),
#                      limits = c(0, 340)) +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = c(0.9, 0.7))
#
# #facet_grid(station_id~.)
# ggsave(filename = "figs/example_cdec_weirs_flow_geom_tile.png", width = 11, height = 7, units = "in", dpi = 250)

# TILE PLOTS ---------------------------------------------------------------

# (tis.plot<-ggplot() +
#    geom_tile(data=tis, aes(x=DOWY, y=WY,
#                            fill=Yr_type_SAC), show.legend = F) +
#    theme_bw(base_size = 10) +
#    labs(x= "", y= "Water Year", title = "Tisdale Overtopping") +
#    scale_fill_viridis_d("Water Year Type", direction = -1, option = "C") +
#    scale_x_continuous(breaks = seq(0, 365, 61),
#                       labels = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"),
#                       limits = c(0, 340)) +
#    theme(plot.title = element_text(hjust = 0.5)) +
#    coord_cartesian(expand = F))
# #coord_polar(clip = "on") # check out with coord_polar()!


# (col.plot<-ggplot()+
#     geom_tile(data=col, aes(x=DOWY, y=WY,
#                             fill=Yr_type_SAC), show.legend = F) +
#     theme_bw(base_size = 10) +
#     labs(x="", y= "Water Year", title = "Colusa Weir Overtopping")+
#     scale_fill_viridis_d("Water Year Type", direction = -1, option = "C") +
#     scale_x_continuous(breaks = seq(0, 365, 61),
#                        labels = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"),
#                        limits = c(0, 340)) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     coord_cartesian(expand = F))
#
#
# (mol.plot<-ggplot()+
#     geom_tile(data=mol, aes(x=DOWY, y=WY,
#                             fill=Yr_type_SAC), show.legend = F) +
#     theme_bw(base_size = 10) +
#     labs(x= "", y= "Water Year", title = "Moulton Weir Overtopping")+
#     scale_fill_viridis_d("Water Year Type", direction = -1, option = "C") +
#     scale_x_continuous(breaks = seq(0, 365, 61),
#                        labels = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"),
#                        limits = c(0, 340)) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     coord_cartesian(expand = F))

#stacked cowplot moo moo
# library(cowplot)
# plot_grid(mol.plot, col.plot, tis.plot, nrow = 3, align = "v")
#
#
# # or all together but with facets
# ggplot() + geom_tile(data=cdec, aes(x=DOWY, y=as.factor(WY),
#                                     fill=Yr_type_SAC),
#                      show.legend = T) +
#   theme_bw(base_size = 10) +
#   labs(x= "", y= "Water Year")+
#   scale_fill_viridis_d("WYT", direction = -1, option = "D") +
#   scale_x_continuous(breaks = seq(0, 365, 61),
#                      labels = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"),
#                      limits = c(0, 340)) +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = c(0.9, 0.85)) +
#   facet_grid(station_id~.)
#
# ggsave(filename = "figs/example_cdec_weirs_flow_faceted.png", width = 11, height = 7, units = "in", dpi = 250)


#save(cdec_hrly, file = "data/cdec_hrly_tis_clw_mlw.rda")


### POLAR
# or all together but with facets
# ggplot() + geom_tile(data=cdec, aes(x=DOWY, y=as.factor(WY),
#                                     fill=Yr_type_SAC),
#                      show.legend = T) +
#   theme_bw(base_size = 8) +
#   labs(x= "", y= "Water Year")+
#   scale_fill_viridis_d("WYT", direction = -1, option = "D") +
#   scale_x_continuous(breaks = seq(0, 365, 61),
#                      labels = c("Oct", "Dec", "Feb", "Apr", "Jun", "Aug"),
#                      limits = c(0, 340)) +
#   facet_grid(.~station_id) +
#   coord_polar()
#
# ggsave(filename = "figs/example_cdec_weirs_flow_polar.png", width = 11, height = 7, units = "in", dpi = 250)
#
