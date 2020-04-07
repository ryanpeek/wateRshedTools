## code to prepare `DATASET` dataset goes here

# use to set up:
# usethis::use_data_raw()

# GET PACKAGE
# devtools::install_git("https://git.sr.ht/~hrbrmstr/reapr")
library(reapr)
library(tidyverse, warn.conflicts = FALSE)
library(usethis)

# get data
x <- reap_url("http://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST")

# pull out data
wydat <- read_delim(as.data.frame(x$tag$pre)$elem_content, skip=20, delim=" ", col_names = F, n_max=112)

# add appropriate col names for each watershed
wydat_sac <- wydat %>%
  select(1:6) %>%
  set_names(c("WY","Oct_Mar", "Apr_Jul", "WYsum","Index", "WYtype")) %>%
  mutate(watershed = "Sacramento Valley")

wydat_sj <- wydat %>%
  select(1, 7:11) %>%
  set_names(c("WY","Oct_Mar", "Apr_Jul", "WYsum","Index", "WYtype")) %>%
  mutate(watershed = "San Joaquin Valley")

# now bind together and save
ca_wytypes <- bind_rows(wydat_sac, wydat_sj)

# now save!
write_csv(ca_wytypes, "data-raw/ca_wytypes.csv")
usethis::use_data(ca_wytypes, overwrite = TRUE)
