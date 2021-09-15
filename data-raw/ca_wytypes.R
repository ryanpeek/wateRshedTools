## code to prepare `DATASET` dataset goes here

# use to set up:
# usethis::use_data_raw()

# GET PACKAGE
library(rvest)
library(readr)
library(tidyr)
library(dplyr)

# get html table
data <- rvest::read_html("http://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST") %>%
  html_element(xpath = '//*[@id="main_content"]/pre/text()')

# now read just the first table and skip rows
wy_df1 <- data %>%
  html_text() %>%
  readr::read_tsv(skip = 20,
                  col_names = FALSE) %>%
  # fix extra spaces
  mutate(X2 = gsub("\\s+", " ", x=X1))

# now format and fix
wy_df_final <- wy_df1 %>%
  separate(X2,
           into=c("WY",
                  "sv_Oct_Mar", "sv_Apr_Jul", "sv_WYsum","sv_Index", "sv_WYtype",
                  "sj_Oct_Mar", "sj_Apr_Jul", "sj_WYsum","sj_Index", "sj_WYtype"),
           sep = " ", extra = "drop") %>%
  # drop orig cols and pull just 1906:current year
  select(-X1) %>%
  slice(1:(lubridate::year(Sys.Date())-1906)) %>%
  mutate(across(-c(contains("WYType")), as.numeric))

# get the 8-station index
wy_df2 <- data %>%
  html_text() %>%
  readr::read_tsv(skip = 153,
                  col_names = FALSE) %>%
  # fix extra spaces
  mutate(X2 = gsub("\\s+", " ", x=X1))

# now format and fix
wy_df_final2 <- wy_df2 %>%
  separate(X2,
           into=c("WY", "Dec", "Jan", "Feb", "Mar", "Apr", "May"),
           sep = " ", extra = "drop") %>%
  # drop orig cols and pull just 1906:current year
  select(-X1) %>%
  slice(1:(lubridate::year(Sys.Date())-1906)) %>%
  mutate(across(-c(contains("WYType")), as.numeric))

# 8-riv_runoff
riv8_runoff <- wy_df_final2

# ca_wytypes
ca_wytypes <- wy_df_final

# now save!
write_csv(ca_wytypes, "data-raw/ca_wytypes.csv")
write_csv(riv8_runoff, "data-raw/riv8_runoff.csv")

# export
usethis::use_data(riv8_runoff, overwrite = TRUE)
usethis::use_data(ca_wytypes, overwrite = TRUE)
