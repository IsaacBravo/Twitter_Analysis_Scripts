#------------------------------------------------------------------------------
# Topic: Check sample data from twitter from Python and R
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
library(academictwitteR)
library(tidyverse)
library(purrr)
library(tidyr)

source("functions.R")

#-------------- Load twitter data from tweepy Python --------------------------#

df_new <- list.files(path = "./data_Tweepy_backup/eng/data_2020/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS)

#-------------- Load twitter data from AcademicTwitter R ----------------------#

df_R <- list.files(path = "./data_AcademicTwitter_backup/data_2020_eng/", pattern = ".rds$", full.names = TRUE) %>% 
  map_dfr(readRDS)






data_month_R <- df_monthly(df_R, 1)
data_daily_R <- df_daily(data_month_R, 1)
data_R_full <- get_data_daily_R(data_daily_R) %>% unnest(urls) %>%
  mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>%
  filter(condition == 1) %>%
  distinct(display_url, .keep_all = TRUE)

df_R <- get_data_annually_R(df_R)
df_R <- df_R %>% 
  unnest(urls) %>%
  mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>%
  filter(condition == 1) %>%
  distinct(display_url, .keep_all = TRUE)

# df_Python <- clean_data_annually_Python(df_new)
# 
# data_master_v2 <- df_R %>% 
#   left_join(data_Python_full, by = c("media_keys"="attachments")) %>% 
#   filter(!is.na(url_image)) %>% distinct(url_image, .keep_all = TRUE)


#-- clean python data

data_month_Python <- df_monthly(df_new, 1)
data_daily_Python <- df_daily(data_month_Python, 1)
data_Python_full <- clean_data_daily_Python(data_daily_Python)
data_Python_full <- data_Python_full %>% select(created_at, text, attachments, url_image)

#-- join dataset

data_master <- data_R_full %>% 
  left_join(data_Python_full, by = c("media_keys"="attachments")) %>% 
  filter(!is.na(url_image)) %>% distinct(url_image, .keep_all = TRUE)

data_master15 <- process_data(15)
data_master16 <- process_data(16)
data_master17 <- process_data(17)
data_master18 <- process_data(18)
data_master19 <- process_data(19)
data_master20 <- process_data(20)
data_master21 <- process_data(21)
data_master22 <- process_data(22)
data_master23 <- process_data(23)
data_master24 <- process_data(24)
data_master25 <- process_data(25)
data_master26 <- process_data(26)
data_master27 <- process_data(27)
data_master28 <- process_data(28)
data_master29 <- process_data(29)
data_master30 <- process_data(30)
data_master31 <- process_data(31)

saveRDS(data_master01, "./Full_data_2019_validated/tweets_data_merge_2019-01-01.rds")
saveRDS(data_master02, "./Full_data_2019_validated/tweets_data_merge_2019-01-02.rds")
saveRDS(data_master03, "./Full_data_2019_validated/tweets_data_merge_2019-01-03.rds")
saveRDS(data_master04, "./Full_data_2019_validated/tweets_data_merge_2019-01-04.rds")
saveRDS(data_master05, "./Full_data_2019_validated/tweets_data_merge_2019-01-05.rds")
saveRDS(data_master06, "./Full_data_2019_validated/tweets_data_merge_2019-01-06.rds")
saveRDS(data_master07, "./Full_data_2019_validated/tweets_data_merge_2019-01-07.rds")
saveRDS(data_master08, "./Full_data_2019_validated/tweets_data_merge_2019-01-08.rds")
saveRDS(data_master09, "./Full_data_2019_validated/tweets_data_merge_2019-01-09.rds")
saveRDS(data_master10, "./Full_data_2019_validated/tweets_data_merge_2019-01-10.rds")
saveRDS(data_master11, "./Full_data_2019_validated/tweets_data_merge_2019-01-11.rds")
saveRDS(data_master12, "./Full_data_2019_validated/tweets_data_merge_2019-01-12.rds")
saveRDS(data_master13, "./Full_data_2019_validated/tweets_data_merge_2019-01-13.rds")
saveRDS(data_master14, "./Full_data_2019_validated/tweets_data_merge_2019-01-14.rds")
saveRDS(data_master15, "./Full_data_2019_validated/tweets_data_merge_2019-01-15.rds")
saveRDS(data_master16, "./Full_data_2019_validated/tweets_data_merge_2019-01-16.rds")
saveRDS(data_master17, "./Full_data_2019_validated/tweets_data_merge_2019-01-17.rds")
saveRDS(data_master18, "./Full_data_2019_validated/tweets_data_merge_2019-01-18.rds")
saveRDS(data_master19, "./Full_data_2019_validated/tweets_data_merge_2019-01-19.rds")
saveRDS(data_master20, "./Full_data_2019_validated/tweets_data_merge_2019-01-20.rds")
saveRDS(data_master21, "./Full_data_2019_validated/tweets_data_merge_2019-01-21.rds")
saveRDS(data_master22, "./Full_data_2019_validated/tweets_data_merge_2019-01-22.rds")
saveRDS(data_master23, "./Full_data_2019_validated/tweets_data_merge_2019-01-23.rds")
saveRDS(data_master24, "./Full_data_2019_validated/tweets_data_merge_2019-01-24.rds")
saveRDS(data_master25, "./Full_data_2019_validated/tweets_data_merge_2019-01-25.rds")
saveRDS(data_master26, "./Full_data_2019_validated/tweets_data_merge_2019-01-26.rds")
saveRDS(data_master27, "./Full_data_2019_validated/tweets_data_merge_2019-01-27.rds")
saveRDS(data_master28, "./Full_data_2019_validated/tweets_data_merge_2019-01-28.rds")
saveRDS(data_master29, "./Full_data_2019_validated/tweets_data_merge_2019-01-29.rds")
saveRDS(data_master30, "./Full_data_2019_validated/tweets_data_merge_2019-01-30.rds")
saveRDS(data_master31, "./Full_data_2019_validated/tweets_data_merge_2019-01-31.rds")




data01 <- data_master01 %>% select(id_original, date, url_image)
data02 <- data_master02 %>% select(id_original, date, url_image)
data03 <- data_master03 %>% select(id_original, date, url_image)
data04 <- data_master04 %>% select(id_original, date, url_image)
data05 <- data_master05 %>% select(id_original, date, url_image)
data06 <- data_master06 %>% select(id_original, date, url_image)
data07 <- data_master07 %>% select(id_original, date, url_image)
data08 <- data_master08 %>% select(id_original, date, url_image)
data09 <- data_master09 %>% select(id_original, date, url_image)
data10 <- data_master10 %>% select(id_original, date, url_image)
data11 <- data_master11 %>% select(id_original, date, url_image)
data12 <- data_master12 %>% select(id_original, date, url_image)
data13 <- data_master13 %>% select(id_original, date, url_image)
data14 <- data_master14 %>% select(id_original, date, url_image)
data15 <- data_master15 %>% select(id_original, date, url_image)
data16 <- data_master16 %>% select(id_original, date, url_image)
data17 <- data_master17 %>% select(id_original, date, url_image)
data18 <- data_master18 %>% select(id_original, date, url_image)
data19 <- data_master19 %>% select(id_original, date, url_image)
data20 <- data_master20 %>% select(id_original, date, url_image)
data21 <- data_master21 %>% select(id_original, date, url_image)
data22 <- data_master22 %>% select(id_original, date, url_image)
data23 <- data_master23 %>% select(id_original, date, url_image)
data24 <- data_master24 %>% select(id_original, date, url_image)
data25 <- data_master25 %>% select(id_original, date, url_image)
data26 <- data_master26 %>% select(id_original, date, url_image)
data27 <- data_master27 %>% select(id_original, date, url_image)
data28 <- data_master28 %>% select(id_original, date, url_image)
data29 <- data_master29 %>% select(id_original, date, url_image)
data30 <- data_master30 %>% select(id_original, date, url_image)
data31 <- data_master31 %>% select(id_original, date, url_image)

df <- rbind(data15,data16,data17,data18,data19,data20,data21,
            data22,data23,data24,data25,data26,data27,data28,
            data29,data30,data31)

rm(data15,data16,data17,data18,data19,data20,data21,
   data22,data23,data24,data25,data26,data27,data28,
   data29,data30,data31)
rm(data_master15,data_master16,data_master17,data_master18,data_master19,
   data_master20,data_master21,data_master22,data_master23,data_master24,
   data_master25,data_master26,data_master27,data_master28,data_master29,
   data_master30,data_master31)
  

saveRDS(df, "./Full_data_2019_validated/df.rds")
  
# %>% select(id_original, date, url_image)

rm(data01,data02,data03,data04,data05,data06,data07)
rm(data08,data09,data10,data11,data12,data13,data14)
df <- rbind(data08,data09,data10,data11,data12,data13,data14)
rm(df)



# key <- c("1080260186210750464", "1080261455231049729")
# 
# data_master %>% 
#   filter(id_original %in% key) %>% 
#   View

download_images(data_master, "2019-01-08")

# Create folders 
base_path <- paste0(getwd(),"/Full_data_2019_img/")
subfolder_names <- paste0("img_", seq(as.Date("2019-01-01"), by = "day", length.out = 365))

for (name in subfolder_names) {
  path <- paste0(base_path, "/", name)
  dir.create(path)
}


download_imagesv2(df)



# check average tweets by day
df_R %>%
  mutate(
    date = as.Date(substr(created_at, 1, 10)),
    month = lubridate::month(date),
    day = lubridate::day(date)) %>% 
  group_by(date) %>% 
  summarize(
    date_avg = n()
  ) 







