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

df_new <- list.files(path = "./data_Tweepy_backup/fren/2022/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) 

#-------------- Load twitter data from AcademicTwitter R ----------------------#

df_R <- list.files(path = "./data_AcademicTwitter_backup/fren/2022/", pattern = ".rds$", full.names = TRUE) %>% 
  map_dfr(readRDS)

data_month_R <- df_monthly(df_R, 5)
data_R_full <- get_data_daily_R(df_R) %>% unnest(urls) %>%
  mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>%
  filter(condition == 1) %>%
  distinct(display_url, .keep_all = TRUE)

#-- clean python data
data_month_Python <- df_monthly(df_new, 5)
data_month_Python <- data_month_Python %>% 
                        filter(!is.na(url_image)) %>% 
                        select(text, attachments, url_image) %>% distinct()
data_Python_full <- clean_data_daily_Python(data_month_Python) 
data_Python_full <- data_Python_full %>% select(attachments, url_image)


#-- join dataset
data_master_june <- data_R_full %>% 
  left_join(data_Python_full, by = c("media_keys"="attachments")) %>% distinct(url_image, .keep_all = TRUE)

rm(df_R, data_month_Python, data_month_R, data_Python_full, data_R_full)


saveRDS(data_master_december, "./data/fre_data/Full_data_2022_validated/data_master_december.rds")
saveRDS(data_master_november, "./data/fre_data/Full_data_2022_validated/data_master_november.rds")
saveRDS(data_master_october, "./data/fre_data/Full_data_2022_validated/data_master_october.rds")
saveRDS(data_master_september, "./data/fre_data/Full_data_2022_validated/data_master_september.rds")
saveRDS(data_master_august, "./data/fre_data/Full_data_2022_validated/data_master_august.rds")
saveRDS(data_master_july, "./data/fre_data/Full_data_2022_validated/data_master_july.rds")
saveRDS(data_master_june, "./data/fre_data/Full_data_2022_validated/data_master_june.rds")
saveRDS(data_master_may, "./data/fre_data/Full_data_2022_validated/data_master_may.rds")
saveRDS(data_master_april, "./data/fre_data/Full_data_2022_validated/data_master_april.rds")
saveRDS(data_master_march, "./data/fre_data/Full_data_2022_validated/data_master_march.rds")
saveRDS(data_master_february, "./data/fre_data/Full_data_2022_validated/data_master_february.rds")
saveRDS(data_master_january, "./data/fre_data/Full_data_2022_validated/data_master_january.rds")

rm(data_master_january, data_master_february, data_master_march,
   data_master_april, data_master_may, data_master_june,
   data_master_july, data_master_august, data_master_september,
   data_master_october, data_master_november, data_master_december)


data_master_december <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_december.rds")
data_master_november <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_november.rds")
data_master_october <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_october.rds")
data_master_september <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_september.rds")
data_master_august <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_august.rds")
data_master_july <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_july.rds")
data_master_june <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_june.rds")
data_master_may <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_may.rds")
data_master_april <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_april.rds")
data_master_march <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_march.rds")
data_master_february <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_february.rds")
data_master_january <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_january.rds")

data_master_december <- data_master_december %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_november <- data_master_november  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_october <- data_master_october  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_september <- data_master_september  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_august <- data_master_august  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_july <- data_master_july  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_june <- data_master_june  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_may <- data_master_may  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_april <- data_master_april  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_march <- data_master_march  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_february <- data_master_february  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)
data_master_january <- data_master_january  %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% select(id_original, date, url_image)

saveRDS(data_master_december, "./data/fre_data/Full_data_2022_validated/data_listIMG_december.rds")
saveRDS(data_master_november, "./data/fre_data/Full_data_2022_validated/data_listIMG_november.rds")
saveRDS(data_master_october, "./data/fre_data/Full_data_2022_validated/data_listIMG_october.rds")
saveRDS(data_master_september, "./data/fre_data/Full_data_2022_validated/data_listIMG_september.rds")
saveRDS(data_master_august, "./data/fre_data/Full_data_2022_validated/data_listIMG_august.rds")
saveRDS(data_master_july, "./data/fre_data/Full_data_2022_validated/data_listIMG_july.rds")
saveRDS(data_master_june, "./data/fre_data/Full_data_2022_validated/data_listIMG_june.rds")
saveRDS(data_master_may, "./data/fre_data/Full_data_2022_validated/data_listIMG_may.rds")
saveRDS(data_master_april, "./data/fre_data/Full_data_2022_validated/data_listIMG_april.rds")
saveRDS(data_master_march, "./data/fre_data/Full_data_2022_validated/data_listIMG_march.rds")
saveRDS(data_master_february, "./data/fre_data/Full_data_2022_validated/data_listIMG_february.rds")
saveRDS(data_master_january, "./data/fre_data/Full_data_2022_validated/data_listIMG_january.rds")

rm(data_master_january, data_master_february, data_master_march,
   data_master_april, data_master_may, data_master_june,
   data_master_july, data_master_august, data_master_september,
   data_master_october, data_master_november, data_master_december)

#--------------------------------
data_master_december <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_december.rds")
data_master_november <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_november.rds")
data_master_october <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_october.rds")
data_master_september <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_september.rds")
data_master_august <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_august.rds")
data_master_july <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_july.rds")
data_master_june <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_june.rds")
data_master_may <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_may.rds")
data_master_april <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_april.rds")
data_master_march <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_march.rds")
data_master_february <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_february.rds")
data_master_january <- readRDS("./data/fre_data/Full_data_2022_validated/data_master_january.rds")

data_replies_january <- data_master_january %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)                   
data_replies_february <- data_master_february %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)
data_replies_march <- data_master_march %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)
data_replies_april <- data_master_april %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)
data_replies_may <- data_master_may %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)
data_replies_june <- data_master_june %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)
data_replies_july <- data_master_july %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)
data_replies_august <- data_master_august %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>% unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)
data_replies_september <- data_master_september %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>%unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)
data_replies_october <- data_master_october %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>%unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)
data_replies_november <- data_master_november %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>%unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)
data_replies_december <- data_master_december %>% mutate(date = as.Date(substr(created_at, 1, 10)), month = lubridate::month(date), day = lubridate::day(date)) %>%unnest(public_metrics) %>% select(id_tweet = id_original, conversation_id, author_id, text_tweet = text, date, reply_count) %>% filter(reply_count !=0)

saveRDS(data_replies_january, "./data/fre_data/Full_data_2022_validated/data_reply_january.rds")
saveRDS(data_replies_february, "./data/fre_data/Full_data_2022_validated/data_reply_february.rds")
saveRDS(data_replies_march, "./data/fre_data/Full_data_2022_validated/data_reply_march.rds")
saveRDS(data_replies_april, "./data/fre_data/Full_data_2022_validated/data_reply_april.rds")
saveRDS(data_replies_may, "./data/fre_data/Full_data_2022_validated/data_reply_may.rds")
saveRDS(data_replies_june, "./data/fre_data/Full_data_2022_validated/data_reply_june.rds")
saveRDS(data_replies_july, "./data/fre_data/Full_data_2022_validated/data_reply_july.rds")
saveRDS(data_replies_august, "./data/fre_data/Full_data_2022_validated/data_reply_august.rds")
saveRDS(data_replies_september, "./data/fre_data/Full_data_2022_validated/data_reply_september.rds")
saveRDS(data_replies_october, "./data/fre_data/Full_data_2022_validated/data_reply_october.rds")
saveRDS(data_replies_november, "./data/fre_data/Full_data_2022_validated/data_reply_november.rds")
saveRDS(data_replies_december, "./data/fre_data/Full_data_2022_validated/data_reply_december.rds")

list_conversation_id_january <- data_replies_january %>% select(conversation_id) %>% unique() 
list_conversation_id_february <- data_replies_february %>% select(conversation_id) %>% unique()
list_conversation_id_march <- data_replies_march %>% select(conversation_id) %>% unique()
list_conversation_id_april <- data_replies_april %>% select(conversation_id) %>% unique() 
list_conversation_id_may <- data_replies_may %>% select(conversation_id) %>% unique()
list_conversation_id_june <- data_replies_june %>% select(conversation_id) %>% unique()
list_conversation_id_july <- data_replies_july %>% select(conversation_id) %>% unique()
list_conversation_id_august <- data_replies_august %>% select(conversation_id) %>% unique()
list_conversation_id_september <- data_replies_september %>% select(conversation_id) %>% unique()
list_conversation_id_october <- data_replies_october %>% select(conversation_id) %>% unique()
list_conversation_id_november <- data_replies_november %>% select(conversation_id) %>% unique()
list_conversation_id_december <- data_replies_december %>% select(conversation_id) %>% unique()

saveRDS(list_conversation_id_january, "./data/fre_data/Full_data_2022_validated/list_conversation_id_january.rds")
saveRDS(list_conversation_id_february, "./data/fre_data/Full_data_2022_validated/list_conversation_id_february.rds")
saveRDS(list_conversation_id_march, "./data/fre_data/Full_data_2022_validated/list_conversation_id_march.rds")
saveRDS(list_conversation_id_april, "./data/fre_data/Full_data_2022_validated/list_conversation_id_april.rds")
saveRDS(list_conversation_id_may, "./data/fre_data/Full_data_2022_validated/list_conversation_id_may.rds")
saveRDS(list_conversation_id_june, "./data/fre_data/Full_data_2022_validated/list_conversation_id_june.rds")
saveRDS(list_conversation_id_july, "./data/fre_data/Full_data_2022_validated/list_conversation_id_july.rds")
saveRDS(list_conversation_id_august, "./data/fre_data/Full_data_2022_validated/list_conversation_id_august.rds")
saveRDS(list_conversation_id_september, "./data/fre_data/Full_data_2022_validated/list_conversation_id_september.rds")
saveRDS(list_conversation_id_october, "./data/fre_data/Full_data_2022_validated/list_conversation_id_october.rds")
saveRDS(list_conversation_id_november, "./data/fre_data/Full_data_2022_validated/list_conversation_id_november.rds")
saveRDS(list_conversation_id_december, "./data/fre_data/Full_data_2022_validated/list_conversation_id_december.rds")


rm(data_replies_january, data_replies_february, data_replies_march,
   data_replies_april, data_replies_may, data_replies_june,
   data_replies_july, data_replies_august, data_replies_september,
   data_replies_october, data_replies_november, data_replies_december)

rm(list_conversation_id_january, list_conversation_id_february, list_conversation_id_march,
   list_conversation_id_april, list_conversation_id_may, list_conversation_id_june,
   list_conversation_id_july, list_conversation_id_august, list_conversation_id_september,
   list_conversation_id_october, list_conversation_id_november, list_conversation_id_december)

rm(data_master_january, data_master_february, data_master_march,
   data_master_april, data_master_may, data_master_june,
   data_master_july, data_master_august, data_master_september,
   data_master_october, data_master_november, data_master_december)

rm(data_reply_full_january, data_reply_january)

