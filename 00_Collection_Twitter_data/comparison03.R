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

df_up <- list.files(path = "./data_updated/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS)
df_or <- list.files(path = "./data_2019_Sean/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS)
df_new <- list.files(path = "./Full_data_2019/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS)

# Getting daily data
data_daily_up <- df_daily(df_up, 1)
data_daily_or <- df_daily(df_or, 1)
data_daily_new <- df_daily(df_new, 1)

#-------------- Load twitter data from AcademicTwitter R ----------------------#

df_R <- list.files(path = "./data_2019/", pattern = ".rds$", full.names = TRUE) %>% map_dfr(readRDS)

data_month_R <- df_monthly(df_R, 1)
data_daily_R <- df_daily(data_month_R, 1)


# data_R <- data_daily_R %>% filter(!grepl("^@", text) & !grepl("^RT", text))
# 
# data_R_full <- data_R %>% unnest(attachments)
# data_R_full <- unnest(data_R_full, entities) %>% 
#   select(-withheld,-possibly_sensitive, -cashtags) %>% 
#   rename("id_original" = "id")
# data_R_replies <- unnest(data_R_full, referenced_tweets) %>%
#   rename("id_referenced_tweets"  = "id") %>% 
#   select(id_original) %>% 
#   pull
# 
# `%!in%` <- Negate(`%in%`)
# 
# data_R_full <- data_R_full %>% unnest(urls) %>% 
#   mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>% 
#   filter(condition == 1) %>% 
#   distinct(display_url, .keep_all = TRUE)
# 
# data_R_full <- data_R_full %>%
#   filter(id_original %!in% data_R_replies)
# 
# data_R_full <- unnest(data_R_full, media_keys)
# data_R_full <- data_R_full %>% distinct(display_url, .keep_all = TRUE)


data_R_full <- get_data_daily_R(data_daily_R) %>% unnest(urls) %>%
  mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>%
  filter(condition == 1) %>%
  distinct(display_url, .keep_all = TRUE)


# a <- data_R_full %>% unnest(urls) %>% 
#   mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>% 
#   filter(condition == 1) %>% 
#   distinct(display_url, .keep_all = TRUE)
# 
# 
# data_R_full_2019 <- get_data_annually_R(df_R)

# rm(data_month_R, data_daily_R)

#-- clean python data


data_month_Python <- df_monthly(df_new, 1)
data_daily_Python <- df_daily(data_month_Python, 1)
data_Python_full <- clean_data_daily_Python(data_daily_Python)
data_Python_full <- data_Python_full %>% select(created_at, text, attachments, url_image)

# data_Python_full_2019 <- df_new %>% select(created_at, text, attachments, url_image)

rm(data_month_Python)

#-- join dataset

data_master <- data_R_full %>% 
  left_join(data_Python_full, by = c("media_keys"="attachments")) %>% 
  filter(!is.na(url_image)) %>% distinct(url_image, .keep_all = TRUE)


# data_R_full %>% select(media_keys) %>% distinct() %>% count()
# data_Python_full %>% select(attachments) %>% distinct() %>% count()
# data_Python_full[duplicated(data_Python_full$attachments), ] %>% view
# 
# b <- data_master %>% distinct(url_image, .keep_all = TRUE)
# 
# c <- data_master[duplicated(data_master$media_keys), ]
# 
# 
# aa <- a %>% 
#   left_join(data_Python_full, by = c("media_keys"="attachments")) %>% 
#   filter(!is.na(url_image))



# All year!

data_master_2019 <- data_R_full_2019 %>% 
  left_join(data_Python_full_2019, by = c("media_keys"="attachments")) %>% 
  filter(!is.na(url_image))


#--------------

data_daily_or
replies_python_or <- data_daily_or %>% filter(grepl("^@", text) | grepl("^RT", text))
data_python_or <- data_daily_or %>% filter(!grepl("^@", text) & !grepl("^RT", text))

#--------------

data_daily_up %>% dim 
replies_python_up <- data_daily_up %>% filter(grepl("^@", text) | grepl("^RT", text))
data_python_up <- data_daily_up %>% filter(!grepl("^@", text) & !grepl("^RT", text))

data_daily_up     # 4799
replies_python_up # 4271
data_python_up    # 528

#--------------

data_daily_new %>% dim
replies_python_new <- data_daily_new %>% filter(grepl("^@", text) | grepl("^RT", text))
data_python_new <- data_daily_new %>% filter(!grepl("^@", text) & !grepl("^RT", text))

data_python_new %>% distinct(text) %>% dim


#--------------

data_python_full <- data_python_or %>% 
  select(created_at, text, attachments, url_image)
  
data_master <- data_R_full %>% 
  left_join(data_python_full, by = c("media_keys"="attachments")) %>% 
  filter(!is.na(url_image))

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



#------------------------------------------------------------------------------#
# Check day 2019-01-02
#------------------------------------------------------------------------------#



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









