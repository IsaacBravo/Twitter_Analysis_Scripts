#------------------------------------------------------------------------------
# Topic: Extract tweets id from data_images
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
library(academictwitteR)
library(tidyverse)
library(purrr)
library(tidyr)

source("functions_02.R")

#-------------- Load twitter data from data validated -------------------------#

# English data
df_eng_2019 <- list.files(path = "./data/eng_data/Full_data_2019_validated/data_tweets/By_month/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% 
  mutate(year = lubridate::year(date),
         id_date = paste0(year, "_", month)) %>% 
  select(id_date, id_tweet = id_original)

df_eng_2020 <- list.files(path = "./data/eng_data/Full_data_2020_validated/data_tweets/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% 
  mutate(year = lubridate::year(date),
         id_date = paste0(year, "_", month)) %>% 
  select(id_date, id_tweet = id_original)

df_eng_2021 <- list.files(path = "./data/eng_data/Full_data_2021_validated/data_tweets/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% 
  mutate(year = lubridate::year(date),
         id_date = paste0(year, "_", month)) %>% 
  select(id_date, id_tweet = id_original)

df_eng_tweet_ids <- rbind(df_eng_2019, df_eng_2020, df_eng_2021)
rm(df_eng_2019, df_eng_2020, df_eng_2021)

saveRDS(df_eng_tweet_ids, "./data/eng_data/df_eng_tweet_ids.rds")
write.csv(df_eng_tweet_ids, "./data/eng_data/df_eng_tweet_ids.csv")

rm(df_eng_tweet_ids)

# Spanish data
df_spa_2019 <- list.files(path = "./data/spa_data/Full_data_2019_validated/data_tweets/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% 
  mutate(year = lubridate::year(date),
         id_date = paste0(year, "_", month)) %>% 
  select(id_date, id_tweet = id_original)

df_spa_2019 <- list.files(path = "./data/spa_data/Full_data_2020_validated/data_tweets/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% 
  mutate(year = lubridate::year(date),
         id_date = paste0(year, "_", month)) %>% 
  select(id_date, id_tweet = id_original)

df_spa_2019 <- list.files(path = "./data/spa_data/Full_data_2021_validated/data_tweets/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% 
  mutate(year = lubridate::year(date),
         id_date = paste0(year, "_", month)) %>% 
  select(id_date, id_tweet = id_original)

df_eng_2019 %>% 
  filter(id_original == '1112864011748589568') %>% View

df_eng_2019 %>% 
  filter(id_original == '1112863972758376455') %>% View

df_eng_2019 %>% 
  filter(id_original == '1112864513358008321') %>% View

