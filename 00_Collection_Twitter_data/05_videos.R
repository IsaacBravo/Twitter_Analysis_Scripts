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

download_videos <- function(data) {
  
  list_images <- data %>% 
    select(id_original, date, url_video) %>% 
    mutate(id = paste0(id_original, "_", date))
  
  downloadPath = paste0(getwd(),"./video_data/french/")
  
  # Define title of each image file
  title_image = c()
  for (i in 1:nrow(list_images)) {
    title_image[i] = paste0("id_",list_images$id[i])
  }
  
  for (i in 1:nrow(list_images)){
    try(
      download.file(list_images$url_video[i], 
                    destfile = paste0(downloadPath, title_image[i],".mp4"),
                    mode = 'wb'),
      silent = TRUE)
    Sys.sleep(1)
  }
}

download_videos(urls_media_2022_spa)

urls_media_2022_ger <- urls_media_2022_ger %>% distinct(url_video, .keep_all = TRUE)



urls_media_2020_ger <- list.files(path = "./data_Tweepy_backup/ger/data_2020/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_media <- urls_media_2020_ger %>% 
  mutate(url_video = sub(".*https://pbs.twimg.com/tweet_video_thumb/", "", link_image),
         url_video = sub(".jpg.*", "", url_video),
         url_video = paste0("https://video.twimg.com/tweet_video/", url_video, ".mp4"), 
         id_original = sub(".*_", "", attachments))

saveRDS(df_media, "urls_media_2020_ger.rds")



# df_media <- df_new %>% filter(link_type != "photo") %>% 
#   select(attachments, date, link_image, link_type, tweetURL)

df_media <- df_new %>% 
  mutate(url_video = sub(".*https://pbs.twimg.com/tweet_video_thumb/", "", link_image),
         url_video = sub(".jpg.*", "", url_video),
         url_video = paste0("https://video.twimg.com/tweet_video/", url_video, ".mp4"), 
         id_original = sub(".*_", "", attachments))

# download_videos(df_media)

saveRDS(df_media, "urls_media_2020_ger.rds")

df_new_jan <- list.files(path = "./data_Tweepy_backup/ger/data_2019/01_January", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_feb <- list.files(path = "./data_Tweepy_backup/ger/data_2019/02_February", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_mar <- list.files(path = "./data_Tweepy_backup/ger/data_2019/03_March", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_apr <- list.files(path = "./data_Tweepy_backup/ger/data_2019/04_April", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_may <- list.files(path = "./data_Tweepy_backup/ger/data_2019/05_May", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_jun <- list.files(path = "./data_Tweepy_backup/ger/data_2019/06_June", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_jul <- list.files(path = "./data_Tweepy_backup/ger/data_2019/07_July", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_aug <- list.files(path = "./data_Tweepy_backup/ger/data_2019/08_August", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_sep <- list.files(path = "./data_Tweepy_backup/ger/data_2019/09_September", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_oct <- list.files(path = "./data_Tweepy_backup/ger/data_2019/10_October", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_nov <- list.files(path = "./data_Tweepy_backup/ger/data_2019/11_November", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_new_dec <- list.files(path = "./data_Tweepy_backup/ger/data_2019/12_December", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% filter(link_type != "photo") %>% 
  select(attachments, date, link_image, link_type, tweetURL) %>% 
  filter(!is.na(link_image)) %>% distinct(.keep_all = TRUE)

df_media_2019 <- rbind(df_new_jan, df_new_feb, df_new_mar, df_new_apr, df_new_may,
                       df_new_jun, df_new_jul, df_new_aug, df_new_sep, df_new_oct,
                       df_new_nov, df_new_dec)

# saveRDS(df_media_2021, "urls_media_2021_eng.rds")

rm(df_new_jan, df_new_feb, df_new_mar, df_new_apr, df_new_may,
      df_new_jun, df_new_jul, df_new_aug, df_new_sep, df_new_oct,
      df_new_nov, df_new_dec)





df_media_2019 <- df_media_2019 %>% 
  mutate(url_video = sub(".*https://pbs.twimg.com/tweet_video_thumb/", "", link_image),
         url_video = sub(".jpg.*", "", url_video),
         url_video = paste0("https://video.twimg.com/tweet_video/", url_video, ".mp4"), 
         id_original = sub(".*_", "", attachments))

saveRDS(df_media_2019, "urls_media_2019_eng.rds")


df_media_2020 <- df_media_2020 %>% 
  mutate(url_video = sub(".*https://pbs.twimg.com/tweet_video_thumb/", "", link_image),
         url_video = sub(".jpg.*", "", url_video),
         url_video = paste0("https://video.twimg.com/tweet_video/", url_video, ".mp4"), 
         id_original = sub(".*_", "", attachments))

saveRDS(df_media_2020, "urls_media_2020_eng.rds")

df_media_2021 <- df_media_2021 %>% 
  mutate(url_video = sub(".*https://pbs.twimg.com/tweet_video_thumb/", "", link_image),
         url_video = sub(".jpg.*", "", url_video),
         url_video = paste0("https://video.twimg.com/tweet_video/", url_video, ".mp4"), 
         id_original = sub(".*_", "", attachments))

saveRDS(df_media_2021, "urls_media_2021_eng.rds")

df_media_2022 <- df_media_2022 %>% 
  mutate(url_video = sub(".*https://pbs.twimg.com/tweet_video_thumb/", "", link_image),
         url_video = sub(".jpg.*", "", url_video),
         url_video = paste0("https://video.twimg.com/tweet_video/", url_video, ".mp4"), 
         id_original = sub(".*_", "", attachments))

saveRDS(df_media_2022, "urls_media_2022_eng.rds")


df_new %>% head(5) %>% view

df_new$link_type %>% table

df_media <- df_new %>% filter(link_type != "photo") 

df_media %>% head(5) %>% view

df_media[1,]

df_media %>% colnames

df <- df_media %>% select(tweetURL, attachments, link_type, date)

saveRDS(df, "urls_media_2019_eng.rds")








