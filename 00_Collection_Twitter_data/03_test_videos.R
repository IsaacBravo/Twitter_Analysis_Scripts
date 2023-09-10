#------------------------------------------------------------------------------
# Topic: Checking images status
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
library(academictwitteR)
library(tidyverse)
library(purrr)
library(tidyr)

df_list_videos <- list.files(path = "./data/eng_data/", pattern = "2021_eng.rds$", full.names = TRUE) %>%
  map_dfr(readRDS) %>% 
  distinct(id_original, .keep_all = TRUE)

video_list <- list.files(path="./video_data/eng/data_video_2021/", pattern = ".mp4$", recursive=TRUE)

df_down_videos <- video_list %>% 
  as.data.frame() %>% 
  rename("id_download" = ".") %>% 
  mutate(id_download_IMG = sub(".*id_([^.]+)\\_2021.*", "\\1", id_download)) %>%
  select(id_download_IMG) %>%
  unique() %>% 
  pull
  

df_videos <- df_list_videos %>% 
  mutate(status = ifelse(id_original %in% df_down_videos, "yes_downloaded", "no_downloaded"))

df_videos$status %>% table

# Check 404 urls
df_gif <- df_videos %>% 
  filter(status == "no_downloaded") %>% 
  filter(link_type == "animated_gif") %>% 
  select(id_original, date, url_video)
  
df_videos <- df_videos %>% 
  filter(status == "no_downloaded") %>%
  filter(link_type == "video") %>% 
  select(id_original, date, url_video) %>%
    mutate(url = sub(".*img/", "", url_video),
           url = paste0("https://video.twimg.com/tweet_video/", url)) %>% 
  select(id_original, date, url_video = url)


df_videos_check <- df_videos %>% 
  filter(grepl("pbs", url_video)) %>% 
  mutate(x = sub(".*media/", "", url_video),  
         url = paste0("https://video.twimg.com/tweet_video/", x),
         )


df_videos_no <- rbind(df_gif, df_videos)


# Download videos
download_videos <- function(data) {
  
  list_videos <- data %>% 
    select(id_original, date, url = url_video) %>% 
    mutate(id = paste0(id_original, "_", date))
  
  downloadPath = paste0(getwd(),"./video_data/eng/")
  
  # Define title of each image file
  title_video = c()
  for (i in 1:nrow(list_videos)) {
    title_video[i] = paste0("id_",list_videos$id[i])
  }
  
  for (i in 1:nrow(list_videos)){
    try(
      download.file(list_videos$url[i], 
                    destfile = paste0(downloadPath, title_video[i],".mp4"),
                    mode = 'wb'),
      silent = TRUE)
    Sys.sleep(1)
  }
}

download_videos(df_videos_no)


# List all objects in the workspace
all_objects <- ls()

# Select objects with names that match the pattern "abc"
abc_objects <- all_objects[grep("img", all_objects)]

# Remove the selected objects
rm(list = abc_objects)


