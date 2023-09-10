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

# source("functions.R")

df_list_IMG <- list.files(path = "./russ_media/img/", pattern = ".jpg$", full.names = TRUE) %>%
  map_dfr(readRDS)

# df_list_IMG <- df_list_IMG %>%
#   mutate(month = lubridate::month(date),
#          day = lubridate::day(date))

img_list <- list.files(path="./data_IMG_2021_spa/12_December", pattern = ".jpg$", recursive=TRUE)

df_down_IMG <- df_list_IMG %>% as.data.frame()
df_down_IMG <- df_down_IMG %>% 
  rename("id_download" = ".") %>% 
  mutate(id_download_IMG = sub(".*id_([^.]+)\\_2021.*", "\\1", id_download)) %>% 
  select(id_download_IMG) %>% 
  unique() %>% 
  pull
  

df_IMG <- urls_img_russian %>% 
  mutate(status = ifelse(id_original %in% df_down_IMG, "yes_downloaded", "no_downloaded"))

df_IMG$status %>% table

# Check 404 urls
df_IMG_no <- df_IMG %>% 
  filter(status == "no_downloaded") 

# Check 404 urls by day
# df_IMG_no %>% 
#   group_by(date) %>% 
#   summarize(n = n()) %>% 
#   view

# Download images

download_imagesv3 <- function(data) {
  
  list_images <- data %>% 
    select(id_original, date, url_image) %>% 
    mutate(id = paste0(id_original, "_", date))
  
  downloadPath = paste0(getwd(),"./data_IMG_2021_spa/")
  
  # Define title of each image file
  title_image = c()
  for (i in 1:nrow(list_images)) {
    title_image[i] = paste0("id_",list_images$id[i])
  }
  
  for (i in 1:nrow(list_images)){
    try(
      download.file(list_images$url_image[i], 
                    destfile = paste0(downloadPath, title_image[i],".jpg"),
                    mode = 'wb'),
      silent = TRUE)
    Sys.sleep(1)
  }
}

download_imagesv3(df_IMG_no)

download_imagesv3(december)

november <- df_IMG_no
december <- df_IMG_no

# List all objects in the workspace
all_objects <- ls()

# Select objects with names that match the pattern "abc"
abc_objects <- all_objects[grep("list", all_objects)]

# Remove the selected objects
rm(list = abc_objects)


