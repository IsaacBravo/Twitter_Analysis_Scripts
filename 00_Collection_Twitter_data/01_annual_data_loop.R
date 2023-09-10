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

#------------------------------ Define Functions ------------------------------#

df_monthly <- function(df, number_month){
  df %>%
    mutate(
      date = as.Date(substr(created_at, 1, 10)),
      month = lubridate::month(date),
      day = lubridate::day(date)) %>%
    filter(month == number_month)
}

get_data_daily_R <- function(data_daily_R){
  
  data_R <- data_daily_R %>% filter(!grepl("^@", text) & !grepl("^RT", text))
  
  data_R_full <- data_R %>% unnest(attachments)
  data_R_full <- unnest(data_R_full, entities) %>% 
    select(-withheld,-possibly_sensitive, -cashtags) %>% 
    rename("id_original" = "id")
  
  data_R_replies <- unnest(data_R_full, referenced_tweets) %>%
    rename("id_referenced_tweets"  = "id") %>% 
    select(id_original) %>% 
    pull
  
  `%!in%` <- Negate(`%in%`)
  
  data_R_full <- data_R_full %>%
    filter(id_original %!in% data_R_replies)
  
  data_R_full <- unnest(data_R_full, media_keys)
  
  return(data_R_full) 
}

clean_data_daily_Python <- function(data_daily_Python){
  data_daily_Python %>% filter(!grepl("^@", text) & !grepl("^RT", text))
}

#-------------------------------- Define Loops --------------------------------#

# Define a list of months
months <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# Initialize empty lists to store the results
data_master_list <- list()

# Loop over the months
for (month in 1:12) {
  
  # Load twitter data from AcademicTwitter R
  df_R <- list.files(path = paste0("./data_AcademicTwitter_backup/fren/2022/"), pattern = ".rds$", full.names = TRUE) %>% 
    map_dfr(readRDS)
  
  # Load twitter data from tweepy Python
  df_new <- list.files(path = paste0("./data_Tweepy_backup/fren/2022/"), pattern = ".rds$", full.names = TRUE) %>% 
    map_dfr(readRDS)
  
  # clean python data
  data_month_Python <- df_monthly(df_new, month)
  data_month_Python <- data_month_Python %>% 
    filter(!is.na(url_image)) %>% 
    select(text, attachments, url_image) %>% distinct()
  data_Python_full <- clean_data_daily_Python(data_month_Python) 
  data_Python_full <- data_Python_full %>% select(attachments, url_image)
  
  # Prepare R data
  data_month_R <- df_monthly(df_R, month)
  data_R_full <- get_data_daily_R(df_R) %>% unnest(urls) %>%
    mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>%
    filter(condition == 1) %>%
    distinct(display_url, .keep_all = TRUE)
  
  # join datasets
  data_master <- data_R_full %>% 
    left_join(data_Python_full, by = c("media_keys"="attachments")) %>% distinct(url_image, .keep_all = TRUE)


  data_master_list[[month]] <- data_master
}

for(i in seq_along(data_master_list)){
  # # Rename the list elements with the month names
  month_names <- c("January", "February", "March", "April", "May", "June", "July",
                   "August", "September", "October", "November", "December")

  names(data_master_list) <- month_names
    # Unlist the list into a data frame
  df <- bind_rows(data_master_list, .id = "id")
    # Split the data frame based on id
  df_list <- split(df, df$id)
}

# Loop over unique IDs in the df
for (id in unique(df$id)) {
  # Subset the data for the current ID
  df_subset <- df[df$id == id, ]
  # Save the subsetted data as an object with the name of the current ID
  assign(paste0("data_master_", id), df_subset)
  saveRDS(df_subset, file = paste0("./data/fre_data/Full_data_2022_validated/data_master_", id, ".rds"))
}








































