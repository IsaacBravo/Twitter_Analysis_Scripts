df_monthly <- function(df, number_month){
  df %>%
    mutate(
      date = as.Date(substr(created_at, 1, 10)),
      month = lubridate::month(date),
      day = lubridate::day(date)) %>%
    filter(month == number_month)
}

df_daily <- function(df, number_day){
  df %>% dplyr::filter(day == number_day)
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

clean_data_annually_Python <- function(data){
  data %>% filter(!grepl("^@", text) & !grepl("^RT", text))
}

get_data_annually_R <- function(data){
  
  data_R <- data %>% filter(!grepl("^@", text) & !grepl("^RT", text))
  
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

process_data <- function(date){
  
  data_daily_R <- df_daily(data_month_R, date)
  data_R_full <- get_data_daily_R(data_daily_R) %>% unnest(urls) %>%
    mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>%
    filter(condition == 1) %>%
    distinct(display_url, .keep_all = TRUE)
  
  #-- clean python data
  
  data_daily_Python <- df_daily(data_month_Python, date)
  data_Python_full <- clean_data_daily_Python(data_daily_Python)
  data_Python_full <- data_Python_full %>% select(created_at, text, attachments, url_image)
  
  # data_Python_full_2019 <- df_new %>% select(created_at, text, attachments, url_image)
  
  #-- join dataset
  
  data_master <- data_R_full %>% 
    left_join(data_Python_full, by = c("media_keys"="attachments")) %>% 
    filter(!is.na(url_image)) %>% distinct(url_image, .keep_all = TRUE)
  
  return(data_master)
}



download_images <- function(data, date) {
  
  list_images <- data %>% 
    select(id_original, date, url_image) %>% 
    mutate(id = paste0(id_original, "_", date))
  
  downloadPath = paste0(getwd(),"/Full_data_2019_img/img_", as.character(date),"/")
  
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
    Sys.sleep(3)
  }
}


download_imagesv2 <- function(data, path) {
  
  list_images <- data %>% 
    select(id_original, date, url_image) %>% 
    mutate(id = paste0(id_original, "_", date))
  
  downloadPath = paste0(getwd(),"/Full_data_2019_img/")
  
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

download_imagesv3 <- function(data) {
  
  list_images <- data %>% 
    select(id_original, date, url_image) %>% 
    mutate(id = paste0(id_original, "_", date))
  
  downloadPath = paste0(getwd(),"./data_IMG_2019_spa/data_IMG_check/")
  
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






# Other functions!

check_diff <- function(tweets_data, data_daily){
  
  a1 <- unnest(tweets_data, entities)
  a2 <- unnest(a1, urls) %>% 
    mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>% 
    filter(condition == 1) %>% 
    distinct(expanded_url, media_key, .keep_all = TRUE) %>% 
    select(created_at, author_id, conversation_id, url, 
           tweet_url = expanded_url, media_key,
           text)
  
  a3 <- a2 %>% 
    left_join(data_daily, by = c("media_key" = "attachments"))
  
  a4 <- a3 %>% filter(is.na(user_engagement)) %>% 
    distinct(tweet_url, .keep_all = TRUE) %>% 
    select(tweet_url) %>% 
    mutate(tweet_url = str_extract(tweet_url, "(?<=status/).+?(?=/photo)")) %>% 
    pull
  
  length(a4)
}

get_diff <- function(tweets_data, data_daily){
  
  a1 <- unnest(tweets_data, entities)
  a2 <- unnest(a1, urls) %>% 
    mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>% 
    filter(condition == 1) %>% 
    distinct(expanded_url, media_key, .keep_all = TRUE) %>% 
    select(created_at, author_id, conversation_id, url, 
           tweet_url = expanded_url, media_key,
           text)
  
  a3 <- a2 %>% 
    left_join(data_daily, by = c("media_key" = "attachments"))
  
  a3 %>% filter(is.na(user_engagement)) %>% 
    distinct(tweet_url, .keep_all = TRUE) 
}

