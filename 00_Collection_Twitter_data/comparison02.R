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

set_bearer()
get_bearer()

source("functions.R")

#-------------- Load twitter data from tweepy Python --------------------------#

df <- list.files(path = "./data_jan_2019/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS)

# Getting daily data
data_daily <- df_daily(1)

#-------------- Collecting twitter data from academic twitter R ---------------#

tweets_data <- get_all_tweets(
  query = c("climate change", "climatechange", "#climatechange"),
  bearer_token = get_bearer(),
  # users = NULL,
  has_image = TRUE,
  is_retweet = FALSE,
  # reply_to = NULL,
  # is_reply = NULL,
  file = "climate_data_new",
  data_path = "./",
  start_tweets = "2021-09-25T00:00:00Z",
  end_tweets = "2021-09-25T23:59:59Z",
  n = 15000
)

saveRDS(tweets_data, "./data_2021/tweets_data_2021-09-25.rds")





#-------------- Loading twitter data from academic twitter R ---------------#

tweets_data <- list.files(path = "./data_2019/", pattern = ".rds$", full.names = TRUE) %>%
  map_dfr(readRDS)


tweets_data %>% colnames()
tweets_data$lang %>% table




tweets_data <- readRDS(file = "./data_2020/tweets_data_2019-01-03.rds")

list_img_id <- get_diff(data, df) %>% 
  select(tweet_url) %>% 
  mutate(tweet_url = str_extract(tweet_url, "(?<=status/).+?(?=/photo)")) %>% 
  pull


check_diff(data, df)
a1 <- get_diff(tweets_data, df)

df %>% 
  mutate(attachments_up = sub(".*_", "", df$attachments)) %>% 
  # filter(attachments_up == "1080248607830208512")
  filter(attachments_up %in% list_img_id)

message("Percentage of imagenes not covered by python: ", paste(round(nrow(a1) / nrow(df), 2)*100,"%"))

rm(tweets_data)


#----- Get Differences -------

data <- tweets_data %>%
  mutate(
    date = as.Date(substr(created_at, 1, 10)),
    month = lubridate::month(date),
    day = lubridate::day(date)
  ) %>%
  filter(month == 1)

x <- data %>% unnest(attachments)
x <- unnest(x, entities) %>% 
  select(-withheld,-possibly_sensitive, -cashtags) %>% 
  rename("id_original" = "id")
x <- unnest(x, referenced_tweets) %>% 
  rename("id_referenced_tweets"  = "id")

data_reply_retweet <- x %>% 
  select(id_original, id_referenced_tweets, text_reply_retweet = text, type)

data_final <- data %>% 
  left_join(data_reply_retweet, by = c("id"="id_original")) %>% 
  select(-withheld, -referenced_tweets)

x <- unnest(data_final, entities) %>% 
  select(-annotations, -mentions, -geo, -hashtags, -cashtags) 
x <- unnest(x, urls) %>% 
  mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>% 
  filter(condition == 1)

x %>% filter(grepl("Trump would suggest putting a wall, ... I mean slats, ...  I mean fence around them ... keep that climate invasion out", text))



list_ids <- x %>% filter(is.na(type)) %>% select(id) %>% pull

#------
# Get users who has retweeted a tweet

data_retweet <- get_retweeted_by(
  list_ids[10:20],
  bearer_token = get_bearer(),
  data_path = NULL,
  verbose = TRUE
)

x1 <- x %>% filter(id == "1080247139320487936")

data_reply_retweet %>% filter(id_original == "1080247139320487936")

#------

x1 <- tweets_data$entities 
x1 <- x1 %>% unnest(mentions) %>% select(-start, -end)
x1 <- x1 %>% unnest(annotations)
x1 <- x1  %>% select(-start, -end, -probability) %>% 
  unnest(urls) %>% 
  mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>% 
  filter(condition == 1) %>% 
  distinct(id, url, .keep_all = TRUE)


x1 %>% colnames()

%>% 
  # filter(type !=  "retweeted") %>% 
  unnest(urls) %>% 
  mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>% 
  filter(condition == 1) %>% 
  filter(is.na(type))

y <- x$mentions %>% bind_rows()
rm(y)

x <- unnest(x, urls) %>% 
  mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>% 
  filter(condition == 1)



x <- unnest(x, mentions)


# x <- unnest(data, attachments)
# x <- unnest(x, media_keys)
x <- unnest(x, entities)
x <- unnest(x, urls) %>% 
  mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>% 
  filter(condition == 1)

openxlsx::write.xlsx(x %>% select(created_at, text), file = "data.xlsx")

x1 <- x %>% 
  select(created_at, conversation_id, media_keys, url, lang) %>% 
  left_join(df %>% select(attachments, user_engagement), by = c("media_keys" = "attachments")) %>% 
  filter(is.na(user_engagement)) %>% 
  distinct(media_keys, .keep_all = TRUE) 
  

# https://stackoverflow.com/questions/71401292/tweepy-get-expanded-url-using-paginator-elevated-access-level

x2 <- data %>% filter(month == 1)
x3 <- unnest(x2, attachments)
x3 <- unnest(x3, media_keys) 
x3 <- unnest(x3, entities)
x3 <- unnest(x3, urls) %>% 
  select(-c(6:8, 12:19, 23, 31))
x4 <- x3 %>% filter(!is.na(media_key))
x4 <- x4 %>% distinct(media_key, .keep_all = TRUE) 

x4 %>% filter(day == 1) %>%  select(-public_metrics) %>% View

x4 %>% filter(day == 1) %>%  select(lang) %>% table
data_daily %>%  select(lang) %>% table

x4 %>% dim()
data_daily %>% dim()
nrow(x4 %>% filter(day == 1)) - nrow(data_daily)
# 1462 - 1340
# 122

data_daily %>% select(text) %>% unique() %>% count() # 1338
x4 %>% filter(day == 1) %>% select(text) %>% unique() %>% count() #1306 

x4 %>% filter(day == 1) %>% select(id, text) %>% distinct(text, .keep_all = TRUE) # 1296
nrow(x4 %>% filter(day == 1)) # 1462

1462-1296

166/1462

tweets_dup <- x4 %>% filter(day == 1) %>% select(id, text) %>% 
  filter(duplicated(text)) %>% distinct(text, .keep_all = TRUE) #%>% view

data_daily %>% select(text) %>% 
  left_join(tweets_dup, by ="text") %>% 
  filter(!is.na(id)) %>% view # 78 tweets with more than one photo
  




nrow(x4) - nrow(df)

data_daily %>% colnames()

x4 %>% colnames()
x4 %>% View()
%>% 
  mutate(condition = ifelse(grepl("https://twitter.com/", expanded_url), 1,0)) %>% 
  filter(condition == 1) %>% 
  distinct(media_keys, .keep_all = TRUE) 

openxlsx::write.xlsx(x4 %>% select(-geo, -annotations, -public_metrics), file = "academictwitter.xlsx")
openxlsx::write.xlsx(data_daily, file = "python.xlsx")


duplicated(x2$media_keys) %>% table


x2$lang %>% table


x1$lang %>% table

rm(stake)
rm(a1, a2, a3)


tweets_data %>% colnames()
tweets_data$entities$annotations
tweets_data$entities$mentions
tweets_data$attachments

