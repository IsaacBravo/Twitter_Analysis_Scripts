#------------------------------------------------------------------------------
# Topic: Checking replies status
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
library(academictwitteR)
library(tidyverse)
library(purrr)
library(tidyr)

df_list_2022_fre <- list.files(path = "./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list", 
                               pattern = "december.rds$",  
                               full.names = TRUE) %>%
  map_dfr(readRDS)


df_list_2022_fre_december <- readRDS("./reply_data/fre/data_reply_2022/data_replies/data_reply_full_december.rds")

id_list <- df_list_2022_fre_december %>% select(conversation_id) %>% distinct() %>% pull()

df_list_2022_fre <- df_list_2022_fre %>% 
  mutate(check = ifelse(conversation_id %in% id_list, "1", "0"))

df_list_2022_fre$check %>% table


df_list_2022_fre_miss <- df_list_2022_fre %>% 
  filter(check == "0") %>%
  select(conversation_id) %>% 
  pull()

data_december <- list()


for (i in 1:length(df_list_2022_fre_miss)) {
  tryCatch({
    data_december[[i]] <- get_all_tweets(start_tweets = "2022-12-01T00:00:00Z",
                                        end_tweets = "2023-02-01T00:00:00Z",
                                        bearer_token = get_bearer(),
                                        n = 250000,
                                        conversation_id = df_list_2022_fre_miss[i])
    
    print(paste("Number:", i, "Resting:", length(df_list_2022_fre_miss)-i))
    Sys.sleep(1)
  }, error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
}


# Filter non-empty data frames
filtered_list <- Filter(function(df) nrow(df) > 0, data_december)
filtered_list <- Filter(function(df) nrow(df) > 0, filtered_list)

data <- filtered_list %>% map(rownames_to_column) %>% reduce(full_join)


data_tidy <- data %>% 
  unnest(public_metrics) %>% 
  unnest(entities) %>%
  unnest(geo) %>%
  unnest(attachments) %>%
  # select(-impression_count) %>%
  mutate(
    place_id =NA,
    annotations=NA,
    copyright=NA,
    media_keys=NA,
    cashtags = NA,
    hashtags = NA,
    # coordinates = NA,
    country_codes=NA,
    poll_ids = NA,
    urls=NA)

data_tidy <- data_tidy[,order(colnames(data_tidy))]
df_list_2022_fre_december <- df_list_2022_fre_december[,order(colnames(df_list_2022_fre_december))]

df_list_2022_fre_december_full <- rbind(df_list_2022_fre_december, data_tidy)

saveRDS(df_list_2022_fre_december, "./reply_data/fre/data_reply_2022/12_December_full_validated.rds")


setdiff(names(df_list_2022_fre_december), names(data_tidy))
setdiff(names(data_tidy), names(df_list_2022_fre_december))




# data_tidy %>% colnames
# df_list_2022_fre_december %>% colnames
# 
# df_list_2022_fre_december <- df_list_2022_fre_december %>% 
#   unnest(entities) %>%
#   unnest(geo) %>%
#   unnest(public_metrics) %>% 
#   unnest(attachments)
# 
# 
# List all objects in the workfrece
all_objects <- ls()

# Select objects with names that match the pattern "abc"
abc_objects <- all_objects[grep("full", all_objects)]

# Remove the selected objects
rm(list = abc_objects)


