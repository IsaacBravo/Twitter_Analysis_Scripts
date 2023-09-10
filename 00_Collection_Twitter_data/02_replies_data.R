#------------------------------------------------------------------------------
# Topic: Getting replies from twitter data
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
library(academictwitteR)
library(tidyverse)
library(purrr)
library(tidyr)

#-------------- Load twitter data from tweepy Python --------------------------#
df_list_2022_eng <- list.files(path = "./data/eng_data/Full_data_2022_validated/data_replies/", pattern = "^list_conversation_id_", full.names = TRUE) %>%
  map_dfr(readRDS)

df_list_2019_spa <- list.files(path = "./data/spa_data/Full_data_2019_validated/data_replies", pattern = "^list_conversation_id_", full.names = TRUE) %>%
  map_dfr(readRDS)
df_list_2020_spa <- list.files(path = "./data/spa_data/Full_data_2020_validated/data_replies", pattern = "^list_conversation_id_", full.names = TRUE) %>%
  map_dfr(readRDS)
df_list_2022_spa <- list.files(path = "./data/spa_data/Full_data_2022_validated/data_replies", pattern = "^list_conversation_id_", full.names = TRUE) %>%
  map_dfr(readRDS)
df_list_2022_spa <- list.files(path = "./data/spa_data/Full_data_2022_validated/data_replies", pattern = "^list_conversation_id_", full.names = TRUE) %>%
  map_dfr(readRDS)

df_list_2019_ger <- list.files(path = "./data/ger_data/Full_data_2019_validated/data_replies", pattern = "^list_conversation_id_", full.names = TRUE) %>%
  map_dfr(readRDS)
df_list_2020_ger <- list.files(path = "./data/ger_data/Full_data_2020_validated/data_replies", pattern = "^list_conversation_id_", full.names = TRUE) %>%
  map_dfr(readRDS)
df_list_2022_ger <- list.files(path = "./data/ger_data/Full_data_2022_validated/data_replies", pattern = "^list_conversation_id_", full.names = TRUE) %>%
  map_dfr(readRDS)
df_list_2022_ger <- list.files(path = "./data/ger_data/Full_data_2022_validated/data_replies", pattern = "^list_conversation_id_", full.names = TRUE) %>%
  map_dfr(readRDS)


df_list_2022_fren <- list.files(path = "./data/eng_data/Full_data_2022_validated/data_replies/tweets_with_replies_list", pattern = "^list_conversation_id_", full.names = TRUE) %>% map_dfr(readRDS)

df_list_2022_fren <- list.files(path = "./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_lis/", pattern = "^list_conversation_id_", full.names = TRUE) %>%
  map_dfr( ~{
    # Load the RDS file into a temporary data frame
    temp_df <- readRDS(.x)
    
    # Get the file name without the '.rds' extension
    file_name <- gsub(".data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_", "", .x)
    file_name <- gsub(".rds", "", file_name) 
    # Add the file name as an ID column in the data frame
    temp_df$month <- file_name 
    temp_df <- temp_df %>% 
      mutate(month = case_when(
        month == "january" ~ 1,
        month == "february" ~ 2,
        month == "march" ~ 3,
        month == "april" ~ 4,
        month == "may" ~ 5,
        month == "june" ~ 6,
        month == "july" ~ 7,
        month == "august" ~ 8,
        month == "september" ~ 9,
        month == "october" ~ 10,
        month == "november" ~ 11,
        month == "december" ~ 12,
        TRUE ~ 0
        ))
    
    return(temp_df)
  })


data_reply <- df_list_2019_spa %>% filter(month %in% c(1:7))

list_conversation_id_january <- list_conversation_id_january %>% pull # check again!
list_conversation_id_february <- list_conversation_id_february %>% pull # check again!
list_conversation_id_march <- list_conversation_id_march %>%  pull # done!
list_conversation_id_april <- list_conversation_id_april %>%  pull # done!
list_conversation_id_may <- list_conversation_id_may %>%  pull # done!
list_conversation_id_june <- list_conversation_id_june %>%  pull # done!
list_conversation_id_july <- list_conversation_id_july %>%  pull # done!
list_conversation_id_august <- list_conversation_id_august %>%  pull # done!
list_conversation_id_september <- list_conversation_id_september %>%  pull # working

data_list <- rbind(list_conversation_id_april, list_conversation_id_august, list_conversation_id_december,
                   list_conversation_id_february, list_conversation_id_january, list_conversation_id_july,
                   list_conversation_id_june, list_conversation_id_march, list_conversation_id_may,
                   list_conversation_id_november, list_conversation_id_october, list_conversation_id_september) %>% pull

data <- list()

for (i in 1:length(data_list)) {
  tryCatch({
    data[[i]] <- get_all_tweets(start_tweets = "2022-01-01T00:00:00Z",
                                             end_tweets = "2023-02-01T00:00:00Z",
                                             bearer_token = get_bearer(),
                                             n = 250000,
                                             conversation_id = data_list[i])
    
    print(paste("Number:", i, "Resting:", length(data_list)-i))
    Sys.sleep(1)
  }, error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
}

# Filter non-empty data frames
filtered_list <- Filter(function(df) nrow(df) > 0, data)
filtered_list <- Filter(function(df) nrow(df) > 0, filtered_list)

data_filtered <- filtered_list %>% map(rownames_to_column) %>% reduce(full_join)

saveRDS(data_filtered, "./data_reply_full_2022_fre.rds")

data_filtered <- data_filtered %>% 
  unnest(public_metrics) %>% 
  unnest(entities) %>% 
  unnest(attachments)

data_filtered %>% colnames


list_conversation_id_january <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_January.rds") %>% pull
list_conversation_id_february <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_February.rds") %>% pull
list_conversation_id_march <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_March.rds") %>% pull
list_conversation_id_april <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_April.rds") %>% pull
list_conversation_id_may <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_May.rds") %>% pull
list_conversation_id_june <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_June.rds") %>% pull
list_conversation_id_july <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_July.rds") %>% pull
list_conversation_id_august <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_August.rds") %>% pull
list_conversation_id_september <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_September.rds") %>% pull
list_conversation_id_october <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_October.rds") %>% pull
list_conversation_id_november <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_November.rds") %>% pull
list_conversation_id_december <- readRDS("./data/fre_data/Full_data_2022_validated/data_replies/tweets_with_replies_list/list_conversation_id_December.rds") %>% pull

data_reply_full_2022_january <- data_filtered %>% filter(conversation_id %in% list_conversation_id_january)
data_reply_full_2022_february <- data_filtered %>% filter(conversation_id %in% list_conversation_id_february)
data_reply_full_2022_march <- data_filtered %>% filter(conversation_id %in% list_conversation_id_march)
data_reply_full_2022_april <- data_filtered %>% filter(conversation_id %in% list_conversation_id_april)
data_reply_full_2022_may <- data_filtered %>% filter(conversation_id %in% list_conversation_id_may)
data_reply_full_2022_june <- data_filtered %>% filter(conversation_id %in% list_conversation_id_june)
data_reply_full_2022_july <- data_filtered %>% filter(conversation_id %in% list_conversation_id_july)
data_reply_full_2022_august <- data_filtered %>% filter(conversation_id %in% list_conversation_id_august)
data_reply_full_2022_september <- data_filtered %>% filter(conversation_id %in% list_conversation_id_september)
data_reply_full_2022_october <- data_filtered %>% filter(conversation_id %in% list_conversation_id_october)
data_reply_full_2022_november <- data_filtered %>% filter(conversation_id %in% list_conversation_id_november)
data_reply_full_2022_december <- data_filtered %>% filter(conversation_id %in% list_conversation_id_december)

saveRDS(data_reply_full_2022_january, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_january.rds")
saveRDS(data_reply_full_2022_february, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_february.rds")
saveRDS(data_reply_full_2022_march, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_march.rds")
saveRDS(data_reply_full_2022_april, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_april.rds")
saveRDS(data_reply_full_2022_may, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_may.rds")
saveRDS(data_reply_full_2022_june, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_june.rds")
saveRDS(data_reply_full_2022_july, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_july.rds")
saveRDS(data_reply_full_2022_august, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_august.rds")
saveRDS(data_reply_full_2022_september, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_september.rds")
saveRDS(data_reply_full_2022_october, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_october.rds")
saveRDS(data_reply_full_2022_november, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_november.rds")
saveRDS(data_reply_full_2022_december, "./reply_data/fre/data_reply_2022/data_replies/data_reply_full_december.rds")




data_reply_full_october_untidy_part2 <- readRDS("C:/Users/isaac/OneDrive/Escritorio/extranting data/data_reply_full_october_untidy_part2.rds")
data_reply_full_october_part2 <- data_reply_full_october_untidy_part2 %>% map(rownames_to_column) %>% reduce(full_join)

data_reply_full_2019_ger_untidy <- readRDS("C:/Users/isaac/OneDrive/Escritorio/extranting data/data_reply_full_2019_ger_untidy.rds")
data_reply_full_2019_ger <- data_reply_full_2019_ger_untidy %>% map(rownames_to_column) %>% reduce(full_join)



saveRDS(data_reply_full_october_part2, "./data_reply_full_october_part2.rds")





saveRDS(list_conversation_id_july_part1, "./Full_data_2019_validated/data_reply_full_july_part1.rds")
saveRDS(list_conversation_id_july_part2, "./Full_data_2019_validated/data_reply_full_july_part2.rds")
saveRDS(list_conversation_id_july_part3, "./Full_data_2019_validated/data_reply_full_july_part3.rds")
saveRDS(list_conversation_id_july_part4, "./Full_data_2019_validated/data_reply_full_july_part4.rds")


saveRDS(data_reply_full_june_1_1000, "./Full_data_2019_validated/data_reply_full_june_part1.rds")
saveRDS(data_reply_full_june_2001_6000, "./Full_data_2019_validated/data_reply_full_june_part2.rds")
saveRDS(data_reply_full_june_1001_2000, "./Full_data_2019_validated/data_reply_full_june_part3.rds")
saveRDS(data_reply_full_june_6001_6794, "./Full_data_2019_validated/data_reply_full_june_part4.rds")
saveRDS(data_reply_full_june_6795_9430, "./Full_data_2019_validated/data_reply_full_june_part5.rds")



saveRDS(data_reply_full_may_1_3302, "./Full_data_2019_validated/data_reply_full_may_part1.rds")
saveRDS(data_reply_full_may_3302_7000, "./Full_data_2019_validated/data_reply_full_may_part2.rds")
saveRDS(data_reply_full_may_7001_7912, "./Full_data_2019_validated/data_reply_full_may_part3.rds")
saveRDS(data_reply_full_may_7014_9156, "./Full_data_2019_validated/data_reply_full_may_part3.rds")
saveRDS(data_reply_full_april_1_3500, "./Full_data_2019_validated/data_reply_full_april_part1.rds")
saveRDS(data_reply_full_april_3501_7000, "./Full_data_2019_validated/data_reply_full_april_part2.rds")
saveRDS(data_reply_full_april_7001_9714, "./Full_data_2019_validated/data_reply_full_april_part3.rds")
saveRDS(data_reply_full_march_1_3500, "./Full_data_2019_validated/data_reply_full_march_part1.rds")
saveRDS(data_reply_full_march_3501_7000, "./Full_data_2019_validated/data_reply_full_march_part2.rds")
saveRDS(data_reply_full_march_7001_9301, "./Full_data_2019_validated/data_reply_full_march_part3.rds")

saveRDS(data_reply_full_february, "./Full_data_2019_validated/data_reply_full_february.rds")
saveRDS(data_reply_full_january, "./Full_data_2019_validated/data_reply_full_january.rds")





