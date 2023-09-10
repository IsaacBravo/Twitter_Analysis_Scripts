python_api <- readRDS("~/Isaac_Files/Emmy_Noether_project/sample/data/archive/twitterClimateScrape_2019-01-28_2019-01-29.rds")
academic_api <- load("~/Isaac_Files/Emmy_Noether_project/sample/df.RData")

library(dplyr)
library(tidyverse)

python_api_up <- python_api %>% 
  as.data.frame() %>% 
  # filter(link_type == "photo") %>% 
  filter(grepl("RT", text) == FALSE) %>% 
  filter(!is.na(link_type))

python_api_up$link_type %>% table

tweets_data_up <- tweets_data %>% 
  filter(grepl("RT", text) == FALSE) %>% 
  bind_rows(.id = "urls") %>% 
  select(created_at, id, text, entities) 
 
a1 <- tweets_data_up %>% 
  bind_rows(tweets_data_up$entities$urls, .id = "id") %>% 
  filter(created_at != "NULL") 




tweets_data_up %>% map(unlist) %>% as_tibble()

# %>%
  unnest_wider(urls) %>%
  unnest_wider(urls)

tweets_data_up <- tweets_data_up %>% 
  mutate(data = map(entities, ~select(.x, "urls")))


df %>%
  nest(B) %>%
  mutate(data = map(data, ~df2),
         data = map(data, ~select(.x, "C") ) )

tweets_data

z1 <- tweets_data %>% select(id, text, created_at, entities)

x1 <- tweets_data$entities$urls  %>% 
  bind_rows(.id = "id") %>% 
  filter(images != "NULL") %>% 
  distinct(description, .keep_all = TRUE) 


a1 <- tidyr::unnest_wider(tweets_data_up, entities)

colnames(tweets_data)


tibble(
  id = tweets_data$id,
  text = tweets_data$text,
  date = tweets_data$created_at,
  urls = tweets_data$entities
) %>% 
  mutate(urls = invoke_map(tibble, urls)) %>% 
  unnest()


column_images <- table_images %>%  filter(images != "NULL") 


images <- column_images$images %>% 
  bind_rows(.id = "images") %>% mutate(
    condition = ifelse(width == 150 & height == 150, "small", "original")
  ) %>% filter(condition == "original", keep.all = TRUE) %>% 
  select(url) %>% unlist(use.names = FALSE)
