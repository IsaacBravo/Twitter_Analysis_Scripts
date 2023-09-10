load_libraries <- function() {
  libraries <- c("tidyEmoji", "rtweet", "readxl", "lubridate", "forcats", "stringr", 
                 "dplyr", "purrr", "readr", "tidyr", "tibble", "ggplot2", "tidyverse",
                 "tm", "NLP", "sentimentr", "cleanNLP")
  
  # Load packages and suppress startup messages
  suppressPackageStartupMessages({
    lapply(libraries, library, character.only = TRUE)
  })
  
  cat("Libraries loaded!\n")
}


importRDS <- function(x){
  data <- paste0("C:/Users/isaac/OneDrive/Escritorio/Twitter_data/data_replies_process/df_", x, "_translated.RDS")
  data <- readRDS(data)
  return(data)
  
  cat("Data loaded!\n")
}

exportxlsx <- function(x, data){
  path_export <- paste0("./data_export/df_", x, "_pysentiment.xlsx")
  openxlsx::write.xlsx(data, path_export)
  
  cat("Data exported!\n")
}

collapse_emojis <- function(emojis_list) {
  collapsed_string <- paste(emojis_list, collapse = " ")
  return(collapsed_string)
}


emoji_extract_unnest_exp <- function(tweet_tbl, tweet_text){
  tweet_tbl %>%
    emoji_extract_nest({{ tweet_text }}) %>%
    dplyr::select({{ tweet_text }}, .emoji_unicode) %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    tidyr::unnest(.emoji_unicode) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(row_number) %>%
    dplyr::summarize(collapsed_emojis = collapse_emojis(.emoji_unicode))
}

emoji_collapse_text <- function(tweet_tbl){
  tweet_tbl %>% 
    mutate(ID = row_number()) %>% 
    select(ID, text) %>% 
    emoji_extract_unnest_exp(text)
}

text_pysentiment <- function(tweet_tbl, collapse_emojis_tbl){
  tweet_tbl %>% 
    select(conversation_id, id, created_at, region, translated) %>% 
    mutate(row_number = row_number()) %>% 
    left_join(collapse_emojis_tbl, by = "row_number") %>% 
    select(icon = collapsed_emojis, everything()) %>% 
    mutate(x = paste( translated, icon)) %>% 
    select(id, text = x)
}