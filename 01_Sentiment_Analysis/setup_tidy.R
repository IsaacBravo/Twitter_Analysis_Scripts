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

import_multiple_excel_files <- function(region, pattern = "\\.xlsx$") {
  folder_path <- paste0("./02_data_sentiment/", region)
  
  excel_files <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)
  imported_data <- map(excel_files, read_excel)
  combined_data <- bind_rows(imported_data)
  return(combined_data)
}

import_multiple_excel_files_eng <- function(pattern = "\\.xlsx$") {
  folder_path <- paste0("./02_data_sentiment/sentiment_no_translated_files/")
  
  excel_files <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)
  imported_data <- map(excel_files, read_excel)
  combined_data <- bind_rows(imported_data)
  return(combined_data)
}



split_sentiment <- function(x){
  # Extract Valence
  valence <- str_extract(x, "output=([A-Z]+)") %>% str_replace("output=", "")
  
  # Extract probabilities
  probas_str <- str_match(x, "probas=\\{(.+?)\\}\\)")[, 2]
  probas <- str_extract_all(probas_str, "\\w+: \\d+\\.\\d+") %>% unlist() %>%
    str_extract_all("(\\w+): (\\d+\\.\\d+)") %>%
    as.data.frame(stringsAsFactors = FALSE)
  colnames(probas) <- c("x1", "x2", "x3")
  final <- cbind(valence, probas) 
  return(final)
}

process_data <- function(data_import, imported_data_sent) {
  library(dplyr)
  library(tidyr)
  
  df <- cbind(data_import, imported_data_sent) %>%
    select(conversation_id, id, created_at, region, text = translated, valence:Value3)
  
  df_1 <- df %>%
    select(conversation_id, id, created_at, region, text, valence:Value1)
  df_2 <- df %>%
    select(conversation_id, id, created_at, region, text, valence, Label1 = Label2, Value1 = Value2)
  df_3 <- df %>%
    select(conversation_id, id, created_at, region, text, valence, Label1 = Label3, Value1 = Value3)
  
  final_df <- rbind(df_1, df_2, df_3) %>%
    group_by(id) %>%
    spread(key = Label1, value = Value1, fill = 0)
  
  return(final_df)
}

process_data2 <- function(data_import, imported_data_sent) {
  library(dplyr)
  library(tidyr)
  
  df <- cbind(data_import, imported_data_sent) %>%
    select(conversation_id, id, created_at, text = text_clean, valence:Value3)
  
  df_1 <- df %>%
    select(conversation_id, id, created_at, text, valence:Value1)
  df_2 <- df %>%
    select(conversation_id, id, created_at, text, valence, Label1 = Label2, Value1 = Value2)
  df_3 <- df %>%
    select(conversation_id, id, created_at, text, valence, Label1 = Label3, Value1 = Value3)
  
  final_df <- rbind(df_1, df_2, df_3) %>%
    group_by(id) %>%
    spread(key = Label1, value = Value1, fill = 0)
  
  return(final_df)
}



exportRDS <- function(x, data){
  path_export <- paste0("./04_data_compiled/df_", x, "_pysentiment.RDS")
  saveRDS(data, path_export)
  
  cat("Data exported!\n")
}

importRDS_processed <- function(x){
  data <- paste0("C:/Users/isaac/OneDrive/Escritorio/Twitter_data/vader_analysis/04_data_compiled/df_", x, "_pysentiment.RDS")
  data <- readRDS(data)
  return(data)
  
  cat("Data loaded!\n")
}

importRDS <- function(x){
  data <- paste0("C:/Users/isaac/OneDrive/Escritorio/Twitter_data/data_replies_process/df_translated.RDS/df_", x, "_translated.RDS")
  data <- readRDS(data)
  return(data)
  
  cat("Data loaded!\n")
}

compile_sentiment <- function(x){
  
  data <- x %>% select(-valence) %>% 
    tidyr::gather(key = Variable, value = Value,
                  -conversation_id, -id, -created_at, -region, -text) %>% 
    mutate(Value = as.numeric(Value))
  
  filtered_df <- data %>% group_by(conversation_id) %>%
    filter(n() == 3) %>% ungroup()
  
  filtered_list <- filtered_df$conversation_id %>% unique()
  
  data <- data %>% 
    filter(conversation_id %!in% filtered_list) %>%  
    ungroup() %>% 
    select(conversation_id, Variable, Value) %>% 
    group_by(conversation_id, Variable) %>% 
    summarize(mean = round(mean(as.numeric(Value)), 2)) %>% 
    ungroup()
  
  
  compiled_sentiment <- rbind(filtered_df %>% 
                                select(conversation_id, Variable, mean = Value), data) %>% 
    spread(key   = Variable,
           value = mean,
           fill  = 0)
  
  
  compiled_sentiment_top <- rbind(filtered_df %>% 
                                    select(conversation_id, Variable, mean = Value), data) %>% 
    group_by(conversation_id) %>% 
    slice(which.max(mean)) %>% 
    select(-mean)
  
  compiled_sentiment <- cbind(compiled_sentiment, compiled_sentiment_top[, 2]) %>% 
    rename("valence" = "Variable")
  
  return(compiled_sentiment)
}

exportRDS_compiled <- function(x, data){
  path_export <- paste0("./04_data_compiled/df_", x, "_pysentiment_compiled.RDS")
  saveRDS(data, path_export)
  
  cat("Data exported!\n")
}

compile_sentiment2 <- function(x){
  
  data <- x %>% select(-valence) %>% 
    tidyr::gather(key = Variable, value = Value,
                  -conversation_id, -id, -created_at, -text, -text_clean) %>% 
    mutate(Value = as.numeric(Value))
  
  filtered_df <- data %>% group_by(conversation_id) %>%
    filter(n() == 3) %>% ungroup()
  
  filtered_list <- filtered_df$conversation_id %>% unique()
  
  data <- data %>% 
    filter(conversation_id %!in% filtered_list) %>%  
    ungroup() %>% 
    select(conversation_id, Variable, Value) %>% 
    group_by(conversation_id, Variable) %>% 
    summarize(mean = round(mean(as.numeric(Value)), 2)) %>% 
    ungroup()
  
  
  compiled_sentiment <- rbind(filtered_df %>% 
                                select(conversation_id, Variable, mean = Value), data) %>% 
    spread(key   = Variable,
           value = mean,
           fill  = 0)
  
  
  compiled_sentiment_top <- rbind(filtered_df %>% 
                                    select(conversation_id, Variable, mean = Value), data) %>% 
    group_by(conversation_id) %>% 
    slice(which.max(mean)) %>% 
    select(-mean)
  
  compiled_sentiment <- cbind(compiled_sentiment, compiled_sentiment_top[, 2]) %>% 
    rename("valence" = "Variable")
  
  return(compiled_sentiment)
}
