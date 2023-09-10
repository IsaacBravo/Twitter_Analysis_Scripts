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
  folder_path <- paste0("./03_data_emotion/emotion_translated_files/", region, "_emotion")
  
  excel_files <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)
  imported_data <- map(excel_files, read_excel)
  combined_data <- bind_rows(imported_data)
  return(combined_data)
}

import_multiple_excel_files_eng <- function(pattern = "\\.xlsx$") {
  folder_path <- paste0("./03_data_emotion/emotion_no_translated_files/")
  
  excel_files <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)
  imported_data <- map(excel_files, read_excel)
  combined_data <- bind_rows(imported_data)
  return(combined_data)
}

exportRDS <- function(x, data){
  path_export <- paste0("./04_data_compiled/emotion_translated_files/df_", x, "_pyemotion.RDS")
  saveRDS(data, path_export)
  
  cat("Data exported!\n")
}

importRDS_processed <- function(x){
  data <- paste0("C:/Users/isaac/OneDrive/Escritorio/Twitter_data/vader_analysis/04_data_compiled/emotion_translated_files/df_", x, "_pyemotion.RDS")
  data <- readRDS(data)
  return(data)
  
  cat("Data loaded!\n")
}

exportRDS_compiled <- function(x, data){
  path_export <- paste0("./04_data_compiled/emotion_translated_files/df_", x, "_pyemotion_compiled.RDS")
  saveRDS(data, path_export)
  
  cat("Data exported!\n")
}

importRDS <- function(x){
  data <- paste0("C:/Users/isaac/OneDrive/Escritorio/Twitter_data/data_replies_process/df_translated.RDS/df_", x, "_translated.RDS")
  data <- readRDS(data)
  return(data)
  
  cat("Data loaded!\n")
}
