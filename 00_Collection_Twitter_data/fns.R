add_dates <- function(data){
  data %>% 
    mutate(month = lubridate::month(date),
           day = lubridate::day(date), 
           year = lubridate::year(date)) 
}

select_sample <- function(data){
  
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
  data_R_full <- data_R_full %>% 
    select(conversation_id, text, created_at) %>% 
    filter(!grepl("^@", text) & !grepl("^RT", text)) %>% 
    mutate(date = lubridate::date(created_at), 
           year = lubridate::year(created_at)) 
  
  return(data_R_full) 
}