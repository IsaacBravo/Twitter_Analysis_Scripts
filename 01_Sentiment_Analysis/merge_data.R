#------------------------------------------------------------------------------
# Topic: Explore Twitter Data
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
source("setup_tidy.R")
`%!in%` <- Negate(`%in%`)

load_libraries()

#------------------------------------------------------------------------------#
#------------------------------- Import Sentiment Data ------------------------#
#------------------------------------------------------------------------------#

df_list_translated <- list.files(path = "./04_data_compiled/sentiment_translated_files", 
                               pattern = "compiled.RDS$",  
                               full.names = TRUE) %>% map_dfr(readRDS)

df_list_no_translated <- readRDS("./04_data_compiled/df_english_pysentiment_compiled.RDS")

df <- rbind(df_list_no_translated, df_list_translated)

df_check_1 <- rbind(
  df_arabic_pysentiment_compiled,
  df_chinese_pysentiment_compiled,
  df_russian_pysentiment_compiled,
  df_french_pysentiment_compiled,
  df_german_pysentiment_compiled,
  df_spanish_pysentiment_compiled,
  df_english_pysentiment_compiled_v1
)

df_list <- df_english_pysentiment_compiled_v2 %>% select(conversation_id) %>% pull

df_check_1 <- df_check_1 %>% 
  filter(conversation_id %!in% df_list) %>% 
  rbind(df_english_pysentiment_compiled_v2)







#------------------------------- Export Sentiment Data ------------------------#

saveRDS(df, "./04_data_compiled/df_sentiment_compiled.RDS")


#------------------------------------------------------------------------------#
#------------------------------- Import Emotion Data --------------------------#
#------------------------------------------------------------------------------#

df_list_translated <- list.files(path = "./04_data_compiled/emotion_translated_files", 
                                 pattern = "compiled.RDS$",  
                                 full.names = TRUE) %>% map_dfr(readRDS)

df_list_no_translated <- readRDS("./04_data_compiled/df_english_pyemotion_compiled.RDS")

df <- rbind(df_list_no_translated, df_list_translated)

#------------------------------- Export Emotion Data --------------------------#

saveRDS(df, "./04_data_compiled/df_emotion_compiled.RDS")


#------------------------------------------------------------------------------#
#------------------------------- Full Merge Data --- --------------------------#
#------------------------------------------------------------------------------#
df_sentiment_compiled <- readRDS("./04_data_compiled/df_sentiment_compiled.RDS")
df_emotion_compiled <- readRDS("./04_data_compiled/df_emotion_compiled.RDS")

df_reaction <- cbind(df_sentiment_compiled, df_emotion_compiled[,2:9]) %>% 
  select(conversation_id, top_valence = valence, top_emotion = Top_Emotion, 
         everything())

#------------------------------- Export Emotion Data --------------------------#

saveRDS(df_reaction, "./04_data_compiled/df_reaction_compiled.RDS")

