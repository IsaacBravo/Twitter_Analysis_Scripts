#------------------------------------------------------------------------------
# Topic: Explore Twitter Data
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
source("setup_tidy_emotion.R")
`%!in%` <- Negate(`%in%`)

load_libraries()

#------------------------------- Import Data ----------------------------------#

imported_data <- import_multiple_excel_files("spanish")

# Extract Emotion
Top_Emo <- imported_data %>% 
  mutate(emotion = sub(".*output=([^,]+),.*", "\\1", emotion)) %>% 
  select(emotion) 

str(Top_Emo)

# Extract probabilities
probas_str <- imported_data %>% 
  select(emotion) %>% 
  mutate(x = str_match(emotion, "probas=\\{(.+?)\\}\\)")[, 2]) %>% 
  select(x) %>% 
  separate(x, into = c("x1", "x2", "x3", "x4", "x5", "x6", "x7"), sep = ",")  %>% 
  as.data.frame()

# Combine Valence and Probabilities
imported_data_emo <- cbind(Top_Emo, probas_str) %>%
  mutate(ID = row_number()) %>% 
  separate(x1, into = c("Label1", "Value1"), sep = ":") %>%
  separate(x2, into = c("Label2", "Value2"), sep = ":") %>%
  separate(x3, into = c("Label3", "Value3"), sep = ":") %>% 
  separate(x4, into = c("Label4", "Value4"), sep = ":") %>% 
  separate(x5, into = c("Label5", "Value5"), sep = ":") %>% 
  separate(x6, into = c("Label6", "Value6"), sep = ":") %>% 
  separate(x7, into = c("Label7", "Value7"), sep = ":") %>% 
  select(ID, emotion:Value7) 

df_1 <- imported_data_emo %>% select(ID, Label1, Value1)
df_2 <- imported_data_emo %>% select(ID, Label1 = Label2, Value1 = Value2)
df_3 <- imported_data_emo %>% select(ID, Label1 = Label3, Value1 = Value3)
df_4 <- imported_data_emo %>% select(ID, Label1 = Label4, Value1 = Value4)
df_5 <- imported_data_emo %>% select(ID, Label1 = Label5, Value1 = Value5)
df_6 <- imported_data_emo %>% select(ID, Label1 = Label6, Value1 = Value6)
df_7 <- imported_data_emo %>% select(ID, Label1 = Label7, Value1 = Value7)



final_df <- rbind(df_1, df_2, df_3, df_4, df_5, df_6, df_7) %>% 
  mutate(Label1 = trimws(Label1)) %>% 
  mutate(Value1 = as.numeric(Value1)) %>% 
  group_by(ID, Label1) %>% 
  summarise(Value1 = sum(Value1)) %>%
  spread(key = Label1, value = Value1, fill = 0) %>% 
  ungroup() %>% 
  select(-ID)

imported_data_emo <- cbind(Top_Emo, final_df)

# Select original data frame:
x <- "spanish" # arabic, chinese, russian, french, german, spanish, english

data_import <- importRDS(x)

# data_import <- readRDS("C:/Users/isaac/OneDrive/Escritorio/Twitter_data/data_replies_process/manual_translation/df_x_english.RDS")

processed_df <- cbind(data_import, imported_data_emo)


#------------------------------- Export Data ----------------------------------#

exportRDS(x, processed_df)


#------------------------------------------------------------------------------#
#------------------------- Compile Sentiment by Tweet -------------------------#
#------------------------------------------------------------------------------#

data_import_processed <- importRDS_processed(x)

data <- data_import_processed %>% 
  select(conversation_id, id, anger:surprise) %>% 
  tidyr::gather(key = Variable, value = Value,
                -conversation_id, -id) %>% 
  mutate(Value = as.numeric(Value)) 

filtered_df <- data %>% select(-id) %>%
  group_by(conversation_id) %>%
  filter(n() == 3) %>% ungroup() 

filtered_list <- filtered_df$conversation_id %>% unique

data <- data %>% 
  filter(conversation_id %!in% filtered_list) %>%  
  ungroup() %>% 
  select(conversation_id, Variable, Value) %>% 
  group_by(conversation_id, Variable) %>% 
  summarize(Value = round(mean(as.numeric(Value)), 2)) %>% 
  ungroup() 

compiled_emotion <- rbind(filtered_df, data)
compiled_emotion <- spread(compiled_emotion, key = Variable, value = Value,
                             fill  = 0)

compiled_emotion_top <- rbind(filtered_df, data) %>% 
  group_by(conversation_id) %>% 
  slice(which.max(Value)) %>% 
  select(-Value)

compiled_emotion <- cbind(compiled_emotion, compiled_emotion_top[, 2]) %>% 
  rename("Top_Emotion" = "Variable")

#------------------------------- Export Data ----------------------------------#

exportRDS_compiled(x, compiled_emotion)

#------------------------------- Clean Workspace ------------------------------#

# List all objects in the workfrece
all_objects <- ls()

# Select objects with names that match the pattern "abc"
abc_objects <- all_objects[grep("valence|collapse", all_objects)]

# Remove the selected objects
rm(list = abc_objects)
