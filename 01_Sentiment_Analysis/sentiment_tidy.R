#------------------------------------------------------------------------------
# Topic: Explore Twitter Data
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
source("setup_tidy.R")
`%!in%` <- Negate(`%in%`)

load_libraries()

#------------------------------- Import Data ----------------------------------#

imported_data <- import_multiple_excel_files_eng("english")

# Extract Valence
valence <- imported_data %>% 
  mutate(valence = str_extract(sentiment, "output=([A-Z]+)") %>% str_replace("output=", "")) %>% 
  select(valence)

# Extract probabilities
probas_str <- imported_data %>% 
  select(sentiment) %>% 
  mutate(x = str_match(sentiment, "probas=\\{(.+?)\\}\\)")[, 2]) %>% 
  select(x) %>% 
  separate(x, into = c("x1", "x2", "x3"), sep = ",") 

# Combine Valence and Probabilities
imported_data_sent <- cbind(Valence = valence, probas_str) %>%
  separate(x1, into = c("Label1", "Value1"), sep = ":") %>%
  separate(x2, into = c("Label2", "Value2"), sep = ":") %>%
  separate(x3, into = c("Label3", "Value3"), sep = ":") %>% 
  select(valence:Value3) %>% 
  mutate(ID = row_number())

df_1 <- imported_data_sent %>% select(ID, Label1, Value1)
df_2 <- imported_data_sent %>% select(ID, Label1 = Label2, Value1 = Value2)
df_3 <- imported_data_sent %>% select(ID, Label1 = Label3, Value1 = Value3)

final_df <- rbind(df_1, df_2, df_3) %>% 
  mutate(Label1 = trimws(Label1)) %>% 
  mutate(Value1 = as.numeric(Value1)) %>% 
  group_by(ID, Label1) %>% 
  summarise(Value1 = sum(Value1)) %>%
  spread(key = Label1, value = Value1, fill = 0) %>% 
  ungroup() %>% 
  select(-ID)

imported_data_sent <- cbind(Valence = valence, final_df %>% select(1:3))

# Select original data frame:
x <- "english" # arabic, chinese, russian, french, german, spanish, english

# data_import <- importRDS(x)

data_import <- readRDS("C:/Users/isaac/OneDrive/Escritorio/Twitter_data/data_replies_process/manual_translation/df_x_english.RDS")

# Example usage:
# processed_df <- process_data2(data_import, imported_data_sent)

processed_df <- cbind(data_import, imported_data_sent)

# Note: English data no translated does not have the column region. Add later!!!
#  
#------------------------------- Export Data ----------------------------------#

exportRDS(x, processed_df)


#------------------------------------------------------------------------------#
#------------------------- Compile Sentiment by Tweet -------------------------#
#------------------------------------------------------------------------------#

data_import_processed <- importRDS_processed(x)

data_import_processed %>% colnames

data <- processed_df %>% 
  select(conversation_id, id, NEG, NEU, POS) %>% 
  tidyr::gather(key = Variable, value = Value,
                -conversation_id, -id) %>% 
  mutate(Value = as.numeric(Value)) %>%
  ungroup() %>%
  select(conversation_id, Variable, Value) %>% 
  group_by(conversation_id, Variable) %>% 
  summarize(Value = round(mean(as.numeric(Value)), 2)) %>% 
  ungroup() 

data %>% filter(id == "1120457284268756992")

filtered_df <- data %>% select(-id) %>%
  group_by(conversation_id) %>%
  filter(n() == 3) %>% ungroup() 

# filtered_df_x <- data %>% select(-id) %>%
#   group_by(conversation_id) %>%
#   filter(n() == 2) %>% ungroup() 
# rm(filtered_df_x)

filtered_df %>% filter(conversation_id == "1162870492862324736")

filtered_list <- filtered_df$conversation_id %>% unique

data <- data %>% 
  filter(conversation_id %!in% filtered_list) %>%
  ungroup() %>%
  select(conversation_id, Variable, Value) %>% 
  group_by(conversation_id, Variable) %>% 
  summarize(Value = round(mean(as.numeric(Value)), 2)) %>% 
  ungroup() 

data %>% select(conversation_id) %>% 
  group_by(conversation_id) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(5)


data_list <- data$conversation_id %>% unique


compiled_sentiment <- rbind(filtered_df, data)
# compiled_sentiment <- spread(compiled_sentiment, key = Variable, value = Value,
#          fill  = 0)

compiled_sentiment <- spread(data, key = Variable, value = Value, fill = 0)

compiled_sentiment %>% select(conversation_id) %>% 
  group_by(conversation_id) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  head(5)


compiled_sentiment_top <- processed_df %>% 
  group_by(conversation_id) %>% 
  slice(which.max(Value)) %>% 
  select(-Value)
  
compiled_sentiment <- cbind(compiled_sentiment, compiled_sentiment_top[, 2]) %>% 
  rename("valence" = "Variable")



# data_compiled <- compile_sentiment(data_import_processed)

#------------------------------- Export Data ----------------------------------#

exportRDS_compiled(x, compiled_sentiment)

#------------------------------- Clean Workspace ------------------------------#

# List all objects in the workfrece
all_objects <- ls()

# Select objects with names that match the pattern "abc"
abc_objects <- all_objects[grep("valence|collapse", all_objects)]

# Remove the selected objects
rm(list = abc_objects)
