#------------------------------------------------------------------------------
# Topic: Explore Twitter Data
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
source("setup.R")

load_libraries()

#------------------------------- Import Data ----------------------------------#

# Select data frame:
x <- "german" # arabic, chinese, russian, french, german, spanish, english

data_import <- importRDS(x)

# Extract & Collapse emojis from original tweet
collapse_emojis_tbl <- emoji_collapse_text(data_import)

# Tidy data to import to pysentiment
df_pysentiment <- text_pysentiment(data_import, collapse_emojis_tbl)

# Just run to check NA
df_pysentiment <- df_pysentiment %>% mutate(text = gsub(" NA", "", text))

#------------------------------- Export Data ----------------------------------#

exportxlsx(x, df_pysentiment)


# English dataset (no translated)
data_import <- readRDS("C:/Users/isaac/OneDrive/Escritorio/Twitter_data/data_replies_process/df_full_replies_x_english_clean.RDS")
# Extract & Collapse emojis from original tweet
collapse_emojis_tbl <- emoji_collapse_text(data_import)

emoji_collapse_text_eng <- function(tweet_tbl){
  tweet_tbl %>% 
    mutate(ID = row_number()) %>% 
    select(ID, text) %>% 
    emoji_extract_unnest_exp(text)
}

text_pysentiment_eng <- function(tweet_tbl, collapse_emojis_tbl){
  tweet_tbl %>% 
    select(conversation_id, id, created_at, translated = text_clean) %>% 
    mutate(row_number = row_number()) %>% 
    left_join(collapse_emojis_tbl, by = "row_number") %>% 
    select(icon = collapsed_emojis, everything()) %>% 
    mutate(x = paste( translated, icon)) %>% 
    select(id, text = x)
}

# Tidy data to import to pysentiment
df_pysentiment <- text_pysentiment_eng(data_import, collapse_emojis_tbl)
# Just run to check NA
df_pysentiment <- df_pysentiment %>% mutate(text = gsub(" NA", "", text))


#------------------------- English Data Sentiment -------------------------#

# Set the batch size to 1000 rows
batch_size <- 30000

# Split the dataframe into a list of smaller dataframes
df_list_eng <- split(df_pysentiment, 
                     ceiling(seq_along(df_pysentiment$id) / batch_size))

# Export each smaller dataframe to a separate Excel file
for (i in seq_along(df_list_eng)) {
  file_name <- paste0("part_", i, ".xlsx")
  path = paste0("./01_data_export/df_x_english_pysentimient/", file_name)
  
  openxlsx::write.xlsx(df_list_eng[[i]], path)
}


#------------------------------- Clean Workspace ------------------------------#

# List all objects in the workfrece
all_objects <- ls()

# Select objects with names that match the pattern "abc"
abc_objects <- all_objects[grep("collapse_emojis_tbl|df_pysentiment", all_objects)]

# Remove the selected objects
rm(list = abc_objects)



