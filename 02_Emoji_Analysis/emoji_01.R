#------------------------------------------------------------------------------
# Topic: Explore Twitter Data
# Created by: Isaac Bravo
# Source: https://twitter.com; https://github.com/PursuitOfDataScience/tidyEmoji
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#

source("setup.R")

#------------------------------- Import Data ----------------------------------#

data_reply_full_complete <- readRDS("C:/Users/isaac/OneDrive/Escritorio/Twitter_data/data_replies/data_replies/data_reply_full_complete.RDS") 

tweet_df <- data_reply_full_complete %>%
  emoji_extract_unnest(text)

tweet_df_categories <- tweet_df %>%
  emoji_categorize(.emoji_unicode)

a <- tweet_df_categories %>% 
  distinct(.emoji_unicode, .keep_all = TRUE)


tweet_df %>%
  group_by(.emoji_count) %>%
  summarize(n = n()) %>%
  ggplot(aes(.emoji_count, n)) +
  geom_col() +
  # scale_x_continuous(breaks = seq(1,15)) +
  ggtitle("How many Emoji does each Emoji Tweet have?")

top_20_emojis <- data_reply_full_complete %>%
  top_n_emojis(text)

top_20_emojis %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col()

top_20_emojis %>%
  mutate(emoji_name = stringr::str_replace_all(emoji_name, "_", " "),
         emoji_name = forcats::fct_reorder(emoji_name, n)) %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col() +
  labs(x = "# of Emoji",
       y = "Emoji name",
       fill = "Emoji category",
       title = "The 20 most popular Emojis")

top_20_emojis %>%
  mutate(emoji_name = stringr::str_replace_all(emoji_name, "_", " "),
         emoji_name = forcats::fct_reorder(emoji_name, n)) %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col() +
  geom_text(aes(label = unicode), hjust = 0.1) +
  labs(x = "# of Emoji",
       y = "Emoji name",
       fill = "Emoji category",
       title = "The 20 most popular Emojis")

tweet_df_categories %>%
  count(.emoji_category) %>%
  filter(n > 20) %>%
  mutate(.emoji_category = forcats::fct_reorder(.emoji_category, n)) %>%
  ggplot(aes(n, .emoji_category)) +
  geom_col()

library(emo)


df_emojis_grouped_by_region <- data_reply_full_complete %>%
  select(conversation_id, id, text, region) %>% 
  group_by(region) %>%
  mutate(emoji = emo::ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(region, emoji, sort = TRUE)

df_emojis_list <- data_reply_full_complete %>%
  select(conversation_id, id, text, region) %>% 
  mutate(emoji = emo::ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE)

emoji_list <- df_emojis_list %>% select(emoji)

openxlsx::write.xlsx(emoji_list, "emoji_list.xlsx")

emojis <- read_csv("data/emojis.csv") %>% select(-1) %>% 
  mutate(
    # Extract the sentiment value
    top_sent = stringr::str_extract(sentiment, "(?<=output=)\\w+"),
    # Extract the probabilities as a string
    prob_sent = stringr::str_extract(sentiment, "(?<=probas=)\\{.+\\}"),
    # Remove curly braces from probabilities string
    prob_sent = stringr::str_remove_all(prob_sent, "[{}]"),
    NEG = stringr::str_extract(prob_sent, "(?<=NEG: )\\d+\\.\\d+"),
    POS = stringr::str_extract(prob_sent, "(?<=POS: )\\d+\\.\\d+"),
    NEU = stringr::str_extract(prob_sent, "(?<=NEU: )\\d+\\.\\d+")
  )

emoji_list <- emoji_list %>% left_join(emojis, by = "emoji")
  
  

emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}

link_to_img <- function(x, size = 25) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

library(rvest)

top_emoji <- df_emojis_grouped %>%
  slice(1:20) %>%
  mutate(url = map_chr(emoji, slowly(~emoji_to_link(.x), rate_delay(1))),
         label = link_to_img(url))


openxlsx::write.xlsx(top_emoji, "top_emoji.xlsx")

library(readr)


top_emoji_list <- cbind(top_emoji, emojis %>% 
                     mutate(match = emoji) %>% 
                     select(-emoji, -prob_sent, -sentiment))


top_emoji %>%
  ggplot(aes(emoji, n, label = label)) +
  geom_richtext(aes(y = n), fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  theme_minimal() +
  facet_wrap(~region, scales = "free")



top_emoji %>% 
  group_by(region, top_sent) %>% 
  summarize(total = sum(n)) %>% 
  view




offset <- max(top_emoji$n) / 20


top_emoji %>%
  ggplot(aes(fct_reorder(emoji, n, .desc = TRUE), n, label = label)) +
  geom_col() +
  geom_richtext(aes(y = n + offset), fill = NA, label.color = NA,
                label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = NULL) +
  theme_minimal()



top_emoji %>%
  ggplot(aes(fct_reorder(label, n, .desc = TRUE), n)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_markdown()) +
  labs(x = NULL)


mean_emoji_color <- function(x) {
  data <- png::readPNG(RCurl::getURLContent(x))
  color_freq <- names(sort(table(rgb(data[,,1], data[,,2], data[,,3])), 
                           decreasing = TRUE))
  setdiff(color_freq, c("#FFFFFF", "#000000"))[1]
}


plot_data <- top_emoji %>%
  mutate(color = map_chr(url, slowly(~mean_emoji_color(.x), rate_delay(1))))

plot_data %>%
  ggplot(aes(fct_reorder(label, n, .desc = TRUE), 
             color = color, 
             fill = unclass(prismatic::clr_lighten(color, 0.4)), n)) +
  geom_col() +
  scale_fill_identity() +
  scale_color_identity() +
  theme_minimal() +
  theme(axis.text.x = element_markdown()) +
  labs(x = NULL, y = "Count",
       title = "Emojis used in (small sample) of 'happy' tweets",
       subtitle = "Displayed in ggplot2!!!",
       caption = "@Emil_Hvitfeldt")


emoji <- readr::read_csv("https://raw.githubusercontent.com/EmilHvitfeldt/Emoji-table/master/emoji.csv")



paste_string <- function(x){
  data <- gsub(" ", "_", x)
  return(data)
}

x <- lexicon::emojis_sentiment %>% 
  mutate(names_up = sapply(name, paste_string))



top_20_emojis %>% 
  left_join(x, by = c("emoji_name" = "names_up")) %>% 
  View()




