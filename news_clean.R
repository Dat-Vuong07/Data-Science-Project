library(tidyverse)

news <- read.csv("news_corona.csv", stringsAsFactors = FALSE, na.strings = "")

# Change variables types
news$publish_date <- as.Date(news$publish_date)
news <- news %>% 
  filter(publish_date < "2020-04-20")
news$source <- as.factor(news$source)
glimpse(news[1,])

# Set 'between' operator
`%between%`<- function(x,rng) {x>=rng[1] & x<=rng[2]}

# Check the length of text
news_addlen <- news %>% 
  mutate(len_text = str_length(text)) %>% 
  filter(len_text %between% c(200, 10000))
summary(news_addlen)

# Function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# Function to clean text
text.clean <- function(txt) {
  txt <- trimws(txt)
  txt <- sapply(txt, fix.contractions)
  return(txt)
}

# Clean the variables
news_addlen$title <- text.clean(news_addlen$title)
news_addlen$summary <- text.clean(news_addlen$summary)
news_addlen$keywords <- text.clean(news_addlen$keywords)
news_addlen$text <- text.clean(news_addlen$text)
news_save <- news_addlen %>% 
  select(title, publish_date, source, text)

saveRDS(news_save, "news_clean.Rds")
