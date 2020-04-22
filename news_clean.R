library(tidyverse)

news <- read.csv("news_corona.csv", stringsAsFactors = FALSE, na.strings = "")

# Change variables types
news$publish_date <- as.Date(news$publish_date)
news$source <- as.factor(news$source)
glimpse(news[1,])

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
  txt <- str_remove_all(txt, "[^a-zA-Z0-9 ]")
  txt <- tolower(txt)
  return(txt)
}

# Clean the variables
news$title <- text.clean(news$title)
news$summary <- text.clean(news$summary)
news$keywords <- text.clean(news$keywords)
news$text <- text.clean(news$text)

saveRDS(news, "news_clean.Rds")
