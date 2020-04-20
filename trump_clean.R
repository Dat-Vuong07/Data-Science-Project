library(tidyverse)

trump_ext <- read.csv("Trump.csv", stringsAsFactors = FALSE, na.strings = "")

# function to expand contractions in an English-language source
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

# fix (expand) contractions
trump_ext$text <- sapply(trump_ext$text, fix.contractions)

# Remove RT, all words starting with https
trump_ext$text <- trump_ext$text %>% 
  str_remove_all("RT") %>% 
  #str_remove_all("(?<=^|\\s)https[^\\s]+") %>% #remove everything starts with "https"
  str_remove_all("(?<=^|\\s)@realD[^\\s]+") %>% #remove everything starts with
  #str_remove_all("[^a-zA-Z0-9 ]") #remove special characters

# Remove duplicates
sum(duplicated(trump_ext$text))
trump_ext <- trump_ext[!duplicated(trump_ext$text),]

# Trim whitespace
trump_ext$text <- trimws(trump_ext$text)

count <- trump_ext %>% 
  mutate(count = str_length(text)) %>% 
  filter(count < 5)

# Save to csv
write.csv(trump_ext, "Trump_clean.csv", row.names = FALSE)

# Reload and remove rows with whitespace
trump_ext2 <- read.csv("Trump_clean.csv", stringsAsFactors = FALSE, na.strings = c ("", " ", "  "))
trump_ext2 <- na.omit(trump_ext2)
write.csv(trump_ext2, "Trump_clean.csv", row.names = FALSE)
