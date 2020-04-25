list.files()

library(tidyverse)
library(quanteda)
library(tidytext)
library(tm)
library(caret)
library(readtext)
library(tokenizers)
library(lubridate)
library(quanteda.dictionaries)


# Load - Copus - Token ----------------------------------------------------

# Load data into R
reddit_data <- readRDS("Reddit CoronavirusUS - Final Data - April 20.rds")

# Clean the data one more time by 1. Remove observation with lower than 5 words
reddit_data <- reddit_data %>%
  filter(str_count(comments, pattern = boundary("word")) > 8) %>% # we will only use sentences with more than 10 word 
  mutate(comments = str_remove_all(comments, "^gt "), Week = isoweek(comment_day)) %>%
  select(comments, Week)

# Create a copus to load data & change the name of the doc by topic name
corpus_reddit_data <- corpus(reddit_data, text_field = c("comments")) %>%
  `docnames<-`(paste(reddit_data$Week))  # Use this code to update the name of document in the copus


# Stop Words & Base formed words

stopwords_extended <- readLines("stopwords_en.txt", encoding = "UTF-8")

lemma_data <- read.csv("baseform_en.tsv", encoding = "UTF-8")


# Token and remove symbols, numbers, lowcase, and seperator

Token_reddit_data <- corpus_reddit_data %>% 
  tokens(remove_punct = TRUE, remove_numbers = FALSE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = c("subreddit","corona", "covid", 'coronavirus')) %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "glob") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)


# Token_reddit_data <- tokens(corpus_reddit_data, remove_punct = T, remove_symbols = TRUE,
#                             remove_url = TRUE, remove_separators = FALSE, remove_numbers = TRUE) %>%
#   tokens_tolower(x = ., keep_acronyms = T) %>%
#   tokens_select(pattern = c("subreddit","corona", "covid", 'coronavirus', 'will', "just", "get", "im", "let", "may", "people", "go", "like", "don't", "need", "dont", "can", "now", "one", "go", " "), selection = 'remove') %>%
#   tokens_select(., pattern = stopwords('en'), selection = 'remove') %>%
#   tokens_wordstem()


# Generate n-grams --------------------------------------------------------

# N_gram_reddit_data <- tokens_ngrams(Token_reddit_data, n = 1:2)



# Collocations, we will this to see how tokens are occured togethe --------

Collocations_reddit_data <- textstat_collocations(Token_reddit_data, min_count = 200)


# Compound collocations ---------------------------------------------------

Token_reddit_data_2 <- tokens_compound(Token_reddit_data, Collocations_reddit_data)


# Create DTM (also remove padding empty term) ------------------------------------------

DFM_reddit_data <- Token_reddit_data %>% 
  tokens_remove("") %>%
  dfm()


# TF-IDF data frame

dfm_reddit_data_2 <- dfm_tfidf(DFM_reddit_data)

# Plot TF - IDF

dfm_reddit_data_2 <- convert(dfm_reddit_data_2, to="tripletlist") 
dfm_reddit_data_df <- data.frame(doc=dfm_reddit_data_2$document,
                        feature=dfm_reddit_data_2$feature,
                        tf_idf=dfm_reddit_data_2$frequency)

res <- dfm_reddit_data_df %>%
  mutate(doc = str_split(doc,"\\.", simplify = TRUE)[,1]) %>%
  distinct(., .keep_all = TRUE) %>%
  arrange(desc(tf_idf)) %>%
  mutate(feature=factor(feature, levels=rev(unique(feature)))) %>%
  group_by(doc) %>%
  slice(1:20) %>%
  ungroup()


ggplot(data = res, aes(x = nrow(res):1, y = tf_idf)) +
  geom_point() +
  facet_wrap(~ book, scales = "free")+
  coord_flip() +
  scale_x_continuous(breaks = nrow(res):1,
                     labels = res$feature)




# Playground --------------------------------------------------------------

# dfm_reddit_data <- dfm(Token_reddit_data)
# dfm_reddit_data <- dfm(N_gram_reddit_data)
dfm_reddit_data <- dfm(toks_comp)

# Create word frequency and group them by week
freq_weight <- textstat_frequency(dfm_reddit_data, n = 10, groups = "Week", ties_method = "first")
freq_weight <- textstat_frequency(dfm_reddit_data)
freq_weight_2 <- dfm_tfidf(dfm_reddit_data)


# Plot the frequency by week

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")

# Word cloud analysis
set.seed(132)
textplot_wordcloud(dfm_reddit_data, max_words = 200)



a <- convert(dfm_reddit_data, to = "data.frame")

topfeatures(dfm_reddit_data, 20)

Frequency_dfm <- dfm_weight(dfm_reddit_data, scheme  = "prop")
Frequency_tf_idf <- dfm_tfidf(dfm_reddit_data)



reddit_data$title[975]
summary(corpus_reddit_data) %>%
  View()

reddit_data2 <- reddit_data %>% 
  filter(str_detect(comments, "[:punct:]"))

 
str_detect(reddit_data$title[975], "\\\\")

text <- "\ hi"

str_detect(text, pattern = "\\\ hi")

set.seed(825)
library(RColorBrewer)
textplot_wordcloud(dfm_reddit_data, min_size = 1000, colors = RColorBrewer::brewer.pal(8, "Dark2"), scale = c(3, 1))


reddit_data_Corona <- kwic(Token_reddit_data_V1, pattern = c('wuhan'), window = 3)




