
# Load packages and data --------------------------------------------------


pacman::p_load("tidyverse", "tidytext", "lubridate", "wordcloud2", 
               "gridExtra", "ggrepel", "radarchart", "tm",
               "quanteda", "quanteda.dictionaries", "reshape2",
               "topicmodels")
news <- readRDS("news_clean.Rds")
glimpse(news[1,])


# Function to plot a word chart ----------------------------------------------


word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic))) +
    #you want the words, not the points
    geom_point(color = "transparent", show.legend = FALSE) +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 4,
                     show.legend = FALSE) +
    facet_grid(~topic) +
    #theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          #axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    coord_flip()
}


# Data transformation -----------------------------------------------------


# Add week, weekday, unique row number
news <- news %>% 
  mutate(week = isoweek(publish_date), 
         wday = wday(publish_date, label = TRUE),
         id = row_number())

news_title <- news %>% select(-text)
news_text <- news %>% select(-title)

# Plot number of articles per week
plot_n_articles <- news %>%
  group_by(week) %>%
  summarise(n_articles = n())

plot_n_articles %>% 
  ggplot() + 
  geom_col(aes(x = week, y = n_articles, fill = -week), show.legend = FALSE)  +
  scale_x_continuous(breaks = plot_n_articles$week) +
  scale_y_continuous(limits = c(0, 2400), n.breaks = 10) +
  ggtitle("Published Articles Weekly") +
  labs(x = "Week", y = "Articles Count")

# Create corpus - token ---------------------------------------------------


# Create a copus to load data & change the name of the doc by topic name

corpus_news <- corpus(news_title, text_field = c("title"), unique_docnames = F) %>% 
  `docnames<-`(news_title$week)# Use this code to update the name of document in the copus

summary(corpus_news)
str(corpus_news)

# Stop Words & Base formed words

stopwords_extended <- readLines("stopwords_en.txt", encoding = "UTF-8")
lemma_data <- read.csv("baseform_en.tsv", encoding = "UTF-8")

# Token and remove symbols, numbers, lowcase, and seperator

token_news <- corpus_news %>% 
  tokens(remove_punct = TRUE, remove_numbers = FALSE, remove_symbols = TRUE, 
         remove_url = TRUE, remove_separators = TRUE) %>% 
  tokens_tolower(keep_acronyms = TRUE) %>%
  tokens_remove(pattern = c("coronavirus", "video", "amid", "updates", 
                            "live", "briefing", "due")) %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "glob") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)
summary(token_news)
str(token_news)
head(token_news, 10)


# 1st - Key terms extraction ----------------------------------


# Extract collocations with 2 words per term

collo_news <- textstat_collocations(token_news, size = 2, min_count = 10)

collo_tokens <- tokens_compound(token_news, collo_news, join = TRUE)
  # leave join = TRUE to join overlapping compounds into a single compound
  # this facilitate the appearance of more than 2-grams, eg joining public_health and health_emergency into public_health_emergency

head(collo_tokens, 10)

# Put into document feature matrix (or document term matrix) and remove padding

news_dfm <- collo_tokens %>% 
  tokens_remove("") %>%
  dfm()
dim(news_dfm)

# Group by week and calculate tf-idf

news_dfm_tfidf <- news_dfm %>%
  dfm_group(groups = "week") %>%
  dfm_tfidf()

# Convert dfm to a list of 3 and put into a data frame

news_dfm_tfidf <- convert(news_dfm_tfidf, to="tripletlist") 

news_df <- data.frame(week = news_dfm_tfidf$document,
                      word = news_dfm_tfidf$feature,
                      tf_idf = news_dfm_tfidf$frequency)

news_df$week <- as.numeric(as.character(news_df$week))

# Transform and plot

top_news_tfidf <- news_df %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = unique(word))) %>%
  group_by(week) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(week, tf_idf) %>%
  mutate(row = row_number())

top_news_tfidf %>%
  ggplot(aes(x = row, tf_idf, fill = -week)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Top Collocations in Title by TF-IDF by Week") +
  facet_wrap(~week, ncol = 4, scales = "free") +
  scale_x_continuous( 
    breaks = top_news_tfidf$row,
    labels = top_news_tfidf$word) +
  coord_flip()


# 2nd - Sentiment analysis ------------------------------------------------------


lsd15 <- data_dictionary_LSD2015

# Create the sentiment word based on Dictionary & group them by week

dfm_news_lsd <- dfm(token_news, dictionary = lsd15[1:2]) %>% 
  dfm_group(group = 'week', fill = TRUE) 

head(dfm_news_lsd, 10)

# Sum of token per week 

rowSums(dfm_news_lsd)

# Create data frame to plot sentiment

lsd_chart <- convert(dfm_news_lsd, to = "data.frame")%>%
  mutate(sum = positive + negative, 
         pos_percent = positive/sum*100,
         neg_percent = negative/sum*100, 
         polarity = round((pos_percent - neg_percent),3)) %>%
  select(week = document, 
         pos_percent,
         neg_percent,
         polarity)

lsd_chart$week <- as.numeric(as.character(lsd_chart$week))

# Melt the data to plot

chart <- melt(lsd_chart, id = "week") %>% 
  arrange(week)


# Plot bar chart between Possitive & Negative Sentiment

ggplot(data = chart, aes(x = week, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = NULL, y = "Frequency") + 
  scale_x_continuous(  # This handles replacement of row 
    breaks = chart$week, # notice need to reuse data frame
    labels = chart$week)

# Plot line chart between Possitive & Negative Sentiment

ggplot(data=chart, aes(x = week, y = value, colour = variable)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = NULL, y = "Frequency") + 
  scale_x_continuous(  # This handles replacement of row 
    breaks = chart$week, # notice need to reuse data frame
    labels = chart$week) +
  theme(legend.position = "bottom")
  #scale_colour_brewer(palette = 1)


# 3rd - Topic modelling -------------------------------------------------------


# Create DTM, but remove terms which occur in less than 0.5% of all documents
DTM <- collo_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.005, max_docfreq = Inf, docfreq_type = "prop")
dim(DTM)

# Due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
news_title <- news_title[sel_idx, ]

# Run LDA model

K <- 8 # number of topics

set.seed(123) # set random number generator seed

# Compute the LDA model, inference via 500 iterations of Gibbs sampling 
  # the progress is reported every 50 iterations
topicModel <- LDA(DTM, K, method="Gibbs", control = list(iter = 500, verbose = 50))

# have a look a some of the results (posterior distributions)

tmResult <- posterior(topicModel)

# Change to tibble format to visualize

top_terms <- as_tibble(terms(topicModel, 10)) %>%
  pivot_longer(cols = seq_len(K), names_to = "topic") %>% 
  mutate(row = row_number())

#create a title to pass to word_chart

lda_title <- paste("LDA Top Terms for", K, "Topics")

word_chart(top_terms, top_terms$value, lda_title)
