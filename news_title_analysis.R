pacman::p_load("tidyverse", "tidytext", "lubridate", "wordcloud2", "gridExtra", "ggrepel")
news <- readRDS("news_clean.Rds")
glimpse(news[1,])
news_addtime <- news %>% 
  mutate(week = isoweek(publish_date), wday = wday(publish_date, label = TRUE)) %>% 
  filter(week < 17)
news_addtime <- news_addtime %>% mutate(id = row_number()) #add unique row numbers

# Set 'between' operator
`%between%`<- function(x,rng) {x>=rng[1] & x<=rng[2]}

# Add fortnight time (2 weeks)
news_addtime <- news_addtime %>%
  mutate(
    fortnight = ifelse(publish_date %between% c("2020-01-16", "2020-01-31"), "Late Jan", 
                  ifelse(publish_date %between% c("2020-02-01", "2020-02-14"), "Early Feb",
                         ifelse(publish_date %between% c("2020-02-15", "2020-02-29"), "Late Feb",
                                ifelse(publish_date %between% c("2020-03-01", "2020-03-15"), "Early Mar",
                                       ifelse(publish_date %between% c("2020-03-16", "2020-03-31"), "Late Mar",
                                              ifelse(publish_date %between% c("2020-04-01", "2020-04-15"), "Early Apr",
                                                     "Late Apr")))))))

news_addtime$fortnight <- factor(news_addtime$fortnight, 
                                   levels = c("Late Jan", "Early Feb", "Late Feb",
                                              "Early Mar", "Late Mar", "Early Apr",
                                              "Late Apr"))

# Count number of articles per week
plot_n_articles <- news_addtime %>%
  group_by(week) %>%
  summarise(n_articles = n())

# Plot number of articles per week
plot_n_articles %>% 
  ggplot() + 
  geom_col(aes(x = week, y = n_articles, fill = -week), show.legend = FALSE)  +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(limits = c(0, 2400), n.breaks = 10) +
  ggtitle("Published Articles Weekly") +
  labs(x = "Week", y = "Articles Count")

# Specify some general words
undesirable_words <- c("coronavirus", "video", "amid", "updates", 
                       "live", "briefing", "due")

# Create tokens for title
news_unnest_title <- news_addtime %>%
  select(-c("summary", "keywords", "text")) %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words) %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 2)


# Plot top 10 most frequent words
news_unnest_title %>%
  distinct() %>% # multiple appearance in a title only count as 1
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  arrange(n) %>% 
  mutate(word = factor(word, levels = word)) %>% # keep the sorted order
  ggplot() +
  geom_col(aes(word, n, fill = word), show.legend = FALSE) +
  xlab("") + 
  ylab("Word Count") +
  ggtitle("Most Frequently Used Words in Articles Title") +
  coord_flip()

# Plot word cloud
news_title_count <- news_unnest_title %>% 
  #filter(week == c(3:4)) %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 50)
wordcloud2(news_title_count, size = 0.6)

# Plot most frequent words each fortnight
top_news_title <- news_unnest_title %>%
  distinct() %>% 
  count(fortnight, word, sort = TRUE) %>% 
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = unique(word))) %>%
  group_by(fortnight) %>% 
  slice(seq_len(10)) %>% # keep top 10 for each group
  ungroup() %>%
  arrange(fortnight, n) %>%
  mutate(row = row_number()) # add index for plotting

top_news_title %>%
  ggplot(aes(x = row, n, fill = fortnight)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Frequency") + 
  ggtitle("Top 10 Most Popular Words in Title by Fortnight") +
  facet_wrap(~fortnight, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = top_news_title$row, # notice need to reuse data frame
    labels = top_news_title$word) +
  coord_flip()

# Using tf-idf analysis
title_tfidf <- news_unnest_title %>%
  distinct() %>%
  count(fortnight, word, sort = TRUE) %>%
  bind_tf_idf(word, fortnight, n)

top_title_tfidf <- title_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = unique(word))) %>%
  group_by(fortnight) %>% 
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(fortnight, tf_idf) %>%
  mutate(row = row_number())

# Plot the top 10 by tf-idf criterion
top_title_tfidf %>%
  ggplot(aes(x = row, tf_idf, fill = fortnight)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words in Title using TF-IDF by Fortnight") +
  facet_wrap(~fortnight, ncol = 3, scales = "free") +
  scale_x_continuous( 
    breaks = top_title_tfidf$row,
    labels = top_title_tfidf$word) +
  coord_flip()

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

news_afinn <- news_unnest_title %>%
  inner_join(afinn)

news_bing <- news_unnest_title %>% 
  inner_join(bing)

news_nrc <- news_unnest_title %>% 
  inner_join(nrc) #%>% 
  #filter(!sentiment %in% c("positive", "negative") )
  
news_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  #scale_y_continuous(limits = c(0, 15000)) + #Hard code the axis limit
  ggtitle("Title NRC Sentiment") +
  coord_flip()

fortnight_nrc <- news_nrc %>%
  distinct() %>% 
  count(fortnight, sentiment, sort = TRUE) %>% 
  arrange(desc(n)) %>%
  mutate(sentiment = factor(sentiment, levels = unique(sentiment))) %>%
  group_by(fortnight) %>% 
  #slice(seq_len(10)) %>% # keep top 10 for each group
  ungroup() %>%
  arrange(fortnight, n) %>%
  mutate(row = row_number()) # add index for plotting

fortnight_nrc %>%
  ggplot(aes(x = row, n, fill = fortnight)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Frequency") + 
  ggtitle("Title NRC Sentiment by Fortnight") +
  facet_wrap(~fortnight, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = fortnight_nrc$row, # notice need to reuse data frame
    labels = fortnight_nrc$sentiment) +
  coord_flip()

fortnight_bing <- news_bing %>%
  distinct() %>% 
  count(fortnight, sentiment, sort = TRUE) %>% 
  arrange(desc(n)) %>%
  mutate(sentiment = factor(sentiment, levels = unique(sentiment))) %>%
  group_by(fortnight) %>% 
  #slice(seq_len(10)) %>% # keep top 10 for each group
  ungroup() %>%
  arrange(fortnight, n) %>%
  mutate(row = row_number()) # add index for plotting

fortnight_bing %>%
  ggplot(aes(x = row, n, fill = fortnight)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Frequency") + 
  ggtitle("Title Bing Sentiment by Fortnight") +
  facet_wrap(~fortnight, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = fortnight_bing$row, # notice need to reuse data frame
    labels = fortnight_bing$sentiment) +
  coord_flip()

title_polarity_chart <- news_bing %>%
  count(sentiment, fortnight) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)

#Polarity by chart
plot1 <- title_polarity_chart %>%
  ggplot(aes(fortnight, polarity, fill = fortnight)) +
  geom_col() +
  #scale_fill_manual(values = my_colors[3:5]) +
  geom_hline(yintercept = 0, color = "red") +
  #theme_lyrics() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Polarity By Fortnight")

#Percent positive by chart
plot2 <- title_polarity_chart %>%
  ggplot(aes(fortnight, percent_positive, fill = fortnight)) +
  geom_col() +
  #scale_fill_manual(values = c(my_colors[3:5])) +
  geom_hline(yintercept = 0, color = "red") +
  #theme_lyrics() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Percent Positive By Fortnight")

grid.arrange(plot1, plot2, ncol = 2)

# Most used word in a sentiment
plot_earlymar <- news_nrc %>%
  filter(fortnight == "Early Mar") %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(8)) %>% #consider top_n() from dplyr also
  ungroup()

plot_earlymar %>%
  #Set `y = 1` to just plot one variable and use word as the label
  ggplot(aes(word, 1, label = word, fill = sentiment)) +
  #Make sure the labels don't overlap by using geom_label_repel in place of geom_label
  geom_label_repel(show.legend = FALSE) +
  facet_grid(~sentiment) +
  #theme_lyrics() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        #axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA)) +
        #strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Early March NRC Sentiment") +
  coord_flip()
