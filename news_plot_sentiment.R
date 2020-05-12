# After running stm model

# Extract beta and gamma matrix to data frame format

news_beta <- tidy(news_stm_model, matrix = "beta")
news_gamma <- tidy(news_stm_model, matrix = "gamma", 
                   document_names = news$week) %>% 
  group_by(document, topic) %>% #calculate the total gamma per topic per week
  summarise(total_gamma = sum(gamma))

# Make a data frame of articles count per week

articles_count <- news %>%
  group_by(week) %>%
  summarise(n_articles = n())

# Get topic proportion per week and extract the top topic

names(news_gamma)[names(news_gamma) == "document"] <- "week"

news_topic_proportion <- left_join(news_gamma, articles_count, by = "week") %>% 
  mutate(topic_proportion = total_gamma / n_articles) %>% 
  arrange(desc(topic_proportion)) %>%
  group_by(week) %>% 
  slice(seq_len(1)) %>%
  ungroup() %>%
  select(-c("total_gamma", "n_articles"))

# Using HuLiu lexicon, make a data frame with the sentiment for each word  

bing <- get_sentiments("bing")
news_sc <- inner_join(news_beta, bing, by = c("term" = "word")) %>% 
  mutate(score = ifelse(sentiment == "negative", beta * (-1), beta)) %>% 
  group_by(topic, sentiment) %>% 
  summarise(sentiment_score = sum(score))

# Combine topic proportion data frame and word sentiment data frame
# Get the sentiment score of the top topic per week

news_top_topic_sentiment <- left_join(news_topic_proportion, 
                                      news_sc, by = "topic") %>% 
  mutate(topic_score = topic_proportion * sentiment_score) %>% 
  group_by(week, topic) %>% 
  summarise(polarity = sum(topic_score))

# Plot

library(ggrepel)
news_top_topic_sentiment %>% 
  ggplot(aes(x = week, y = polarity)) +
  geom_point(size = 0.8) +
  geom_path() +
  geom_text_repel(aes(label = topic),
                  nudge_x = -0.05,
                  direction = "y",
                  size = 3) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Week", y = "Polarity") + 
  scale_x_continuous(breaks = news_top_topic_sentiment$week,
                     labels = news_top_topic_sentiment$week) +
  scale_y_continuous(limits = c(-0.05, 0.03),
                     n.breaks = 10) +
  #theme(legend.position = "bottom") +
  #scale_colour_manual(values = c("#1F78B4", "#E3211C", "#33A02B")) +
  theme(panel.grid.major.x = element_blank()) #remove vertical gridlines
