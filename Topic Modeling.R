library(tidyverse)
library(quanteda)
library(tidytext)
library(tm)
library(caret)
library(readtext)
library(tokenizers)
library(lubridate)
library(quanteda.dictionaries)
library(qdapRegex)
library(topicmodels)
library(textmineR)


# Load Data & Basic Clean -------------------------------------------------
Reddit_Comment <- read_csv("Reddit Comment - CoronavirusUS - April 20.csv")

Reddit_Comment_column <- map_dbl(Reddit_Comment, function(x){
  sum(is.na(x))
}) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  `names<-`(., c("Name", "Evaluate")) %>%
  filter(Evaluate < 200) %>%
  .[1] %>%
  unlist(use.names = FALSE) %>%
  as.vector()

# Clean the data by remove observation with lower than 5 words
reddit_data <- Reddit_Comment %>%
  mutate(comment_day = as_datetime(created_utc),
         retrieved_on = as_datetime(retrieved_on),
         comments = body) %>%
  filter(!str_detect(comments, "^\\[[:lower:]+\\]$")) %>%
  filter(str_count(comments, pattern = boundary("word")) > 8) %>%
  mutate(comments = rm_url(comments, clean = T)) %>%
  mutate(comments = str_remove_all(comments, "^gt "), Week = isoweek(comment_day)) %>%
  select(comments, Week)

# Load - Copus - Token ----------------------------------------------------

# Create a copus to load data & change the name of the doc by topic name
corpus_reddit_data <- corpus(reddit_data, text_field = c("comments"), unique_docnames = F) %>%
  `docnames<-`(reddit_data$Week)# Use this code to update the name of document in the copus

summary(corpus_reddit_data)
ndoc(corpus_reddit_data)

# Stop Words & Base formed words

stopwords_extended <- readLines("stopwords_en.txt", encoding = "UTF-8")

lemma_data <- read.csv("baseform_en.tsv", encoding = "UTF-8")


# Token and remove symbols, numbers, lowcase, and seperator

Token_reddit_data <- corpus_reddit_data %>% 
  tokens(remove_punct = TRUE, remove_numbers = FALSE, remove_symbols = TRUE, remove_url = TRUE, remove_separators = TRUE) %>% 
  tokens_tolower(keep_acronyms = TRUE) %>%
  tokens_remove(pattern = c("subreddit", "subredditmessagecomposetorcoronavirusus", "652000", "discussion", "thread", "subreddits")) %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "glob") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)


# Multi-word tokenization ----------------------------------

Collocations_reddit_data <- textstat_collocations(Token_reddit_data, min_count = 50)

Collocations_tokens <- tokens_compound(Token_reddit_data, Collocations_reddit_data)

# Further, after removal of function words and punctuation in dfm(), 
# we keep only the top 5% of the most frequent features (min_termfreq = 0.95) that appear in less than 10% of all documents (max_docfreq = 0.1) 
# using dfm_trim() to focus on common but distinguishing features.

Collocations_DFM <- Collocations_tokens %>% 
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile", 
           max_docfreq = 0.1, docfreq_type = "prop")

# We remove the token = O 
Collocations_DFM <- Collocations_DFM[ntoken(Collocations_DFM) > 0,]


# quanteda does not implement topic models, but you can easily access LDA() from the topicmodel package through convert()

dtm <- convert(Collocations_DFM, to = "topicmodels")


# For parameterized models such as Latent Dirichlet Allocation (LDA), the number of topics K is the most important parameter to define in advance. 
# How an optimal K should be selected depends on various factors. If K is too small, the collection is divided into a few very general semantic contexts. If K is too large, the collection is divided into too many topics of which some may overlap and others are hardly interpretable.

K <- 15
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel2 <- LDA(dtm, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))

terms(topicModel2, 10) %>%
  as.data.frame() %>%
  View()

# Topic Model Tunning -----------------------------------------------------

# Run this code could take upto 10 hours. 

# For more detailed https://rpubs.com/siri/ldatuning

library(ldatuning)
# result <- FindTopicsNumber(
#   Collocations_DFM,
#   topics = seq(from = 2, to = 15, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# FindTopicsNumber_plot(result)



# Another option to do topic tuning with Topic coherence ----------------

# https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25 

# Probabilistic coherence measures how associated words are in a topic, controlling for statistical independence. 
# For example, suppose you have a corpus of articles from the sports section of a newspaper.
# A topic with the words {sport, sports, ball, fan, athlete} would look great if you look at correlation, without correcting for independence.
# But we actually know that it’s a terrible topic because the words are so frequent in this corpus as to be meaningless.
# In other words, they are highly correlated with each other but they are statistically-independent of each other.
# For each pair of words {a,b} in the top M words in a topic, probabilistic coherence calculates P(b|a) - P(b) where {a} is more probable than {b} in the topic

### Step 1. Convert dfm to dtm object

dtm <- convert(dfm_reddit_data, to = "tm")

### Step 2. Convert dtm to MakeSparseDTM

MakeSparseDTM <- function(dtm){
  # dtm is a simple triplet matrix
  dtm.sparse <- Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, 
                                     dims=c(dtm$nrow, dtm$ncol))
  
  rownames(dtm.sparse) <- tm::Docs(dtm)
  colnames(dtm.sparse) <- tm::Terms(dtm)
  
  return(dtm.sparse)
}

dtm <- MakeSparseDTM(dtm)

### Step 3. Create vocabulary object 

tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)

vocabulary <- tf$term

dtm = dtm

### Step 4. Run the tuning model

k_list <- seq(1, 20, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines


#model tuning choosing the best model


coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")


# Topic similarity --------------------------------------------------------

# The words are in ascending order of phi-value. The higher the ranking, the more probable the word will belong to the topic. It seems like there are a couple of overlapping topics. 
# It’s up to the analyst to think if we should combine the different topics together by eyeballing or we can run a Dendogram to see which topics should be grouped together. 
# A Dendogram uses Hellinger distance(distance between 2 probability vectors) to decide if the topics are closely related. 
# For instance, the Dendogram below suggests that there are greater similarity between topic 10 and 11.

lda.similarity <- as.data.frame(topicModel2@beta) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")
par(mar = c(0, 4, 4, 2))
plot(lda.similarity,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")


# Plot the topic ----------------------------------------------------------

# beta column, which contains the probability of a word occuring in a certain topic

# gamma value for each document, how much of each topic could be found back in each of the documents. 

topicModel_plot_2 <- tidy(topicModel2, matrix = "beta")
topicModel_plot_3 <- tidy(topicModel2, matrix = "gamma")

####### Plot topic by Beta

# If we look at the data-set, we see that it is much smaller and has the topics nicely ordered with the largest beta values. 
# Specifically, we have to make sure that (seen from top to bottom), all the beta for the first topic come first, then for the second topic, etc.

topicModel_plot_2 <- topicModel_plot_2 %>% group_by(topic) %>% 
  top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)

topicModel_plot_2 %>% mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") + coord_flip()


# Plot topic in document --------------------------------------------------

topicModel_plot_3 <- topicModel_plot_3 %>% group_by(document) %>% 
  top_n(10, gamma) %>% ungroup() %>% arrange(topic, -gamma)

########## Plot topic by Gamma

# We then go through similar steps to make the data-set ready for use and prepare the graph. For the graph, the only steps we do different are to “force” R to label each and every topic on the axis (as otherwise it will treat it as a continuous variable and come up with useless values such as 7.5), and to give it a different look (using the theme_classic() command):

British_lda10_toptopics %>% mutate(term = reorder(topic, gamma)) %>% 
  ggplot(aes(topic, gamma, fill = factor(topic))) + geom_col(show.legend = FALSE) + 
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 
                                10)) + facet_wrap(~document, scales = "free") + coord_flip() + 
  theme_classic()

summary(model$coherence)


# Topic distributions within the documents --------------------------------

exampleIds <- c(2, 100, 200)
cat(sotu_corpus[exampleIds[1]])
cat(sotu_corpus[exampleIds[2]])
cat(sotu_corpus[exampleIds[3]])







# Save the topic ----------------------------------------------------------

# We save topic into the model for further analysis.

Collocations_DFM$topic <- topics(topicModel2)
head(topics(topicModel2), 20)

docvars(Collocations_DFM)


# Merge the topic toghether -----------------------------------------------

# We will merge topic 8 to topic 1; 3 to 12

Replace_topic <- docvars(x = Collocations_DFM, field = "topic") %>%
  str_replace_all(., "^8$", replacement = "1") %>%
  str_replace_all(., "^3$", replacement = "12")
  

docvars(x = Collocations_DFM, field = "topic") <- Replace_topic

# old <- c(1,10,11,12,13,14,15,3,4,6,7,8,9)
# new <- Replace_topic %>%
#   table() %>%
#   as.vector()
# 
# length(old)
# length(new)
# 
# Replace_topic[Replace_topic %in% old] <- new[match(Replace_topic, old, nomatch = 0)]



# Topic Sentiment ---------------------------------------------------------

# Sentiment the topic
sentiment <- dfm(Collocations_DFM, dictionary = data_dictionary_LSD2015[1:2])

# Sum of negative & positive 
sum <- rowSums(sentiment) %>%
  as.data.frame()

# Create data frame group by Week & Topic
sentiment_frame <- convert(sentiment, to = "data.frame") %>%
  mutate(Week = docvars(sentiment, "Week"),
         Topic = docvars(sentiment, "topic"), 
         Sum = sum$.) %>%
  group_by(Week, Topic) %>%
  summarise(Total = sum(Sum), 
            Negative = sum(negative),
            Positive = sum(positive))

# Calculate total document per week
Total_doc_per_week <- convert(sentiment, to = "data.frame") %>%
  mutate(Week = docvars(sentiment, "Week"),
         Topic = docvars(sentiment, "topic"),
         count = 1) %>%
  group_by(Week) %>%
  summarise(Total_Document_by_week = sum(count))

# Select Top 1 topic in each week by percentage
sentiment_frame_2 <- convert(sentiment, to = "data.frame") %>%
  mutate(Week = docvars(sentiment, "Week"),
         Topic = docvars(sentiment, "topic")) %>%
  count(Week, Topic) %>%
  arrange(desc(n)) %>%
  group_by(Week) %>%
  left_join(Total_doc_per_week, by = c("Week" = "Week")) %>%
  mutate(Doc_topic = n/Total_Document_by_week) %>%
  ungroup() %>%
  arrange(desc(Doc_topic)) %>%
  group_by(Week) %>%
  slice(1L)

# Topic by Sensitive analysis
Final <- sentiment_frame_2 %>%
  left_join(sentiment_frame, by = c("Week" = "Week", "Topic" = "Topic"))





