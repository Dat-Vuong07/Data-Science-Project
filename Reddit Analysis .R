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


# # Load data into R
# 
# # Clean the data one more time by remove observation with lower than 5 words
# reddit_data <- reddit_data %>%
#   filter(str_count(comments, pattern = boundary("word")) > 8) %>% # we will only use sentences with more than 10 word 
#   mutate(comments = str_remove_all(comments, "^gt "), Week = isoweek(comment_day)) %>%
#   select(comments, Week)

# Create a copus to load data & change the name of the doc by topic name
corpus_reddit_data <- corpus(reddit_data, text_field = c("comments"), unique_docnames = F) %>%
  `docnames<-`(reddit_data$Week)# Use this code to update the name of document in the copus

summary(corpus_reddit_data)

# Stop Words & Base formed words

stopwords_extended <- readLines("stopwords_en.txt", encoding = "UTF-8")

lemma_data <- read.csv("baseform_en.tsv", encoding = "UTF-8")


# Token and remove symbols, numbers, lowcase, and seperator

Token_reddit_data <- corpus_reddit_data %>% 
  tokens(remove_punct = TRUE, remove_numbers = FALSE, remove_symbols = TRUE, remove_url = TRUE, remove_separators = TRUE) %>% 
  tokens_tolower(keep_acronyms = TRUE) %>%
  tokens_remove(pattern = c("subreddit","corona", "covid", 'coronavirus', "subredditmessagecomposetorcoronavirusus", "652000", "discussion", "thread", "subreddits")) %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "glob") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)

# 

# Token_reddit_data <- tokens(corpus_reddit_data, remove_punct = T, remove_symbols = TRUE,
#                             remove_url = TRUE, remove_separators = FALSE, remove_numbers = TRUE) %>%
#   tokens_tolower(x = ., keep_acronyms = T) %>%
#   tokens_select(pattern = c("subreddit","corona", "covid", 'coronavirus', 'will', "just", "get", "im", "let", "may", "people", "go", "like", "don't", "need", "dont", "can", "now", "one", "go", " "), selection = 'remove') %>%
#   tokens_select(., pattern = stopwords('en'), selection = 'remove') %>%
#   tokens_wordstem()


# Generate n-grams --------------------------------------------------------

# N_gram_reddit_data <- tokens_ngrams(Token_reddit_data, n = 1:2)



# Create DTM (also remove padding empty term) ------------------------------------------

dfm_reddit_data <- Token_reddit_data %>%
  tokens_remove("") %>%
  dfm()


# 1st Analysis Words Uniqness  --------------------------------------------


dfm_reddit_data_TF.IDF <- dfm_reddit_data %>%
  dfm_group(groups = "Week") %>%
  dfm_tfidf()


# Plot TF - IDF

dfm_reddit_data_TF.IDF <- convert(dfm_reddit_data_TF.IDF, to="tripletlist") 

dfm_reddit_data_df <- data.frame(doc=dfm_reddit_data_TF.IDF$document,
                        feature=dfm_reddit_data_TF.IDF$feature,
                        tf_idf=dfm_reddit_data_TF.IDF$frequency)

res <- dfm_reddit_data_df %>%
  arrange(desc(tf_idf)) %>%
  mutate(feature=factor(feature, levels=rev(unique(feature)))) %>%
  group_by(doc) %>%
  slice(1:10) %>%
  ungroup()

ggplot(data = res, aes(x = nrow(res):1, y = tf_idf)) +
  geom_point() +
  facet_wrap(~ doc, scales = "free")+
  coord_flip() +
  scale_x_continuous(breaks = nrow(res):1,
                     labels = res$feature)


# 2nd Analysis - Multi-word tokenization ----------------------------------

Collocations_reddit_data <- textstat_collocations(Token_reddit_data, min_count = 200)

Collocations_tokens <- tokens_compound(Token_reddit_data, Collocations_reddit_data)


Collocations_DFM <- Collocations_tokens %>% 
  tokens_remove("") %>%
  dfm()

Collocations_dfm.tfidf <- Collocations_DFM %>%
  dfm_group(groups = "Week") %>%
  dfm_tfidf()
  

Collocations_dfm.tfidf <- convert(Collocations_dfm.tfidf, to="tripletlist") 

Collocations_dfm_reddit_data_df <- data.frame(doc=Collocations_dfm.tfidf$document,
                                 feature=Collocations_dfm.tfidf$feature,
                                 tf_idf=Collocations_dfm.tfidf$frequency)

res2 <- Collocations_dfm_reddit_data_df %>%
  arrange(desc(tf_idf)) %>%
  mutate(feature=factor(feature, levels=rev(unique(feature)))) %>%
  group_by(doc) %>%
  slice(1:10) %>%
  ungroup()

ggplot(data = res2, aes(x = nrow(res2):1, y = tf_idf)) +
  geom_point() +
  facet_wrap(~ doc, scales = "free")+
  coord_flip() +
  scale_x_continuous(breaks = nrow(res2):1,
                     labels = res2$feature)


# 3rd Grouping of sentiments --------------------------------------------------

# Create the sentiment word based on Dictionary & group them by week

dfmat_immig_lsd <- dfm(Token_reddit_data, dictionary = data_dictionary_LSD2015[1:2]) %>% 
  dfm_group(group = 'Week', fill = TRUE) 

# Sum of token per week 
sum <- rowSums(dfmat_immig_lsd_2) %>%
  as.data.frame()

# Create data frame to plot sentiment
chart <- convert(dfmat_immig_lsd, to = "data.frame") %>%
  mutate(sum = sum$., 
         positive_2 = positive/sum,
         negative_2 = negative/sum, 
         gap = round((positive_2- negative_2),5)) %>%
  select(Week = document, 
         positive_2,
         negative_2,
         gap) %>%
  mutate(Week = as.numeric(Week))

# Melt the data to plot
chart <- melt(chart, id = "Week")

# Plot bar chart between Possitive & Negative Sentiment
ggplot(data = chart, aes(x = Week, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_x_continuous(breaks = c(6:17)) +
  scale_colour_brewer(palette = "Set2") +
  coord_flip() 

# Plot line chart between Possitive & Negative Sentiment
ggplot(data=chart,
       aes(x = Week, y=value, colour=variable)) +
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = c(6:17)) +
  theme(legend.position = "bottom") +
  scale_colour_brewer(palette = "Set2")

# Plot comparision
ggplot(data=chart,
       aes(x = Week, y=gap)) +
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = c(6:17)) +
  scale_colour_brewer(palette = "Set2") +
  labs(y = "Gap = Possitive - Negative")


# Co-occurrence analysis --------------------------------------------------

corpus_reddit_data

corpus_sentences <- corpus_reshape(corpus_reddit_data, to = "sentences")

ndoc(corpus_sentences)
ndoc(corpus_reddit_data)


# Build a dictionary of lemmas
lemma_data <- read.csv("baseform_en.tsv", encoding = "UTF-8")

# read an extended stop word list
stopwords_extended <- readLines("stopwords_en.txt", encoding = "UTF-8")

# Preprocessing of the corpus of sentences
corpus_tokens <- corpus_sentences %>% 
  tokens(remove_punct = TRUE, remove_numbers = FALSE, remove_symbols = TRUE, remove_url = TRUE, remove_separators = TRUE) %>% 
  tokens_remove(pattern = c("subreddit", "gt_gt", "gt")) %>%
  tokens_tolower() %>% 
  tokens_replace(., pattern = lemma_data$inflected_form, replacement = lemma_data$lemma, valuetype = "glob") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)

# calculate multi-word unit candidates
sotu_collocations <- textstat_collocations(corpus_tokens, min_count = 200)

corpus_tokens <- tokens_compound(corpus_tokens, sotu_collocations)



minimumFrequency <- 10

# Create DTM, prune vocabulary and set binary values for presence/absence of types
binDTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = minimumFrequency, max_docfreq = Inf) %>% 
  dfm_weight("boolean")


# Matrix multiplication for cooccurrence counts
coocCounts <- t(binDTM) %*% binDTM

as.matrix(coocCounts[202:205, 202:205])


coocTerm <- c("coronavirus")
k <- nrow(binDTM)
ki <- sum(binDTM[, coocTerm])
kj <- colSums(binDTM)
names(kj) <- colnames(binDTM)
kij <- coocCounts[coocTerm, ]


########## MI: log(k*kij / (ki * kj) ########
mutualInformationSig <- log(k * kij / (ki * kj))
mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]

########## DICE: 2 X&Y / X + Y ##############
dicesig <- 2 * kij / (ki + kj)
dicesig <- dicesig[order(dicesig, decreasing=TRUE)]

########## Log Likelihood ###################
logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
               + (k - ki - kj + kij) * log(k - ki - kj + kij) 
               + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
               - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
logsig <- logsig[order(logsig, decreasing=T)]


# Put all significance statistics in one Data-Frame
resultOverView <- data.frame(
  names(sort(kij, decreasing=T)[1:10]), sort(kij, decreasing=T)[1:10],
  names(mutualInformationSig[1:10]), mutualInformationSig[1:10], 
  names(dicesig[1:10]), dicesig[1:10], 
  names(logsig[1:10]), logsig[1:10],
  row.names = NULL)
colnames(resultOverView) <- c("Freq-terms", "Freq", "MI-terms", "MI", "Dice-Terms", "Dice", "LL-Terms", "LL")
print(resultOverView)


# Read in the source code for the co-occurrence calculation
source("calculateCoocStatistics.R")
# Definition of a parameter for the representation of the co-occurrences of a concept
numberOfCoocs <- 15
# Determination of the term of which co-competitors are to be measured.
coocTerm <- "coronavirus"

coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")
# Display the numberOfCoocs main terms
print(coocs[1:numberOfCoocs])


resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))


# The structure of the temporary graph object is equal to that of the resultGraph
tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

# Fill the data.frame to produce the correct number of lines
tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
# Entry of the search word into the first column in all lines
tmpGraph[, 1] <- coocTerm
# Entry of the co-occurrences into the second column of the respective line
tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
# Set the significances
tmpGraph[, 3] <- coocs[1:numberOfCoocs]

# Attach the triples to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)

# Iteration over the most significant numberOfCoocs co-occurrences of the search term
for (i in 1:numberOfCoocs){
  
  # Calling up the co-occurrence calculation for term i from the search words co-occurrences
  newCoocTerm <- names(coocs)[i]
  coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")
  
  #print the co-occurrences
  coocs2[1:10]
  
  # Structure of the temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
  tmpGraph[, 1] <- newCoocTerm
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
  
  #Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
}


# Sample of some examples from resultGraph
resultGraph[sample(nrow(resultGraph), 6), ]


require(igraph)

# Set the graph and type
graphNetwork <- graph.data.frame(resultGraph, directed = F)

# Identification of all nodes with less than 2 edges
graphVs <- V(graphNetwork)[degree(graphNetwork) < 2]
# These edges are removed from the graph
graphNetwork <- delete.vertices(graphNetwork, graphVs) 

# Assign colors to edges and nodes (searchterm blue, rest orange)
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 

# Edges with a significance of at least 50% of the maximum sig- nificance in the graph are drawn in orange
halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")

# Disable edges with radius
E(graphNetwork)$curved <- 0 
# Size the nodes by their degree of networking
V(graphNetwork)$size <- log(degree(graphNetwork)) * 5

# All nodes must be assigned a standard minimum-size
V(graphNetwork)$size[V(graphNetwork)$size < 5] <- 3 

# edge thickness
E(graphNetwork)$width <- 2

# Define the frame and spacing for the plot
par(mai=c(0,0,1,0)) 

# Finaler Plot
plot(graphNetwork,              
     layout = layout.fruchterman.reingold,  # Force Directed Layout 
     main = paste(coocTerm, ' Graph'),
     vertex.label.family = "sans",
     vertex.label.cex = 0.8,
     vertex.shape = "circle",
     vertex.label.dist = 0.5,           # Labels of the nodes moved slightly
     vertex.frame.color = 'darkolivegreen',
     vertex.label.color = 'black',      # Color of node names
     vertex.label.font = 2,         # Font of node names
     vertex.label = V(graphNetwork)$name,       # node names
     vertex.label.cex = 1 # font size of node names 
)