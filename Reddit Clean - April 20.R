list.files()
setwd()
# Reddit Topic ------------------------------------------------------------

library(readxl)
library(readr)
library(tidyverse)
library(lubridate)

Reddit_Topic <- read_csv("Reddit - CoronavirusUS - April 20.csv")

# Reddit_Topic <- read_csv("Reddit Submission - Coronavirus - April 20.csv")


Reddit_Topic_column <- map_dbl(Reddit_Topic, function(x){
  sum(is.na(x))
}) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  `names<-`(., c("Name", "Evaluate")) %>%
  filter(Evaluate < 14000) %>%
  .[1] %>%
  unlist(use.names = FALSE) %>%
  as.vector()

# Save comments one more time
Reddit_Topic2 <- Reddit_Topic %>%
  mutate(created_utc = as_datetime(created_utc),
         retrieved_on = as_datetime(retrieved_on))  %>%
  select(Reddit_Topic_column, -all_awardings, -author_flair_richtext, -awarders, -gildings, -total_awards_received, -link_flair_richtext, 
         -author_flair_type, -author_fullname, -link_flair_text_color, -link_flair_type, -full_link, -permalink, link_flair_text) %>%
  distinct(id, created_utc, retrieved_on, title, .keep_all = T) %>%
  select_if(function(a) {is.numeric(a)|is.character(a)})

which(Reddit_Topic == "Europe", arr.ind = T)

# Reddit Comments ---------------------------------------------------------

Reddit_Comment <- read_csv("Reddit Comment - CoronavirusUS - April 20.csv")

# Reddit_Comment <- read_csv("Reddit Comment - Coronavirus - April 20.csv")

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

# Save comments one more time
Reddit_Comment <- Reddit_Comment %>%
  mutate(created_utc = as_datetime(created_utc),
         retrieved_on = as_datetime(retrieved_on)) %>%
  select(Reddit_Comment_column, -all_awardings, -author_flair_richtext, -awarders, -gildings, -total_awards_received) %>%
  mutate(id_2 = str_sub(link_id, start = 4, end = 9)) %>%
  select(id_2, comment_author = author, comments = body, comment_day = created_utc, is_submitter,  retrieved_comments_on = retrieved_on, send_replies) %>%
  filter(!str_detect(comments, "^\\[[:lower:]+\\]$")) %>%
  mutate(comments = rm_url(comments, clean = T)) %>%
  mutate(comments = str_replace_all(comments, "[^[:alnum:]|^[:blank:]]", "")) %>%
  filter(str_count(comments, pattern = boundary("word")) > 2)


# Combine  ----------------------------------------------------------------

Reddit_Comment$link_id %>%
  str_detect(., "^[:alnum:]{2}_[:alnum:]{6}$") %>%
  sum()

Reddit_Comment_Final <-  Reddit_Comment %>%
  left_join(., Reddit_Topic, by = c("id_2" = "id"))


names(Reddit_Comment_Final)

Reddit_Comment_Final$link_flair_text

write_csv2(Reddit_Comment_Final, "Reddit CoronavirusUS - Final Data - April 20")


