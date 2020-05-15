library(tidyverse)
library(lubridate)

tweets <- read_csv("twitter_coronavirus.csv")

# Transform the date column

tweets_data <- tweets %>% 
  separate(created_at, c("wday", "month", "day", "time", "plus", "year"), 
           sep = " ") %>% 
  mutate(date = paste(month, day, year, sep = " ")) %>% 
  select(c("text", "date"))

tweets_data$date <- as.Date(tweets_data$date, format = "%b %d %Y")
tweets_data <- tweets_data %>% 
  mutate(week = isoweek(date)) %>% 
  filter(week > 2)

# Clean hashtags, links and special characters

tweets_data$text <- tweets_data$text %>% 
  str_replace_all("#", " ") %>%
  str_remove_all("(?<=^|\\s)http[^\\s]+") %>% 
  str_remove_all("[^a-zA-Z0-9 ]")
