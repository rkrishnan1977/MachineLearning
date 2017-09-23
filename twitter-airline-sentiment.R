## Case Study - twitter-airline-sentiment

library(dplyr)
library(tidytext)
library(ggplot2)

setwd("E:\\Ramesh\\DataScience\\Project\\twitter-airline-sentiment")
twitterData <- read.csv("Tweets.csv",header=TRUE,stringsAsFactors=FALSE)

#Explore the data
str(twitterData)
View(twitterData)
colnames(twitterData)

#twitterData %>% filter(retweet_count!=0)

twitterDataSelectedColumn <- twitterData %>%
                              select(tweet_id,airline_sentiment,negativereason,airline,name,text)


###########################################################
##

## bing lexicon  - out of 121895 rows/words , sentiments found only for 11798 (only 9%)
TwitterTokens <- twitterDataSelectedColumn %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"))

## afinn lexicon  - out of 121895 rows/words , sentiments found only for 16102 (only 13%)
TwitterTokens <- twitterDataSelectedColumn %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("afinn"))

## nrc lexicon  - out of 121895 rows/words , sentiments found only for 51140 (only 41%)
TwitterTokens <- twitterDataSelectedColumn %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("nrc"))


## loughran lexicon  - out of 121895 rows/words , sentiments found only for 7697 (only 6%)
TwitterTokens <- twitterDataSelectedColumn %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("loughran"))

## Type of sentiments in nrc
get_sentiments("nrc") %>%
  count(sentiment, sort = TRUE)

get_sentiments("nrc") %>%
  filter(word=="art")

## Type of sentiments in afinn
get_sentiments("afinn") %>%
  count(score, sort = TRUE)

get_sentiments("afinn") %>%
  count(word, sort = TRUE) %>%
  filter(n > 1)

## Type of sentiments in nrc
get_sentiments("nrc") %>%
  count(sentiment, sort = TRUE)

get_sentiments("loughran") %>%
  count(sentiment, sort = TRUE)

## To explore type of sentiment found in Twitter data set using nrc sentiment lexicon 
TwitterTokens <- twitterDataSelectedColumn %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, sort = TRUE)

## Number of word/Twit after applying Stop word and Sentiment
TwitterTokens <- twitterDataSelectedColumn %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("nrc")) %>%
  count(tweet_id,sort=FALSE)


## Words not found in sentiment database
TwitterTokens <- twitterDataSelectedColumn %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  left_join(get_sentiments("bing")) %>%
  #  filter(!(complete.cases(sentiment)))
  filter(!is.na(sentiment))

###########################################################

#Using afinn sentiment as there are duplicate words in Sentiment lexicon
TwitterTokens <- twitterDataSelectedColumn %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("afinn"))

TwitterTokensWithScore <- TwitterTokens %>%
  group_by(tweet_id) %>%
  summarise(score=sum(score)) %>%
  mutate(Calculated_Sentiment=ifelse(score > 0,"positive",ifelse(score < 0,"negative","neutral")))
  

TwitterTokensFinal <- TwitterTokens %>%
  inner_join(TwitterTokensWithScore,by = "tweet_id") %>%
  distinct(tweet_id,.keep_all = TRUE)

colnames(TwitterTokensFinal)

## Mismatch in Airline Sentiment and calculated Sentiment with Twit
Test <- TwitterTokensFinal %>%
  filter(airline_sentiment != Calculated_Sentiment) %>%
  inner_join(twitterDataSelectedColumn,by = "tweet_id")

## Negative Reason based on Airline Sentiment
TwitterTokensFinal  %>%
  filter(airline_sentiment == "negative") %>%
  count(negativereason,sort=TRUE) %>%
  mutate(negativereason = reorder(negativereason, n)) %>%
  ggplot(aes(negativereason, n)) +
  geom_col() +
  coord_flip()


## Negative Reason based on Airline Sentiment
TwitterTokensFinal  %>%
  filter(Calculated_Sentiment == "negative") %>%
  count(negativereason,sort=TRUE) %>%
  mutate(negativereason = reorder(negativereason, n)) %>%
  ggplot(aes(negativereason, n)) +
  geom_col() +
  coord_flip()



