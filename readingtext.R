######################################
## Text as Data Workshop
## Author: Lily Fesler
## Adapted from Chris Bail's materials (https://compsocialscience.github.io/summer-institute/2018/teaching-learning-materials)
## Modified: 5/26/19
######################################

rm(list = ls())

### Install Packages (only need to do this once)
install.packages("tm")
install.packages("tidytext")
install.packages("dplyr")
install.packages("SnowballC")
install.packages("ggplot2")
install.packages("SnowballC")
install.packages("stringr")

### Load packages (need to do this each time you start a new R session)
library(tm) # text mining (option 1)
library(tidytext) # tidytext (option 2)
library(dplyr)
library(SnowballC) # allows us to stem in tidytext
library(ggplot2) # for pretty graphs
library(stringr) # string functions for dictionaries
  
  
# Read in data
load(url("https://cbail.github.io/Trump_Tweets.Rdata"))
head(trumptweets$text)


# STEP 1: Preprocess data

# OPTION A: using text mining package
# tm vignette: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
trump_corpus <- VCorpus(VectorSource(as.vector(trumptweets$text))) 
trump_corpus
inspect(trump_corpus[[1]])

trump_corpus <- tm_map(trump_corpus, removeWords, stopwords("english"))
inspect(trump_corpus[[1]])
stopwords("english")
trump_corpus <- tm_map(trump_corpus, content_transformer(removePunctuation))
inspect(trump_corpus[[1]])
trump_corpus <- tm_map(trump_corpus, content_transformer(removeNumbers))
trump_corpus <- tm_map(trump_corpus, content_transformer(tolower)) 
trump_corpus <- tm_map(trump_corpus, content_transformer(stripWhitespace))
trump_corpus <- tm_map(trump_corpus, content_transformer(stemDocument), language = "english")
inspect(trump_corpus[[1]])
trump_corpus

# OPTION B: using tidytext
trumptweets
tidy_trump_tweets<- trumptweets %>%
  select(created_at,text,status_id) %>%
  unnest_tokens("word", text)
tidy_trump_tweets

# remove stop words
data("stop_words")
stop_words
tidy_trump_tweets<-tidy_trump_tweets %>%
  anti_join(stop_words, by="word")
tidy_trump_tweets<-tidy_trump_tweets[-grep("\\b\\d+\\b", tidy_trump_tweets$word),]
# remove whitespace
tidy_trump_tweets$word <- gsub("\\s+","",tidy_trump_tweets$word)
# stemming
tidy_trump_tweets<-tidy_trump_tweets %>%
  mutate_at("word", funs(wordStem((.), language="en")))
# remove additional characters specific to these data
tidy_trump_tweets<-
  tidy_trump_tweets[-grep("https|t.co|amp|rt",
                          tidy_trump_tweets$word),]
tidy_trump_tweets



# STEP 2: Convert to DTM

# using text mining package
trump_DTM <- DocumentTermMatrix(trump_corpus, control = list(wordLengths = c(2, Inf)))
inspect(trump_DTM)


# using tidytext
tidy_trump_DTM <-
  tidy_trump_tweets %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, status_id)
tidy_trump_DTM



# EXPLORE: WORD COUNTING

#select only top words
trump_tweet_top_words<-
  tidy_trump_tweets %>%
  anti_join(stop_words, by="word") %>%
  count(word) %>%
  arrange(desc(n))
trump_tweet_top_words
top_20<-trump_tweet_top_words[1:20,]

#create factor variable to sort by frequency
trump_tweet_top_words$word <- factor(trump_tweet_top_words$word, levels = trump_tweet_top_words$word[order(trump_tweet_top_words$n,decreasing=TRUE)])
trump_tweet_top_words

ggplot(top_20, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Trump's Tweets")+
  xlab("")+
  guides(fill=FALSE)


## TF-IDF: unusual words

tidy_trump_tfidf<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text) %>%
  anti_join(stop_words, by="word") %>%
  count(word, created_at) %>%
  bind_tf_idf(word, created_at, n)
tidy_trump_tfidf

top_tfidf<-tidy_trump_tfidf %>%
  arrange(desc(tf_idf))
top_tfidf



### DICTIONARIES AND SENTIMENT ANALYSIS TOOLS

# you can choose to create your own dictionary (list of terms),
# like this economic_dictionary
economic_dictionary<-c("economy","unemployment","trade","tariffs")
economic_tweets<-trumptweets[str_detect(trumptweets$text, economic_dictionary),]
head(economic_tweets$text)

# or you can choose to use a preexisting dictionary
get_sentiments("bing")
trump_tweet_sentiment <- tidy_trump_tweets %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(created_at, sentiment) 
head(trump_tweet_sentiment)


# plot negative sentiment by time
tidy_trump_tweets$date<-as.Date(tidy_trump_tweets$created_at, 
                                format="%Y-%m-%d %x")
trump_sentiment_plot <-
  tidy_trump_tweets %>%
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(sentiment=="negative") %>%
  count(date, sentiment)

ggplot(trump_sentiment_plot, aes(x=date, y=n))+
  geom_line(color="red")+
  theme_minimal()+
  ylab("Frequency of Negative Words in Trump's Tweets")+
  xlab("Date")

